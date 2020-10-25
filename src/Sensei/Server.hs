{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.Server where

import qualified Control.Exception.Safe as Exc
import Control.Monad.Trans
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as LBS
import Data.Function (on, (&))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Time
import Sensei.API
import Servant
import System.IO

traceS :: FilePath -> Trace -> Handler ()
traceS file trace = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode trace <> "\n"
    hFlush out

flowS :: FilePath -> FlowType -> FlowState -> Handler ()
flowS file flowTyp flow = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode (Flow flowTyp flow) <> "\n"
    hFlush out

-- | Read all the views for a given `usr`
--  It seems we could simply not filter on user name at this stage but
--  actually it does not make sense to build a streamm of `FlowView` for
--  different users as each `Flow` is supposed to be contiguous to the
--  previous one and `flowView` uses next `Flow`'s start time as end time
--  for previous `Flow`.
readViews :: FilePath -> String -> IO [FlowView]
readViews file usr = withBinaryFile file ReadMode $ loop []
  where
    loop :: [FlowView] -> Handle -> IO [FlowView]
    loop acc hdl = do
      res <- Exc.try $ LT.hGetLine hdl
      case res of
        Left (_ex :: Exc.IOException) -> pure (reverse acc)
        Right ln ->
          case eitherDecode (LT.encodeUtf8 ln) of
            Left _err -> loop acc hdl
            Right flow -> loop (flowView flow usr acc) hdl

queryFlowDayS :: FilePath -> [Char] -> Day -> Handler [FlowView]
queryFlowDayS file usr day = do
  views <- liftIO $ readViews file usr
  pure $ filter (sameDayThan day) views

-- | "pipe" operator common in other languages
-- this is basically `flip apply` which is defined as `&` in
-- the standard library
(|>) :: a -> (a -> b) -> b
(|>) = (&)

queryFlowDaySummaryS :: FilePath -> [Char] -> Day -> Handler [(FlowType, NominalDiffTime)]
queryFlowDaySummaryS file usr day = do
  views <- liftIO $ readViews file usr
  pure $
    views
      |> filter (sameDayThan day)
      |> List.sortBy (compare `on` flowType)
      |> NE.groupBy ((==) `on` flowType)
      |> fmap summarize
  where
    summarize :: NE.NonEmpty FlowView -> (FlowType, NominalDiffTime)
    summarize flows@(f NE.:| _) = (flowType f, sum $ fmap duration flows)

queryFlowS :: FilePath -> [Char] -> Handler [FlowView]
queryFlowS file usr =
  liftIO $ readViews file usr

flowView :: Flow -> String -> [FlowView] -> [FlowView]
flowView Flow {..} usr views =
  if _flowUser _flowState == usr
    then
      let view = FlowView st st 0 _flowType
          st = _flowStart _flowState
       in case views of
            (v : vs) -> view : v {flowEnd = st, duration = diffUTCTime st (flowStart v)} : vs
            [] -> [view]
    else views
