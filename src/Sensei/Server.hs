{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.Server where

import qualified Control.Exception.Safe as Exc
import Control.Monad.Trans
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as LBS
import Data.Function (on, (&))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
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
readViews file usr = withBinaryFile file ReadMode $ loop f usr []
  where
    f Flow {_flowType=Note} views = views
    f Flow {..} views =
      let view = FlowView st st 0 _flowType
          st = _flowStart _flowState
       in case views of
            (v : vs) -> view : fillFlowEnd v st : vs
            [] -> [view]

fillFlowEnd :: FlowView -> UTCTime -> FlowView
fillFlowEnd v st =
  if utctDay st == utctDay (flowStart v)
  then v {flowEnd = st, duration = diffUTCTime st (flowStart v)}
  else let end = UTCTime (utctDay (flowStart v)) endOfWorkDay
           oneHour = secondsToNominalDiffTime 3600
           plus1hour = addUTCTime oneHour (flowStart v)
       in if end < (flowStart v)
          then v {flowEnd = plus1hour, duration = oneHour }
          else v {flowEnd = end, duration = diffUTCTime end (flowStart v)}

loop :: (Flow -> [a] -> [a]) -> String -> [a] -> Handle -> IO [a]
loop f usr acc hdl = do
      res <- Exc.try $ LT.hGetLine hdl
      case res of
        Left (_ex :: Exc.IOException) -> pure (reverse acc)
        Right ln ->
          case eitherDecode (LT.encodeUtf8 ln) of
            Left _err -> loop f usr acc hdl
            Right flow -> loop f usr (flowView flow usr f acc) hdl


readNotes :: FilePath -> String -> IO [(UTCTime, Text)]
readNotes file usr = withBinaryFile file ReadMode $ loop f usr []
  where
    f :: Flow -> [(UTCTime, Text)] -> [(UTCTime, Text)]
    f (Flow Note (FlowNote _ st _ note)) fragments =
      (st, note) : fragments
    f _ fragments = fragments

notesDayS :: FilePath -> [Char] -> Day -> Handler [(UTCTime, Text)]
notesDayS file usr day = do
  notes <- liftIO $ readNotes file usr
  pure $ filter (sameDayThan day fst) notes

queryFlowDayS :: FilePath -> [Char] -> Day -> Handler [FlowView]
queryFlowDayS file usr day = do
  views <- liftIO $ readViews file usr
  pure $ filter (sameDayThan day flowStart) views

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
      |> filter (sameDayThan day flowStart)
      |> List.sortBy (compare `on` flowType)
      |> NE.groupBy ((==) `on` flowType)
      |> fmap summarize
  where
    summarize :: NE.NonEmpty FlowView -> (FlowType, NominalDiffTime)
    summarize flows@(f NE.:| _) = (flowType f, sum $ fmap duration flows)

queryFlowS :: FilePath -> [Char] -> Handler [FlowView]
queryFlowS file usr =
  liftIO $ readViews file usr

flowView :: Flow -> String -> (Flow -> [a] -> [a]) -> [a] -> [a]
flowView f@Flow {..} usr mkView views =
  if _flowUser _flowState == usr
    then mkView f views
    else views
