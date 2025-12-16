module Sensei.MarkdownSpec where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import Sensei.Markdown (Block (Paragraph), Inline (Txt), parseMarkdown)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "can parse sample Markdown document" $ do
    content <- parseMarkdown . Text.decodeUtf8 <$> BS.readFile "test/sample-markdown.md"
    head . tail <$> content `shouldBe` Right (Paragraph [Txt "title: Comprendre les catamorphismes"])
