module Sensei.Markdown.Markdown
  ( Markdown,
    Block (..),
    Inline (..),
    ListType (..),
    HeadingType,
  )
where

import Data.Text (Text)

data Block
  = Paragraph [Inline]
  | Headering HeadingType [Inline]
  | Quote [Inline]
  | List ListType [[Inline]]
  | Divider
  | Code Language Code
  deriving (Eq, Show)

data HeadingType = H1 | H2 | H3 | H4 | H5 | H6 deriving (Eq, Show, Enum)

data Inline
  = Link LinkName LinkAddress
  | Image ImageAlt ImageAddress
  | Txt Text
  | Italic [Inline]
  | Strong [Inline]
  deriving (Eq, Show)

data ListType = OrderedList | UnorderedList deriving (Eq, Show)

type Markdown = [Block]

type Language = Maybe Text

type Code = Text

type LinkName = Text

type LinkAddress = Text

type ImageAlt = Text

type ImageAddress = Text
