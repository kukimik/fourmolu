{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | Configuration options used by the tool.
module Ormolu.Config
  ( Config (..),
    RegionIndices (..),
    RegionDeltas (..),
    defaultConfig,
    PrinterOpts (..),
    PrinterOptsPartial,
    PrinterOptsTotal,
    defaultPrinterOpts,
    loadConfigFile,
    configFileName,
    ConfigFileLoadResult (..),
    fillMissingPrinterOpts,
    CommaStyle (..),
    HaddockPrintStyle (..),
    regionIndicesToDeltas,
    DynOption (..),
    dynOptionToLocatedStr,
    ToCLIArgument (..),
    FromCLIArgument (..),
    OptionDescription (..),
    printerOptsDescr,
    showAllValues
  )
where

import Control.Monad(mzero)
import Data.Aeson
  ( FromJSON (..),
    Value (..),
    (.:?),
    (.:)
  )
import Data.Aeson.Types (typeMismatch,Parser)
import qualified Data.ByteString.Lazy as BS
import Data.Functor.Identity (Identity (..))
import Data.YAML (Pos)
import Data.YAML.Aeson (decode1)
import Data.Text(pack, unpack, Text)
import GHC.Generics (Generic)
import qualified SrcLoc as GHC
import System.Directory
  ( XdgDirectory (XdgConfig),
    findFile,
    getXdgDirectory,
    makeAbsolute,
  )
import System.FilePath (splitPath, (</>))

-- | Ormolu configuration.
data Config region = Config
  { -- | Dynamic options to pass to GHC parser
    cfgDynOptions :: ![DynOption],
    -- | Do formatting faster but without automatic detection of defects
    cfgUnsafe :: !Bool,
    -- | Output information useful for debugging
    cfgDebug :: !Bool,
    -- | Checks if re-formatting the result is idempotent
    cfgCheckIdempotence :: !Bool,
    -- | Region selection
    cfgRegion :: !region,
    cfgPrinterOpts :: !PrinterOptsTotal
  }
  deriving (Eq, Show, Functor)

-- | Region selection as the combination of start and end line numbers.
data RegionIndices = RegionIndices
  { -- | Start line of the region to format
    regionStartLine :: !(Maybe Int),
    -- | End line of the region to format
    regionEndLine :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | Region selection as the length of the literal prefix and the literal
-- suffix.
data RegionDeltas = RegionDeltas
  { -- | Prefix length in number of lines
    regionPrefixLength :: !Int,
    -- | Suffix length in number of lines
    regionSuffixLength :: !Int
  }
  deriving (Eq, Show)

-- | Default @'Config' 'RegionIndices'@.
defaultConfig :: Config RegionIndices
defaultConfig =
  Config
    { cfgDynOptions = [],
      cfgUnsafe = False,
      cfgDebug = False,
      cfgCheckIdempotence = False,
      cfgRegion =
        RegionIndices
          { regionStartLine = Nothing,
            regionEndLine = Nothing
          },
      cfgPrinterOpts = defaultPrinterOpts
    }

-- | Options controlling formatting output.
data PrinterOpts f = PrinterOpts
  { -- | Number of spaces to use for indentation
    poIndentation :: f Int,
    -- | Whether to place commas at start or end of lines
    poCommaStyle :: f CommaStyle,
    -- | Whether to indent `where` blocks
    poIndentWheres :: f Bool,
    -- | Leave space before opening record brace
    poRecordBraceSpace :: f Bool,
    -- | Trailing commas with parentheses on separate lines
    poDiffFriendlyImportExport :: f Bool,
    -- | Be less opinionated about spaces/newlines etc.
    poRespectful :: f Bool,
    -- | How to print doc comments
    poHaddockStyle :: f HaddockPrintStyle,
    -- | Number of newlines between top-level decls
    poNewlinesBetweenDecls :: f Int
  }
  deriving (Generic)

-- | A version of 'PrinterOpts' where any field can be empty.
-- This corresponds to the information in a config file or in CLI options.
type PrinterOptsPartial = PrinterOpts Maybe

deriving instance Eq PrinterOptsPartial

deriving instance Show PrinterOptsPartial

instance Semigroup PrinterOptsPartial where
  (<>) = fillMissingPrinterOpts

instance Monoid PrinterOptsPartial where
  mempty = PrinterOpts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A version of 'PrinterOpts' without empty fields.
type PrinterOptsTotal = PrinterOpts Identity

deriving instance Eq PrinterOptsTotal

deriving instance Show PrinterOptsTotal

defaultPrinterOpts :: PrinterOptsTotal
defaultPrinterOpts =
  PrinterOpts
    { poIndentation = pure 4,
      poCommaStyle = pure Leading,
      poIndentWheres = pure False,
      poRecordBraceSpace = pure False,
      poDiffFriendlyImportExport = pure True,
      poRespectful = pure True,
      poHaddockStyle = pure HaddockMultiLine,
      poNewlinesBetweenDecls = pure 1
    }

-- | Fill the field values that are 'Nothing' in the first argument
-- with the values of the corresponding fields of the second argument.
fillMissingPrinterOpts ::
  forall f.
  Applicative f =>
  PrinterOptsPartial ->
  PrinterOpts f ->
  PrinterOpts f
fillMissingPrinterOpts p1 p2 =
  PrinterOpts
    { poIndentation = fillField poIndentation,
      poCommaStyle = fillField poCommaStyle,
      poIndentWheres = fillField poIndentWheres,
      poRecordBraceSpace = fillField poRecordBraceSpace,
      poDiffFriendlyImportExport = fillField poDiffFriendlyImportExport,
      poRespectful = fillField poRespectful,
      poHaddockStyle = fillField poHaddockStyle,
      poNewlinesBetweenDecls = fillField poNewlinesBetweenDecls
    }
  where
    fillField :: (forall g. PrinterOpts g -> g a) -> f a
    fillField f = maybe (f p2) pure $ f p1

data CommaStyle
  = Leading
  | Trailing
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

data HaddockPrintStyle
  = HaddockSingleLine
  | HaddockMultiLine
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

-- | Convert 'RegionIndices' into 'RegionDeltas'.
regionIndicesToDeltas ::
  -- | Total number of lines in the input
  Int ->
  -- | Region indices
  RegionIndices ->
  -- | Region deltas
  RegionDeltas
regionIndicesToDeltas total RegionIndices {..} =
  RegionDeltas
    { regionPrefixLength = maybe 0 (subtract 1) regionStartLine,
      regionSuffixLength = maybe 0 (total -) regionEndLine
    }

-- | A wrapper for dynamic options.
newtype DynOption = DynOption
  { unDynOption :: String
  }
  deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.
dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

instance FromJSON PrinterOptsPartial where
  parseJSON (Object v) =
    PrinterOpts <$>
      o poIndentation <*>
      o poCommaStyle <*>
      o poIndentWheres <*>
      o poRecordBraceSpace <*>
      o poDiffFriendlyImportExport <*>
      o poRespectful <*>
      o poHaddockStyle <*>
      o poNewlinesBetweenDecls
    where o f = v .:? (pack . odName . f $ printerOptsDescr)
  parseJSON _ = fail "Incorrect configuration file."

-- | Read options from a config file, if found.
-- Looks recursively in parent folders, then in 'XdgConfig',
-- for a file named /fourmolu.yaml/.
loadConfigFile :: FilePath -> IO ConfigFileLoadResult
loadConfigFile path = do
  root <- makeAbsolute path
  xdg <- getXdgDirectory XdgConfig ""
  let dirs = reverse $ xdg : scanl1 (</>) (splitPath root)
  findFile dirs configFileName >>= \case
    Nothing -> return $ ConfigNotFound dirs
    Just file ->
      either (ConfigParseError file) (ConfigLoaded file)
        . decode1
        <$> BS.readFile file

-- | The result of calling 'loadConfigFile'.
data ConfigFileLoadResult
  = ConfigLoaded FilePath PrinterOptsPartial
  | ConfigParseError FilePath (Pos, String)
  | ConfigNotFound [FilePath]
  deriving (Eq, Show)

-- | Expected file name for YAML config.
configFileName :: FilePath
configFileName = "fourmolu.yaml"

data OptionDescription a = OptionDescription
  {
  odName :: String,
  odMetavar :: String,
  odHelp :: String
  }

printerOptsDescr :: PrinterOpts OptionDescription
printerOptsDescr = PrinterOpts
  {
    poIndentation = OptionDescription
      "indentation"
      "WIDTH"
      "Number of spaces per indentation step"
    ,poCommaStyle = OptionDescription
      "comma-style"
      "STYLE"
      ("How to place commas in multi-line lists, records etc: "
        <> (showAllValues @CommaStyle))
    ,poIndentWheres = OptionDescription
      "indent-wheres"
      "BOOL"
      ("Whether to indent 'where' bindings past the preceding body"
        <> " (rather than half-indenting the 'where' keyword)")
    ,poRecordBraceSpace = OptionDescription
      "record-brace-space"
      "BOOL"
      "Whether to leave a space before an opening record brace"
    ,poDiffFriendlyImportExport = OptionDescription
      "diff-friendly-import-export"
      "BOOL"
      ("Whether to make use of extra commas in import/export lists"
        <> " (as opposed to Ormolu's style)")
    ,poRespectful = OptionDescription
      "respectful"
      "BOOL"
      "Give the programmer more choice on where to insert blank lines"
    ,poHaddockStyle = OptionDescription
      "haddock-style"
      "STYLE"
      ("How to print Haddock comments: "
        <> (showAllValues @HaddockPrintStyle))
     ,poNewlinesBetweenDecls = OptionDescription
      "newlines-between-decls"
      "HEIGHT"
      "Number of spaces between top-level declarations"
  }

-- | Values that appear as arguments of CLI options and thus have
-- a corresponding textual representation.
class ToCLIArgument a where
  -- | Convert a value to its representation as a CLI option argument.
  toCLIArgument :: a -> String

  -- | Convert a value to its representation as a CLI option argument wrapped
  -- in apostrophes.
  toCLIArgument' :: a -> String
  toCLIArgument' x = "'" <> toCLIArgument x <> "'"

class FromCLIArgument a where
  fromCLIArgument :: String -> Maybe a

  fromCLIArgument' :: Text -> Maybe a
  fromCLIArgument' = fromCLIArgument . unpack

  stdParseJSON :: String -> Value -> Parser a
  stdParseJSON _ (Object o)  = do
    tag <- o .: "tag"
    case fromCLIArgument' tag of
        Just v -> return v
        Nothing -> mzero
  stdParseJSON typeName v = typeMismatch typeName v

instance (Enum a, Bounded a, ToCLIArgument a) => FromCLIArgument a where
  fromCLIArgument s = lookup s $ map (\x -> (toCLIArgument x, x)) [minBound ..]

instance ToCLIArgument Int where
  toCLIArgument = show

instance ToCLIArgument Bool where
  toCLIArgument True = "true"
  toCLIArgument False = "false"

instance ToCLIArgument CommaStyle where
  toCLIArgument Leading = "leading"
  toCLIArgument Trailing = "trailing"

instance ToCLIArgument HaddockPrintStyle where
  toCLIArgument HaddockSingleLine = "single-line"
  toCLIArgument HaddockMultiLine = "multi-line"

instance FromJSON CommaStyle where
  parseJSON = stdParseJSON "CommaStyle"

instance FromJSON HaddockPrintStyle where
  parseJSON = stdParseJSON "HaddockPrintStyle"

showAllValues :: forall a. (Enum a, Bounded a, ToCLIArgument a) => String
showAllValues = format (map toCLIArgument' [(minBound :: a) ..])
  where
    format [] = []
    format [x] = x
    format [x1, x2] = x1 <> " or " <> x2
    format (x : xs) = x <> ", " <> format xs
