{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module Wiki where

import Runix.FileSystem.Effects (FileSystem, readFile, writeFile, listFiles, fileExists)
import Runix.Logging.Effects (Logging)
import Polysemy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Prelude hiding (writeFile, readFile)
import Data.Kind

-- | Page name wrapper for type safety
newtype PageName = PageName Text deriving (Show, Eq)

-- | Page content using strict Text
type PageContent = Text

-- | Universal wiki effect for page-based knowledge management
data Wiki (m :: Type -> Type) a where
  ReadPage :: PageName -> Wiki m PageContent        -- Read page by name
  WritePage :: PageName -> PageContent -> Wiki m () -- Write page content
  ListPages :: Wiki m [PageName]                    -- List all page names
  PageExists :: PageName -> Wiki m Bool             -- Check if page exists

makeSem ''Wiki

-- | Directory wrapper for logseq graphs
newtype LogseqDirectory = LogseqDirectory FilePath deriving (Show)

-- | Reinterpret Wiki effect for logseq graphs via filesystem
runWikiLogseqFilesystem :: Member FileSystem r => LogseqDirectory -> Sem (Wiki ': r) a -> Sem r a
runWikiLogseqFilesystem (LogseqDirectory _baseDir) = 
  interpret $ \case
    ReadPage pageName -> do
      content <- readFile (pageFilePath pageName)
      return $ TE.decodeUtf8 $ BSL.toStrict content
    WritePage pageName content -> writeFile (pageFilePath pageName) (BSL.fromStrict $ TE.encodeUtf8 content)
    ListPages -> do
      files <- listFiles "pages/"
      return $ map (PageName . unsanitizePageName) $ filter (Text.isSuffixOf ".md") $ map Text.pack files
    PageExists pageName -> fileExists (pageFilePath pageName)
  -- TODO: Apply chroot and dotfile filtering using existing Runix functions
  -- . runChrootedFileSystem baseDir 
  -- . runFilteredFileSystem (not . isDotFile)
  where
    pageFilePath :: PageName -> FilePath
    pageFilePath (PageName pageName) = "pages/" <> Text.unpack (sanitizePageName pageName) <> ".md"
    
    -- Sanitize page name for logseq: replace special chars with _, subpaths "/" with "___"
    sanitizePageName :: Text -> Text
    sanitizePageName = Text.replace "/" "___" . Text.map sanitizeChar
      where
        sanitizeChar c
          | c `elem` ['/', '\\', ':', '*', '?', '"', '<', '>', '|'] = '_'
          | otherwise = c
    
    -- Reverse sanitization for listing (basic version)
    unsanitizePageName :: Text -> Text
    unsanitizePageName = Text.replace "___" "/" . Text.dropEnd 3  -- remove .md extension
