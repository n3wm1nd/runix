module Main (main) where

import Test.Hspec

import qualified FileSystemSecuritySpec
import qualified FileSystemSecurityProperties
import qualified FileSystemProjectSpec
import qualified DummyFileSystemSpec
import qualified GrepSecuritySpec
import qualified PathResolutionSpec
import qualified ChrootTranslationSpec
import qualified HTTPStreamingInterceptorSpec
import qualified LangFuseSpec
import qualified LLMProviderSpec
import qualified RestAPIStreamingSpec

main :: IO ()
main = hspec $ do
    describe "Dummy FileSystem" DummyFileSystemSpec.spec
    describe "FileSystem Security" FileSystemSecuritySpec.spec
    describe "FileSystem Security Properties" FileSystemSecurityProperties.spec
    describe "Path Resolution (Walking)" PathResolutionSpec.spec
    describe "Chroot Translation Logic" ChrootTranslationSpec.spec
    describe "FileSystem Project Effects" FileSystemProjectSpec.spec
    describe "Grep Security" GrepSecuritySpec.spec
    describe "HTTP Streaming Interceptors" HTTPStreamingInterceptorSpec.spec
    LangFuseSpec.spec
    LLMProviderSpec.spec
    RestAPIStreamingSpec.spec
