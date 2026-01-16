module PathResolutionSpec (spec) where

import Test.Hspec
import Runix.FileSystem.Path

spec :: Spec
spec = describe "Path Resolution (Walking)" $ do

  describe "resolvePath - basic functionality" $ do
    it "resolves simple absolute path" $
      resolvePath "/foo/bar" `shouldBe` "/foo/bar"

    it "removes trailing slash" $
      resolvePath "/foo/bar/" `shouldBe` "/foo/bar"

    it "handles root" $
      resolvePath "/" `shouldBe` "/"

    it "handles empty components" $
      resolvePath "/foo//bar" `shouldBe` "/foo/bar"

    it "handles multiple slashes" $
      resolvePath "///foo///bar///" `shouldBe` "/foo/bar"

  describe "resolvePath - dot handling" $ do
    it "removes . components" $
      resolvePath "/foo/./bar" `shouldBe` "/foo/bar"

    it "handles multiple dots" $
      resolvePath "/foo/./././bar" `shouldBe` "/foo/bar"

    it "handles . at start" $
      resolvePath "/./foo" `shouldBe` "/foo"

    it "handles . at end" $
      resolvePath "/foo/." `shouldBe` "/foo"

  describe "resolvePath - .. handling (THE CRITICAL PART)" $ do
    it "walks back one directory" $
      resolvePath "/foo/bar/.." `shouldBe` "/foo"

    it "walks back two directories" $
      resolvePath "/foo/bar/baz/../.." `shouldBe` "/foo"

    it "handles .. at root stays at root" $
      resolvePath "/.." `shouldBe` "/"

    it "handles multiple .. at root stay at root" $
      resolvePath "/../../.." `shouldBe` "/"

    it "handles escaping attempt from deep path" $
      resolvePath "/a/b/c/../../../../../.." `shouldBe` "/"

    it "THE CRITICAL TEST: /test/../../../test/a.txt from /test" $
      -- This is the example from the user: from /test, open ../../../test/a.txt
      -- Should resolve to /test/a.txt
      resolveRelative "/test" "../../../test/a.txt" `shouldBe` "/test/a.txt"

    it "complex path with .. in middle" $
      resolvePath "/foo/bar/../baz/qux" `shouldBe` "/foo/baz/qux"

    it ".. cancels previous component" $
      resolvePath "/foo/bar/../baz/../qux" `shouldBe` "/foo/qux"

  describe "resolveRelative - relative to CWD" $ do
    it "simple relative path" $
      resolveRelative "/home/user" "file.txt" `shouldBe` "/home/user/file.txt"

    it "relative path with .." $
      resolveRelative "/home/user/project" "../other/file.txt" `shouldBe` "/home/user/other/file.txt"

    it "relative path that escapes root" $
      resolveRelative "/test" "../../../../etc/passwd" `shouldBe` "/etc/passwd"

    it "handles absolute path (ignores CWD)" $
      resolveRelative "/home/user" "/etc/passwd" `shouldBe` "/etc/passwd"

    it "relative . stays at CWD" $
      resolveRelative "/home/user" "." `shouldBe` "/home/user"

    it "relative .. goes to parent" $
      resolveRelative "/home/user" ".." `shouldBe` "/home"

  describe "Edge cases" $ do
    it "path with multiple consecutive dots is NOT special" $
      -- ... is a valid directory name, not .. twice
      resolvePath "/foo/.../bar" `shouldBe` "/foo/.../bar"

    it "handles . in filename" $
      resolvePath "/foo/file.txt" `shouldBe` "/foo/file.txt"

    it "handles .. in filename" $
      resolvePath "/foo/file..txt" `shouldBe` "/foo/file..txt"
