import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles="resources" } $ do
    phony "build" $ do
      need ["resources/macros/Header.hs"]
      cmd "stack" "build"
