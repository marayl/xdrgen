import Distribution.Simple
import Distribution.PackageDescription(PackageDescription)
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)
import System.Cmd(system)
import Distribution.Simple.LocalBuildInfo

test:: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
test _ _ _ _ = system "runhaskell Test.hs" >> return()

userHooks :: UserHooks
userHooks = simpleUserHooks { runTests = test }

main :: IO ()
main = defaultMainWithHooks userHooks
