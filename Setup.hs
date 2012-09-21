import Distribution.Simple
import Distribution.Simple.Program
import System.Process (system)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
  hookedPrograms = [c2hsc],
  preBuild = \args buildFlags -> do
      system "cd SNet/Interfaces && c2hsc CHandle.h --prefix SNet.Interfaces"
      preBuild simpleUserHooks args buildFlags
}

c2hsc :: Program
c2hsc = (simpleProgram "c2hsc") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        (_:('v':ver):_) -> takeWhile (/=',') ver
        _             -> ""
  }
