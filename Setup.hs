import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, hookedPrograms)
import Distribution.Simple.Program.Types (simpleProgram)

main = defaultMainWithHooks simpleUserHooks {
    hookedPrograms = [simpleProgram "java"]
  }
