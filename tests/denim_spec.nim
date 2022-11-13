## spec / test out the language

import experimental/sexp

type
  TestProgram = object
    inputSpec: SexpNode
    outputSpec: string

  ProgramSpec = object
    # currently only supports single file programs
    moduleName: string
    content: string

proc sexp(p: ProgramSpec): SexpNode =
  result = convertSexp([newSSymbol("programSpec"), p.moduleName, p.content])

echo ProgramSpec(moduleName: "foo", content: "stuff").sexp

type
  DenimExecutor = object
    # xxx: move this to the compiler part
    discard

  DenimResult = object
    exitCode: int
    stdOut: string

proc doDenim(cmd: DenimExecutor, p: ProgramSpec): DenimResult =
  cmd.run(p.moduleName, p.src)
  discard

# let specs = [
#   TestProgram(inputSpec: ProgramSpec(projectModuleSrc: convertSexp([])))
# ]