## bootstrap compiler for Denim, written in Nim

import
  denim/[
    idioms,
    main,
  ],
  std/[
    os,
    parseopt,
    strutils,
  ]

const usage = """
denim <file.dnm>
""".strip

type
  ExitCode = enum
    exitSuccess
    exitErr

  ParseCliResult = enum
    parseSuccess         ## successfully parsed cli params
    parseQuitWithUsage   ## parsing failed, quit with usage message

  CliState = object
    fileArg: string

func initCliState(): CliState =
  discard

var
  cliOptParser = initOptParser()
  cliState = initCliState()

proc parseCli(s: var CliState, p: var OptParser): ParseCliResult =
  p.next()
  case p.kind
  of cmdArgument:
    s.fileArg = p.key.strip
    parseSuccess
  else:
    parseQuitWithUsage

template quit(msg: string, code: ExitCode) =
  quit msg, code.int

proc runDenimMain(cli: CliState) =
  discard

case paramCount()
of 0:
  quit "no file name provided:\p\p" & usage, exitErr
of 1:
  case parseCli(cliState, cliOptParser)
  of parseSuccess:
    let (dir, name, ext) = cliState.fileArg.splitFile

    if ext != "." & DenimFileExt:
      quit "'" & cliState.fileArg & "' has the wrong file extension\p" & usage
    
    if name.invalidModuleName:
      quit "'" & cliState.fileArg & "' is not a valid file name\p" & usage
    
    runDenimMain(cliState)
  of parseQuitWithUsage:
    quit usage, exitErr
else:
  quit usage, exitErr