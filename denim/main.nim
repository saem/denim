## main denim module, runs the interpreter/compiler and other denim tools

import denim/infra/[host]

type
  ProjectFile = object
    name: string
    content: string

  DenimCommandKind = enum
    dnmCmdRun

  DenimCommand = object
    kind: DenimCommandKind
    projectFile: ProjectFile

  DenimCompiler = object
    projFile: ProjectFile

proc initCompiler(): DenimCompiler =
  discard

proc exec(c: DenimCompiler, cmd: DenimCommand) =
  discard

proc run*(name, content: string) =
  let
    cmd = DenimCommand(
      kind: dnmCmdRun,
      projectFile: ProjectFile(name: name, content: content))
    compiler = initCompiler()
  
  compiler.exec(cmd)

template example(name: string, n: untyped): untyped =
  template given(n: untyped): untyped =
    template sourceFile(name: string, n: untyped): untyped =
      discard

    template directory(n: untyped): untyped =
      discard
  
  template works_because(n: untyped): untyped =
    discard

example "basic":
  given:
    sourceFile "hello.dnm":
      echo "Hello, World"
  works_because:
    ## - each file is a namespace
    ## - namespaces implicitly `open` the std/interpreter namespace
    ## - echo is a `proc`edure exported by std/interpreter
    ## - when passed to denim it's immediately run

example "build":
  given:
    sourceFile "build.dnm":
      open std/build

      project:
        name: "hello"
    
    sourceFile "hello.dnm":
      echo "Hello, World!"

  works_because:
    ## build.dnm:
    ## - `open std/build` opens the build namespace
    ##   - implicit `open std/interpreter` still happens
    ## - `std/build` exposes `project` when called:
    ##   - takes `name` to allow declarative
    ##   - exposes a cli interface for build, test, run, etc
    ##
    ## hello.dnm
    ## - hello can now be built as an exe
    ## - implicitly `open std/system`
    ## - echo is provided there

example "hello":
  given:
    sourceFile "hello.dnm":
      # assume the build file exists
      
      proc hello(subject: string) =
        echo "Hello, ", subject, "!"
      
      hello()
  works_because:
    ## issues:
    ## - allocation of subject
    ## - minor: echo writing to stdio, blocking/control flow

##[
Docs

# Overview

## Glossary/Map

### Program
- Source Code in files
  - project file: the source file passed to denim executable
- `namespace`: each file is a namespace

### Run/Build/Compile
- code is only ever immediately run
- a build program is required to compile an executable

### System
- standard interpreter namespace is implictly added
  - exposes the system interface provided by the interpreter
- 

## Source Code

- source code is arranged into files
- file extension: `dnm`

## Open Questions

- with the standard interpreter name space implicitly added, what's the fundamental mechanism?

]##