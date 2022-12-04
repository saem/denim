## main denim module, runs the interpreter/compiler and other denim tools

type
  ProjectFile = object
    name: string
    content: string

  DenimCommandKind = enum
    dnmCmdRun

  DenimCommand = object
    kind: DenimCommandKind
    projectFile: ProjectFile
  

proc run*(name, content: string) =
  let cmd = DenimCommand(
    kind: dnmCmdRun,
    projectFile: ProjectFile(name: name, content: content))
  
  