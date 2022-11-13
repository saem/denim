## bootstrap compiler for Denim, written in Nim

let source = """(:cmd echo (:strLit "Hello, World!"))"""

# type
#   SyntaxFragmentKind = enum
#     sfk
#   SyntaxFragment = object
#     kind: SyntaxFragmentKind

type
  AstNodeDataKind = enum
    astCommand
    astIdent
    astStrLit

  # SyntaxId = distinct int32
  IdentId = distinct int32
  StringLitId = distinct int32

  AstFragEntry = object
    # syntaxId: SyntaxId
    case kind: AstNodeDataKind
    of astCommand:
      discard
    of astIdent:
      identId: IdentId
    of astStrLit:
      strId: StringLitId

  StringLitStorage = seq[string]
  IdentStorage = seq[string]

  AstFragment = object
    # syntax: SyntaxFragment
    nodes: seq[AstFragEntry]
    strLits: StringLitStorage
    idents: IdentStorage

  AstModule = object
    name: string
    ast = AstFragment

import experimental/sexp

proc parseModule(name: string, moduleSyn: SexpNode): AstModule =
  

let
  actualModule =
    parseModule:
      convertSexp([newSSymbol("cmd"), "echo", [newSSymbol("strLit"), "Hello, World!"]])
  expectedModule =
    AstModule(
      name: "foo",
      ast: AstFragment(
        nodes: [
          AstFragEntry(kind: astCommand),
          AstFragEntry(kind: astIdent, identId: IdentId 1),
          AstFragEntry(kind: astStrLit, identId: StringLitId 1)
        ],
        idents: @["echo"],
        strLits: @["Hello, World!"]
      )
    )
