## start from the backends and work forward

import experimental/sexp
import std/options
import std/strutils

type
  CBkndContainerKind = enum
    cbkcProject = "cProject"
    cbkcHeader  = "cHeader"
    cbkcCFile   = "cFile"

  CBkndSrcGlobal = enum
    cbsgType        ## a typedef
    cbsgCompTag     ## declare composite type (struct/union) and fields
    cbsgCompTagDecl ## forward declare composite type (struct/union)
    cbsgEnumTag     ## declare enum type
    cbsgEnumTagDecl ## forward declare enum type
    cbsgVarDef      ## variable definition, can have an initializer
    cbsgVarDecl     ## variable declaration without definition
    cbsgFuncDef     ## function definition
    cbsgFuncDecl    ## function declaration (prototype)
    cbsgAsm         ## assembly statement
    cbsgPragma      ## top level pragma
    cbsgComment     ## comment
    cbsgDirective   ## define/conditional compliation

  CBkndSrcKind = enum
    # basics
    cbsComment = "comment"
    cbsIdent   = "ident"
    cbsInclude = "include"

    # declarations/prototypes
    cbsTypeDecl = "typeDecl"
    cbsVarDecl  = "varDecl"
    cbsFuncDecl = "funcDecl"
    cbsEnumDecl = "enumDecl"

    # defintions, include initiatlization
    cbsTypeDef = "typeDef"
    cbsVarDef  = "varDef"
    cbsFuncDef = "funcDef"
    cbsEnumDef = "enumDef"
    cbsDefBody = "body"
    cbsParam   = "param"

    # expressions - literals
    cbsLitBool   = "litBool"
    cbsLitChar   = "litChar"
    cbsLitInt    = "litInt"
    cbsLitFloat  = "litFloat"
    cbsLitNull   = "litNull"
    cbsLitString = "litString"

    # expressions
    cbsInfix = "infix"

    # expressions - operations
    cbsOpCast = "opCast"
    cbsCall   = "call"

    # type expression
    cbsTypVoid   = "typVoid"
    cbsTypBool   = "typBool"
    cbsTypChar   = "typChar"
    cbsTypInt    = "typInt"
    cbsTypLong   = "typLong"
    cbsTypFloat  = "typFloat"
    cbsTypDouble = "typDouble"
    cbsTypPtr    = "typPtr"
    cbsTypPtrPtr = "typPtrPtr"

    # pre-processor
    cbsPreDefine = "preDefine"
    cbsPreLine   = "preLine"
    cbsPreError  = "preError"

    # pre-processor - conditional compliation
    cbsPreIf     = "preIf"
    cbsPreIfDef  = "preIfDef"
    cbsPreIfNDef = "preIfNDef"
    cbsPreElif   = "preElif"
    cbsPreElse   = "preElse"
    cbsPreEndIf  = "preEndIf"

let foo = """
(cfile foo (
  (include "stdlib.h")
  (funcDef "main" (typInt)
    (
      (param (typInt) "argc")
      (param (typPtrPtr typChar) "argv")
    )
    (
      (call "puts" (litStr "Hello, World!"))
    )
  )
))"""

type
  CSrcNodeId = int

  CSrcFragNode = object
    kind: CBkndSrcKind
    extraId, width: int

  CSrcFrag = object
    node: seq[CSrcFragNode]
    extraNode: seq[CSrcNodeId]
    extraStr: seq[string]

  CBkndSrcTypKind = range[cbsTypVoid..cbsTypPtrPtr]

func toCBkndSrcKind(s: string): Option[CBkndSrcKind] =
  for e in CBkndSrcKind.items:
    if $e == s:
      result = some(e)
      return

func toCBkndSrcTypKind(s: string): Option[CBkndSrcTypKind] =
  for e in CBkndSrcTypKind.items:
    if $e == s:
      result = some(e)
      return

func parseType(frag: var CSrcFrag, stmt: SexpNode) =
  assert stmt.kind == SList
  assert stmt[0].kind == SSymbol

  let typK = stmt[0].symbol.toCBkndSrcTypKind.unsafeGet()

  case typK
  of cbsTypBool, cbsTypChar, cbsTypInt, cbsTypLong, cbsTypFloat,
      cbsTypDouble:
    frag.node.add CSrcFragNode(kind: typK)
  of cbsTypPtr, cbsTypPtrPtr:
    assert stmt.len == 2
    let pointedTyp = stmt[1].symbol.toCBkndSrcTypKind.unsafeGet()
    frag.node.add CSrcFragNode(kind: typK, extraId: int(pointedTyp))
  else:
    assert false, "unhandled type kind: " & $typK

func toCSrcFrag(s: SexpNode): CSrcFrag =
  var frag: CSrcFrag

  func cSrcFragStmt(frag: var CSrcFrag, k: CBkndSrcKind, stmt: SexpNode) =
    case k
    of cbsInclude:
      let strId = frag.extraStr.len
      frag.extraStr.add stmt[1].str
      frag.node.add CSrcFragNode(kind: k, extraId: strId)
    of cbsFuncDef:
      let
        id = frag.node.len
        strId = frag.extraStr.len
      frag.extraStr.add stmt[1].str
      frag.node.add CSrcFragNode(kind: k, extraId: frag.extraNode.len)
      
      parseType(frag, stmt[2])

      let bodyPos = stmt.len - 1

      for i in 3..<stmt.len:
        let s = stmt[i]
        if i == bodyPos:
          discard "implement me"
        else:
          assert s[0].kind == SSymbol
          assert s[0].symbol == "param"

          # add param name
          let strId = frag.extraStr.len
          frag.extraStr.add s[2].str
          
          let paramId = frag.node.len

          frag.node.add CSrcFragNode(kind: cbsParam, extraId: strId)
          
          let typId = frag.node.len
          parseType(frag, stmt[1])

          frag.node[paramId].width = typId - paramId
    of cbsDefBody:
      assert false
    else:
      assert false

  case s.kind
  of SList:
    for i, c in s.pairs:
      let k = c[0].symbol.toCBkndSrcKind.unsafeGet()
      cSrcFragStmt(frag, k, c)
  else:
    assert false

# func toCSrcFrag(s: SexpNode): CSrcFrag =
#   case s.kind
#   of SList:
#     let container = s[0]
#     case container.kind
#     of SSymbol:
#       case container.symbol.toLower
#       of "cfile":
#         var frag: CSrcFrag
        
#         for i, c in s[2].pairs:
#           case c.kind
#           of SList:


#   discard

echo parseSexp(foo).toCSrcFrag