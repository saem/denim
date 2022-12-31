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
    cbsTypeDef  = "typeDef"
    cbsVarDef   = "varDef"
    cbsFuncDef  = "funcDef"
    cbsEnumDef  = "enumDef"
    cbsBlock    = "blk"
    cbsParam    = "param"

    # expressions - literals
    cbsLitBool   = "litBool"
    cbsLitChar   = "litChar"
    cbsLitInt    = "litInt"
    cbsLitFloat  = "litFloat"
    cbsLitNull   = "litNull"
    cbsLitString = "litStr"

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
    cbsPreEndIf  = "preEndIf"  # TODO: likely removable

let foo = """
(cfile foo (
  (include "stdlib.h")
  (funcDef "main" (typInt)
    (
      (param (typInt) "argc")
      (param (typPtrPtr typChar) "argv")
    )
    (blk
      (call "puts" ((litStr "Hello, World!")))
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


func debug(frag: CSrcFrag): string =
  result.add "extraNode: " & $frag.extraNode & "\n"
  result.add "extraStr: " & $frag.extraStr & "\n"
  result.add "tree:\n"
  for i, n in frag.node.pairs:
    result.add "$1$2:$3 " % ["  ", $n.kind, $i]
    result.add "(extraId: $1, width: $2)" % [$n.extraId, $n.width]
    result.add "\n"


func `$`(frag: CSrcFrag): string =
  # TODO: convert to a tree traversal while loop
  var currId = 0

  while currId < frag.node.len:
    let
      currNode = frag.node[currId]
    
    case currNode.kind
    of cbsInclude:
      result.add "include $1" % frag.extraStr[currNode.extraId]
    of cbsFuncDef:
      result.add
    else:
      assert false, "Traversal not implemented for kind: " & $currNode.kind

    result.add "\n"
    inc currId

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
  ## parse a type node, eg: (typInt):
  ##
  ## - simple types don't use `extraId` or `width`
  ## - pointers to simple types store the type directly in `extraId`
  ## - TODO: implement the rest
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


func reserveExtra(frag: var CSrcFrag, owner: CSrcNodeId, width: int) {.inline.} =
  ## reserve `width` amount of nodes in `frag.extraNode`, used to pre-allocate
  ## extra slots so they can be assigned during reduction
  for _ in 1..width:
    frag.extraNode.add -owner


func parseParam(frag: var CSrcFrag, param: SexpNode) =
  ## parse a `cbsParam` node, eg: (param (<type>) "name"):
  ##
  ## - `extraId` is the type node
  ## - `width` is the id to `extraStr` for the param name
  assert param.kind == SList
  assert param.len == 3
  assert param[0].kind == SSymbol
  assert param[0].symbol == $cbsParam

  let id = frag.node.len
  frag.node.add CSrcFragNode(kind: cbsParam)
  
  const
    typPos = 1
    namePos = 2

  frag.node[id].extraId = frag.node.len
  parseType(frag, param[typPos])

  assert param[namePos].kind == SString
  frag.node[id].width = frag.extraStr.len
  frag.extraStr.add param[namePos].str


func parseExpr(frag: var CSrcFrag, expr: SexpNode) =
  ## TOOD: finish implementing and documenting me
  assert expr.kind == SList, $expr
  assert expr.len >= 1

  let kind = expr[0].symbol.toCBkndSrcKind.unsafeGet()

  case kind
  of cbsLitString:
    let
      id = frag.node.len
      extraStrId = frag.extraStr.len
    
    frag.node.add CSrcFragNode(kind: kind, extraId: extraStrId)
  else:
    assert false, "IMPLEMENT ME"


func parseStmt(frag: var CSrcFrag, stmt: SexpNode) =
  ## parse a stmt of some sort
  ##
  ## TODO: finish implementing and documenting me
  assert stmt.kind == SList, $stmt
  assert stmt.len >= 1
  assert stmt[0].kind == SSymbol

  let kind = stmt[0].symbol.toCBkndSrcKind.unsafeGet()
  
  case kind
  of cbsCall:
    assert stmt.len == 3 # call, name, argsList

    const
      namePos = 1
      argsPos = 2

    let
      id = frag.node.len
      extraId = frag.extraNode.len
      argCount = stmt[argsPos].len
      width = argCount + 1 # + 1 for the name
      nameStrId = frag.extraStr.len
    
    frag.reserveExtra(id, width)

    frag.node.add CSrcFragNode(kind: cbsCall, extraId: extraId, width: width)
    frag.extraStr.add stmt[namePos].str
    frag.extraNode[extraId] = nameStrId

    for i, arg in stmt[argsPos].pairs:
      frag.extraNode[extraId + 1 + i] = frag.node.len # + 1 for namePos
      parseExpr(frag, arg)
  else:
    discard


func parseBlk(frag: var CSrcFrag, blk: SexpNode) =
  ## parse a `cbsBlk` node (blk (<0..n|stmts>))
  ##
  ## TODO: finish implementing and documenting me
  const blkSymPos = 0

  assert blk.kind == SList
  assert blk.len == 2
  assert blk[blkSymPos].kind == SSymbol
  assert blk[blkSymPos].symbol == $cbsBlock
  assert blk[1].kind == SList
  
  let
    id = frag.node.len
    width = blk.len - 1
    extraId = frag.extraNode.len
  frag.node.add CSrcFragNode(kind: cbsBlock, extraId: extraId, width: width)

  frag.reserveExtra(id, width)

  for i, stmt in blk.pairs:
    case i
    of blkSymPos:
      discard
    else:
      frag.extraNode[extraId + i - 1] = frag.node.len # - 1 for `blk`
      parseStmt(frag, stmt)


func toCSrcFrag(s: SexpNode): CSrcFrag =
  var frag: CSrcFrag

  func cSrcFragStmt(frag: var CSrcFrag, k: CBkndSrcKind, stmt: SexpNode) =
    case k
    of cbsInclude:
      let strId = frag.extraStr.len
      frag.extraStr.add stmt[1].str
      frag.node.add CSrcFragNode(kind: k, extraId: strId)
    of cbsFuncDef:
      assert stmt.len == 5 # 1: funcDef, 2: name, 3: type, 4: params, 5: body
      
      const
        namePos = 1
        typPos = 2
        paramsPos = 3
        bodyPos = 4

      let
        id = frag.node.len
        strId = frag.extraStr.len
        params = stmt[paramsPos]
        fnArity = params.len
        extraId = frag.extraNode.len
        bodyExtraId = extraId + fnArity + 1 # name, params, body
        width = fnArity + 2 # + 2 for the name and body
      
      frag.node.add CSrcFragNode(kind: k, extraId: extraId, width: width)
      
      frag.reserveExtra(id, width)

      frag.extraStr.add stmt[namePos].str
      frag.extraNode[extraId] = strId

      parseType(frag, stmt[typPos])

      assert params.kind == SList, $stmt

      case fnArity
      of 0:
        discard
      else:
        for i, maybeParam in params.pairs:
          frag.extraNode[extraId + 1 + i] = frag.node.len
          parseParam(frag, maybeParam)

      # remember the body block id in extraNode and parse the body
      frag.extraNode[extraId + fnArity + 1] = frag.node.len
      parseBlk(frag, stmt[bodyPos])
    else:
      assert false

  case s.kind
  of SList:
    for i, c in s.pairs:
      let k = c[0].symbol.toCBkndSrcKind.unsafeGet()
      cSrcFragStmt(frag, k, c)
  else:
    assert false
  
  result = frag

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

echo parseSexp(foo)[2].toCSrcFrag


# # thinking about creating a schema to handle deserialization
# type
#   SexpSchemaKind* = enum
#     ssStruct      ## fixed size list with a symbol
#     ssList0toN    ## list of 0 to N of a schema
#     ssSym         ## a specific symbol
#     ssSymAny      ## any symbol, as long as it is one
#     ssStrNonEmpty ## non-empty string
#     ssStr         ## string that might be empty
  
#   SexpSchema* = object
#     case kind: SexpSchemaKind:
#       of ssStruct, ssList0toN:
#         struct: seq[SexpSchema]
#       of ssSym:
#         sym: string
#       of ssSymAny, ssStrNonEmpty, ssStr:
#         discard
