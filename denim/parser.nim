
import
  npeg,
  lexer

from npeg/codegen import MatchResult

type
  ParserDataKind = enum
    pdStmt
    pdIdent
    pdStrLit
    pdCmd

  ExtraId = distinct int32

  ParserData = object
    kind: ParserDataKind
    tokenId: TokenId
    extraId: ExtraId

  ParsedFrag = object
    lexedFrag: LexFrag
    data: seq[ParserData]
    extraData: seq[TokenId]
    matchRes: MatchResult[TokenData]

template unreachable() =
  assert false, "should never have gotten here"

proc addCmd(f: var ParsedFrag, cmd: int) =
  f.data.add ParserData(kind: pdCmd, tokenId: TokenId(cmd + 1))

proc addCmdNthArg(f: var ParsedFrag, arg: int) =
  let tokenId = TokenId(arg + 1)
  case f.lexedFrag[tokenId].kind
  of tkSymbol:
    f.data.add ParserData(kind: pdIdent, tokenId: tokenId)
  of tkStrLit:
    f.data.add ParserData(kind: pdStrLit, tokenId: tokenId)
  else:
    unreachable()

proc addStrLit(f: var ParsedFrag, arg: int) =
  f.data.add ParserData(kind: pdStrLit, tokenId: TokenId(arg + 1))

let parser = peg(denim, TokenData, frag: ParsedFrag):
  literals    <- [tkStrLit]

  callCmd     <- >[tkSymbol] * >callCmdArg * *([tkComma] * >callCmdArg):
    frag.addCmd @1
    for i in 2..<capture.len:
      frag.addCmdNthArg capture[i].si
  callCmdArg  <- [tkSymbol] | literals

  stmt        <- (callCmd)

  denim       <- *stmt * !1

proc parse*(frag: var ParsedFrag, s: string) =
  frag.lexedFrag = lex(s)
  let res = parser.match(frag.lexedFrag.tokens, frag)
  frag.matchRes = res

proc parse*(s: string): ParsedFrag =
  var frag: ParsedFrag
  parse(frag, s)
  result = frag

func `$`*(eid: ExtraId): string {.inline.} =
  $eid.int

echo parse("echo \"Hello, World!\"").data
