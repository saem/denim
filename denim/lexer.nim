import
  npeg
from npeg/codegen import MatchResult

type
  TokenId* = distinct int

func `$`*(tid: TokenId): string {.inline.} =
  $tid.int

type
  TokenKind* = enum
    tkInvalid
    tkEof
    tkComma
    tkIdent
    tkStrLit
    tkCharLit
    # tkSpace
  
  TokenData* = object
    kind*: TokenKind
    ln, col: int
    str: string         # TODO: move to extra data
    
  LexFrag* = object
    tokens*: seq[TokenData]
    ln, col: int
    matchRes: MatchResult[char]

proc `==`*(data: TokenData, token: TokenKind): bool =
  data.kind == token

grammar "strLit":
  escape    <- '\\' * ( spaceChar | punctChar | ctrlChar | charPoint | codePoint )
  spaceChar <- { 'p', 'r', 'c', 'n', 'l', 'f', 't', 'v' }
  punctChar <- { '\\', '"', '\'' }
  charPoint <- +Digit | ('x' * Xdigit[2])
  ctrlChar  <- { 'a', 'b', 'e' }
  codePoint <- 'u' * ( Xdigit[4] | '{' * +Xdigit * '}' )
  strBody   <- ?escape * *( +({'\x20'..'\xff'} - {'"'} - {'\\'}) * *escape)
  strLit    <- '"' * strBody * '"'

grammar "charLit":
  escape    <- '\\' * ( spaceChar | punctChar | ctrlChar | charPoint )
  spaceChar <- { 'r', 'c', 'n', 'l', 'f', 't', 'v' }
  punctChar <- { '\\', '"', '\'' }
  charPoint <- +Digit | ('x' * Xdigit[2])
  ctrlChar  <- { 'a', 'b', 'e' }
  charBody  <- escape | ({'\x20'..'\xff'} - {'\''} - {'\\'})
  charLit   <- '"' * charBody * '"'

proc addComma(f: var LexFrag, width: int) =
  f.tokens.add TokenData(kind: tkComma,
                         ln: f.ln,
                         col: f.col)
  f.col += width

proc addIdent(f: var LexFrag, ident: string, width: int) =
  f.tokens.add TokenData(kind: tkIdent,
                         ln: f.ln,
                         col: f.col,
                         str: ident)
  f.col += width

proc addStrLit(f: var LexFrag, lit: string, width: int) =
  f.tokens.add TokenData(kind: tkStrLit,
                         ln: f.ln,
                         col: f.col,
                         str: lit)
  f.col += width

proc addCharLit(f: var LexFrag, lit: string, width: int) =
  f.tokens.add TokenData(kind: tkCharLit,
                         ln: f.ln,
                         col: f.col,
                         str: lit)
  f.col += width

let lexer = peg(tokens, frag: LexFrag):
  S        <- *Blank

  # Basic Tokens

  tokComma <- "," * S:
    frag.addComma capture[0].s.len

  tokIdent <- >(Alpha | '_') * *(Alpha | Digit | '_') * S:
    frag.addIdent $1, capture[0].s.len

  strLit   <- >strLit.strLit * S:
    frag.addStrLit $1, capture[0].s.len
  
  charLit  <- >charLit.charLit * S:
    frag.addCharLit $1, capture[0].s.len
  
  literal  <- strLit | charLit

  token    <- +(tokIdent | tokComma | literal)

  tokens   <- *token * !1

proc lex*(frag: var LexFrag, s: string) =
  let res = lexer.match(s, frag)
  frag.matchRes = res

proc lex*(s: string): LexFrag =
  var frag: LexFrag
  lex(frag, s)
  result = frag

proc `[]`*(frag: LexFrag, tid: TokenId): TokenData =
  frag.tokens[tid.int - 1]

# echo lex("echo \"Hello, World!\"")

# TODO: move this off to string literal tests

# let parserTest = peg "strLitTest":
#   S          <- *Space
#   strLitTest <- ?S * >strLit.strLit * ?S:
#     echo $1, " from: ", $0

# let testStrings = [
#   "\"\"",
#   "\"test\"",
#   "      \"foo  \\11   \\n bar\"",
#   "      \"foo  \"    ",
#   "\"boourns\"    ",
# ]

# for s in testStrings:
#   echo "given: ", s
#   discard parserTest.match(s)
