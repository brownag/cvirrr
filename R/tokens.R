# tokens

#' Convert CVIR Tokens IDs, Labels and Syntax
#'
#' @param token Token Label
#' @param id Internal Token Byte Value
#' @param convert_punctuation For `id_to_token()`: Convert Token ID to user-level syntax (`TRUE`) or internal label (`FALSE`); Default: `TRUE`
#' @return a numeric vector of token IDs (`token_to_id()`), or a character vector of token labels or syntax (`id_to_token()`)
#' @export
#' @rdname Tokens
#'
#' @examples
#'
#' token_to_id("DERIVE")
#'
#' id_to_token(317)
token_to_id <- function(token) {
  CVIR_TOKEN <- getCVIRTokens()
  CVIR_PUNCTUATION <- getCVIRPunctuation()
  # load(system.file("data/CVIR_TOKEN.rda", package = "cvirrr")[1])
  res <- CVIR_TOKEN[toupper(trimws(token))]
  if (any(is.na(res))) {
    idx <- which(is.na(res))
    # load(system.file("data/CVIR_PUNCTUATION.rda", package = "cvirrr")[1])
    punctuation <- CVIR_PUNCTUATION[match(token[idx], CVIR_PUNCTUATION)]
    res[idx[!is.na(punctuation)]] <- CVIR_TOKEN[names(punctuation[!is.na(punctuation)])]
    newnames <- names(res)
    newnames[idx[!is.na(punctuation)]] <- names(punctuation[!is.na(punctuation)])
    names(res) <- newnames
  }
  res
}

#' @export
#' @rdname Tokens
id_to_token <- function(id, convert_punctuation = TRUE) {
  CVIR_TOKEN <- getCVIRTokens()
  CVIR_PUNCTUATION <- getCVIRPunctuation()
  # load(system.file("data/CVIR_TOKEN.rda", package = "cvirrr")[1])
  idx <- match(id, CVIR_TOKEN)
  res <- names(CVIR_TOKEN)[idx]
  if (!convert_punctuation) return(res)
  # load(system.file("data/CVIR_PUNCTUATION.rda", package = "cvirrr")[1])
  idx2 <- which(res %in% names(CVIR_PUNCTUATION))
  res[idx2] <- CVIR_PUNCTUATION[res[idx2]]
  # names(res) <- id
  res
}

getCVIRTokens <- function() c(
  EOF = 1,
  NULL_TREE_LOOKAHEAD = 3,
  RUN_STMT = 4,
  ARGS = 5,
  ALIAS = 6,
  AT_LINE = 7,
  CVIR = 8,
  DEFINITIONS = 9,
  DITEM = 10,
  SETITEM = 11,
  VLIST = 12,
  SQL_COLUMN = 13,
  SQL_COLUMNS = 14,
  SQL_TABLE_SPEC = 15,
  SQL_CVIR_WHERE_CONDITION = 0x10,
  RULE_LIST = 0x11,
  DEBUG_LIST = 0x12,
  OBJECT_NAME = 0x13,
  INPUT_LIST = 20,
  INPUT_COLUMN = 0x15,
  FILE_NAME = 0x16,
  SORT_LIST = 0x17,
  SORT_COLUMN = 0x18,
  AGGCOL_LIST = 0x19,
  AGGCOL = 0x1a,
  ROW_LIST = 0x1b,
  VALUE_LIST = 0x1c,
  LABEL_LIST = 0x1d,
  CELL_LIST = 30,
  CROSSTAB_COLUMN = 0x1f,
  ACCEPT_MAP = 0x20,
  DERIVE_LIST = 0x21,
  DERIVE_MAP = 0x22,
  ARG_LIST = 0x23,
  EXPR = 0x24,
  ELIST = 0x25,
  ARITH_EXPR = 0x26,
  COND_EXPR = 0x27,
  SUBSTR = 40,
  SECTION_SPEC_BLOCK = 0x29,
  SECTION_SPEC = 0x2a,
  DATA_SECTION_SPEC = 0x2b,
  HEAD_SECTION_SPEC = 0x2c,
  ID_LIST = 0x2d,
  CONDITION_SECTION_SPEC = 0x2e,
  SECTION_LIST = 0x2f,
  KEEP_SECTION_SPEC = 0x30,
  LINE_LIST = 0x31,
  COLUMN_LIST = 50,
  SIMPLE_COLUMN_LIST = 0x33,
  COLUMN_SPEC = 0x34,
  SET_COLUMN_LIST = 0x35,
  SET_COLUMN_SPEC = 0x36,
  UNARY_PLUS = 0x37,
  UNARY_MINUS = 0x38,
  BOOL_EXPR = 0x39,
  LOGICAL_EXPR = 0x3a,
  COMPARISON = 0x3b,
  COMMENT_STMT = 60,
  PARAMETER_ATTR_LIST = 0x3d,
  PARAMETER_ATTR = 0x3e,
  PARAMETER_VALUE_TYPE = 0x3f,
  SCRIPT_VARIABLE = 0x40,
  DOT = 0x41,
  COMMENT = 0x42,
  RUN = 0x43,
  SUBREPORT = 0x44,
  COMMA = 0x45,
  PARAMETER = 70,
  ELEMENT = 0x47,
  PROMPT = 0x48,
  MULTIPLE = 0x49,
  REQUIRED = 0x4a,
  DESCRIPTIONID = 0x4b,
  MAX = 0x4c,
  NUM_INT = 0x4d,
  SEARCH = 0x4e,
  SELECTED = 0x4f,
  DEPENDSON = 80,
  DISPLAY = 0x51,
  CHARACTER = 0x52,
  CHAR = 0x53,
  NUMERIC = 0x54,
  NUM = 0x55,
  BOOLEAN = 0x56,
  BOOL = 0x57,
  CODEVAL = 0x58,
  CODESEQ = 0x59,
  CODENAME = 90,
  OBJECT = 0x5b,
  OBJECTID = 0x5c,
  ACCEPT = 0x5d,
  FROM = 0x5e,
  BASE = 0x5f,
  TABLE = 0x60,
  VARIABLE = 0x61,
  INTERPRET = 0x62,
  REASONS = 0x63,
  RULEDEPTH = 100,
  COLON = 0x65,
  INPUT = 0x66,
  FILE = 0x67,
  EXEC = 0x68,
  SQL = 0x69,
  AS = 0x6a,
  DIVIDE = 0x6b,
  DELIMITER = 0x6c,
  SORT = 0x6d,
  BY = 110,
  ASCENDING = 0x6f,
  ASC = 0x70,
  DESCENDING = 0x71,
  DESC = 0x72,
  LEXICAL = 0x73,
  LEX = 0x74,
  SYMBOL = 0x75,
  SYM = 0x76,
  INSENSITIVE = 0x77,
  INSEN = 120,
  AGGREGATE = 0x79,
  ROWS = 0x7a,
  COLUMN = 0x7b,
  NONE = 0x7c,
  SUM = 0x7d,
  AVERAGE = 0x7e,
  FIRST = 0x7f,
  LAST = 0x80,
  MIN = 0x81,
  UNIQUE = 130,
  LIST = 0x83,
  GLOBAL = 0x84,
  CROSSTAB = 0x85,
  LABELS = 0x86,
  CELLS = 0x87,
  VALUES = 0x88,
  LPAREN = 0x89,
  RPAREN = 0x8a,
  INTERVALS = 0x8b,
  MINUS = 140,
  DERIVE = 0x8d,
  USING = 0x8e,
  SET = 0x8f,
  FORMVARIABLE = 0x90,
  WHEN = 0x91,
  DEFINE = 0x92,
  ASSIGN = 0x93,
  INITIAL = 0x94,
  TBLELM = 0x95,
  QUESTION = 150,
  THEN = 0x97,
  ELSE = 0x98,
  IF = 0x99,
  AND = 0x9a,
  OR = 0x9b,
  NOT = 0x9c,
  ANY = 0x9d,
  ALL = 0x9e,
  ISNULL = 0x9f,
  EQ = 160,
  NE = 0xa1,
  LESSTHAN = 0xa2,
  GT = 0xa3,
  LE = 0xa4,
  GE = 0xa5,
  MATCHES = 0xa6,
  IMATCHES = 0xa7,
  PLUS = 0xa8,
  CAT = 0xa9,
  POWER = 170,
  STAR = 0xab,
  LSQR = 0xac,
  RSQR = 0xad,
  `NULL` = 0xae,
  LCURLY = 0xaf,
  RCURLY = 0xb0,
  COUNT = 0xb1,
  NEW = 0xb2,
  ARRAYSUM = 0xb3,
  ARRAYAVG = 180,
  ARRAYMAX = 0xb5,
  ARRAYMIN = 0xb6,
  ARRAYCOUNT = 0xb7,
  ARRAYMEDIAN = 0xb8,
  ARRAYMODE = 0xb9,
  ARRAYSTDEV = 0xba,
  ARRAYPOSITION = 0xbb,
  LOGN = 0xbc,
  LOG10 = 0xbd,
  EXP = 190,
  COS = 0xbf,
  SIN = 0xc0,
  TAN = 0xc1,
  ACOS = 0xc2,
  ASIN = 0xc3,
  ATAN = 0xc4,
  ATAN2 = 0xc5,
  SQRT = 0xc6,
  ABS = 0xc7,
  CLIP = 200,
  UPCASE = 0xc9,
  LOCASE = 0xca,
  NMCASE = 0xcb,
  SECASE = 0xcc,
  TEXTURENAME = 0xcd,
  ARRAYSHIFT = 0xce,
  ARRAYROT = 0xcf,
  ARRAYCAT = 0xd0,
  POW = 0xd1,
  MOD = 210,
  WTAVG = 0xd3,
  APPEND = 0xd4,
  DATEFORMAT = 0xd5,
  FIND = 0xd6,
  ROUND = 0xd7,
  CODELABEL = 0xd8,
  SPRINTF = 0xd9,
  LOOKUP = 0xda,
  NAMECAP = 0xdb,
  STRING_LITERAL = 220,
  GEOMORDESC = 0xdd,
  STRUCTPARTS = 0xde,
  REPLACE = 0xdf,
  TODAY = 0xe0,
  USER = 0xe1,
  REGROUP = 0xe2,
  LST = 0xe3,
  STRING = 0xe4,
  CHAR_LITERAL = 0xe5,
  NUM_REAL = 230,
  PAGE = 0xe7,
  LENGTH = 0xe8,
  WIDTH = 0xe9,
  INCH = 0xea,
  IN = 0xeb,
  UNLIMITED = 0xec,
  MARGIN = 0xed,
  LEFT = 0xee,
  RIGHT = 0xef,
  TOP = 240,
  BOTTOM = 0xf1,
  INCLUDE = 0xf2,
  PITCH = 0xf3,
  HORIZONTAL = 0xf4,
  VERTICAL = 0xf5,
  FONT = 0xf6,
  SKIP = 0xf7,
  FILL = 0xf8,
  LINE = 0xf9,
  LINES = 250,
  INCHES = 0xfb,
  SEMI = 0xfc,
  AT = 0xfd,
  CENTER = 0xfe,
  NEXT = 0xff,
  SAME = 0x100,
  ARRAY = 0x101,
  OPEN = 0x102,
  CLOSE = 0x103,
  VALUETAG = 260,
  ATTRIB = 0x105,
  FIELD = 0x106,
  PAGES = 0x107,
  LABEL = 0x108,
  DIGITS = 0x109,
  DECIMAL = 0x10a,
  SIGDIG = 0x10b,
  ALIGN = 0x10c,
  PAD = 0x10d,
  INDENT = 270,
  TRUNCATE = 0x10f,
  SEPARATOR = 0x110,
  SUPPRESS = 0x111,
  DUPLICATES = 0x112,
  REPEAT = 0x113,
  NO = 0x114,
  COMMA_ = 0x115,
  NEST = 0x116,
  PER = 0x117,
  QUOTE = 280,
  QUOTED = 0x119,
  ESCAPE = 0x11a,
  BOLD = 0x11b,
  ITALIC = 0x11c,
  PT = 0x11d,
  TAG = 0x11e,
  SPECIAL = 0x11f,
  ZERO = 0x120,
  NULLS = 0x121,
  WITH = 290,
  HEADER = 0x123,
  END = 0x124,
  FOOTER = 0x125,
  FINAL = 0x126,
  TEMPLATE = 0x127,
  SECTION = 0x128,
  KEEP = 0x129,
  OF = 0x12a,
  START = 0x12b,
  DATA = 300,
  HEADING = 0x12d,
  SCRIPT_FUNCTION = 0x12e,
  SCRIPT_VARREF = 0x12f,
  BORDER = 0x130,
  CAPTION = 0x131,
  EDIT = 0x132,
  FORMAT = 0x133,
  PX = 0x134,
  REASON = 0x135,
  TRUNC = 310,
  VALUE = 0x137,
  STRING_ESC = 0x138,
  CHAR_ESC = 0x139,
  WS = 0x13a,
  ASSIGNMENT = 0x13b,
  HASH = 0x13c,
  MODULO = 0x13d,
  NUM_FRACTION = 0x13e,
  NUMBER = 0x13f,
  DIGIT = 320,
  ARRAYSTDDEV = 0x141
)

getCVIRPunctuation <- function() c(
  # UNARY_PLUS = "+",# 0x37,
  # UNARY_MINUS = "-",#0x38,
  SQL_COLUMNS = "SELECT",
  SQL_TABLE_SPEC = "JOIN",
  SQL_TABLE_SPEC = "TO",
  SQL_TABLE_SPEC = "OUTER",
  SQL_TABLE_SPEC = "INNER",
  SQL_CVIR_WHERE_CONDITION = "WHERE",
  SQL_COLUMNS = "select",
  SQL_TABLE_SPEC = "join",
  SQL_TABLE_SPEC = "to",
  SQL_TABLE_SPEC = "outer",
  SQL_TABLE_SPEC = "inner",
  SQL_CVIR_WHERE_CONDITION = "where",
  DOT = ".",# 0x41,
  # COMMENT = "#", #0x42, # repurposed in lexer for DECIMAL, assuming comments have been stripped out
  COMMA = ",",# 0x45,
  COLON = ":", #0x65,
  DIVIDE = "/", #  0x6b,
  LPAREN = "(", # 0x89,
  RPAREN = ")", #0x8a,
  MINUS = "-",# 140,
  QUESTION = "?", # 150,
  EQ = "=", # 160,
  NE = "<>", # 0xa1,
  LESSTHAN = "<", # 0xa2,
  GT = ">", #0xa3,
  LE = "<=", #0xa4,
  GE = ">=", #0xa5,
  PLUS = "+", #0xa8,
  POWER = "^", #170,
  STAR = "*", #0xab,
  LSQR = "[", # 0xac,
  RSQR = "]",#0xad,
  LCURLY = "{",#0xaf,
  RCURLY = "}",#0xb0,
  QUOTE = "\"",#280,
  ASSIGNMENT = "=",#0x13b,
  MODULO = "%",#0x13d,
  SEMI = ";" #0xfc,# not sure about this one...
)
