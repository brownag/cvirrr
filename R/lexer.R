# lex phase 1 uses a series of regex patterns to classify individual characters into one of 4 classes
# this allows a little more context to be derived than working with individual characters
#' @importFrom data.table rbindlist
.lex1 <- function(chr) {
  y <- vector("numeric", length(chr))

  .is_whitespace <- function(x)  x %in% c("\n", " ", "\t")
  .is_identifier <- function(x) grepl("[A-Za-z_0-9]", x)
  .is_punctuation <- function(x)  grepl("[\\[\\]\\{\\}]|-|,|\\(|\\)|\\.|\\:|\\?|\\*|<|\\=|>|/|\\\"|\\;", x)
  .is_quoted <- function(x) as.logical(cumsum(x == "\"") %% 2)

  widx <- .is_whitespace(chr)
  iidx <- .is_identifier(chr)
  pidx <- .is_punctuation(chr)
  qidx <- .is_quoted(chr)
  y[widx] <- 1
  y[iidx] <- 2
  y[pidx] <- 3
  y[qidx] <- 4

  lr <- factor(y, levels = 0:4, labels = c("unknown", "whitespace", "identifier", "punctuation", "string"))
  gr <- cumsum(c(0, abs(diff(as.integer(lr))) > 0)) + 1
  res <- split(1:length(lr), f = list(gr))
  .N <- NULL
  .dt <- data.table::data.table(type = lr, group = gr)
  attr(res, 'token_info') <- as.data.frame(.dt[, list(length = .N), by = list(type, group)])

  res
}

# lex phase 2 finds blocks where all characters are keywords or multiple symbols
.lex2 <- function(block, token, symbol) {
  token_info <- attr(block, 'token_info')
  out <- vector('list', length(block))
  for (i in 1:length(block)) {
    tb <- token[block[[i]]]

    # if no tokens are matched
    if (all(is.na(tb))) {
      # combine block elements and do token lookup
      sym <- symbol[block[[i]]]
      ctb <- token_to_id(paste0(sym, collapse = ""))

      if (!is.na(ctb)) {
        # block matches a multi-character token
        tb <- ctb
      } else {
        # otherwise, just pass the character values
        tb <- sym
      }

     # some tokens are combinations of two symbols
    } else if (length(tb) == 2) {
      if (!any(is.na(tb))) {
        if (tb[1] == 162 && tb[2] == 160) {
          tb <- c(LE = 164) # LESS THAN OR EQUAL
        } else if (tb[1] == 163 && tb[2] == 160) {
          tb <- c(GE = 165) # GREATER THAN OR EQUAL
        } else if (tb[1] == 162 && tb[1] == 163) {
          tb <- c(NE = 161) # NOT EQUAL
        }
      }
    } else {
      symbol[block[[i]]]
    }
    out[[i]] <- tb
  }

  strings <- lapply(block[token_info[token_info$type == "string", ]$group],
                    function(x) paste0(symbol[x[-1]], collapse = ""))

  out[as.numeric(names(strings))] <- strings

  attr(out, 'token_info') <- token_info
  out
}

# .lex3 finds and fixes string literals (may have contained keywords; TODO: move this before lex2)
#                       numeric literals
#                   and "variables" that will need to be defined by sql query, derive statement, accept etc.
#   the blocks continue to be processed toward a list of single-value numeric bytecodes
#   strings, numeric values, and variable name references get stored with uid, group id and block ID in attributes of result
.lex3 <- function(block) {

  # first parse quotes to get string literals
  # qut <- sapply(sapply(block, function(x) x == 280), any)
  # nstr <- (sum(qut, na.rm = TRUE) / 2) # calculate number of string literals expected
  # stopifnot(nstr %% 2 == 0) # all quotes must be paired

  # # break up the list of blocks into sets that correspond to unquoted/quoted regions
  # qid <- cumsum(qut)
  # quoted <- as.logical(qid %% 2)

  # stl <- split(block, qid)
  # stl1 <- sapply(stl, function(x) sum(sapply(x, function(y) sum(y == 280))) %in% 1:2)
  #
  # quoted <- vector('logical', length(stl))
  # strings <- vector('character', nstr)
  # stridx <- vector('numeric', nstr)
  # strid <- 1
  #
  # bad.idx <- vector("list", length(stl))
  #
  # # iterate over quoted chunks, removing quote character,
  # # undoing any keywords converted in .lex2 within quoted blocks
  # for (i in seq_along(stl)) {
  #   if (stl1[i]) {
  #     idx <- which(unlist(stl[[i]]) == 280)
  #     if (length(idx) == 1) {
  #       quoted[i] <- !quoted[i - 1] # toggle quoted var
  #     } else if (length(idx) == 2) {
  #       quoted[i] <- quoted[i - 1] # keep status of quoted var (new string)
  #     }
  #     if (length(idx) > 0 & quoted[i]) {
  #       strings[strid] <- paste0(do.call('c', lapply(stl[[i]], function(x) {
  #         res <- x[x != 101 & x != 280] # TODO: handle colons
  #         if (!is.null(names(res)))
  #           return(id_to_token(res)) # TODO: avoid case change of tokens between quotes e.g. Data -> DATA
  #         res
  #       })), collapse = "")
  #       stridx[strid] <- i
  #       strid <- strid + 1
  #     } else if(length(idx) > 0 & !quoted[i]) {
  #       bad.idx[[i]] <- data.frame(idx = idx, i = i)
  #     }
  #   }
  # }

  # replace the first block in the string with a reference to string literal
  # stq <- lapply(split(qid, qid)[which(quoted)], function(x) which(x[1] == qid))
  # stq_replace <- sapply(stq, function(x) x[1])
  # stq_remove <- lapply(stq, function(x) x[2:length(x)])

  # # remove closing quotes from otherwise unquoted blocks
  # bad_quoteblk <- sapply(split(qid, qid)[do.call('rbind', bad.idx)$i],
  #                        function(x) which(x[1] == qid)[1])
  # block[bad_quoteblk] <- lapply(block[bad_quoteblk], function(x) x[x != 280])

  # data.frame of string literal references
  # string_literals <- data.frame(uid = 1:nstr, value = strings)
  # string_literals$group_id <- match(strings, unique(strings))
  # string_literals$block_id <- stq_replace

  # remove whitespace now that strings have been parsed
  token_info <- attr(block, 'token_info')
  wht <- token_info[token_info$type == "whitespace",]$group
  if (length(wht) > 0) {
    block[wht] <- list(0)[rep(1, length(wht))]
  }

  stri <- which(token_info$type == "string")
  strv <- unlist(block[stri])
  if (length(stri) > 0) {
    block[stri] <- c(`STRING_LITERAL` = 220)
    block[stri + 1] <- sapply(block[stri + 1], function(x) x[x != 280])
  }

  # #  then remove any subsequent blocks corresponding to that quoted string
  # for (i in seq_along(strings)) {
  #   x <- block[[stq_replace[i]]]
  #   block[[stq_replace[i]]] <- c(x[x != 101 & x != 280], `STRING_LITERAL` = 220)
  # }
  # block <- block[-do.call('c', stq_remove)]

  # remaining "unknowns" are assumed to be (expressions involving) identifiers for numbers or other tabular data,
  # each unique instance will be assigned a pointer that identifies their label and value as needed
  # once the source data for script have been loaded can be compared against defined data elements or evaluated as needed
  identifiers <- sapply(block, function(x) length(x) >= 1 & is.character(x))
  identifiers[!identifiers] <- NA

  # get the full names and corresponding block/identifier IDs
  all_block_ids <- which(!is.na(identifiers))
  identifiers[all_block_ids] <- 1:sum(identifiers, na.rm = TRUE)
  all_identifiers <- sapply(block[all_block_ids], paste0, collapse = "")

  # identify those that can be converted to numeric (start/end/only contain digits; TODO: decimal point?)
  numerics <- suppressWarnings(as.numeric(all_identifiers))
  is_numeric <- !is.na(numerics)
  numeric_literals <- data.frame(uid = which(is_numeric), value = numerics[is_numeric])
  numeric_literals$group_id <- match(numerics[is_numeric], unique(numerics[is_numeric]))
  numeric_literals$block_id <- all_block_ids[is_numeric]
  block[!is.na(identifiers)][is_numeric] <- c(`NUMERIC` = 84)

  # the rest of the identifers need are defined by some data reference
  data_identifiers <- data.frame(uid = which(!is_numeric), value = all_identifiers[!is_numeric])
  data_identifiers$group_id <- match(all_identifiers[!is_numeric], unique(all_identifiers[!is_numeric]))
  data_identifiers$block_id = all_block_ids[!is_numeric]
  block[!is.na(identifiers)][!is_numeric] <- c(`VARIABLE` = 97) # TODO: is this the best bytecode for these?
                                         # there are more specific things like COLUMN, SQL_COLUMN... next lex step?
  string_literals <- data.frame(
    uid = 1:length(stri) + length(is_numeric),
    value = strv,
    group_id = as.numeric(factor(strv)),
    block_id = stri
  )

  # fix names
  blockvals <- do.call('c', block)
  block <- lapply(seq_along(block), function(i) {
    blocknames <- names(block[[i]])
    blocknames[block[[i]] == 84] <- "NUMERIC"
    blocknames[block[[i]] == 97] <- "VARIABLE"
    blocknames[block[[i]] == 220] <- "STRING_LITERAL"
    names(block[[i]]) <- blocknames
    block[[i]]
  })

  # set attributes
  attr(block, 'string_literals') <- string_literals
  attr(block, 'numeric_literals') <- numeric_literals
  attr(block, 'data_identifiers') <- data_identifiers

  block
}
