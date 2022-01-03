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
  CVIR_TOKEN <- NULL
  CVIR_PUNCTUATION <- NULL
  load(system.file("data/CVIR_TOKEN.rda", package = "cvirrr")[1])
  res <- CVIR_TOKEN[toupper(trimws(token))]
  if (any(is.na(res))) {
    idx <- which(is.na(res))
    load(system.file("data/CVIR_PUNCTUATION.rda", package = "cvirrr")[1])
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
  CVIR_TOKEN <- NULL
  CVIR_PUNCTUATION <- NULL
  load(system.file("data/CVIR_TOKEN.rda", package = "cvirrr")[1])
  idx <- match(id, CVIR_TOKEN)
  res <- names(CVIR_TOKEN)[idx]
  if (!convert_punctuation) return(res)
  load(system.file("data/CVIR_PUNCTUATION.rda", package = "cvirrr")[1])
  idx2 <- which(res %in% names(CVIR_PUNCTUATION))
  res[idx2] <- CVIR_PUNCTUATION[res[idx2]]
  # names(res) <- id
  res
}


