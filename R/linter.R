# functions not so much for use in lexing/parsing by computers but rather standardizing format for humans

#' Clean a vector of CVIR expressions
#'
#' Removes comments and tabs, shortens white space, removes zero-character lines, and combines lines that do not end with `"."`
#'
#' @param cvir character vector of CVIR expressions
#'
#' @return character
#' @export
cleanCVIR <- function(cvir) {
  stopifnot(is.character(cvir))
  if (grepl("\n", cvir[1])) {
    cvir <- strsplit(cvir, "\\n")[[1]]
  }

  .rmComments <- function(x) gsub("([^#]*)#.*", "\\1", x)
  .rmTabs <- function(x) gsub("\\t", " ", x)
  .shortWhitespace <- function(x) gsub("\\s+", " ", trimws(x))
  .rmZeroLength <- function(x) x[nchar(x) > 0]

  cvir2 <- .rmZeroLength(.shortWhitespace(.rmTabs(.rmComments(cvir))))
  idx <- grep("[.;]$", cvir2)

  as.character(lapply(lapply(c(0, seq_len(length(idx) - 1)), function(i) {
    idx2 <- idx[i+1]
    if (length(idx[i]) > 0)
      idx2 <- (idx[i]+1):idx[i+1]
    do.call('c', as.list(cvir2[idx2]))
  }), paste0, collapse = " "))
}

#' Capitalize Keywords in CVIR expressions
#'
#' @param x character CVIR expressions
#'
#' @return character
#' @export
capitalizeKeywords <- function(x) {
  CVIR_TOKEN <- getCVIRTokens()
  # load(system.file("data/CVIR_TOKEN.rda", package = "cvirrr")[1])

  # TODO: this probably needs to be smarter about preserving contents of string literals
  #       to do it properly we can't just berate the raw CVIR with regex patterns
  #       need lexed to figure out context of potential keyword matches... but for now this makes scripts a bit more readable
  k <- names(CVIR_TOKEN)
  for (i in seq_along(k)) {
    x <- gsub(paste0("\\b", k[i], "\\b([^\"])"), paste0(toupper(k[i]), "\\1"), x, ignore.case = TRUE)
  }
  x
}

