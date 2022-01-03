# keywords

#' @export
#' @rdname Tokens
is_keyword <- function(token) {

  # This may be expanded... for now just a logical wrapper around token_to_id()

  res <- !is.na(token_to_id(token))
  names(res) <- token
  res
}
