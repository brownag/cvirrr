# NASIS tables

is_NASIS_table <- function(x) {
  x %in% "component" #TODO: extend
}

#' @importFrom soilDB NASIS dbQueryNASIS
get_table_columns <- function(x, dsn = NULL) {
  colnames(soilDB::dbQueryNASIS(soilDB::NASIS(dsn = dsn), paste0("SELECT TOP 1 * FROM ", x)))
}
