library(cvirrr)
library(soilDB)
library(InterpretationEngine)

cvir <- readLines("https://gist.githubusercontent.com/brownag/9e108e3b66251794556660dc1607d695/raw/47a5b916598bfc3ae62da566c0a3fbd5d20d901a/DustfromGypsumContent2to15Percent.cvir.sql", warn = FALSE)

parseCVIR <- function(cvir) {
  res <- paste0(capitalizeKeywords(cleanCVIR(cvir)), collapse = "\n")
  xhr <- strsplit(res, "")[[1]]
  blk <- cvirrr:::.lex3(cvirrr:::.lex2(cvirrr:::.lex1(xhr), cvirrr::token_to_id(xhr), xhr))
  instructions <- do.call('c', blk)
  idx <- cumsum(instructions == 65)
  idx[which(instructions == 65)] <- idx[which(instructions == 65)] - 1
  statements <- split(instructions, idx)
  statement_blocks <- split(1:length(do.call('c', statements)), cumsum(instructions == 65))
  statement_blocks <- lapply(1:length(statements), function(i) statement_blocks[[i]][statements[[i]] != 65 & statements[[i]] != 0])
  attributes(statements) <- attributes(blk)
  attr(statements, 'class') <- 'CVIRScript' # S3 class CVIRScript, a list of (named) numeric vector with attributes
  statements
}

cvir |>
  cleanCVIR() |>
  capitalizeKeywords() |> paste0("\n") |> cat()

res <- parseCVIR(cvir)

res2 <- process(res)
attr(res2,"INFORMIX_DEFINE_EXPRESSION")[[2]] |> paste(collapse = " ")

all_instructions <- do.call('c', res)
table(all_instructions)

sum(all_instructions == 220) == nrow(attr(res, 'string_literals'))
sum(all_instructions == 84) == nrow(attr(res, 'numeric_literals'))
sum(all_instructions == 97) == nrow(attr(res, 'data_identifiers'))

attr(res, 'string_literals')
attr(res, 'numeric_literals')
attr(res, 'data_identifiers')

attr(res2, "CVIR_INFORMIX_DEFINE") == names(attr(res2, "INFORMIX_DEFINE_EXPRESSION"))

co <- attr(res2, "CVIR_SQL_COLUMNS")
bt <- attr(res2, "CVIR_BASE_TABLE")
bpk <- soilDB::get_NASIS_pkey_by_name(bt)
jt <- attr(res2, "CVIR_SQL_TABLES")
jt <- jt[jt != bt]
jpk <- paste0(soilDB::get_NASIS_pkey_by_name(jt))
jpkr <- paste0(soilDB::get_NASIS_pkeyref_by_name(jt), "ref")
co <- c(co, bpk, jpk)
wh <- attr(res2, "CVIR_SQL_WHERE")
if (wh[1] == "AND") {
  wh = wh[-1]
}
if (wh[length(wh)] == ";") {
  wh <- wh[-length(wh)]
}
d <- dbQueryNASIS(NASIS(), paste0("SELECT ",
                                  paste0(co, collapse=', '),
                                  " FROM ", bt, " ",
                                  paste0(paste0("INNER JOIN ", jt, " ON ",
                                                bt, ".", bpk, " = ",
                                                jt, ".", jpkr), collapse="\n"),
                                  " WHERE ", paste0(wh, collapse=" "),
                                  " ORDER BY ", paste0(attr(res2, 'CVIR_INFORMIX_SORT_BY'), collapse=",")))
prop <- InterpretationEngine::NASIS_properties
propdef <- InterpretationEngine::NASIS_property_def

# TODO: filter on NASIS group attr(res2, "CVIR_INFORMIX_DERIVE_NASIS_GROUP")
propsub <- match(attr(res2, "CVIR_INFORMIX_DERIVE_PROPERTY_SCRIPT"), prop$propname)
to_derive <- attr(res2, "CVIR_INFORMIX_DERIVE")
deriv_prop <- lapply(seq_along(to_derive), function(i) propdef[prop[propsub[i],]$propiid == propdef$propiid,]$prop)
names(deriv_prop) <-  to_derive
deriv_prop2 <- lapply(deriv_prop, function(x) process(parseCVIR(capitalizeKeywords(cleanCVIR(x)))))
# deriv_prop2 <- lapply(deriv_prop, function(x) process(parseCVIR(capitalizeKeywords(cleanCVIR(x)))))


# serialization to binary
# x <- lapply(token_to_id(c("BASE", "TABLE")), writeBin, con = raw(), endian = "little")
# x
#
# tokenize <- function(id, con = raw()) writeBin(id, con = con, endian = "little")
#
# tokens <- lapply(instructions, tokenize)
#
# readBin(writeBin(instructions, raw(), size = 4),
#         what = "numeric", size = 4, n = length(instructions))
