library(cvirrr)

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

process(res)

all_instructions <- do.call('c', res)
table(all_instructions)

sum(all_instructions == 220) == nrow(attr(res, 'string_literals'))
sum(all_instructions == 84) == nrow(attr(res, 'numeric_literals'))
sum(all_instructions == 97) == nrow(attr(res, 'data_identifiers'))

attr(res, 'string_literals')
attr(res, 'numeric_literals')
attr(res, 'data_identifiers')

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
