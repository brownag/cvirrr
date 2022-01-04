library(cvirrr)

cvir <- readLines("https://gist.githubusercontent.com/brownag/9e108e3b66251794556660dc1607d695/raw/47a5b916598bfc3ae62da566c0a3fbd5d20d901a/DustfromGypsumContent2to15Percent.cvir.sql", warn = FALSE)

res <- cvir |>
  cleanCVIR() |>
  capitalizeKeywords() |>
  paste0(collapse = "\n")
cat(res)

xhr <- strsplit(res, "")[[1]]
system.time({
  # reconstruct segments for assigning token IDs
  blk <- cvirrr:::.lex1(xhr)
  # # all characters should get classified
  # lapply(subset(token_info, type == "unknown")$iid, function(i) xhr[blk[[i]]])
  tkn <- cvirrr::token_to_id(xhr)
  block <- blk; token <- tkn
  blk2 <- cvirrr:::.lex2(blk, tkn, xhr)
  blk2[1:10]
  blk3 <- cvirrr:::.lex3(blk2)
  instructions <- do.call('c', blk3)
})

table(instructions)

sum(instructions == 220) == nrow(attr(blk3, 'string_literals'))
sum(instructions == 84) == nrow(attr(blk3, 'numeric_literals'))
sum(instructions == 97) == nrow(attr(blk3, 'data_identifiers'))

attr(blk3, 'string_literals')

x <- lapply(token_to_id(c("BASE", "TABLE")), writeBin, con = raw(), endian = "little")
x

tokenize <- function(id, con = raw()) writeBin(id, con = con, endian = "little")

tokens <- lapply(instructions, tokenize)

readBin(writeBin(instructions, raw(), size = 4),
        what = "numeric", size = 4, n = length(instructions))
