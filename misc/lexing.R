library(cvirrr)
library(soilDB)
library(InterpretationEngine)

cvir <- readLines("https://gist.githubusercontent.com/brownag/9e108e3b66251794556660dc1607d695/raw/47a5b916598bfc3ae62da566c0a3fbd5d20d901a/DustfromGypsumContent2to15Percent.cvir.sql", warn = FALSE)

cvir |>
  cleanCVIR() |>
  capitalizeKeywords() |> cat(sep = "\n")

res <- CVIRScript(cvir)
# attr(res, 'TSQL') |> (\(x) dbQueryNASIS(NASIS(), x))()

all_instructions <- do.call('c', res)
table(all_instructions)
q <- attr(res, 'TSQL')
cat(q)
d <- dbQueryNASIS(NASIS(), q)
prop <- InterpretationEngine::NASIS_properties
propdef <- InterpretationEngine::NASIS_property_def

# TODO: filter on NASIS group attr(res2, "CVIR_INFORMIX_DERIVE_NASIS_GROUP")

# DERIVE statements (properties in properties)
propsub <- match(attr(res, "CVIR_INFORMIX_DERIVE_PROPERTY_SCRIPT"), prop$propname)
to_derive <- attr(res, "CVIR_INFORMIX_DERIVE")
deriv_prop <- lapply(seq_along(to_derive), function(i) propdef[prop[propsub[i],]$propiid == propdef$propiid,]$prop)
names(deriv_prop) <-  to_derive
deriv_prop2 <- lapply(deriv_prop, function(x) CVIRScript(x))

lapply(deriv_prop2, function(x) attr(x, 'TSQL')  |> (\(x) dbQueryNASIS(NASIS(), x))())

# process(parseCVIR(capitalizeKeywords(cleanCVIR(deriv_prop[[3]]))))

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
