## code to prepare `CVIR_TOKEN` dataset goes here
CVIR_TOKEN <- cvirrr:::getCVIRTokens()
CVIR_PUNCTUATION <- cvirrr:::getCVIRPunctuation()
usethis::use_data(CVIR_TOKEN, overwrite = TRUE)
usethis::use_data(CVIR_PUNCTUATION, overwrite = TRUE)
