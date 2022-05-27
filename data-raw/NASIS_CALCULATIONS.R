# NASIS Calculations
NASIS_CALCULATIONS <- soilDB::dbQueryNASIS(NASIS(), "SELECT * FROM calculation")
usethis::use_data(NASIS_CALCULATIONS, overwrite = TRUE)
