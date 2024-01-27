library(cvirrr)

# property script for "Xeric Biologic Climate" used in crisp eval "Xeric Biologic Climate True"
#
# (InterpretationEngine::NASIS_evaluations |>
#   subset(grepl("Xeric Biologic Climate True", evalname)))
#
#>      propiid evaliid                    evalname evaldesc
#> 9410   35953   42993 Xeric Biologic Climate True
#>                                                                                                                                                                                                                                                                                      eval
#> 9410 <?xml version="1.0" encoding="utf-16"?>\r\n<EvaluationParameter xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">\r\n  <DomainPoints />\r\n  <RangePoints />\r\n  <CrispExpression>= 1</CrispExpression>\r\n</EvaluationParameter>
#>      evaluationtype invertevaluationresults propuom propmin propmax
#> 9410          Crisp                   FALSE    <NA>       0       1
#>                   propmod propdefval               propname
#> 9410 representative value       <NA> XERIC BIOLOGIC CLIMATE


cvir <-
'base table component.

exec sql
select coiid, taxsuborder, taxsubgrp
from component;.

derive xericmlra from rv using "NSSC Data":"XERIC MLRA".

assign taxsuborder	codename(taxsuborder).
assign taxsubgrp	codename(taxsubgrp).


define rv	isnull(taxsuborder) then 0 else if
		taxsuborder imatches "xer*" then 1 else if
		taxsubgrp imatches "*xer*" then 1 else  if
		xericmlra == 1 then 1 else .8.'

cleanCVIR(cvir) |>
  capitalizeKeywords() -> cvir2

cvir2 |>
  cat(sep = "\n")
script <- CVIRScript(cvir2)
parseCVIR(script)

parseCVIR(script) |>
  attr('TSQL') #|>
  # soilDB::dbQueryNASIS(soilDB::NASIS(), q = _) |>
  # soilDB::uncode()

# get child property "XERIC MLRA" (propiid=15380)
# (
#   InterpretationEngine::NASIS_property_def |>
#     subset(propiid == 15380, select = prop)
# )[[1]] |> cat()
 
cvir3 <- 
 '#
  #Returns the MLRA identifier (symbol) as the rv.
  #
  
BASE TABLE component.

EXEC SQL
SELECT area_symbol mumlra
FROM component, datamapunit, correlation,
mapunit, lmapunit, lmuaoverlap, laoverlap, area, area_type
WHERE area_type_name = "MLRA" AND
JOIN component TO datamapunit AND
JOIN datamapunit TO correlation and
join correlation to mapunit and
join mapunit to lmapunit and
join lmapunit to lmuaoverlap and
join lmuaoverlap to laoverlap by default and
#   join lmuaoverlap to laoverlap and
join laoverlap to area and
join area to area_type;.


define rv1	isnull(mumlra) then mumlra else if
mumlra matches "2" then "Y" else if
mumlra matches "4B" then "Y" else if
mumlra matches "5" then "Y" else if
mumlra matches "6" then "Y" else if
mumlra matches "7" then "Y" else if
mumlra matches "8" then "Y" else if
mumlra matches "9" then "Y" else if
mumlra matches "10" then "Y" else if
mumlra matches "11" then "Y" else if
mumlra matches "12" then "Y" else if
mumlra matches "13" then "Y" else if
mumlra matches "14" then "Y" else if
mumlra matches "15" then "Y" else if
mumlra matches "16" then "Y" else if
mumlra matches "17" then "Y" else if
mumlra matches "18" then "Y" else if
mumlra matches "19" then "Y" else if
mumlra matches "20" then "Y" else if
mumlra matches "21" then "Y" else if
mumlra matches "22A" then "Y" else if
mumlra matches "22B" then "Y" else if
mumlra matches "23" then "Y" else if
mumlra matches "24" then "Y" else if
mumlra matches "25" then "Y" else if
mumlra matches "26" then "Y" else if
mumlra matches "28A" then "Y" else if
mumlra matches "28B" then "Y" else if
mumlra matches "29" then "Y" else if
mumlra matches "43A" then "Y" else if
mumlra matches "43B" then "Y" else if
mumlra matches "43C" then "Y" else if
mumlra matches "44" then "Y" else if
mumlra matches "44A" then "Y" else if
mumlra matches "47" then "Y" else "N".


define rv2 	isnull(rv1) then 0 else if rv1 matches "Y" then 1 else 0.
define rv	arraymax(rv2).'

cleanCVIR(cvir3) |>
  capitalizeKeywords() -> cvir4

cvir4 |>
  cat(sep = "\n")
script <- CVIRScript(cvir4)
parseCVIR(script)
# TODO: SQL aliases in CVIR_SQL_COLUMNS do not work properly
# TODO: SQL WHERE/JOIN columns and syntax are not correct for "XERIC MLRA" property