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

parseCVIR(CVIRScript(cvir2))

parseCVIR(CVIRScript(cvir2)) |>
  attr('TSQL') |>
  soilDB::dbQueryNASIS(soilDB::NASIS(), q = _) |>
  soilDB::uncode()

# TODO:
#   - Need to fix handling of "DOT" "." for decimals... e.g. "if xericmlra == 1 then 1 else .8"
