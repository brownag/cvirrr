
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cvirrr

<!-- badges: start -->

[![R-CMD-check](https://github.com/brownag/cvirrr/workflows/R-CMD-check/badge.svg)](https://github.com/brownag/cvirrr/actions)
[![cvirrr
Manual](https://img.shields.io/badge/docs-HTML-informational)](http://humus.rocks/cvirrr/)
<!-- badges: end -->

The primary goal of {cvirrr} is to evaluate Calculation Validation
Interpretation Report (CVIR) scripts outside of [National Soil
Information System
(NASIS)](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/tools/?cid=nrcs142p2_053554).
NASIS makes use of the [ANTLR](https://www.antlr.org/) (ANother Tool for
Language Recognition) parser generator to implement conversion of CVIR
to SQL internally.

Methods in {cvirrr} handle lexing and parsing of CVIR outside of
NASIS/ANTLR. Scripts are pre-processed, tokenized and parsed into an
Abstract Syntax Tree. The Abstract Syntax Tree can then be evaluated as
a combination of R code and SQL.

## What is CVIR?

CVIR is the scripting language for NASIS Calculations, Validations,
Interpretations, and Reports. CVIR is used in the latest versions of
NASIS and Web Soil Survey Rule and Report Manager.

The [NASIS CVIR Language
Manual](https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/nrcs142p2_053305.pdf)
provides details on CVIR syntax.

The language implements a mixture of syntax from SQL and Informix to
facilitate doing calculations on data sources (generally queried from
the database) and generating structured reports. The syntax is
“familiar,” but unique, and NASIS is currently the only tool that can
evaluate CVIR scripts.

## Motivation

A significant amount of the business logic of the Soil and Plant Science
Division is encoded in CVIR. While the NASIS interface is a powerful and
unique platform for data analysis and managing CVIR scripts, there would
be significant benefit to being able to derive information from and
process CVIR scripts using R. For instance this would allow us to more
readily re-use internal NASIS logic with external data sources, more
readily conduct automated/batch analyses, and would provide
opportunities for enhanced graphics and formatting.

There is an immediate interest in being able to evaluate CVIR from R in
order to dynamically derive inputs for the new
[ncss-tech/InterpretationEngine](http://github.com/ncss-tech/interpretation-engine)
package. {cvirrr} is intended to be lightweight and focused; implemented
as a layer beneath the functionality planned to be provided by
InterpretationEngine.

## Installation

You can install the latest version of {cvirrr} from GitHub using the
{remotes} package

``` r
# install.packages("remotes")
remotes::install_github("brownag/cvirrr")
```

## Example

For an example, we will load a CVIR script

``` r
library(cvirrr)

cvir <- readLines("https://gist.githubusercontent.com/brownag/9e108e3b66251794556660dc1607d695/raw/47a5b916598bfc3ae62da566c0a3fbd5d20d901a/DustfromGypsumContent2to15Percent.cvir.sql", warn = FALSE)
```

`cleanCVIR()` is how CVIR scripts are pre-processed to remove
non-essential markup and get expressions on a single line.

### Before

Initially we have a CVIR script with comments, white space, and
statements spanning multiple lines.

``` r
cat(cvir, sep = "\n")
#> base table component.
#> 
#> # Retrieves the weighted average gypsum the surface to 50cm or to a restrictive layer.  The weighted average gypsum is for that portion of each horizon in the depth range.
#>  
#> EXEC SQL select hzdept_r, hzdepb_r, gypsum_l, gypsum_r, gypsum_h
#>     from component,  chorizon
#>     where join component to chorizon and 
#>     hzdepb_r > hzdept_r;
#>     SORT BY hzdept_r
#>     AGGREGATE column  hzdept_r none, hzdepb_r none, gypsum_l none, gypsum_r none, gypsum_h none.
#> 
#> # Determine the depth to RESTRICTIVE LAYER.
#> # Determine the LAYER THICKNESS IN RANGE; ABOVE A RESTRICTIVE LAYER.
#> DERIVE  layer_thickness from  rv using "NSSC Data":"LAYER THICKNESS IN RANGE; ABOVE VSTR RESTRICT BELOW O" (0,50).
#> DERIVE depth from rv using "NSSC Data":"DEPTH TO FIRST STR/VSTR CEMENTED BELOW ORGANIC LAYER".
#> DERIVE o_thickness from rv using "NSSC Pangaea":"THICKNESS OF SURFACE ORGANIC HORIZON".
#> 
#> # Find minimum of restriction depth and 200cm 
#> DEFINE min_depth  depth < 250 and not isnull(depth) ? depth : 250.
#> DEFINE in_range   isnull (hzdepb_r) ? hzdepb_r :
#>        (hzdepb_r  - o_thickness <= min_depth ? 1 : hzdepb_r - hzdept_r >= min_depth ? 1 : 0).
#>        
#> 
#> ASSIGN gypsum_l      isnull(gypsum_l) ? 0 : gypsum_l.
#> ASSIGN gypsum_r      isnull(gypsum_r) ? 0 : gypsum_r.
#> ASSIGN gypsum_h      isnull(gypsum_h) ? 0 : gypsum_h.
#> 
#> DEFINE default 0*layer_thickness.
#>  
#> # Find the weighted average #10 sieve in the depth 0-100cm.
#> 
#> 
#> define low       wtavg((if hzdepb_r - o_thickness <=0 THEN default ELSE lookup(1, in_range, gypsum_l)), layer_thickness).
#> define rv        wtavg((if hzdepb_r - o_thickness <=0 THEN default ELSE lookup(1, in_range, gypsum_r)), layer_thickness).
#> define high     wtavg((if hzdepb_r - o_thickness <=0 THEN default ELSE lookup(1, in_range, gypsum_h)), layer_thickness).
```

### After

After “cleaning” we have the same script with comments removed, white
space reduced and individual statements collapsed into single lines.

``` r
cleanCVIR(cvir) |>
  cat(sep = "\n")
#> base table component.
#> EXEC SQL select hzdept_r, hzdepb_r, gypsum_l, gypsum_r, gypsum_h from component, chorizon where join component to chorizon and hzdepb_r > hzdept_r;
#> SORT BY hzdept_r AGGREGATE column hzdept_r none, hzdepb_r none, gypsum_l none, gypsum_r none, gypsum_h none.
#> DERIVE layer_thickness from rv using "NSSC Data":"LAYER THICKNESS IN RANGE; ABOVE VSTR RESTRICT BELOW O" (0,50).
#> DERIVE depth from rv using "NSSC Data":"DEPTH TO FIRST STR/VSTR CEMENTED BELOW ORGANIC LAYER".
#> DERIVE o_thickness from rv using "NSSC Pangaea":"THICKNESS OF SURFACE ORGANIC HORIZON".
#> DEFINE min_depth depth < 250 and not isnull(depth) ? depth : 250.
#> DEFINE in_range isnull (hzdepb_r) ? hzdepb_r : (hzdepb_r - o_thickness <= min_depth ? 1 : hzdepb_r - hzdept_r >= min_depth ? 1 : 0).
#> ASSIGN gypsum_l isnull(gypsum_l) ? 0 : gypsum_l.
#> ASSIGN gypsum_r isnull(gypsum_r) ? 0 : gypsum_r.
#> ASSIGN gypsum_h isnull(gypsum_h) ? 0 : gypsum_h.
#> DEFINE default 0*layer_thickness.
#> define low wtavg((if hzdepb_r - o_thickness <=0 THEN default ELSE lookup(1, in_range, gypsum_l)), layer_thickness).
#> define rv wtavg((if hzdepb_r - o_thickness <=0 THEN default ELSE lookup(1, in_range, gypsum_r)), layer_thickness).
#> define high wtavg((if hzdepb_r - o_thickness <=0 THEN default ELSE lookup(1, in_range, gypsum_h)), layer_thickness).
```
