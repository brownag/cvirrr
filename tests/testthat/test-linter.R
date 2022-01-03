test_that("cleaning CVIR code for humans works", {
  cvir <- "# set the primary table property is generated for
           base table component.
           # evaluate SQL to build the dataset the script operates on
            exec   SQL SELECT compname, hzdept_r, hzdepb_r, hzname,
                              claytotal_r from component, chorizon
   WHERE JOIN component TO chorizon and hzdept_r > hzdepb_r;
           # TODOs:
           # do some sorting and aggregation
           # derive values from other required problems
           # do some calculations
           # return a 'fuzzy set' (low, rv, high)"
  cvir2 <- capitalizeKeywords(cleanCVIR(cvir))
  expect_length(cvir2, 2)
})
