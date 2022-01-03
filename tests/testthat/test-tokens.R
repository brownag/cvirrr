
test_that("token identification works", {
  # round trip
  expect_equal(id_to_token(token_to_id(c("BASE", "TABLE", "component", "."))),
               c("BASE", "TABLE", NA, "."))
})
