test_that("data.frame lu works", {
  x <- 1:10
  lu <- data.frame(x = 1:26, y = letters)
  expect_equal(lookup(x, ~y, lu), letters[x])
  # also test warning for missing value
  x <- 27
  expect_warning(lookup(x, ~y, lu, verbose = TRUE), "missing")
})

test_that("character x works", {
  x <- 1:10 %>% as.character()
  lu <- data.frame(id = 1:26, y = letters)
  expect_equal(lookup(x, id ~ y, lu), letters[as.numeric(x)])
})

test_that("Rejecting remote lu works", {
  x <- 1:10
  lu <- dbplyr::memdb_frame(x = 1:26, y = letters)
  expect_error(lookup(x, ~y, lu), "data.frame")
})
