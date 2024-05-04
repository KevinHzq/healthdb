test_that("basic works", {
  expect_invisible(.onLoad())
  expect_invisible(.onUnload())
})
