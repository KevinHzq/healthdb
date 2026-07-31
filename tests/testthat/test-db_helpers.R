test_that("memdb_con() caches one connection and recovers from a closed one", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- memdb_con()
  expect_true(DBI::dbIsValid(con))
  # the same database has to be reused, otherwise tables copied by separate
  # memdb_tbl() calls would live in different databases and could not be joined
  expect_identical(memdb_con(), con)

  DBI::dbDisconnect(con)
  new_con <- memdb_con()
  expect_true(DBI::dbIsValid(new_con))
  expect_false(identical(new_con, con))
})

test_that("tables from separate memdb_tbl() calls can be joined", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db1 <- memdb_tbl(dplyr::tibble(id = 1:3, x = letters[1:3]))
  db2 <- memdb_tbl(dplyr::tibble(id = 2:4, y = LETTERS[1:3]))

  out <- dplyr::inner_join(db1, db2, by = "id") %>%
    dplyr::collect() %>%
    dplyr::arrange(id)
  expect_equal(out$id, 2:3)
  expect_equal(out$x, c("b", "c"))
  expect_equal(out$y, c("A", "B"))
})
