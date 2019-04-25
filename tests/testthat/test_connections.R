context("Access connections")

accdb_file <- "../data/testdb.accdb"
df1 <- data.frame(id = letters[1:10], values = 1:10)

test_that("Server can be started", {
  con <- odbc32::start_server(arch = .Platform$r_arch, debug = FALSE, invisible = TRUE)
  expect_is(con, "externalptr")
})

if (file.exists(accdb_file)) {
  accdb_con <- odbc32::odbcConnectAccess2007(access.file = accdb_file)
} else stop("Test data could not be found")

test_that("Connections can be opened", {
  expect_is(accdb_con, "odbc32")
  # odbc32::odbcConnectAccess2007()
})

test_that("sqlTables works", {
  table_list <- odbc32::sqlTables(accdb_con)
  expect_is(table_list, "data.frame")
})

test_that("Tables can be created", {
  res <- 
    odbc32::sqlSave(
      con = accdb_con, 
      data = df1, 
      name = "testtable1", 
      rownames = FALSE
    )
  expect_equal(res, 1)
  table_list <- odbc32::sqlTables(accdb_con)
  expect_true("testtable1" %in% table_list$TABLE_NAME)
})


test_that("Queries can be executed", {
  res <- odbc32::sqlQuery(accdb_con, query = "select * from testtable1")
  expect_is(res, "data.frame")
  expect_identical(res, df1)
})


test_that("Tables can be deleted", {
  odbc32::sqlDrop(accdb_con, name = "testtable1")
  table_list <- odbc32::sqlTables(accdb_con)
  expect_false("testtable1" %in% table_list$TABLE_NAME)
})


test_that("Connections can be closed", {
  expect_true(odbc32::odbcClose(accdb_con))
})

test_that("Server can be stopped", {
  expect_true(odbc32::stop_server())
})
