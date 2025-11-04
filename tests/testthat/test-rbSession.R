test_that("rbSession handles missing executable", {
  rbPath <- Sys.getenv("rb.exe")
  Sys.setenv("rb.exe" = "")
  expect_error(rbSession(), "Path to RevBayes executable not specified")
  Sys.setenv("rb.exe" = rbPath)

  expect_error(rbSession(rb_path = "not_a_file"), "not_a_file does not exist")
})
