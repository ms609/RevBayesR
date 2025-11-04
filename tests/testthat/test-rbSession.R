test_that("rbSession handles missing executable", {
  rbPath <- Sys.getenv("rb.exe")
  Sys.setenv("rb.exe" = "")
  expect_error(rbSession(), "Path to RevBayes executable not specified")
  Sys.setenv("rb.exe" = rbPath)

  expect_error(rbSession(rb_path = "not_a_file"), "not_a_file does not exist")
})

test_that("rbSession can relaunch", {
  rb <- tryCatch(rbSession(), error = function(e) FALSE)
  skip_if(isFALSE(rb))
  input <- c("x = 1 + 2", "y = 3 + 4", "z = error")
  sapply(input, rb$do)
  expect_equal(rb$input(), input)
})
