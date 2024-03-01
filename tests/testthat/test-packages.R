test_that("extract_pkg_fun_calls works", {
  res <- extract_pkg_fun_calls('utils', 'getOption')
  expect_snapshot_value(res, style="json2")
})
