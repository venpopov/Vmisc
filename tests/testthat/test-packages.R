test_that("extract_pkg_fun_calls works", {
  res <- extract_pkg_fun_calls("utils", "getOption")
  expect_snapshot_value(res, style = "json2")
})

test_that("parse_pkg_version works", {
  expect_error(parse_pkg_version(brms("2.20.0a")), "Invalid version")
  expect_error(parse_pkg_version(brms("2.20")), "Invalid version")
  expect_error(parse_pkg_version(brms("2.20.0-a")), "Invalid version")
  expect_error(parse_pkg_version(brms("2.20.0.")), "Invalid version")
  expect_equal(parse_pkg_version(utils), list(
    names = "utils",
    versions = package_version(NA, strict = FALSE)
  ))
  expect_equal(parse_pkg_version(brms("2.20.4")), list(
    names = "brms",
    versions = package_version("2.20.4")
  ))
  expect_equal(
    parse_pkg_version(brms("2.20.4"), bmm("0.4-0")),
    list(
      names = c("brms", "bmm"),
      versions = package_version(c("2.20.4", "0.4.0"))
    )
  )
})

test_that("pkg_vavailable works", {
  utilsver <- packageVersion("utils")
  statsver <- packageVersion("stats")
  expect_equal(pkg_vavailable(stats, utils, xfun("7.0.0")), list(
    available = c(TRUE, TRUE, FALSE),
    pkg_name = c("stats", "utils", "xfun"),
    pkg_version = c(statsver, utilsver, package_version("7.0.0")),
    pkg_version_specified = c(
      package_version(NA, strict = FALSE),
      package_version(NA, strict = FALSE),
      package_version("7.0.0")
    ),
    pkg_folder = c("stats", "utils", "xfun-7.0.0"),
    path = c(
      file.path(.libPaths()[2], "stats"),
      file.path(.libPaths()[2], "utils"),
      file.path(.libPaths()[1], "xfun-7.0.0")
    )
  ))
})

test_that("require_pkg works", {
  expect_error(require_pkg(brms("10.1.1")), "version 10.1.1 or higher")
  expect_error(require_pkg(bgdfrms("10.1.1")), "version 10.1.1 or higher")
  expect_error(require_pkg(brms("10.1.1"), bmm("8.4.0")), "version 8.4.0 or higher")
  expect_error(require_pkg(brgsms("10.1.1"), bmgfdm("8.4.0")), "version 8.4.0 or higher")
  expect_silent(require_pkg(stats))
  expect_silent(require_pkg("stats"))
  expect_silent(require_pkg(stats, utils))
  expect_silent(require_pkg(stats("0.0.1")))
  expect_error(require_pkg(stats("10.0.1")))
})
