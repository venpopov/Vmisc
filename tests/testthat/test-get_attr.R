test_that("%a% gets an attribute", {
  x <- 1
  attr(x,"name") <- "ven"
  expect_equal(attr(x,"name"), x%a%name)
})
