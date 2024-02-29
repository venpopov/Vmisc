test_that("%+% works", {
  expect_equal('a' %+% 'b', 'ab')
  expect_equal('a' %+% 1:10, paste0('a', 1:10))

  names <- c("John", "Sarah")
  ages <- c(34,23)
  res <- names %+% " is " %+% ages %+% " years old"
  identical(res, c("John is 34 years old", "Sarah is 23 years old"))
})

test_that("%a% works", {
  x <- 1
  attr(x, "name") <- "John"
  expect_equal(attr(x, "name"), x%a%name)

  x%a%name <- "Alice"
  expect_equal(attr(x, "name"), "Alice")

  x <- 1:10
  x%A%dim <- c(2,5)
  expect_equal(dim(x), c(2,5))
})
