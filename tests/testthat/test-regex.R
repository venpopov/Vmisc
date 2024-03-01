test_that("str_extract_nested_balanced works with vector input", {
 x <- c("something myFunction(x = 1, y = mean(x)) otherFunction()",
        "myFunction(x = 1, y = mean(x))",
        "something myFunction(x = 1, y = mean(x)) otherFunction(abc())",
        "myFunction myFunction(x = 1, y = mean(x)) otherFunction(abc())")
 exp <- list("myFunction(x = 1, y = mean(x))",
             "myFunction(x = 1, y = mean(x))",
             "myFunction(x = 1, y = mean(x))",
             "myFunction(x = 1, y = mean(x))")
 class(exp) <- c('attr_list', 'list')
 expect_equal(str_extract_nested_balanced(x, "myFunction", "(", ")"), exp)
})

test_that("str_extract_nested_balanced works with multiple instances per string", {
  x <- c("myFunction(x = 1, y = mean(x)) somethingelse() myFunction(y = mean(x))")
  exp <- list(c("myFunction(x = 1, y = mean(x))", "myFunction(y = mean(x))"))
  class(exp) <- c('attr_list', 'list')
  expect_equal(str_extract_nested_balanced(x, "myFunction", "(", ")"),exp)
})

