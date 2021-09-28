context('Test features of the proposed new user interface')

test_that('Example dataset loads correctly', {
  expect_equal(nrow(example_therapy), 30L)
  expect_equal(colnames(example_therapy),
               c('patid', 'pracid', 'prodcode', 'qty', 'ndd'))
})

test_that('Replacing missing quantities with NA is equivalent to identity', {
  expect_equivalent(impute_qty(example_therapy, is.na, 'ignore'),
                    identity(example_therapy))
  expect_equivalent(impute_qty(example_therapy, is.na, 'missing'),
                    identity(example_therapy)) ##
  expect_equivalent(impute_qty(example_therapy, function(x) rep(TRUE, length(x)), 'ignore'),
                    identity(example_therapy))
})

test_that('Do not permit which_fun to return vector with wrong length', {
  expect_error(impute_qty(example_therapy, function(x) TRUE, 'ignore'))
  expect_error(impute_qty(example_therapy, function(x) FALSE))
})
