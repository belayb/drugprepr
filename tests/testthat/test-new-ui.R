context('Test features of the proposed new user interface')

test_that('Example dataset loads correctly', {
  expect_equal(nrow(example_therapy), 30L)
  expect_equal(colnames(example_therapy),
               c('patid', 'pracid', 'prodcode', 'qty', 'ndd'))
})

test_that('Replacing missing quantities with NA is equivalent to identity', {
  expect_equivalent(impute_qty(example_therapy, 'ignore'),
                    identity(example_therapy))
  expect_equivalent(impute_qty(example_therapy, 'missing'),
                    identity(example_therapy))
  expect_equivalent(impute_qty(example_therapy, 'ignore', function(x) TRUE),
                    identity(example_therapy))
  expect_equivalent(impute_qty(example_therapy, 'ignore', function(x) rep(TRUE, length(x))),
                    identity(example_therapy))
})

test_that('Replacing missing numerical daily doses with NA is equivalent to identity', {
  expect_equivalent(impute_ndd(example_therapy, 'ignore'),
                    identity(example_therapy))
  expect_equivalent(impute_ndd(example_therapy, 'missing'),
                    identity(example_therapy))
  expect_equivalent(impute_ndd(example_therapy, 'ignore', function(x) TRUE),
                    identity(example_therapy))
  expect_equivalent(impute_ndd(example_therapy, 'ignore', function(x) rep(TRUE, length(x))),
                    identity(example_therapy))
})

test_that('Impute function does what it says on the tin', {
  library(dplyr)
  expect_equivalent(impute_qty(example_therapy, 'mean'),
                    example_therapy %>%
                      group_by(prodcode) %>%
                      mutate(qty = ifelse(is.na(qty), mean(qty, na.rm = TRUE), qty)))
  expect_equivalent(impute_ndd(example_therapy, 'mean'),
                    example_therapy %>%
                      group_by(prodcode) %>%
                      mutate(ndd = ifelse(is.na(ndd), mean(ndd, na.rm = TRUE), ndd)))
})
