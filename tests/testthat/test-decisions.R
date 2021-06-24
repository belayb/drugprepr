context('Decision 1: handle implausible quantities')

all_plausible <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = 1:12,
  implausible_qty = FALSE
)

test_that('Plausible quantities are unchanged', {
  expect_equivalent(dec1_implausible_qty(all_plausible, '1a'),
                    all_plausible)
  expect_equivalent(dec1_implausible_qty(all_plausible, '1b'),
                    all_plausible)
  expect_equivalent(dec1_implausible_qty(all_plausible, '1c'),
                    all_plausible)
  expect_equivalent(dec1_implausible_qty(all_plausible, '1d'),
                    all_plausible)
  expect_equivalent(dec1_implausible_qty(all_plausible, '1e'),
                    all_plausible)
})

test_that('Throw error if decision rule is missing or unrecognised', {
  expect_error(dec1_implausible_qty())
  expect_error(dec1_implausible_qty(decision = NA))
  expect_error(dec1_implausible_qty(decision = '1'))
  expect_error(dec1_implausible_qty(decision = 'foo'))
  expect_error(dec1_implausible_qty(decision = ''))
})

some_implausible <- within(all_plausible, {
  implausible_qty <- rep(0:1, 6)
})

test_that('Decision 1a leaves returns input unchanged', {
  expect_equivalent(dec1_implausible_qty(some_implausible, '1a'),
                    some_implausible)
})
