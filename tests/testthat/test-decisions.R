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

test_that('Decision 1a returns input unchanged', {
  expect_equivalent(dec1_implausible_qty(some_implausible, '1a'),
                    some_implausible)
})

test_that('Decision 1b sets implausible qty to missing', {
  expect_error(expect_equivalent(dec1_implausible_qty(some_implausible, '1b'),
                    some_implausible))
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, '1b')$qty)),6)
})

test_that('Decision 1c1 sets implausible qty to mean individuals prescriptions for that drug', {
  expect_equivalent(dec1_implausible_qty(some_implausible, '1c1'),
                                 some_implausible)
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, '1c1')$qty)),0)
  expect_equal(sum(dec1_implausible_qty(some_implausible, '1c1')$qty),78)
})

test_that('Decision 1c2 sets implausible qty to mean for practices prescriptions for that drug', {
  expect_error(expect_equivalent(dec1_implausible_qty(some_implausible, '1c2'),
                    some_implausible))
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, '1c2')$qty)),0)
  expect_equal(sum(dec1_implausible_qty(some_implausible, '1c2')$qty),75)
})

test_that('Decision 1c3 sets implausible qty to mean for populationss prescriptions for that drug', {
  expect_error(expect_equivalent(dec1_implausible_qty(some_implausible, '1c3'),
                                 some_implausible))
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, '1c3')$qty)),0)
  expect_equal(sum(dec1_implausible_qty(some_implausible, '1c3')$qty),75)
})

test_that('Decision 1d2 sets implausible qty to to median for practices prescriptions for that drug', {
  expect_error(expect_equivalent(dec1_implausible_qty(some_implausible, '1d2'),
                                 some_implausible))
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, '1d2')$qty)),0)
  expect_equal(sum(dec1_implausible_qty(some_implausible, '1d2')$qty),75)
})

test_that('Decision 1e2 sets implausible qty to to mode for practices prescriptions for that drug', {
  expect_error(expect_equivalent(dec1_implausible_qty(some_implausible, '1e2'),
                                 some_implausible))
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, '1e2')$qty)),0)
})


context('Decision 2: handle missing quantities')

all_available <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = 1:12
)

some_missing <- within(all_available, {
  qty[seq(1,12,2)]<-NA
})


test_that('quantities are unchanged', {
  expect_equivalent(dec2_missing_qty(all_available, c('1a','2a')),
                    all_available)
  expect_equivalent(dec2_missing_qty(all_available, c('1a', '2b1')),
                    all_available)
  expect_equivalent(dec2_missing_qty(all_available, c('1a', '2c1')),
                    all_available)
  expect_equivalent(dec2_missing_qty(all_available, c('1a', '2d1')),
                    all_available)
  expect_equivalent(dec2_missing_qty(all_available, c('1a', '2e1')),
                    all_available)
})

test_that('Throw error if decision rule is missing or unrecognised', {
  expect_error(dec2_missing_qty())
  expect_error(dec2_missing_qty(decision = c(NULL, NA)))
  expect_error(dec2_missing_qty(decision = c(NULL, '1')))
  expect_error(dec2_missing_qty(decision = c(NULL, 'foo')))
  expect_error(dec2_missing_qty(decision = c(NULL, '')))
})

test_that('Decision 2a returns input unchanged', {
  expect_equivalent(dec2_missing_qty(some_missing, c('1a','2a')),
                    some_missing)
})



context('Decision 3: handle implausible ndd')

all_plausible <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = 1:12,
  ndd = rep(1:2,6),
  implausible_ndd = FALSE
)

test_that('Plausible quantities are unchanged', {
  expect_equivalent(dec3_implausible_ndd(all_plausible, c('1a', '2a', '3a')),
                    all_plausible)
  expect_equivalent(dec3_implausible_ndd(all_plausible, c('1a', '2a', '3b')),
                    all_plausible)
  expect_equivalent(dec3_implausible_ndd(all_plausible, c('1a', '2a', '3c')),
                    all_plausible)
  expect_equivalent(dec3_implausible_ndd(all_plausible, c('1a', '2a', '3d')),
                    all_plausible)
  expect_equivalent(dec3_implausible_ndd(all_plausible, c('1a', '2a', '3e')),
                    all_plausible)
})

test_that('Throw error if decision rule is missing or unrecognised', {
  expect_error(dec3_implausible_ndd())
  expect_error(dec3_implausible_ndd(decision = NA))
  expect_error(dec3_implausible_ndd(decision = '1'))
  expect_error(dec3_implausible_ndd(decision = 'foo'))
  expect_error(dec3_implausible_ndd(decision = ''))
  expect_error(dec3_implausible_ndd(decision = c('1a', '2a', NA)))
  expect_error(dec3_implausible_ndd(decision = c('1a', '2a', NULL)))
  expect_error(dec3_implausible_ndd(decision = c('1a', '2a')))

})

some_implausible <- within(all_plausible, {
  implausible_ndd <- rep(0:1, 6)
})

test_that('Decision 3a returns input unchanged', {
  expect_equivalent(dec3_implausible_ndd(some_implausible, c('1a', '2a','3a')),
                    some_implausible)
})

test_that('Decision 3b sets implausible ndd to missing', {
  expect_error(expect_equivalent(dec3_implausible_ndd(some_implausible, c('1a', '2a','3b')),
                                 some_implausible))
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c('1a', '2a','3b'))$ndd)),6)
})

test_that('Decision 3c1 sets implausible ndd to mean individuals prescriptions for that drug', {
  expect_equivalent(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c1')),
                    some_implausible)
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c1'))$ndd)),0)
  expect_equal(sum(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c1'))$ndd),18)
})

test_that('Decision 3c2 sets implausible ndd to mean for practices prescriptions for that drug', {
  expect_error(expect_equivalent(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c2')),
                                 some_implausible))
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c2'))$ndd)),0)
  expect_equal(sum(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c2'))$ndd),15)
})

test_that('Decision 3c3 sets implausible ndd to mean for populationss prescriptions for that drug', {
  expect_error(expect_equivalent(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c3')),
                                 some_implausible))
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c3'))$ndd)),0)
  expect_equal(sum(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c3'))$ndd),15)
})

test_that('Decision 3d2 sets implausible ndd to to median for practices prescriptions for that drug', {
  expect_error(expect_equivalent(dec3_implausible_ndd(some_implausible, c('1a', '2a','3d2')),
                                 some_implausible))
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c('1a', '2a','3d2'))$ndd)),0)
  expect_equal(sum(dec3_implausible_ndd(some_implausible, c('1a', '2a','3c3'))$ndd),15)
})

test_that('Decision 3e2 sets implausible ndd to to mode for practices prescriptions for that drug', {
  expect_error(expect_equivalent(dec3_implausible_ndd(some_implausible, c('1a', '2a','3e2')),
                                 some_implausible))
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c('1a', '2a','3e2'))$ndd)),0)
})


context('Decision 4: handle missing ndd')

all_available <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = 1:12,
  ndd = rep(1:2,6),
  implausible_qty = FALSE,
  implausible_ndd = FALSE
)

some_missing <- within(all_available, {
  ndd[seq(1,12,4)]<-NA
})


test_that('Missing ndd are unchanged', {
  expect_equivalent(dec4_missing_ndd(all_available, c('1a', '2a', '3a','4a')),
                    all_available[,-c(6:7)])
  expect_equivalent(dec4_missing_ndd(all_available, c('1a', '2a', '3a','4b')),
                    all_available[,-c(6:7)])
  expect_equivalent(dec4_missing_ndd(all_available, c('1a', '2a', '3a','4c')),
                    all_available[,-c(6:7)])
  expect_equivalent(dec4_missing_ndd(all_available, c('1a', '2a', '3a','4d')),
                    all_available[,-c(6:7)])
  expect_equivalent(dec4_missing_ndd(all_available, c('1a', '2a', '3a','4e')),
                    all_available[,-c(6:7)])
})

test_that('Throw error if decision rule is missing or unrecognised', {
  expect_error(dec4_missing_ndd())
  expect_error(dec4_missing_ndd(decision = NA))
  expect_error(dec4_missing_ndd(decision = '1'))
  expect_error(dec4_missing_ndd(decision = 'foo'))
  expect_error(dec4_missing_ndd(decision = ''))
  expect_error(dec4_missing_ndd(decision = c('1a', '2a', NA)))
  expect_error(dec4_missing_ndd(decision = c('1a', '2a', NULL)))
  expect_error(dec4_missing_ndd(decision = c('1a', '2a')))
  expect_error(dec4_missing_ndd(decision = c('1a', '2a','3a','1')))
  expect_error(dec4_missing_ndd(decision = c('1a', '2a','3a','foo')))
  expect_error(dec4_missing_ndd(decision = c('1a', '2a','3a',NA)))
  expect_error(dec4_missing_ndd(decision = c('1a', '2a','3a',NULL)))

})

test_that('Decision 4a returns input unchanged', {
  expect_equivalent(dec4_missing_ndd(some_missing, c('1a', '2a','3a','4a')),
                    some_missing[-c(6:7)])
})

test_that('Decision 4b1 sets missing ndd to mean ndd of same prodcode and patid ', {
  expect_error(expect_equivalent(dec4_missing_ndd(some_missing, c('1a', '2a','3a','4b1')),
                    some_missing[-c(6:7)]))
})

test_that('Decision 4b2 sets missing ndd to mean ndd of same prodcode and pracid ', {
  expect_error(expect_equivalent(dec4_missing_ndd(some_missing, c('1a', '2a','3a','4b2')),
                                 some_missing[-c(6:7)]))
})

test_that('Decision 4b3 sets missing ndd to mean ndd of same prodcode', {
  expect_error(expect_equivalent(dec4_missing_ndd(some_missing, c('1a', '2a','3a','4b3')),
                                 some_missing[-c(6:7)]))
})


context('Decision 5: clean duration')

all_acceptable <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = rep(30,12),
  ndd = rep(2:3,6),
  implausible_qty = FALSE,
  implausible_ndd = FALSE,
  numdays = rep(10,12),
  dose_duration = rep(10,12)

)

test_that('Throw error if decision rule is missing or unrecognised', {
  expect_error(dec5_clean_duration())
  expect_error(dec5_clean_duration(decision = NA))
  expect_error(dec5_clean_duration(decision = '1'))
  expect_error(dec5_clean_duration(decision = 'foo'))
  expect_error(dec5_clean_duration(decision = ''))
  expect_error(dec5_clean_duration(decision = c('1a', '2a','3a','4a' ,NA)))
  expect_error(dec5_clean_duration(decision = c('1a', '2a','3a','4a', NULL)))
  expect_error(dec5_clean_duration(decision = c('1a', '2a','3a','4a')))
  expect_error(dec5_clean_duration(decision = c('1a', '2a','3a','4a','1')))
  expect_error(dec5_clean_duration(decision = c('1a', '2a','3a','4a','foo')))

})

some_notacceptable <- within(all_acceptable, {
  qty[seq(1,12,4)]<-1000
})

test_that('Decision 5b_6 sets new duration to NA if it is greater than 6 month',{
  expect_equal(sum(is.na(dec5_clean_duration(some_notacceptable,decision = c('1a', '2a','3a','4a','5b_6'))$new_duration)),3)
})

test_that('Decision 5b_12 sets new duration to NA if it is greater than 12 month',{
  expect_equal(sum(is.na(dec5_clean_duration(some_notacceptable,decision = c('1a', '2a','3a','4a','5b_12'))$new_duration)),3)
})

test_that('Decision 5b_24 sets new duration to NA if it is greater than 24 month',{
  expect_equal(sum(is.na(dec5_clean_duration(some_notacceptable,decision = c('1a', '2a','3a','4a','5b_24'))$new_duration)),3)
})


test_that('Decision 5c_6 sets new duration to 6 month if it is greater than 6 month',{
  expect_equal(sum(dec5_clean_duration(some_notacceptable,decision = c('1a', '2a','3a','4a','5c_6'))$new_duration),645)
})

test_that('Decision 5c_12 sets new duration to 12 month if it is greater than 12 month',{
  expect_equal(sum(dec5_clean_duration(some_notacceptable,decision = c('1a', '2a','3a','4a','5c_12'))$new_duration),1200)
})

test_that('Decision 5c_24 sets new duration to 24 month if it is greater than 24 month',{
  expect_equal(sum(dec5_clean_duration(some_notacceptable,decision = c('1a', '2a','3a','4a','5c_24'))$new_duration),2295)
})
