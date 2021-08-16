context("Decision 1: handle implausible quantities")

all_plausible <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = 1:12,
  implausible_qty = FALSE
)

test_that("Plausible quantities are unchanged", {
  expect_equivalent(
    dec1_implausible_qty(all_plausible, "1a"),
    all_plausible
  )
  expect_equivalent(
    dec1_implausible_qty(all_plausible, "1b"),
    all_plausible
  )
  expect_equivalent(
    dec1_implausible_qty(all_plausible, "1c"),
    all_plausible
  )
  expect_equivalent(
    dec1_implausible_qty(all_plausible, "1d"),
    all_plausible
  )
  expect_equivalent(
    dec1_implausible_qty(all_plausible, "1e"),
    all_plausible
  )
})

test_that("Throw error if decision rule is missing or unrecognised", {
  expect_error(dec1_implausible_qty())
  expect_error(dec1_implausible_qty(decision = NA))
  expect_error(dec1_implausible_qty(decision = "1"))
  expect_error(dec1_implausible_qty(decision = "foo"))
  expect_error(dec1_implausible_qty(decision = ""))
})

some_implausible <- within(all_plausible, {
  implausible_qty <- rep(0:1, 6)
})

test_that("Decision 1a returns input unchanged", {
  expect_equivalent(
    dec1_implausible_qty(some_implausible, "1a"),
    some_implausible
  )
})

test_that("Decision 1b sets implausible qty to missing", {
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, "1b")$qty)), 6)
})

test_that("Decision 1c1 sets implausible qty to mean individuals prescriptions for that drug", {
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, "1c1")$qty)), 0)
  expect_equal(sum(dec1_implausible_qty(some_implausible, "1c1")$qty), 78)
})

test_that("Decision 1c2 sets implausible qty to mean for practices prescriptions for that drug", {
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, "1c2")$qty)), 0)
  expect_equal(sum(dec1_implausible_qty(some_implausible, "1c2")$qty), 75)
})

test_that("Decision 1c3 sets implausible qty to mean for populationss prescriptions for that drug", {
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, "1c3")$qty)), 0)
  expect_equal(sum(dec1_implausible_qty(some_implausible, "1c3")$qty), 75)
})

test_that("Decision 1d2 sets implausible qty to to median for practices prescriptions for that drug", {
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, "1d2")$qty)), 0)
  expect_equal(sum(dec1_implausible_qty(some_implausible, "1d2")$qty), 75)
})

test_that("Decision 1e2 sets implausible qty to to mode for practices prescriptions for that drug", {
  expect_equal(sum(is.na(dec1_implausible_qty(some_implausible, "1e2")$qty)), 0)
})


context("Decision 2: handle missing quantities")

all_available <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = 1:12
)

some_missing <- within(all_available, {
  qty[seq(1, 12, 4)] <- NA
})


test_that("quantities are unchanged", {
  expect_equivalent(
    dec2_missing_qty(all_available, c("1a", "2a")),
    all_available
  )
  expect_equivalent(
    dec2_missing_qty(all_available, c("1a", "2b1")),
    all_available
  )
  expect_equivalent(
    dec2_missing_qty(all_available, c("1a", "2c1")),
    all_available
  )
  expect_equivalent(
    dec2_missing_qty(all_available, c("1a", "2d1")),
    all_available
  )

})

test_that("Throw error if decision rule is missing or unrecognised", {
  expect_error(dec2_missing_qty())
  expect_error(dec2_missing_qty(decision = c(NULL, NA)))
  expect_error(dec2_missing_qty(decision = c(NULL, "1")))
  expect_error(dec2_missing_qty(decision = c(NULL, "foo")))
  expect_error(dec2_missing_qty(decision = c(NULL, "")))
})

test_that("Decision 2a returns input unchanged", {
  expect_equivalent(
    dec2_missing_qty(some_missing, c("1a", "2a")),
    some_missing
  )
})


test_that("Decision 2b1 sets missing qty to mean qty of same prodcode and patid ", {
  sum_qty_orginal = sum(some_missing$qty,na.rm=T)
  sum_qty_dec_2b1 = sum(dec2_missing_qty(some_missing, c("1a", "2b1"))$qty,na.rm=T)
  expect_equal(sum_qty_orginal,sum_qty_dec_2b1)
}) # For now this is ok, since the mean value is undefined

test_that("Decision 2b2 sets missing qty to mean qty of same prodcode and pracid ", {
  sum_qty_dec_2b2 = sum(dec2_missing_qty(some_missing, c("1a", "2b2"))$qty,na.rm=T)
  expect_equal(sum_qty_dec_2b2, 81)
}) # This should have passed the test. Missing qty can be fully replaced by mean values by prodcode and pracid if it was working.

test_that("Decision 2b3 sets missing qty to mean qty of same prodcode", {
  sum_qty_dec_2b3 = sum(dec2_missing_qty(some_missing, c("1a", "2b2"))$qty,na.rm=T)
  expect_equal(sum_qty_dec_2b3, 84)

}) # This should have passed the test and all missing qty replaced by 7




context("Decision 3: handle implausible ndd")

all_plausible <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = 1:12,
  ndd = rep(1:2, 6),
  implausible_ndd = FALSE
)

test_that("Plausible quantities are unchanged", {
  expect_equivalent(
    dec3_implausible_ndd(all_plausible, c("1a", "2a", "3a")),
    all_plausible
  )
  expect_equivalent(
    dec3_implausible_ndd(all_plausible, c("1a", "2a", "3b")),
    all_plausible
  )
  expect_equivalent(
    dec3_implausible_ndd(all_plausible, c("1a", "2a", "3c")),
    all_plausible
  )
  expect_equivalent(
    dec3_implausible_ndd(all_plausible, c("1a", "2a", "3d")),
    all_plausible
  )
  expect_equivalent(
    dec3_implausible_ndd(all_plausible, c("1a", "2a", "3e")),
    all_plausible
  )
})

test_that("Throw error if decision rule is missing or unrecognised", {
  expect_error(dec3_implausible_ndd())
  expect_error(dec3_implausible_ndd(decision = NA))
  expect_error(dec3_implausible_ndd(decision = "1"))
  expect_error(dec3_implausible_ndd(decision = "foo"))
  expect_error(dec3_implausible_ndd(decision = ""))
  expect_error(dec3_implausible_ndd(decision = c("1a", "2a", NA)))
  expect_error(dec3_implausible_ndd(decision = c("1a", "2a", NULL)))
  expect_error(dec3_implausible_ndd(decision = c("1a", "2a")))
})

some_implausible <- within(all_plausible, {
  implausible_ndd <- rep(0:1, 6)
})

test_that("Decision 3a returns input unchanged", {
  expect_equivalent(
    dec3_implausible_ndd(some_implausible, c("1a", "2a", "3a")),
    some_implausible
  )
})

test_that("Decision 3b sets implausible ndd to missing", {
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3b"))$ndd)), 6)
})

test_that("Decision 3c1 sets implausible ndd to mean individuals prescriptions for that drug", {
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3c1"))$ndd)), 0)
  expect_equal(sum(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3c1"))$ndd), 18)
})

test_that("Decision 3c2 sets implausible ndd to mean for practices prescriptions for that drug", {
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3c2"))$ndd)), 0)
  expect_equal(sum(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3c2"))$ndd), 15)
})

test_that("Decision 3c3 sets implausible ndd to mean for populationss prescriptions for that drug", {
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3c3"))$ndd)), 0)
  expect_equal(sum(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3c3"))$ndd), 15)
})

test_that("Decision 3d2 sets implausible ndd to to median for practices prescriptions for that drug", {
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3d2"))$ndd)), 0)
  expect_equal(sum(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3c3"))$ndd), 15)
})

test_that("Decision 3e2 sets implausible ndd to to mode for practices prescriptions for that drug", {
  expect_equal(sum(is.na(dec3_implausible_ndd(some_implausible, c("1a", "2a", "3e2"))$ndd)), 0)
})


context("Decision 4: handle missing ndd")

all_available <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = 1:12,
  ndd = rep(1:2, 6),
  implausible_qty = FALSE,
  implausible_ndd = FALSE
)

some_missing <- within(all_available, {
  ndd[seq(1, 12, 4)] <- NA
})


test_that("Missing ndd are unchanged", {
  expect_equivalent(
    dec4_missing_ndd(all_available, c("1a", "2a", "3a", "4a")),
    all_available[, -c(6:7)]
  )
  expect_equivalent(
    dec4_missing_ndd(all_available, c("1a", "2a", "3a", "4b")),
    all_available[, -c(6:7)]
  )
  expect_equivalent(
    dec4_missing_ndd(all_available, c("1a", "2a", "3a", "4c")),
    all_available[, -c(6:7)]
  )
  expect_equivalent(
    dec4_missing_ndd(all_available, c("1a", "2a", "3a", "4d")),
    all_available[, -c(6:7)]
  )
})

test_that("Throw error if decision rule is missing or unrecognised", {
  expect_error(dec4_missing_ndd())
  expect_error(dec4_missing_ndd(decision = NA))
  expect_error(dec4_missing_ndd(decision = "1"))
  expect_error(dec4_missing_ndd(decision = "foo"))
  expect_error(dec4_missing_ndd(decision = ""))
  expect_error(dec4_missing_ndd(decision = c("1a", "2a", NA)))
  expect_error(dec4_missing_ndd(decision = c("1a", "2a", NULL)))
  expect_error(dec4_missing_ndd(decision = c("1a", "2a")))
  expect_error(dec4_missing_ndd(decision = c("1a", "2a", "3a", "1")))
  expect_error(dec4_missing_ndd(decision = c("1a", "2a", "3a", "foo")))
  expect_error(dec4_missing_ndd(decision = c("1a", "2a", "3a", NA)))
  expect_error(dec4_missing_ndd(decision = c("1a", "2a", "3a", NULL)))
})

test_that("Decision 4a returns input unchanged", {
  expect_equivalent(
    dec4_missing_ndd(some_missing, c("1a", "2a", "3a", "4a")),
    some_missing[-c(6:7)]
  )
})

test_that("Decision 4b1 sets missing ndd to mean ndd of same prodcode and patid ", {
  sum_ndd_dec_4b1 = sum(dec4_missing_ndd(some_missing, c("1a", "2a", "3a", "4b1"))$ndd,na.rm=T)
  expect_equal(sum_ndd_dec_4b1,15)

}) # This is ok as the mean ndd is undefind given the data

test_that("Decision 4b2 sets missing ndd to mean ndd of same prodcode and pracid ", {
  sum_ndd_dec_4b2 = sum(dec4_missing_ndd(some_missing, c("1a", "2a", "3a", "4b2"))$ndd,na.rm=T)
  expect_equal(sum_ndd_dec_4b2,21)
}) # This should have passed the test and all missing ndd replaced by mean values by prodcode and pracid which 2 giving a sum of ndd = 21.

test_that("Decision 4b3 sets missing ndd to mean ndd of same prodcode", {
  sum_ndd_dec_4b3 = sum(dec4_missing_ndd(some_missing, c("1a", "2a", "3a", "4b3"))$ndd,na.rm=T)
  expect_equal(sum_ndd_dec_4b3,20)
}) # This should have passed the test and all missing ndd replaced by mean values by prodcode which is 1.67


context("Decision 5: clean duration")

all_acceptable <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = rep(30, 12),
  ndd = rep(2:3, 6),
  implausible_qty = FALSE,
  implausible_ndd = FALSE,
  numdays = rep(10, 12),
  dose_duration = rep(10, 12)
)

test_that("Throw error if decision rule is missing or unrecognised", {
  expect_error(dec5_clean_duration())
  expect_error(dec5_clean_duration(decision = NA))
  expect_error(dec5_clean_duration(decision = "1"))
  expect_error(dec5_clean_duration(decision = "foo"))
  expect_error(dec5_clean_duration(decision = ""))
  expect_error(dec5_clean_duration(decision = c("1a", "2a", "3a", "4a", NA)))
  expect_error(dec5_clean_duration(decision = c("1a", "2a", "3a", "4a", NULL)))
  expect_error(dec5_clean_duration(decision = c("1a", "2a", "3a", "4a")))
  expect_error(dec5_clean_duration(decision = c("1a", "2a", "3a", "4a", "1")))
  expect_error(dec5_clean_duration(decision = c("1a", "2a", "3a", "4a", "foo")))
})

some_notacceptable <- within(all_acceptable, {
  qty[seq(1, 12, 4)] <- 1000
})

test_that("Decision 5b_6 sets new duration to NA if it is greater than 6 month", {
  expect_equal(sum(is.na(dec5_clean_duration(some_notacceptable, decision = c("1a", "2a", "3a", "4a", "5b_6"))$new_duration)), 3)
}) # 3 of the new duration values should have been set to NA

test_that("Decision 5b_12 sets new duration to NA if it is greater than 12 month", {
  expect_equal(sum(is.na(dec5_clean_duration(some_notacceptable, decision = c("1a", "2a", "3a", "4a", "5b_12"))$new_duration)), 3)
})

test_that("Decision 5b_24 sets new duration to NA if it is greater than 24 month", {
  expect_equal(sum(is.na(dec5_clean_duration(some_notacceptable, decision = c("1a", "2a", "3a", "4a", "5b_24"))$new_duration)), 0)
})


test_that("Decision 5c_6 sets new duration to 6 month if it is greater than 6 month", {
  expect_equal(sum(dec5_clean_duration(some_notacceptable, decision = c("1a", "2a", "3a", "4a", "5c_6"))$new_duration), 651)
}) # 3 of the new duration values should have been replaced to 6 month

test_that("Decision 5c_12 sets new duration to 12 month if it is greater than 12 month", {
  expect_equal(sum(dec5_clean_duration(some_notacceptable, decision = c("1a", "2a", "3a", "4a", "5c_12"))$new_duration), 1200)
})

test_that("Decision 5c_24 sets new duration to 24 month if it is greater than 24 month", {
  expect_equal(sum(dec5_clean_duration(some_notacceptable, decision = c("1a", "2a", "3a", "4a", "5c_24"))$new_duration), 1605)
}) # 3 of the new duration values was supposed to be replaced by 24 months now they are set to 182 days


context("Decision 6: Select stop date")

start_stop <- data.frame(
  patid = rep(1:4, each = 3),
  prodcode = rep(1:3, times = 4),
  pracid = rep(1:2, each = 6),
  qty = rep(30, 12),
  ndd = rep(2:3, 6),
  implausible_qty = FALSE,
  implausible_ndd = FALSE,
  numdays = rep(10, 12),
  dose_duration = rep(10, 12),
  event_date = rep(c(lubridate::dmy("15/05/2021", "30/05/2021", "15/06/2021")), 4)
)

test_that("Throw error if decision rule is missing or unrecognised", {
  expect_error(dec6_select_stop_date())
  expect_error(dec6_select_stop_date(decision = NA))
  expect_error(dec6_select_stop_date(decision = "1"))
  expect_error(dec6_select_stop_date(decision = "foo"))
  expect_error(dec6_select_stop_date(decision = ""))
  expect_error(dec6_select_stop_date(decision = c("1a", "2a", "3a", "4a", "5a", NA)))
  expect_error(dec6_select_stop_date(decision = c("1a", "2a", "3a", "4a", "5a", NULL)))
  expect_error(dec6_select_stop_date(decision = c("1a", "2a", "3a", "4a", "5a")))
  expect_error(dec6_select_stop_date(decision = c("1a", "2a", "3a", "4a", "5a", "1")))
  expect_error(dec6_select_stop_date(decision = c("1a", "2a", "3a", "4a", "5a", "foo")))
})

test_that("Decision 6a sets real_stopdate to start+numdays", {
  expect_equal(with(dec6_select_stop_date(start_stop, decision = c("1a", "2a", "3a", "4a", "5a", "6a")), unique(real_stop - start)[1]), 10)
})

test_that("Decision 6b sets real_stopdate to start+dose_duration", {
  expect_equal(with(dec6_select_stop_date(start_stop, decision = c("1a", "2a", "3a", "4a", "5a", "6b")), unique(real_stop - start)[1]), 10)
})

test_that("Decision 6b sets real_stopdate to start+qty/ndd", {
  unique_duration <- with(
    dec6_select_stop_date(start_stop, decision = c("1a", "2a", "3a", "4a", "5a", "6c")),
    unique(real_stop - start))

  unique_real_stop <- with(
    dec6_select_stop_date(start_stop, decision = c("1a", "2a", "3a", "4a", "5a", "6c")),
    unique(real_stop))

  expected_real_stop <- c(lubridate::dmy("2021-05-30", "2021-06-09", "2021-06-30", "2021-05-25", "2021-06-14", "2021-06-25"))

  expect_equivalent(unique_duration, c(15, 10))
  expect_equivalent(unique_real_stop, expected_real_stop)

}) # unable to get new_duration column. It might because of the place where this column is defined in dec 5


context("Decision 7: Missing stop date")


context("Decision 8: Handle multiple prescriptions for same product on same day")


context("Decision 9: Handle overlapping prescriptions")


context("Decision 10: Handle sequential prescriptions with short gaps")
