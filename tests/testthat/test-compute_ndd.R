test_that("compute_ndd works", {
  expect_equal(with(compute_ndd(dataset1,"min_min"), mean(ndd,na.rm=T)), 3.428571)
  expect_equal(with(compute_ndd(dataset1,"min_max"), mean(ndd,na.rm=T)), 3.428571)
  expect_equal(with(compute_ndd(dataset1,"max_max"), mean(ndd,na.rm=T)), 6.857143)
  expect_equal(with(compute_ndd(dataset1,"min_min"), sd(ndd,na.rm=T)), 0.5135526)
  expect_equal(with(compute_ndd(dataset1,"min_min"), nrow(ndd,na.rm=T)), 18)
})
