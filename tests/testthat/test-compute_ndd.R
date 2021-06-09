test_that("compute_ndd works", {
  expect_equal(with(compute_ndd(dataset1,"min_min"), mean(ndd,na.rm=T)), 3.428571,tolerance=0.001)
  expect_equal(with(compute_ndd(dataset1,"min_max"), mean(ndd,na.rm=T)), 3.428571,tolerance=0.001)
  expect_equal(with(compute_ndd(dataset1,"max_max"), mean(ndd,na.rm=T)), 6.857143,tolerance=0.001)
  expect_equal(with(compute_ndd(dataset1,"min_min"), sd(ndd,na.rm=T)), 0.5135526,tolerance=0.001)
  expect_equal(nrow(compute_ndd(dataset1,"min_min")), 18,tolerance=0.001)
})
