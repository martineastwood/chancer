test_that("chance.misc works as expected", {
  
  set.seed(86858246)
  
  expect_that(chance.pick(c(1, 2, 3)), equals(1))
  
  expect_that(chance.weighted(c(1, 2, 3), c(0.5, 0.3, 0.2)), equals(2))
  
  expect_that(chance.hash(), equals("3c996c26d0e9675cad9d1025783791ad7e6d936d94c058f3f9e3797eb897d138"))
  expect_that(chance.hash(case='upper'), equals("D5F58A87560D1E0839558F49325F77CFFA19FF2A1DBE909439291868D97F4ADA"))
  
  expect_that(chance.capitalise('test'), equals("Test"))
  
  expect_that(chance.guid(), equals("92b7fbbb-fb42-0a0e-ad5b-b0ffa2506a5f"))
  
})