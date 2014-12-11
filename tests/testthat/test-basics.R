test_that("chance.basics work as expected", {
  
  set.seed(86858246)
  
  expect_that(chance.integer(), equals(-914649738))
  
  expect_that(chance.bool(), is_false())
  
  expect_that(chance.character(), equals('N'))
  
  expect_that(chance.floating(), equals(77.13))
  
  expect_that(chance.natural(), equals(61))
  
  expect_that(chance.string(), equals('lYyIY!B*ma'))
  
  expect_that(chance.string(case='upper', pool='alpha'), equals('MJVQWPWBBE'))
    
})