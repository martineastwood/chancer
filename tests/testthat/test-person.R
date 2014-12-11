test_that("chance.person works as expected", {
  
  set.seed(86858246)
  
  expect_that(chance.age(), equals(29))
  expect_that(chance.age(type='child'), equals(10))
  expect_that(chance.age(type='senior'), equals(72))
  
  expect_that(chance.first(), equals("Marion"))
  expect_that(chance.first(gender='female'), equals("Beatrice"))
  
  expect_that(chance.last(), equals("Hale"))
  
  expect_that(chance.name(), equals("Alvin Ramirez"))
  expect_that(chance.name(initial=TRUE), equals("Gordon Y Thomas"))
  expect_that(chance.name(middle_name=TRUE), equals("Kathryn Olive Kim"))
  expect_that(chance.name(middle_name=TRUE, prefix=TRUE), equals("Doctor Sylvia Birdie Waters"))

  
})