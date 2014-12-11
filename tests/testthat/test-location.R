test_that("chance.location works as expected", {
  
  set.seed(86858246)
  
  expect_that(chance.street.suffix(), equals("Highway"))
  expect_that(chance.street.suffix(abbreviation=TRUE), equals("Rd"))
  
  expect_that(chance.street(), equals("Tikolub Glen"))
  expect_that(chance.street(abbreviation=TRUE), equals("Mijuepuc Tpke"))
  
  expect_that(chance.address(), equals("1356 Kimnoc Center"))
  expect_that(chance.address(abbreviation=TRUE), equals("544 Okugevo Sq"))
  
  expect_that(chance.altitude(), equals(6962.2952))
  expect_that(chance.altitude(max=25), equals(1.3835))
  
  expect_that(chance.areacode(), equals("(448)"))
  expect_that(chance.areacode(parens=FALSE), equals("486"))
  
  expect_that(chance.city(), equals("Hinizipidemu"))
  
  expect_that(chance.latitude(), equals(-28.4101))
  
  expect_that(chance.longitude(), equals(172.6893))
  
  expect_that(chance.coordinates(format='string'), equals("-25.3118, 21.7405"))
  
  expect_that(chance.depth(), equals(-807.958))
  expect_that(chance.depth(min=-50), equals(-22.3648))
  
  expect_that(chance.geohash(), equals("gbd1u3x"))
  expect_that(chance.geohash(len=3), equals("0j6"))
  
})