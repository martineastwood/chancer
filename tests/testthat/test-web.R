test_that("chance.web works as expected", {
  
  set.seed(86858246)
  
  expect_that(chance.colour(), equals('#4c3c99'))
  expect_that(chance.colour(format='rgb'), equals("rgb(100,201,36)"))
  expect_that(chance.colour(format='rgb', greyscale=TRUE), equals("rgb(98,98,98)"))
  
  expect_that(chance.domain(), equals('oniluptuca.org'))
  expect_that(chance.domain(tld='ie'), equals('relcoti.ie'))
  
  expect_that(chance.email(), equals("enaepecemu@tugi.co.uk"))
  expect_that(chance.email(domain='rmail.com'), equals("zipidemu@rmail.com"))
  
  expect_that(chance.facebook(), equals("1000034216634"))
  
  expect_that(chance.google(), equals("UA-956777-45"))
  
  expect_that(chance.hashtag(), equals("#dubi"))
  
  expect_that(chance.ip(), equals('50.145.90.81'))
  
  expect_that(chance.ipv6(), equals('8f49:325f:77cf:fa19:ff2a:1dbe:9094:3929'))
  
  expect_that(chance.klout(), equals(9))
  
  expect_that(chance.twitter(), equals("@runuho"))
  
})