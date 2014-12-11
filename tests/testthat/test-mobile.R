test_that("chance.mobile works as expected", {
  
  set.seed(86858246)
  
  expect_that(chance.android_id(), equals("APA91jOeNDCpO8pQ1XCquoQFSDT43blvzeuD4HRwXqSAcpSEiO3lxYf-DXgtCtVLxDuS5czUm-nAFzump2R7W1ycAmkxYhDf9k-twN-YH7CYY9G7RLUA1CjeBaC5zsySBvZiFQGWv6Pm677l7Dv-2_i0gE64Ynl3tG-J1GoWLYYqewTFvvaYAVH"))
    
  expect_that(chance.apple_token(), equals("45c7e1c93ab81a9421ec5c1cb5e9ea1c6a41845ee92b1b8f03a6340272b7d715"))
  
  expect_that(chance.bb_pin(), equals("49511dd1"))
  
  
})