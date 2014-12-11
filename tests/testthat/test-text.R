test_that("chance.text functions work as expected", {
  
  set.seed(86858246)
  
  expect_that(chance.syllable(), equals("afo"))
  
  expect_that(chance.word(), equals("lodebuni"))
  
  expect_that(chance.sentence(), equals("Tupucah gipaaluk gevoabeogup nolzipidemu lipikeb obisehogog uli oranododwuna gidciki nuhovulakada tizuha daze nozboju okeltila irorzekina."))
  
  expect_that(chance.paragraph(), equals("Inihap far hakaciku hewimat pasiolol hoko kacucoza ori zomtigi dene opiduno acak uniguve namikaf pico udumtare herenanute. Wasoigut cislumo wanucohopuro duncabjava siz onar jaju wocavab esagef mabajabzade katuhisoami ires. Jencog noveehid fudeose kapi juca bar cukzuv guvegim todihugtav ocedmofo osawhal wihaebiowow osobgah edad ramufuv."))
  
  expect_that(chance.string(), equals('Ac)Vbw^E*V'))
  
  expect_that(chance.string(case='upper', pool='alpha'), equals('AGLEZDXLSC'))
  
})