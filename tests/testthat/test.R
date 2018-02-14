library(testthat)
library(IBCF.MTME)

context('Data transformation tests')

test_that('getMatrixForm and getTidyForm functions', {
  data('Wheat_IBCF')
  data('Year_IBCF')

  M <- getMatrixForm(Wheat_IBCF)
  Tidy <- getTidyForm(M)

  M.Y <- getMatrixForm(Year_IBCF, onlyTrait = T)
  Tidy.Y <- getTidyForm(M.Y, onlyTrait = T)

  expect_output(str(M), '250 obs. of  13 variables')
  expect_output(str(M.Y), '60 obs. of  14 variables')

  expect_output(str(Tidy), '3000 obs. of  4 variables')
  expect_output(str(Tidy.Y), '720 obs. of  4 variables')
})

context('CrossValidation Tests')

test_that('Crossvalidation function', {
  data('Wheat_IBCF')
  CrossV1 <- CV.RandomPart(Wheat_IBCF, Set_seed = 123)
  CrossV2 <- CV.RandomPart(Wheat_IBCF, NPartitions = 10, Set_seed = 123)
  CrossV3 <- CV.RandomPart(Wheat_IBCF, NPartitions = 10, PTesting = 0.35, Set_seed = 123)
  expect_equal(CrossV1, CrossV2)
  expect_equal(CrossV1, CrossV3)

  expect_output(str(CrossV1), 'List of 5')
  expect_is(CrossV1, 'CrossValidation')
  expect_false(any(is.na(CrossV1$DataSet)))
  expect_false(any(is.na(CrossV1$CrossValidation_list)))
})

context('IBCF Tests')
test_that('IBCF function', {
  data('Wheat_IBCF')
  CrossV <- CV.RandomPart(Wheat_IBCF, NPartitions = 10, PTesting = 0.25, Set_seed = 123)
  pm <- IBCF(CrossV)
  expect_is(pm, 'IBCF')
  expect_output(str(pm), 'List of 3')
  expect_false(any(is.na(pm$predictions_Summary)))
  expect_is(pm$predictions_Summary, 'data.frame')
  expect_is(pm$predictions_Summary[1, 1], 'character')
  expect_is(pm$predictions_Summary[1, 2], 'numeric')
  expect_equal(pm$predictions_Summary, summary(pm))
  expect_is(pm$NPartitions, 'integer')
  expect_false(any(is.null(pm$Predictions)))
})


test_that('IBCF function without Trait', {
  data('Wheat_IBCF')
  data <- Wheat_IBCF[which(Wheat_IBCF$Trait == 'DH'), ]
  data <- data[,c(1,3,4)]

  CrossV <- CV.RandomPart(data, NPartitions = 10, PTesting = 0.25, Set_seed = 123)
  pm <- IBCF(CrossV)
  expect_is(pm, 'IBCF')
  expect_output(str(pm), 'List of 3')
  expect_false(any(is.na(pm$predictions_Summary)))
  expect_is(pm$NPartitions, 'integer')
})

test_that('IBCF function without Env', {
  data('Wheat_IBCF')
  data <- Wheat_IBCF[which(Wheat_IBCF$Env == 'Bed5IR'), ]
  data <- data[,c(1,2,4)]

  CrossV <- CV.RandomPart(data, NPartitions = 10, PTesting = 0.25, Set_seed = 123)
  pm <- IBCF(CrossV)
  expect_is(pm, 'IBCF')
  expect_output(str(pm), 'List of 3')
  expect_false(any(is.na(pm$predictions_Summary)))
  expect_is(pm$NPartitions, 'integer')
})

context('IBCFY Tests')
test_that('IBCFY function', {
  data('Year_IBCF')
  DataSet <- getMatrixForm(Year_IBCF, onlyTrait = T)

  pm <- IBCF.Years(DataSet, colYears = 'Years' , Years.testing = c('2015', '2016'), Traits.testing = c('T5', 'T6'))

  expect_is(pm, 'IBCFY')
  expect_output(str(pm), 'List of 4')
  expect_false(any(is.na(pm$predictions_Summary)))
  expect_is(pm$predictions_Summary, 'data.frame')
  expect_is(pm$predictions_Summary[1, 1], 'factor')
  expect_is(pm$predictions_Summary[1, 2], 'numeric')
  expect_equal(pm$predictions_Summary, summary(pm))
  expect_is(pm$Years.testing, 'character')
  expect_is(pm$Traits.testing, 'character')
})

test_that('IBCFY function one Trait for test', {
  data('Year_IBCF')
  DataSet <- getMatrixForm(Year_IBCF, onlyTrait = T)

  pm <- IBCF.Years(DataSet , Years.testing = c('2015', '2016'), Traits.testing = c('T5'))

  expect_is(pm, 'IBCFY')
  expect_output(str(pm), 'List of 4')
  expect_false(any(is.na(pm$predictions_Summary)))
  expect_is(pm$Years.testing, 'character')
  expect_is(pm$Traits.testing, 'character')
})

test_that('IBCFY function for one year and one trait', {
  data('Year_IBCF')
  DataSet <- getMatrixForm(Year_IBCF, onlyTrait = T)

  pm <- IBCF.Years(DataSet , Years.testing = c('2015'), Traits.testing = c('T5'))

  expect_is(pm, 'IBCFY')
  expect_output(str(pm), 'List of 4')
  expect_false(any(is.na(pm$predictions_Summary)))
  expect_is(pm$Years.testing, 'character')
  expect_is(pm$Traits.testing, 'character')
})

test_that('IBCFY function with Wheat_IBCF Training', {
  data('Wheat_IBCF')

  DataSet <- getMatrixForm(Wheat_IBCF, onlyTrait = T)
  pm <- IBCF.Years(DataSet, colYears = "Env", Years.testing = 'Drip', Traits.testing = c('DH','GY'))

  expect_is(pm, 'IBCFY')
  expect_output(str(pm), 'List of 4')
  expect_false(any(is.na(pm$predictions_Summary)))
  expect_is(pm$Years.testing, 'character')
  expect_is(pm$Traits.testing, 'character')
})





