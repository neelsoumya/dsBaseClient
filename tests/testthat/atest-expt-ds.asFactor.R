source("definition_tests/def-ds.asFactor.R")


context("ds.asFactor()::expt::multiple")
test_that("combined data set",
{
  connect.all.datasets()
  .test.find.factor('D$INTEGER','INT.f',ds.test_env$local.values,6)
  .test.find.factor('D$NON_NEGATIVE_INTEGER','NON_NEG_INT.f',ds.test_env$local.values,7)
  .test.find.factor('D$POSITIVE_INTEGER', 'POS_INT.f',ds.test_env$local.values,8)
  .test.find.factor('D$NEGATIVE_INTEGER','NEG_INT.f',ds.test_env$local.values,9)
  .test.find.factor('D$NUMERIC','NUM.f',ds.test_env$local.values,10)
  .test.find.factor('D$NON_NEGATIVE_NUMERIC','NON_NEG_NUM.f',ds.test_env$local.values,11)
  .test.find.factor('D$POSITIVE_NUMERIC','POS_NUM.f',ds.test_env$local.values,12)
  .test.find.factor('D$NEGATIVE_NUMERIC','NEG_NUM.f',ds.test_env$local.values,13)
  .test.find.factor('D$FACTOR_INTEGER', 'FACTOR_INT.f',ds.test_env$local.values,15)
})

context("ds.asFactor()::expt::single")
test_that("single dataset ",
{
  connect.dataset.1()
  .test.find.factor('D$INTEGER','INT.f',ds.test_env$local.values.1,6)
  .test.find.factor('D$NON_NEGATIVE_INTEGER','NON_NEG_INT.f',ds.test_env$local.values.1,7)
  .test.find.factor('D$POSITIVE_INTEGER', 'POS_INT.f',ds.test_env$local.values.1,8)
  .test.find.factor('D$NEGATIVE_INTEGER','NEG_INT.f',ds.test_env$local.values.1,9)
  .test.find.factor('D$NUMERIC','NUM.f',ds.test_env$local.values.1,10)
  .test.find.factor('D$NON_NEGATIVE_NUMERIC','NON_NEG_NUM.f',ds.test_env$local.values.1,11)
  .test.find.factor('D$POSITIVE_NUMERIC','POS_NUM.f',ds.test_env$local.values.1,12)
  .test.find.factor('D$NEGATIVE_NUMERIC','NEG_NUM.f',ds.test_env$local.values.1,13)
  .test.find.factor('D$FACTOR_INTEGER', 'FACTOR_INT.f',ds.test_env$local.values.1,15)
})
