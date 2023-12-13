library(dplyr)
test_that('Compute capital stock in Beijing province in China',{
  ans <- CompK(prv = 'beijing', bt = 2000)
  expect_equal(ans[ans$yr == 2000, 'K'],7066.979, tolerance = 0.01)
  ans <- CompK(prv = 'beijing', bt = 1952)
  expect_equal(ans[ans$yr == 1952, 'K'],7.9, tolerance = 0.01)
})

test_that('Compute capital stock in Chongqing province in China',{
  ans <- CompK(prv = 'chongqing', bt = 2000)
  expect_equal(ans[ans$yr == 2000,'K'],2143.611, tolerance = 0.01)
  ans <- CompK(prv = 'chongqing', bt = 1952)
  expect_equal(ans[ans$yr == 1997,'K'],434.2679, tolerance = 0.01)
})

test_that('Compute capital stock in Beijing province in China',{
  expect_equal((CompK(prv = 'beijing',method = 'CP', bt = 2000) |> filter(yr == 2000) |> select(K) |> as.numeric()),7632.866, tolerance = 0.01)
  expect_equal((CompK(prv = 'beijing', method = 'CP',bt = 1952) |> filter(yr == 2000) |> select(K) |> as.numeric()),5335.9, tolerance = 0.01)
})


