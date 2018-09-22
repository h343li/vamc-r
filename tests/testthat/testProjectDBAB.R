context("cash flow projection of DBAB")
library(vamc)

fundScen <- genFundScen(fundMap, indexScen)
mortFactors <- calcMortFactors(testDBAB, mortTable, dT = 1 / 12)
pq <- mortFactors[, "pq"]
p <- mortFactors[, "p"]

test_that("test for the cash flow projection", {
  expect_equal(projectDBAB(testDBAB, fundScen[1, 1:163, ], 1 / 12, pq, p,
                           cForwardCurve)$DA[1, 1],
               8.566897, tolerance = 1)
  expect_equal(projectDBAB(testDBAB, fundScen[1, 1:163, ], 1 / 12, pq, p,
                           cForwardCurve)$LA[1, 1],
               0, tolerance = 1)
  expect_equal(projectDBAB(testDBAB, fundScen[1, 1:163, ], 1 / 12, pq, p,
                           cForwardCurve)$RC[1, 1],
               10.84219, tolerance = 1)
})

test_that("Test for error message when pq, p, and df cannot cover maturity", {
  expect_error(projectDBAB(testDBAB, fundScen[1, 1:163, ], 1 / 12, c(1), p,
                           cForwardCurve)$DA[1, 1],
               "df, pq, and p must have length > numStep from oneFundScen")
  expect_error(projectDBAB(testDBAB, fundScen[1, 1:163, ], 1 / 12, pq, c(1),
                           cForwardCurve)$DA[1, 1],
               "df, pq, and p must have length > numStep from oneFundScen")
  expect_error(projectDBAB(testDBAB, fundScen[1, 1:163, ], 1 / 12, pq, p,
                           c(1))$DA[1, 1],
               "df, pq, and p must have length > numStep from oneFundScen")
})
