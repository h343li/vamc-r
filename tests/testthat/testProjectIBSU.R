context("cash flow projection of IBSU")
library(vamc)

fundScen <- genFundScen(fundMap, indexScen)
mortFactors <- calcMortFactors(testIBSU, mortTable, dT = 1 / 12)
pq <- mortFactors[, "pq"]
p <- mortFactors[, "p"]

test_that("test for the cash flow projection", {
  expect_equal(projectIBSU(testIBSU, fundScen[1, 1:163, ], 1 / 12, pq, p,
                           cForwardCurve)$DA[1, 1],
               0, tolerance = 1)
  expect_equal(projectIBSU(testIBSU, fundScen[1, 1:163, ], 1 / 12, pq, p,
                           cForwardCurve)$LA[1, 1],
               0, tolerance = 1)
  expect_equal(projectIBSU(testIBSU, fundScen[1, 1:163, ], 1 / 12, pq, p,
                           cForwardCurve)$RC[1, 1],
               630.2431, tolerance = 1)
})

test_that("Test for error message when pq, p, and df cannot cover maturity", {
  expect_error(projectIBSU(testIBSU, fundScen[1, 1:163, ], 1 / 12, c(1), p,
                           cForwardCurve)$DA[1, 1],
               "df, pq, and p must have length > numStep from oneFundScen")
  expect_error(projectIBSU(testIBSU, fundScen[1, 1:163, ], 1 / 12, pq, c(1),
                           cForwardCurve)$DA[1, 1],
               "df, pq, and p must have length > numStep from oneFundScen")
  expect_error(projectIBSU(testIBSU, fundScen[1, 1:163, ], 1 / 12, pq, p,
                           c(1))$DA[1, 1],
               "df, pq, and p must have length > numStep from oneFundScen")
})
