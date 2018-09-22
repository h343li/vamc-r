context("valuation of one guaratee contract")
library(vamc)

fundScen <- genFundScen(fundMap, indexScen)[1:2, , ]
test_that("test for the correctness of valuation", {
  expect_equal(valuateOnePolicy(testDBRP, mortTable, fundScen,
                                1 / 12, cForwardCurve),
               list(policyValue = 0, riskCharge = 454.1419), tolerance = 1)
})
