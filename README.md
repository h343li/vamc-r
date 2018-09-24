# vamc-r
# Package Info
This package uses Monte Carlo simulation to estimate the fair market value of a large portfolio of synthetic variable annuities.
The portfolio of variable annuities under consideration is generated based on realistic features of common types of guarantee riders in North America.
The Monte Carlo simulation engine generates sample paths of asset prices based on Black-Scholes model.
In this vignette, we will demonstrate the functionalities provided in this package. 

For illustrative purposes, we will use few scenarios to valuate a pool of two
variable annuities. Users may obtain a more robust valuation result by increasing
the amount of risk-neutral scenarios.
