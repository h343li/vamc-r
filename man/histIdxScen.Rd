% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{histIdxScen}
\alias{histIdxScen}
\title{Historical index scenario for 5 indices over 175 months}
\format{A data frame with dimensions 175 rows and 10 columns:
\describe{
  \item{FIXED}{historical return for index "FIXED" in one month}
  \item{INT}{historical return for index "INT" in one month}
  \item{MONEY}{historical return for index "MONEY" in one month}
  \item{SMALL}{historical return for index "SMALL" in one month}
  \item{US}{historical return for index "US" in one month}
  ...
}}
\source{
\url{http://www.math.uconn.edu/~gan/software.html}
}
\usage{
histIdxScen
}
\description{
A dataset containing a matrix, number of indices (5) by
number of time steps (175), of observed historical returns
for each index in each of time step in the past.
}
\section{Remark}{

These historical index scenarios were assessed on 2008-09-12
}

\keyword{datasets}
