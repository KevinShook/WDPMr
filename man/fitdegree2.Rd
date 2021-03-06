% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitDegree2.R
\name{fitdegree2}
\alias{fitdegree2}
\title{Fits 2nd order polynomial to Korcak data}
\usage{
fitdegree2(korcakdata)
}
\arguments{
\item{korcakdata}{Required. Data frame produced by function \code{Korcak}.}
}
\value{
Returns coefficients of second-order polynomial (intercept, slope quadratic, r2).
}
\description{
Fits 2nd order polynomial to Korcak data
}
\examples{
\dontrun{
coeffs <- fitdegree2(korcakvals)}
}
\seealso{
\code{\link{korcak}}
}
\author{
Kevin Shook
}
