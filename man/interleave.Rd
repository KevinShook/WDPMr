% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interleave.R
\name{interleave}
\alias{interleave}
\title{Interleaves 2 vectors.
From bogdan romocea http://r.789695.n4.nabble.com/Interleaving-elements-of-two-vectors-td795123.html}
\usage{
interleave(v1, v2)
}
\arguments{
\item{v1}{Required. Vector 1.}

\item{v2}{Required. Vector 2.}
}
\value{
Returns a vector with elements of vectors 1 and 2 interleaved.
}
\description{
Interleaves 2 vectors.
From bogdan romocea http://r.789695.n4.nabble.com/Interleaving-elements-of-two-vectors-td795123.html
}
\examples{
{
a <- c(1,2,3)
b <- c(4,5,6)
interleave(a,b)}
}
