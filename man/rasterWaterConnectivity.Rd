% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasterWaterConnectivity.R
\name{rasterWaterConnectivity}
\alias{rasterWaterConnectivity}
\title{Find connectivity of ponds in raster}
\usage{
rasterWaterConnectivity(elevations, quiet = TRUE)
}
\arguments{
\item{elevations}{List of pond data as returned by \code{waterPatchElevation}, using \option{all} for \code{return}.}

\item{quiet}{Optional. If \code{TRUE}, the default, there is no extra output. If \code{FALSE}, the patches are listed as they are located.}
}
\value{
Returns a data frame of patch parameters (ID, area, perimeter, and destination). The destination of the first pond is always \code{0} (zero), as used by the model PCM and the function \code{graphPCMPonds}.
}
\description{
Find connectivity of ponds in raster
}
\examples{
\dontrun{conn <- rasterWaterConnectivity(el, FALSE)
}
}
