#' @title Contains functions to perform post- processing on data used with the Wetland DEM Ponding Model (WDPM)
#' @docType package
#' @name WDPMr-package
#'
#' @description Functions to extract pond areas and connected (contributing) areas from WDPM output, and to calculate and plot statistics.\cr
#' Many of the functions read data output from WDPM runs. It is assumed that the file names will be of the format \option{add1_subtract1_add2_subtract2_d.asc}, where:
#'  \describe{
#'  \item{add1}{first addition of water}
#'  \item{subtract1}{first subtraction of water}
#'  \item{add2}{second addition of water}
#'  \item{subtract2}{second subtraction of water}
#'  \item{d}{indicates that the drain module was run}
#' }\cr
#'Many of the functions reference \dQuote{loop1}, which is the sequence of completely filling the basin from an initially-empty state, followed by completely draining the basin. This loop defines the maximum degree of hysteresis in the basin.
#' @references
#' To cite \pkg{CRHMr} in publications, use the command \code{citation('WDPMr')} to get the current version of the citation.\cr
#' The WDPM program is described in:\cr
#'\cite{Shook, K., Pomeroy, J. W., Spence, C., and Boychuk, L. (2013). \dQuote{Storage dynamics simulations in prairie wetland hydrology models: evaluation and parameterization}. Hydrological Processes, 27(13), 1875-1889. http://doi.org/10.1002/hyp.9867}\cr
#'The WDPM model may be downloaded from \url{http://www.usask.ca/hydrology/CRHM.php}.\cr
#' @import ggplot2 stringr reshape2 grid RColorBrewer ismev
#' @importFrom SDMTools read.asc ConnCompLabel PatchStat
#' @importFrom raster as.matrix cellStats raster mask
#' @importFrom utils read.table type.convert
#' @importFrom igraph graph simplify delete_edges E V
#' @importFrom graphics hist
#' @importFrom ineq Lc
#' @importFrom fExtremes pgpd rgpd
NULL
