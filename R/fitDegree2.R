#' Fits 2nd order polynomial to Korcak data
#'
#' @param korcakdata Required. Data frame produced by function \code{Korcak}.
#'
#' @return Returns coefficients of second-order polynomial (intercept, slope quadratic, r2).
#' @author Kevin Shook
#' @seealso \code{\link{korcak}}
#' @export
#'
#' @examples \dontrun{
#' coeffs <- fitdegree2(korcakvals)}
fitdegree2 <- function(korcakdata){

 coeffs <- c(0)
 degree <- 2

# select and log-transform data
 korcakdata <- korcakdata[korcakdata$p>0,]
 area <- log10(korcakdata$value)
 exceedance <- log10(korcakdata$p)
 log.transformed <- stats::na.omit(data.frame(area, exceedance))
# fit 2nd order polynomial
 fm2 <- stats::lm(exceedance ~ area + I(area^2), log.transformed)

 # get coefficients of fit
 for (coeff.num in 1:(degree+1))
   coeffs[coeff.num] <- stats::coefficients(fm2)[coeff.num]

 r <- summary(fm2)["r.squared"][1]
 r2 <- as.numeric(r[["r.squared"]])
 coeffs[degree+2] <- r2[1]
 return(coeffs)
}
