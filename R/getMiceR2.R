# =============================================================================
# R^2 for nonlinear least squares regression with multiply imputed repeated 
# analyses
# earlycapistran@comunidad.unam.mx - July 2020
# =============================================================================

#'  Returns a R^2 value for nonlinear least squares (nls) 
#'  regression. This version is for use with 'mice' and does
#'  not return adjusted R^2.
#'  Adapted from the R^2 calculation from 'easynls' by
#'  Emmanuel Arnhold: https://rdrr.io/cran/easynls/
#' 
#' @param m An object of class 'nls'
#' @return R^2 value
#' @examples
#' getMiceR2(lek_model)
#' 
#' @import stats

getMiceR2 <- function(m) {
  gl <- length(fitted(m)) - 1
  sqt <- var((fitted(m) + resid(m))) * gl
  r1 <- (sqt - deviance(m))/sqt
  r1=round(r1,4)
  return(r1)
}
