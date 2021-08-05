# =============================================================================
# Get Descriptive Statistics
# earlycapistran@comunidad.unam.mx - April 2021
# =============================================================================

#' Calculates descriptive statistics (mean, median, standard deviation, 
#' minumum, maximum, and n)
#'
#' @param data A data frame
#' @param variable A column with a numerical variable of interest
#' @return A dataframe with descriptive statistics
#' 
#' @export
#' @usage
#' getDescStats(data, variable)
#' 
#' @examples 
#' getDescStats(iris, Sepal.Length)
#' 
#' @import dplyr

getDescStats <- function(data, variable) {
  require(tidyverse)
  var <- enquo(variable)  
  desc_stats <- data %>% 
    summarize(mean = mean(!!var), 
              median = median(!!var),
              sd = sd(!!var), 
              min = min(!!var), 
              max = max(!!var),
              n = length(!!var))
  return(desc_stats)
}