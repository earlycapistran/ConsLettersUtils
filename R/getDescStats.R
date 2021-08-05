# =============================================================================
# Get Descriptive Statistics
# earlycapistran@comunidad.unam.mx - April 2021
# =============================================================================

#' Calculates descriptive statistics (mean, median, standard deviation, 
#' minumum, maximum, and n)
#'
#' @param data A data frame
#' @param variable A column with the variable of interest
#' @return A dataframe with descriptive statistics
#' @examples
#' getDescStats(lek_data, cpue)

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