#' sd_r_ploting
#'
#' Results plotting functions
#'
#' @param ccs data table with Climate Change Scenario
#' @param t_period time period for plotting
#'
#' @return Graph with transformed precipitation(time_period)
#' @import data.table
#'
#' @export
#'
#' @example
#' ccs = ccs
#' t_period = c(1805:2004)
#'
#' sd_r_ploting(ccs, t_period)

sd_r_ploting <- function(ccs, t_period){
  ccs[year(DTM) %in% t_period, plot(sd_r)]
}
