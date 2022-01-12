#' Obs_to_CCS
#'
#' Transformation of the observed series into the climate change scenario
#'
#' @param obs prepared observed data
#' @param stat transformation parameters
#' @param obs_date_range (list) range of period for observation data
#'
#' @return Climate Change Scenario in form data table
#' @import data.table
#' @export
#'
#' @examples
#' obs = Example_output_extr[["obs"]]
#' stat = Example_output_trans_param
#' obs_date_range = c(1805:2004)
#' ccs = obs_to_CCS(obs, stat, obs_date_range)
#'
obs_to_CCS <- function(obs, stat, obs_date_range){
  #Addind information about statistic parameters to the prepared observed data
  obs = stat[, .(BLOCK, a, b, E = e_s/e_c, mt_o, mt_c, mt_s, sd_r = sdt_s/sdt_c)][obs, on = 'BLOCK']

  #Getting quntile over 90% for the observed data for control period
  obs[, q90_o := quantile(pr_o[year(DTM) %in% obs_date_range], .9), by = BLOCK]

  #Transformation previous data to the Climate Change Scenario
  obs[pr_o < q90_o, pr_trans := a * pr_o ^ b]
  obs[pr_o >= q90_o, pr_trans := E * (pr_o - q90_o) + a * q90_o ^ b]
  obs[, tas_trans := mt_o + mt_s - mt_c + (tas_o - mt_o) * sd_r]

  return(list("css" = obs))
}
