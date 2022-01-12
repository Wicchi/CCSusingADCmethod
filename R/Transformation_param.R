#' Getting the coefficients a and b were derived from the 60% quantile (P60)
#' and the 90% quantile (P90) of the 5-day precipitation sums
#' and the (future) changes therein
#'
#' @param sim Data Table with prepared simulation data
#' @param obs Data Table with prepared observed data
#' @param obs_date_range (list) range of period for observation data
#' @param sim_date_range (list) range of period for future simulation or scenario period can be start from nowadays till 2099 year.
#'
#' @return stats data, transormation parameters
#' @import data.table
#' @importFrom stats sd
#' @export
#'
#' @examples
#' sim = Example_output_extr[["sim"]]
#' obs = Example_output_extr[["obs"]]
#' obs_date_range = c(1805:2004)
#' sim_date_range = c(1951:2099)
#' Example_output_trans_param = Tranformation_param(sim, obs, obs_date_range, sim_date_range)
#'
Tranformation_param <- function(sim, obs, obs_date_range, sim_date_range){

  #Observed statistic
  #Getting q60 (quantile 60%) and q90 (quantile 90%) as well as p60 and p90 () on observed data for control period
  stat_o = obs[year(DTM) %in% obs_date_range, .(q = quantile(pr_o5, c(.6, .9)), p = c(.6, .9)), by = BLOCK]
  stat_o = dcast.data.table(stat_o, BLOCK ~ p, value.var = 'q')
  setnames(stat_o, c('0.6', '0.9'), c('q60_o', 'q90_o'))

  #Getting q60 (quantile 60%) and q90 (quantile 90%) as well as p60 and p90 () on simulated data for control period
  stat_c = sim[year(DTM) %in% obs_date_range, .(q = quantile(pr_sim5, c(.6, .9)), p = c(.6, .9)), by = BLOCK]
  stat_c = dcast.data.table(stat_c, BLOCK ~ p, value.var = 'q')
  setnames(stat_c, c('0.6', '0.9'), c('q60_c', 'q90_c'))

  #Getting q60 (quantile 60%) and q90 (quantile 90%) as well as p60 and p90 () on simulated data for future period or scenario period
  stat_s = sim[year(DTM) %in% sim_date_range, .(q = quantile(pr_sim5, c(.6, .9)), p = c(.6, .9)), by = BLOCK]
  stat_s = dcast.data.table(stat_s, BLOCK ~ p, value.var = 'q')
  setnames(stat_s, c('0.6', '0.9'), c('q60_s', 'q90_s'))

  #Calculate the mean exceedances
  #Exceedance over q90 (quantile 90%) for simulated data over control period
  E_c = sim[year(DTM) %in% obs_date_range]
  E_c[, q90 := quantile(pr, .9), by = BLOCK]
  E_c = E_c[pr > q90, .(e_c = mean(pr - q90)), by = BLOCK]

  #Exceedance over q90 (quantile 90%) for simulated data over future period or scenario period
  E_s = sim[year(DTM) %in% sim_date_range]
  E_s[, q90 := quantile(pr, .9), by = BLOCK]
  E_s = E_s[pr > q90, .(e_s = mean(pr - q90)), by = BLOCK]

  #Calculate temperature parameters
  #Getting mean of temperature value for observed data over control period
  temp_o = obs[year(DTM) %in% obs_date_range, .(mt_o = mean(tas_o)), by = BLOCK]

  #Getting mean and standard deviation of temperature value for simulated data over control period
  temp_c = sim[year(DTM) %in% obs_date_range, .(mt_c = mean(tas), sdt_c = sd(tas)), by = BLOCK]

  #Getting mean and standard deviation of temperature value for simulated data over future period or scenario period
  temp_s = sim[year(DTM) %in% sim_date_range, .(mt_s = mean(tas), sdt_s = sd(tas)), by = BLOCK]

  stat = stat_o[stat_c, on = 'BLOCK'][stat_s, on = 'BLOCK'][E_c, on = 'BLOCK'][E_s, on = 'BLOCK'][temp_o, on = 'BLOCK'][temp_c, on = 'BLOCK'][temp_s, on = 'BLOCK'][order(BLOCK)]
  #Getting biascorrection factors
  #Over quntile 60 for observed and simulation data for the control period
  stat[, g1 := q60_o / q60_c]

  #Over quntile 90 for observed and simulation data for the control period
  stat[, g2 := q90_o / q90_c]

  #Getting the coefficients a and b were derived from the 60% quantile (P60) and the 90% quantile (P90) of the 5-day precipitation sums and the (future) changes therein.
  stat[, b := log((g2 * q90_s)/(g1 * q60_s))/log((g2 * q90_c)/(g1 * q60_c) ) ]
  stat[, a := q60_s / (q60_c^b) * g1^(1-b)]

  return(stat)
}
