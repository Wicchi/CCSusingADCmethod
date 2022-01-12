#' Output file with list with transformation parameters.
#'
#'
#'
#' @format List with transformation parameters consist of 52 rows and 18 columns:
#' \describe{
#'   \item{BLOCK}{BLOCK value - shows number of separating parameter}
#'   \item{q60_o}{Quantile of 60\% for observed data over control period}
#'   \item{q90_o}{Quantile of 90\% for observed data over control period}
#'   \item{q60_c}{Quantile of 60\% for simulated data over control period}
#'   \item{q90_c}{Quantile of 90\% for simulated data over control period}
#'   \item{q60_s}{Quantile of 60\% for simulated data over future period or scenario period}
#'   \item{q90_s}{Quantile of 90\% for simulated data over future period or scenario period}
#'   \item{e_c}{Exceedance over q90 quantile 90\% for simulated data over control period}
#'   \item{e_s}{Exceedance over q90 quantile 90\% for simulated data over future period or scenario period}
#'   \item{mt_o}{mean of temperature value for observed data over control period}
#'   \item{mt_c}{mean of temperature value for simulated data over control period}
#'   \item{mt_s}{mean of temperature value for simulated data over future period or scenario period}
#'   \item{sdt_c}{standart deviation of temperature value for simulated data over control period}
#'   \item{sdt_s}{standart deviation of temperature value for simulated data over future period or scenario period}
#'   \item{g1}{biascorrection factors over quntile 60\% for observed and simulation data for the control period}
#'   \item{g2}{biascorrection factors over quntile 90\% for observed and simulation data for the control period}
#'   \item{b}{The coefficients a and b were derived from the 60\% quantile P60 and the 90\% quantile P90 of the 5-day precipitation sums and the future changes therein.}
#'   \item{a}{The coefficients a and b were derived from the 60\% quantile P60 and the 90\% quantile P90 of the 5-day precipitation sums and the future changes therein.}
#' }
"Example_output_trans_param"
