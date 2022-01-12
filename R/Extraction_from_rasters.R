#' Extracting information from raster files. Example using observed precipitation and temperature for Klementinum station
#' And simulating data
#'
#' @param path (string) path to your working directory with input files
#' @param name_of_file_pr (string) name of your file with daily precipitation based on station
#' @param name_of_file_tem (string) name of your file with daily temperature based on station
#' @param lon (string) longitude of station in format hhh:mm:ss
#' @param lat (string) latitude of station in format hhh:mm:ss
#' @param name_of_hist_pr (string) name of your historical data by default used: EU_day_pr
#' @param name_of_hist_tem (string) name of your historical data by default used: EU_day_tas
#' @param n_pr_sim (string) name of presimulated data for precipitation
#' @param n_tas_sim (string) name of presimulated data for temperature
#' @param sim_date_range (list) range of period for future simulation or scenario period can be start from nowadays till 2099 year.
#' @param obs_date_range (list) range of period for observation data
#'
#'
#' @return list with prepared data include: simulation data, observed data
#' @import terra data.table ncdf4 utils raster
#'
#' @export
#'
#' @examples
#' name_of_file_pr = system.file("extdata", "RR_STAID000027.txt", package = "CCSusingADCmethod")
#' name_of_file_tem = system.file("extdata", "TG_STAID000027.txt", package = "CCSusingADCmethod")
#' lon = "+014:24:59"
#' lat = "+50:05:11"
#' name_of_hist_pr = system.file("extdata", "EUR_pr_day_CanESM2_historical_rcp85_r25i1p1_19500101-21001231.nc", package = "CCSusingADCmethod")
#' name_of_hist_tem = system.file("extdata", "EUR_tas_day_CanESM2_historical_rcp85_r50i1p1_19500101-21001231.nc", package = "CCSusingADCmethod")
#' n_pr_sim = system.file("extdata", "pr_sim.rds", package = "CCSusingADCmethod")
#' n_tas_sim = system.file("extdata", "tas_sim.rds", package = "CCSusingADCmethod")
#' sim_date_range = c(1951:2099)
#' obs_date_range = c(1805:2004)
#' Example_output_extr = Extraction_from_rasters(, name_of_file_pr, name_of_file_tem, lon, lat, name_of_hist_pr, name_of_hist_tem, n_pr_sim, n_tas_sim, sim_date_range, obs_date_range)

Extraction_from_rasters <- function(path,
                                    name_of_file_pr,
                                    name_of_file_tem,
                                    lon,
                                    lat,
                                    name_of_hist_pr,
                                    name_of_hist_tem,
                                    n_pr_sim,
                                    n_tas_sim,
                                    sim_date_range,
                                    obs_date_range
)
{
  #function to convert coordinate from form hhh:mm:ss to hh
  coordinate_converter <- function(x) {
    xlist <- strsplit(x,split=":")
    h <- as.numeric(sapply(xlist,"[",1))
    m <- as.numeric(sapply(xlist,"[",2))
    s <- as.numeric(sapply(xlist,"[",3))
    xdec <- h+(m/60)+(s/60/60)
    return(xdec)
  }
  #Convert lon, lat from function attribute
  lon = coordinate_converter(lon)
  lat = coordinate_converter(lat)

  #Set working directory, read precipitation and tempature observed data
  if (!missing(path)){
    setwd(path)
  }
  pr = fread(name_of_file_pr, na.strings = '-9999')
  tas = fread(name_of_file_tem, na.strings = '-9999')

  #Transformation input observed Data
  #Delete NA from RR - precipitation amount in 0.1 mm
  pr = pr[!is.na(RR)]
  pr = pr[, pr_o := RR/10]

  #Transform decimal number format to DATE format
  pr[, DTM := as.Date(as.character(DATE), format = '%Y%m%d')]

  #Leave only DATE and Precipitation
  pr = pr[, .(DTM, pr_o)]

  #Delete NA
  tas[, tas_o := TG/10]
  tas = tas[!is.na(tas_o)]
  #Transform decimal number format to DATE format
  tas[, DTM := as.Date(as.character(DATE), format = '%Y%m%d')]

  #Leave only DATE and Temperature
  tas = tas[, .(DTM, tas_o)]

  #concat 2 tables in obs table
  obs = pr[tas, on = 'DTM', nomatch = NULL]

  #Getting station as SpatVector
  station = vect(data.frame(lon, lat))

  #Simulation Data From History
  #Checking input attribute if specified extract value from raster
  #else using brick and pre_simulated values
  if (is.null(n_pr_sim)){
    pr_gcm = rast(name_of_hist_pr)
    pr_sim  = extract(pr_gcm, station)
  } else {
    pr_gcm = brick(name_of_hist_pr)
    pr_sim = readRDS(n_pr_sim)}

  if (is.null(n_tas_sim )){
    tas_gcm = rast(pr_gcm, station)
    tas_sim  = extract(tas_gcm, station)
  } else {
    tas_gcm = brick(name_of_hist_tem)
    tas_sim = readRDS(n_tas_sim)}

  #Transform Simulation Data
  #Transform Precipitation simulated Data format to data.table
  pr_sim = data.table(DTM = pr_gcm@z$Date, pr = as.matrix(pr_sim)[1, -1])

  #Transform Data to corresponding to observer data format
  pr_sim[, pr := pr * 60 * 60 * 24]

  #Transform Temperature simulated data format to data.table
  tas_sim = data.table(DTM = tas_gcm@z$Date, tas = as.matrix(tas_sim)[1, -1])

  #Temperature in degree C to K
  tas_sim[, tas := tas-273.15]

  #Concat Precipitation and temperature Data in one table
  sim = pr_sim[tas_sim, on = 'DTM']

  #Getting simulation data over 5-days for Precipitation
  sim[, pr_sim5 := frollsum(pr, 5, align = 'center')]

  #Getting simulation data with whole-year-value
  sim = sim[year(DTM) %in% sim_date_range]

  #Based on paper, we need 7 days calculation, here we use delimiter
  sim[, BLOCK := (yday(DTM)-1) %/% 7]
  sim[BLOCK == 52, BLOCK := 51]

  #Getting observed data over 5-days for temperature
  obs[, pr_o5 := frollsum(pr_o, 5, align = 'center')]

  #Getting the data with whole-year-value
  obs = obs[year(DTM) %in% obs_date_range]

  #Based on paper, we need 7 days calculation, here we use delimiter
  obs[, BLOCK := (yday(DTM)-1) %/% 7]
  obs[BLOCK == 52, BLOCK := 51]

  output = list("sim" = sim,
                "obs" = obs
                )
  return(output)
}



