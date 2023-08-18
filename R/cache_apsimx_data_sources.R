#' @title Cache ApsimX SSURGO Data
#'
#' @description Wrapper for apsimx::get_ssurgo_soil_profile. Saves to a desired location.
#'
#' @param lonlat Array containing the longitude and latitude to be downloaded.
#' @param dir_path Desired path for data cache.
#' @param force Boolean that will overwrite cached file if `TRUE`.
#' @param delay Delay between retrievals.
#'
#' @author Daniel Kick (\email{daniel.kick@usda.gov})
#'
#' @export
#'
#' @examples
require(apsimx)
cache_apsimx_ssurgo <- function(
    lonlat = c(-93.4345, 42.049),
    dir_path = '../data/env_data/apsimx_ssurgo',
    force = FALSE,
    delay = 0
){
  ensure_dir_path_exists(dir_path = dir_path)
  save_name <- paste(c(paste(lonlat, collapse = '_'), '.RDS'), collapse = '')
  save_path <- paste(dir_path, save_name, sep = '/')
  # make sure the coordinates are within usa (otherwise ssurgo won't have them)
  if( ((!file.exists(save_path))|(force == TRUE)) ){
    # Check if file exists or should be downloaded
    Sys.sleep(delay)
    surgo_profile <- apsimx::get_ssurgo_soil_profile(lonlat = lonlat)
    saveRDS(surgo_profile, file = save_path)
  }
}


#' @title Cache ApsimX Daymet Data
#'
#' @description Wrapper for apsimx::get_daymet2_apsim_met. Saves to a desired location.
#'
#' @param lonlat Array containing the longitude and latitude to be downloaded.
#' @param years Year range to be downloaded. For a single year pass the same value twice e.g. `c(2013, 2013)`
#' @param dir_path Desired path for data cache.
#' @param force Boolean that will overwrite cached file if `TRUE`.
#' @param delay Delay between retrievals.
#'
#' @author Daniel Kick (\email{daniel.kick@usda.gov})
#'
#' @export
#'
#' @examples
require(apsimx)
require(daymetr)
cache_apsimx_daymet <- function(
    lonlat = c(-93.4345, 42.049),
    years = c(2013, 2013),
    dir_path = '../data/env_data/apsimx_daymet',
    force = FALSE,
    delay = 0
){
  ensure_dir_path_exists(dir_path = dir_path)
  save_name <- paste(c(paste(c(lonlat, years), collapse = '_'), '.met'), collapse = '')

  save_path <- paste(dir_path, save_name, sep = '/')
  if( ((!file.exists(save_path))|(force == TRUE)) ){
    Sys.sleep(delay)
    get_daymet2_apsim_met(
      lonlat = lonlat,
      years = years,
      wrt.dir = dir_path,
      filename = save_name
    )
  }
}


#' @title Cache ApsimX Daymet Data
#'
#' @description Wrapper for apsimx::get_power_apsim_met Saves to a desired location.
#'
#' @param lonlat Array containing the longitude and latitude to be downloaded.
#' @param dates Date range to be downloaded as an array of two values of format `'YYYY-MM-DD'`.
#' @param dir_path Desired path for data cache.
#' @param force Boolean that will overwrite cached file if `TRUE`.
#' @param delay Delay between retrievals.
#'
#' @author Daniel Kick (\email{daniel.kick@usda.gov})
#'
#' @export
#'
#' @examples
require(apsimx)
require(nasapower)
cache_apsimx_power <- function(
    lonlat = c(-93.4345, 42.049),
    dates = c('2013-01-01', '2013-12-31'),
    dir_path = '../data/env_data/apsimx_power',
    force = FALSE,
    delay = 0 # add courtesy delay if batch downloading.
){
  ensure_dir_path_exists(dir_path = dir_path)
  save_name <- paste(c(paste(c(lonlat, dates), collapse = '_'), '.met'), collapse = '')

  save_path <- paste(dir_path, save_name, sep = '/')
  if( ((!file.exists(save_path))|(force == TRUE)) ){
    Sys.sleep(delay)
    get_power_apsim_met(
      lonlat = lonlat,
      dates = dates,
      wrt.dir = dir_path,
      filename = save_name
    )
  }
}

