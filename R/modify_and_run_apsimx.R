#' @title Modify and Run ApsimX Simulation
#'
#' @description The key wrapper function to change and run an apsimx simulation.
#'
#' @param met_path Path to the weather met file to be used.
#' @param clock_start Beginning of simulation.
#' @param clock_end End of simulation.
#' @param ssurgo_path Path to the cached SSURGO data to be used (.RDS).
#' @param planting_date Planting date in format day-month.abbr
#' @param base_file ApsimX simulation file to be used as the starting template.
#' @param src.dir Directory contaiing `base_file`
#'
#' @author Daniel Kick (\email{daniel.kick@usda.gov})
#'
#' @export
#'
#' @examples
require(apsimx)
modify_and_run_apsimx <- function(
    met_path = './power/-92.328636_38.951561_2013-01-01_2013-12-31.met',
    clock_start = '2013-01-01T00:00:00',
    clock_end = '2014-01-01T00:00:00',
    ssurgo_path = paste0('./output/usa_grid/apsimx_ssurgo/', "-117.120232582092_42.9967880249023.RDS"),
    planting_date = '1-Apr',
    cultivar = 'A_80',
    base_file = "BasicSimulation.apsimx",
    src.dir = "./",
    save_modifications = FALSE,
    save_path = ''
){
  # met_path = './power/-92.328636_38.951561_2013-01-01_2013-12-31.met'
  # clock_start = '2013-01-01T00:00:00'
  # clock_end = '2014-01-01T00:00:00'
  # ssurgo_path = paste0('./output/usa_grid/apsimx_ssurgo/', "-117.120232582092_42.9967880249023.RDS")
  # planting_date = '1-Apr'
  # cultivar = 'A_80'
  # base_file = "BasicSimulation.apsimx"



  edit_tag = '-temp'
  temp_file <- paste0(str_replace(base_file, '\\.apsimx$', ''), edit_tag, '.apsimx')

  # Make edits ----
  ## Metadata ====
  # Planting Date
  edit_apsimx(
    file = base_file,
    src.dir = src.dir,
    node = 'Crop',
    parm = 'SowDate',
    value = planting_date,
    edit.tag = edit_tag
  )
  # Cultivar
  edit_apsimx(
    file = temp_file,
    src.dir = src.dir,
    node = 'Crop',
    parm = 'CultivarName',
    value = cultivar,
    overwrite = TRUE
  )
  ## Soil ====
  surgo_dat <- readRDS(ssurgo_path)

  edit_apsimx_replace_soil_profile(
    file = temp_file,
    src.dir = src.dir,
    soil.profile = surgo_dat[[1]],
    overwrite = TRUE
  )
  ## Weather and Clock ====
  edit_apsimx(
    file = temp_file,
    src.dir = src.dir,
    node = 'Clock',
    parm = 'Start',
    value = clock_start,
    overwrite = TRUE
  )
  edit_apsimx(
    file = temp_file,
    src.dir = src.dir,
    node = 'Clock',
    parm = 'End',
    value = clock_end,
    overwrite = TRUE
  )
  edit_apsimx(
    file = temp_file,
    src.dir = src.dir,
    node = 'Weather',
    value = met_path,
    overwrite = TRUE
  )


  # Inspect if needed
  # inspect_apsimx(
  #   file = temp_file,
  #   src.dir = ".",
  #   node = 'Crop')

  # Run Simulation ----
  res = apsimx(
    file = temp_file,
    src.dir = src.dir,
    value = "report")


  if(save_modifications){
    file.copy(paste0(src.dir, base_file),
              save_path)
  }
  # remove temp file
  unlink(paste0(src.dir, temp_file))
  return(res)
}





#' @title Modify and Run ApsimX Simulation for a lon/lat pair.
#'
#' @description Wrapper for `modify_and_run_apsimx` to prepare the needed met and ssurgo files before running. If one or more file is unavailable, the simulation will not be run and a data.frame with missing values will be returned.
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param met_dir Path to directory the weather met file is or will be stored in.
#' @param clock_start Beginning of simulation.
#' @param clock_end End of simulation.
#' @param ssurgo_dir Path to directory the cached SSURGO data is or will be stored in.
#' @param planting_date Planting date in format day-month.abbr
#' @param base_file ApsimX simulation file to be used as the starting template.
#' @param src.dir Directory contaiing `base_file`
#' @param only_dl_data If TRUE, only download the needed enviromental files, don't run the simulation.
#'
#' @author Daniel Kick (\email{daniel.kick@usda.gov})
#'
#' @export
#'
#' @examples

run_apsimx_for_gps <- function(
    lon = -90.76036,
    lat = 34.72952,
    year_start = '2014',
    year_end = '2014',
    met_dir = paste0(cache_path, 'power/'),
    ssurgo_dir = paste0(cache_path, 'ssurgo/'),
    planting_date = '1-Apr',
    cultivar = 'A_80',
    base_file = "BasicSimulation.apsimx",
    src.dir = "./",
    only_dl_data = FALSE
){

  # lon = -90.76036
  # lat = 34.72952
  # year = '2014'
  # met_dir = paste0(cache_path, 'power/')
  # ssurgo_dir = paste0(cache_path, 'ssurgo/')
  # planting_date = '1-Apr'
  # cultivar = 'A_80'
  # base_file = "BasicSimulation.apsimx"

  # Make sure there's weather data to use
  daterange = c(paste0(year_start, '-01-01'), paste0(year_end, '-12-31'))

  try(
    cache_apsimx_power(
      lonlat = c(lon, lat),
      dates = daterange,
      dir_path = met_dir,
      force = FALSE,
      delay = 0
    )
  )

  met_file <- paste(lon, '_', lat, '_', daterange[1], '_', daterange[2], '.met', sep = '')

  # Lookup ssurgo data
  try( # This is in case lon/lats are passed in that are not in the database. The model is not run if the files don't exist so this a way of preventing a batch download through a loop from terminating early.
    cache_apsimx_ssurgo(
      lonlat = c(lon, lat),
      dir_path = ssurgo_dir,
      force = FALSE,
      delay = 0 # add courtesy delay if batch downloading.
    )
  )

  ssurgo_file <- paste(lon, '_', lat, '.RDS', sep = '')

  # option for only downloading data
  if(!only_dl_data){
    # check if the needed files exist
    if((file.exists(paste0(met_dir, met_file)) &
        file.exists(paste0(ssurgo_dir, ssurgo_file)))){
      # Run
      res <- modify_and_run_apsimx(
        met_path = paste0(met_dir, met_file),
        clock_start = paste0(daterange[1], 'T00:00:00'),
        clock_end = paste0(daterange[2], 'T00:00:00'),
        ssurgo_path = paste0(ssurgo_dir, ssurgo_file),
        planting_date = planting_date,
        cultivar = cultivar,
        base_file = base_file,
        src.dir = src.dir
        )
    } else {
      res = data.frame()
    }
    # if there was an issue, return an empty df
    if(nrow(res) == 0){
      res <- data.frame(
        CheckpointID = NA,SimulationID = NA,Zone = NA,Clock.Today = NA,
        Maize.AboveGround.Wt = NA,Maize.LAI = NA, yield_Kgha = NA,Date = NA)
    }
    return(cbind(data.frame(lon, lat, year_start, year_end, planting_date, cultivar, base_file), res))
  }
}





#' @title Run and Cache Simulation.
#'
#' @description Wrapper for `run_apsimx_for_gps`  to aid in caching many results. Only runs the simulation if it doesn't already exist. Takes the same arguments as `run_apsimx_for_gps` and `cache_dir` which stores the output. `run_apsimx_for_gps`will be responsible for NOT running a simulation that lacks at least one of the input files.
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param met_dir Path to directory the weather met file is or will be stored in.
#' @param clock_start Beginning of simulation.
#' @param clock_end End of simulation.
#' @param ssurgo_dir Path to directory the cached SSURGO data is or will be stored in.
#' @param planting_date Planting date in format day-month.abbr
#' @param base_file ApsimX simulation file to be used as the starting template.
#' @param src.dir Directory contaiing `base_file`
#' @param cache_dir Directory to store simulation results.
#' @param only_dl_data If TRUE, only download the needed enviromental files, don't run the simulation.
#'
#' @author Daniel Kick (\email{daniel.kick@usda.gov})
#'
#' @export
#'
#' @examples

run_and_store_apsimx_for_gps <- function(
    lon = -90.76036,
    lat = 34.72952,
    year_start = '2014',
    year_end = '2014',
    met_dir = paste0(cache_path, 'power/'),
    ssurgo_dir = paste0(cache_path, 'ssurgo/'),
    planting_date = '1-Apr',
    cultivar = 'A_80',
    base_file = "BasicSimulation.apsimx",
    src.dir = "./",
    cache_dir = paste0(cache_path, 'result_cache/'),
    only_dl_data = FALSE
){
  ensure_dir_path_exists(cache_dir)

  save_name <- paste0(c(as.character(lon),
                        as.character(lat),
                        as.character(year_start),
                        as.character(year_end),
                        planting_date,
                        cultivar,
                        str_replace(base_file, '\\.apsimx$', '')
  ), collapse = '__')
  save_name <- paste0(save_name, '.csv')

  if(!file.exists(paste0(cache_dir,save_name))){
    # not running on bad input is handeld by `run_apsimx_for_gps`.
    # An empty df will be returned and written preventing that dead end from being explored again.
    res = run_apsimx_for_gps(
      lon = lon,
      lat = lat,
      year_start = year_start,
      year_end = year_end,
      met_dir = met_dir,
      ssurgo_dir = met_dir,
      planting_date = planting_date,
      cultivar = cultivar,
      base_file = base_file,
      src.dir = src.dir,
      only_dl_data = only_dl_data
    )
    write.csv(res, paste0(cache_dir,save_name))
  }
}
