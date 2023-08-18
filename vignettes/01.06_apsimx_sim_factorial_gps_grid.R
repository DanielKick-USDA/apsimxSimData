library(tidyverse)
library(apsimx)

# apsimx_options(exe.path = 'C:\\Users\\drk8b9\\AppData\\Local\\Programs\\APSIM2023.5.7219.0\\bin\\ApsimNG.exe')
# apsimx_options(examples.path = 'C:\\Users\\drk8b9\\AppData\\Local\\Programs\\APSIM2023.5.7219.0\\Examples')

require(soilDB)
require(sp)
require(sf)
require(spData)

devtools::load_all()


cache_path <- '../data/00.06_apsimx_sim_factorial_gps_grid/'
ensure_dir_path_exists(dir_path = cache_path)


modify_and_run_apsimx_factorial <- function(
    ith_lon = -86.52960,
    ith_lat = 34.72952,
    cache_path = "../data/00.06_apsimx_sim_factorial_gps_grid/",
    factorial_file = 'SimulateFactorial_Tiny.apsimx',
    # this should be fixed. If I change the base apsimx file then it should change
    ith_expect_met = '-86.5296_34.72952_2014-01-01_2014-12-31.met',
    ith_year_start = '1984',
    ith_year_end = '2022',
    ith_base_file = "SimulateBasic_-90_34.apsimx"
){
  # Setup ----
  # set up some state variables so that the simulation is run only if the needed inputs are available.
  ssurgo_data_ready  <- FALSE # SSURGO downloaded?
  ssurgo_names_match <- FALSE # Names in json match? (Is the position in the list of lists right?)
  met_data_ready     <- FALSE # Met downloaded?
  met_lines_match    <- FALSE # Exactly one line in the apsimx file to update?
  factorial_temp_file <- NA # If there is no soil data this file won't exist.

  # get weather and soil data
  try(
    run_apsimx_for_gps(
      lon = ith_lon,
      lat = ith_lat,
      year_start = ith_year_start,
      year_end = ith_year_end,
      met_dir = paste0(cache_path, 'power/'),
      ssurgo_dir = paste0(cache_path, 'ssurgo/'),
      planting_date = '1-Apr',
      cultivar = 'A_80',
      base_file = "SimulateBasic_-90_34.apsimx",
      src.dir = cache_path,
      only_dl_data = TRUE
    )
  )

  # Work with Soil Data ----
  # Modify a base_file to insert soil data
  ith_ssurgo_path <- paste0(cache_path,
                            'ssurgo/',
                            paste0(as.character(ith_lon),'_', as.character(ith_lat),'.RDS'))

  # update state variable
  ssurgo_data_ready <- file.exists(ith_ssurgo_path)

  # put soil data in simple file
  if(ssurgo_data_ready){
    edit_tag = '_temp_soil'
    temp_file <- paste0(str_replace(ith_base_file, '\\.apsimx$', ''), edit_tag, '.apsimx')

    surgo_dat <- readRDS(ith_ssurgo_path)

    edit_apsimx_replace_soil_profile(
      file = ith_base_file,
      src.dir = cache_path,
      soil.profile = surgo_dat[[1]],
      edit.tag = edit_tag
    )
  }

  # Move soil dat into complex file
  if(ssurgo_data_ready){
    # Now look at the json of the new file and target experiment
    factorial_file_json <- jsonlite::read_json(paste0(cache_path, factorial_file))
    temp_file_json      <- jsonlite::read_json(paste0(cache_path, temp_file))

    replacement_soil <- temp_file_json$Children[[2]]$Children[[5]]

    current_soil <- factorial_file_json$Children[[2]]$Children[[1]]$Children[[2]]$Children[[5]]

    # check if the names match
    ssurgo_names_match <- 1 == mean(names(replacement_soil) == names(current_soil))

    if(ssurgo_names_match){
      # overwrite with the values
      factorial_file_json$Children[[2]]$Children[[1]]$Children[[2]]$Children[[5]] <- replacement_soil

      factorial_temp_file <- paste0(str_replace(factorial_file, '\\.apsimx', ''), edit_tag, '.apsimx')

      jsonlite::write_json(factorial_file_json,
                           paste0(cache_path, factorial_temp_file),
                           pretty = TRUE,
                           digits = NA, auto_unbox = TRUE, null = "null",
                           na = "null")
    }
  }
  # Work with Weather Data ----
  ith_met_path <- paste0(cache_path,
                         'power/',
                         paste0(as.character(ith_lon),'_',
                                as.character(ith_lat),'_',
                                as.character(ith_year_start),'-',
                                as.character('01-01'),'_',
                                as.character(ith_year_end),'-',
                                as.character('12-31'),
                                '.met'))

  met_data_ready <- file.exists(ith_met_path)
  if((met_data_ready & !is.na(factorial_temp_file))){

    factorial_file_text <- read_file(paste0(cache_path, factorial_temp_file))
    factorial_file_text <- unlist(str_split(factorial_file_text, '\\n'))

    ith_met_path <- str_split(ith_met_path, pattern = '/')[[1]]
    replace_met <- ith_met_path[length(ith_met_path)]


    mask <- str_detect(string = factorial_file_text, ith_expect_met)
    relevant_line <- factorial_file_text[mask]

    met_lines_match <- (length(relevant_line) == 1)
    if(met_lines_match){
      replacement_line <- str_replace(relevant_line, pattern = ith_expect_met, replacement = replace_met)
      factorial_file_text[mask] <- replacement_line
      factorial_file_text <- paste(factorial_file_text, collapse = '\n')
      write_file(factorial_file_text,
                 paste0(cache_path, factorial_temp_file))
    }
  }

  # If the tests pass, then run the file ----
  if((ssurgo_data_ready &
      ssurgo_names_match &
      met_data_ready &
      met_lines_match &
      !is.na(factorial_temp_file))){

    res <- apsimx(
      file = factorial_temp_file,
      src.dir = cache_path,
      value = "report")

  }  else {
    res = data.frame()
  }
  # if there was an issue, return an empty df
  if(nrow(res) == 0){
    res <- data.frame(
      CheckpointID = NA, SimulationID = NA, Experiment = NA, FolderName = NA,
      SowDate = NA, Genotype = NA, Zone = NA, Clock.Today = NA,
      Maize.AboveGround.Wt = NA, Maize.LAI = NA, yield_Kgha = NA, Date = NA
    )
  }
  return(cbind(data.frame(ith_lon, ith_lat, ith_year_start, ith_year_end, factorial_file), res))
}


wrapper_apsimx_factorial <- function(
    cache_path = "../data/00.06_apsimx_sim_factorial_gps_grid/",
    save_path = "../data/00.06_apsimx_sim_factorial_gps_grid/factorial_exps/", # This is for the output not the inputs.
    factorial_file = 'SimulateFactorial_Tiny.apsimx',
    # this should be fixed. If I change the base apsimx file then it should change
    ith_expect_met = '-86.5296_34.72952_2014-01-01_2014-12-31.met',
    ith_lon = -86.52960,
    ith_lat = 34.72952,
    ith_year_start = '1984',
    ith_year_end = '2022',
    ith_base_file = "SimulateBasic_-90_34.apsimx"
){
  # Same approach as `run_and_store_apsimx_for_gps` but for a factorial experiment
  ensure_dir_path_exists(cache_path)
  ensure_dir_path_exists(save_path)

  save_name <- paste0(c(as.character(ith_lon),
                        as.character(ith_lat),
                        as.character(ith_year_start),
                        as.character(ith_year_end),
                        str_replace(factorial_file, '\\.apsimx$', '')
  ), collapse = '__')
  save_name <- paste0(save_name, '.csv')

  print(save_name)
  if(!file.exists(paste0(save_path,save_name))){
    res = modify_and_run_apsimx_factorial(
      cache_path = cache_path,
      factorial_file = factorial_file,
      ith_expect_met = ith_expect_met,
      ith_lon = ith_lon,
      ith_lat = ith_lat,
      ith_year_start = ith_year_start,
      ith_year_end = ith_year_end,
      ith_base_file = ith_base_file
    )

    write.csv(res, paste0(save_path,save_name))
  }
}



# run on all g2f locations ----
gps <- read.csv('../inst/extdata/gps_coords.csv')
gps <- gps[!stringr::str_detect(gps[, 'Env'], '^ONH.+'), ]
gps <- gps[!stringr::str_detect(gps[, 'Env'], '^GEH.+'), ]

gps <- gps %>%
  rename(lat = Latitude_of_Field,
         lon = Longitude_of_Field) %>%
  select(lat, lon) %>%
  distinct()


tic_outer <- Sys.time()
tics <- c()
for(i in seq(1, nrow(gps))){
  ## all because I don't have tqdm ====
  tics[length(tics)+1] <- Sys.time()
  if(length(tics)>1){
    #      # Remaining iter    # Worst case time per iter (and a way to ignore cached results)
    secs = (nrow(gps)-(i-1)) * max(tics[2:length(tics)]-tics[1])
    minutes = as.character(secs %/% 60)
    sec = as.character(round(secs %% 60))
    print(paste(rep('-', 80), collapse = ''))
    print(paste0(minutes, ':', if(as.numeric(sec)>=9){sec}else{paste0('0', sec)}))
    print(paste(rep('-', 80), collapse = ''))
  }
  wrapper_apsimx_factorial(
    ith_lon = gps[i, 'lon'],
    ith_lat = gps[i, 'lat'],
    save_path = "../data/00.06_apsimx_sim_factorial_gps_grid/factorial_exps/", # This is for the output not the inputs.
    # factorial_file = 'SimulateFactorial_Tiny.apsimx', # 'SimulateFactorial.apsimx',
    factorial_file = 'SimulateFactorial.apsimx',
    ith_expect_met = '-86.5296_34.72952_2014-01-01_2014-12-31.met',
    cache_path = "../data/00.06_apsimx_sim_factorial_gps_grid/",
    ith_year_start = '1984',
    ith_year_end = '2022',
    ith_base_file = "SimulateBasic_-90_34.apsimx"
  )
}
toc_outer <- Sys.time()
print(toc_outer-tic_outer)


# Repeat for grid over usa ----
gps <- read.csv('../data/00.01_setup_USA_env_gps_grid/gps_grid.csv')

# no seed needed here. This is just to make sure that the sampling order is random.
gps[['order']] <- sample(1:nrow(gps))
gps <- gps %>% arrange(order)

tic_outer <- Sys.time()
tics <- c()
for(i in seq(1, nrow(gps))){
  ## all because I don't have tqdm ====
  tics[length(tics)+1] <- Sys.time()
  if(length(tics)>1){
    #      # Remaining iter    # Worst case time per iter (and a way to ignore cached results)
    secs = (nrow(gps)-(i-1)) * max(tics[2:length(tics)]-tics[1])
    minutes = as.character(secs %/% 60)
    sec = as.character(round(secs %% 60))
    print(paste(rep('-', 80), collapse = ''))
    print(paste0(minutes, ':', if(as.numeric(sec)>=9){sec}else{paste0('0', sec)}))
    print(paste(rep('-', 80), collapse = ''))
  }
  wrapper_apsimx_factorial(
    ith_lon = gps[i, 'lon'],
    ith_lat = gps[i, 'lat'],
    save_path = "../data/00.06_apsimx_sim_factorial_gps_grid/factorial_exps/", # This is for the output not the inputs.
    factorial_file = 'SimulateFactorial.apsimx',
    ith_expect_met = '-86.5296_34.72952_2014-01-01_2014-12-31.met',
    cache_path = "../data/00.06_apsimx_sim_factorial_gps_grid/",
    ith_year_start = '1984',
    ith_year_end = '2022',
    ith_base_file = "SimulateBasic_-90_34.apsimx"
  )
}
toc_outer <- Sys.time()
print(toc_outer-tic_outer)
