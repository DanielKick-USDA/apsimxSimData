#' @title Find State from GPS
#'
#' @description This function checks if coordinates in a dataframe are in the United States and which state they are in. NA will be returned for coordinates outside the US. It is from [StackOverflow](https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r). Requires `sf` and `spData`.
#'
#' @param pointsDF A data.frame that contains two columns: longitude and latitude. They are interpreted by order not name.
#' @param states A object with the desired state polygons. Not tested with anything but `spData::us_states`.
#' @param name_col The column with state data.
#'
#' @author Josh O'Brien
#'
#' @export
#'
#' @examples lonlat_to_state(data.frame(x = c(-90, -120, 0),  y = c(44, 44, 44)))

lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  # library(sf)
  # library(spData)
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)

  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)

  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}
