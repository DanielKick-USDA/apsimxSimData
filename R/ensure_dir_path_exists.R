#' @title Ensure Directory Path Exists.
#'
#' @description Create path if directory doesn't already exist. R version of the same functions in my python notebooks.
#'
#' @param dir_path Desired path. Can include several layers of non-existant directories.
#'
#' @author Daniel Kick (\email{daniel.kick@usda.gov})
#'
#' @export
#'
#' @examples
#'
ensure_dir_path_exists <- function(
    dir_path # path to create if it doesn't already exist.
){
  if (!dir.exists(dir_path)){
    dir.create(dir_path, recursive = T)
  }
}
