.regionalepi_package_data <- function(name) {
  data_environment <- new.env(parent = emptyenv())
  utils::data(
    list = name, package = "regionalepi", envir = data_environment
  )
  if (!exists(name, envir = data_environment, inherits = FALSE)) {
    stop("Installed regionalepi package data are incomplete.", call. = FALSE)
  }
  get(name, envir = data_environment, inherits = FALSE)
}
