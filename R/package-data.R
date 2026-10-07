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

.regionalepi_geography_resources <- function() {
  contract <- "regionalepi geography resources"
  resource <- .regionalepi_package_data(
    "regionalepi_geography_resources_2024"
  )
  expected <- c(
    "bkg_districts", "survstat_aliases", "survstat_incidence_aliases",
    "survstat_spatial_units", "survstat_source_spatial_relations",
    "survstat_incidence_assembly_spec", "provenance"
  )
  if (!identical(names(resource), expected) ||
      !identical(resource$provenance$resource_id,
                 "regionalepi_geography_resources_2024")) {
    .stop_contract(contract, "installed resource structure is invalid")
  }
  validate_geography(resource$bkg_districts)
  validate_geography_aliases(resource$survstat_aliases)
  validate_geography_aliases(resource$survstat_incidence_aliases)
  .validate_spatial_units(resource$survstat_spatial_units)
  validate_source_spatial_relations(
    resource$survstat_source_spatial_relations
  )
  .validate_incidence_assembly_spec(
    resource$survstat_incidence_assembly_spec, contract
  )
  resource
}
