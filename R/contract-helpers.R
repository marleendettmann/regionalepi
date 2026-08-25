.stop_contract <- function(contract, message) {
  stop(sprintf("Invalid %s: %s", contract, message), call. = FALSE)
}

.require_data_frame <- function(x, contract) {
  if (!is.data.frame(x)) {
    .stop_contract(contract, "must be a data frame or tibble")
  }
}

.require_columns <- function(x, columns, contract) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    .stop_contract(
      contract,
      sprintf("missing required column(s): %s", paste(missing, collapse = ", "))
    )
  }
}

.check_character <- function(x, field, contract, allow_na = FALSE) {
  if (!is.character(x)) {
    .stop_contract(contract, sprintf("`%s` must be character", field))
  }
  if (!allow_na && anyNA(x)) {
    .stop_contract(contract, sprintf("`%s` must not contain NA", field))
  }
  if (any(!is.na(x) & !nzchar(x))) {
    .stop_contract(contract, sprintf("`%s` must not contain empty strings", field))
  }
}

.check_date <- function(x, field, contract, allow_na = FALSE) {
  if (!inherits(x, "Date")) {
    .stop_contract(contract, sprintf("`%s` must be Date", field))
  }
  if (!allow_na && anyNA(x)) {
    .stop_contract(contract, sprintf("`%s` must not contain NA", field))
  }
}

.check_posixct <- function(x, field, contract, allow_na = FALSE) {
  if (!inherits(x, "POSIXct")) {
    .stop_contract(contract, sprintf("`%s` must be POSIXct", field))
  }
  if (!allow_na && anyNA(x)) {
    .stop_contract(contract, sprintf("`%s` must not contain NA", field))
  }
}

.check_numeric <- function(x, field, contract, non_negative = FALSE,
                           whole = FALSE, allow_na = FALSE) {
  if (!is.numeric(x)) {
    .stop_contract(contract, sprintf("`%s` must be numeric", field))
  }
  if (!allow_na && anyNA(x)) {
    .stop_contract(contract, sprintf("`%s` must not contain NA", field))
  }
  values <- x[!is.na(x)]
  if (any(!is.finite(values))) {
    .stop_contract(contract, sprintf("`%s` must contain only finite values", field))
  }
  if (non_negative && any(values < 0)) {
    .stop_contract(contract, sprintf("`%s` must be non-negative", field))
  }
  if (whole && any(values != floor(values))) {
    .stop_contract(contract, sprintf("`%s` must be whole-valued", field))
  }
}

.check_whole_number <- function(x, field, contract, allow_na = FALSE,
                                non_negative = FALSE) {
  .check_numeric(x, field, contract, non_negative = non_negative,
                 whole = TRUE, allow_na = allow_na)
}

.check_optional <- function(x, field, checker, contract, ...) {
  if (field %in% names(x)) {
    checker(x[[field]], field, contract, ...)
  }
}

.require_named_list <- function(x, fields, contract) {
  if (!is.list(x) || is.data.frame(x)) {
    .stop_contract(contract, "must be a named list")
  }
  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    .stop_contract(contract, "must be a named list")
  }
  missing <- setdiff(fields, names(x))
  if (length(missing)) {
    .stop_contract(
      contract,
      sprintf("missing required element(s): %s", paste(missing, collapse = ", "))
    )
  }
}

.check_scalar_character <- function(x, field, contract) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .stop_contract(contract, sprintf("`%s` must be one non-empty character value", field))
  }
}

.check_scalar_number <- function(x, field, contract, non_negative = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    .stop_contract(contract, sprintf("`%s` must be one finite numeric value", field))
  }
  if (non_negative && x < 0) {
    .stop_contract(contract, sprintf("`%s` must be non-negative", field))
  }
}
