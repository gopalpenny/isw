#' Check dimensionality
#'
#' Check dimensionality of object
#' @param object Object of which to check dimensionality
#' @param desired_units Required dimensionality, given as units (e.g., "ft/s")
#' @param variable_name Optional name of the object, for error reporting
#' @description
#' Internal function to check the dimensionality (i.e., units) of an object.
#' First, check that the object is a units object. Second, try to convert the
#' object to `desired_units` to ensure dimensionality is correct.
#' @returns
#' As of right now, nothing is returned
#' @examples
#' \dontrun{
#' object <- set_units(1, "ft")
#' desired_units <- "m"
#' check_dimensionality(object, "m") # passes
#'
#' # error because object has wrong dimensionality
#' check_dimensionality(object, "m^2", "object")
#' check_dimensionality(object, "m^2") # error, note difference in error reporting
#'
#' check_dimensionality(1, "m", "object") # error because object not a units object
#' }
#' @noRd
check_dimensionality <- function(object, desired_units, variable_name = NULL) {

  if (is.null(variable_name)) {
    variable_name <- "The variable passed to check_dimensionality()"
  }
  if(!any(class(object)=="units")) {
    stop(paste(variable_name, 'is not a units object. Make sure to provide appropriate units'))
  }
  tryCatch(
    expr = {
      converted_object <- units::set_units(object, desired_units, mode = "standard")
    },
    error = function(cond) {
      conversion_message <- paste(variable_name,"is",object,".",
                        "It should have units that can be converted to",desired_units,
                        "but the conversion failed\n")
      original_error_message <- paste("Here's the original error message:", conditionMessage(cond))
      stop(paste(conversion_message, original_error_message))
      NA
    })
}
