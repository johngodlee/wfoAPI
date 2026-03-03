#' Helper function to convert NULL values to NA
#'
#' @param x Object that is potentially NULL
#'
#' @noRd
#' 
null2na <- function(x) {
  if (is.null(x) | length(x) == 0) {
    NA_character_ 
  } else { 
    x
  }
}

