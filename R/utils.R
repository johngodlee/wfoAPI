#' Helper function to convert NULL values to NA
#'
#' @param x Object that is potentially NULL
#'
#' @noRd
#' 
null2na <- function(x) {
  if (is.null(x)) {
    NA_character_ 
  } else { 
    x
  }
}

