#' Check that a URL is responsive
#'
#' @param x URL to check
#'
#' @return logical
#' 
#' @noRd
#' 
checkURL <- function(x) {
  tryCatch(
    {
      req <- httr2::request(x)
      req <- httr2::req_method(req, "HEAD")
      httr2::req_perform(req)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

