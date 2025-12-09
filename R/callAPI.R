#' Call the WorldFlora API to match a taxonomic name
#'
#' @param x taxonomic name to be searched 
#' @param query GraphQL API query string, e.g. as returned
#'     `query_taxonNameMatch()`, `query_taxonNameById()`
#'     `query_taxonConceptById()`
#' @param fallbackToGenus logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param checkRank logical, if TRUE consider matches to be ambiguous if it is
#'     possible to estimate taxonomic rank from the search string and the rank
#'     does not match that in the name record
#' @param checkHomonyms logical, if TRUE consider matches to be ambiguous if
#'     there are other names with the same words but different author strings
#' @param fuzzyNameParts integer value of 3 (default) or greater. The maximum
#'     Levenshtein distance used for fuzzy matching words in `x`
#' @param capacity maximum number of API calls which can accumulate over the 
#'     duration of `fill_time_s`. See documentation for `httr2::req_throttle()`
#' @param fill_time_s time in seconds to refill the capacity for repeated API 
#'     calls. See documentation for `httr2::req_throttle()`
#' @param timeout time in seconds to wait before disconnecting from an
#'     unresponsive request
#'
#' @return object containing API call
#' 
#' @noRd
#' @examples
#' callAPI("Burkea africana", query_taxonNameMatch())
#' callAPI("wfo-0000214110", query_taxonNameById())
#'
callAPI <- function(x, query, fallbackToGenus = FALSE, checkRank = FALSE, 
  checkHomonyms = FALSE, fuzzyNameParts = 3, capacity = 60, fill_time_s = 60, timeout = 10) {

  # Create request 
  req <- httr2::request(getOption("wfo.api_uri"))

  # Prepare body of call
  variables <- list(
    searchString = x, 
    fallbackToGenus = fallbackToGenus,
    checkRank = checkRank,
    checkHomonyms = checkHomonyms,
    fuzzyNameParts = fuzzyNameParts
  )
  payload <- list(query = query, variables = variables)

  # Set body
  req <- httr2::req_body_json(req, payload, auto_unbox = TRUE)
  
  # Set timeout
  req <- httr2::req_options(req, timeout = timeout)

  # Set throttle to avoid rate-limiting 
  req <- httr2::req_throttle(req, 
    capacity = capacity, 
    fill_time_s = fill_time_s)

  # Return
  return(req)
}

