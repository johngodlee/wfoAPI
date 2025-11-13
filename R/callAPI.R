#' Call the WorldFlora API to match a taxonomic name
#'
#' @param x taxonomic name to be searched 
#' @param query GraphQL API query string, e.g. as returned `query_taxonNameMatch()`
#'     or `query_taxonNameById()`
#' @param fallbackToGenus logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param checkRank logical, if TRUE consider matches to be ambiguous if it is
#'     possible to estimate taxonomic rank from the search string and the rank
#'     does not match that in the name record
#' @param checkHomonyms logical, if TRUE consider matches to be ambiguous if
#'     there are other names with the same words but different author strings
#' @param fuzzyNameParts integer value of 0 (default) or greater. The maximum
#'     Levenshtein distance used for fuzzy matching words in `x`
#' @param delay number of seconds to pause between API calls. Used to
#'     rate-limit repeated API calls
#'
#' @importFrom httr2 request req_body_json req_perform resp_body_json
#' @return list representation of JSON returned by API call 
#' 
#' @noRd
#' @examples
#' callAPI("Burkea africana", query_taxonNameMatch())
#' callAPI("wfo-0000214110", query_taxonNameById())
#'
callAPI <- function(x, query, fallbackToGenus = FALSE, checkRank = FALSE, 
  checkHomonyms = FALSE, fuzzyNameParts = 0, delay = 0) {

  # Create request 
  req <- httr2::request(getOption("wfo.api_uri"))

  # Convert empty strings to NA
  if (trimws(x) == "" | is.na(x)) { 
    x <- NA
  }

  # If entry is valid
  if (!is.na(x)) { 
    # prepare the body
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

    # Rate limit: pause before making the request
    Sys.sleep(delay)

    # Run request
    resp <- httr2::req_perform(req)

    # return the whole thing as a list of lists
    return(httr2::resp_body_json(resp))
  } else {
    return(NULL)
  }
}

