#' Call the WorldFlora API to match a taxonomic name
#'
#' @param x taxonomic name to be searched 
#' @param query GraphQL API query string, e.g. as returned `query_NameMatch()`
#'     or `query_taxonNameById()`
#' @param genus_fallback logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#'
#' @importFrom httr2 request req_body_json req_perform resp_body_json
#' @return list representation of JSON returned by API call 
#' 
#' @noRd
#' @examples
#' callAPI("Burkea africana", query_NameMatch())
#' callAPI("wfo-0000214110", query_taxonNameById())
#'
callAPI <- function(x, query, genus_fallback = FALSE) {

  # Create request 
  req <- httr2::request(paste(unlist(options("wfo.api_uri"))))

  # prepare the body
  variables <- list(searchString = x, fallbackToGenus = genus_fallback)
  payload <- list(query = query, variables = variables)

  # set the body
  req <- httr2::req_body_json(req, data = payload, auto_unbox = TRUE)

  # actually run the request
  resp <- httr2::req_perform(req)

  # return the whole thing as a list of lists
  return(httr2::resp_body_json(resp))

}

