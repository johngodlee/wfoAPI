#' Match a taxonomic name against the WFO GraphQL API
#'
#' @param x taxonomic name to be searched 
#' @param fallbackToGenus logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param checkRank logical, if TRUE consider matches to be ambiguous if it is
#'     possible to estimate taxonomic rank from the search string and the rank
#'     does not match that in the name record
#' @param checkHomonyms logical, if TRUE consider matches to be ambiguous if
#'     there are other names with the same words but different author strings
#' @param fuzzyNameParts integer value of 0 (default) or greater. The maximum
#'     Levenshtein distance used for fuzzy matching words in `x`
#' @param preferAccepted logical, if TRUE, if multiple ambiguous matches are
#'     found, and if there is only one candidate is an "accepted" name,
#'     automatically choose that name
#' @param useCache logical, if TRUE use cached values in
#'     `.GlobalEnv : wfo_cache` preferentially, to reduce the number of API
#'     calls
#' @param useAPI logical, if TRUE (default) allow API calls
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#' @param delay number of seconds to pause before sending the API call. Used to
#'     rate-limit repeated API calls
#'
#' @return list representation of JSON returned by API call 
#' 
#' @noRd
#' @examples
#' matchName("Burkea africana")
#' 
matchName <- function(x, fallbackToGenus = FALSE, checkRank = FALSE, 
  checkHomonyms = FALSE, fuzzyNameParts = 0, preferAccepted = FALSE, 
  useCache = FALSE, useAPI = TRUE, interactive = TRUE, delay = 0) {

  # Search cache for name 
  if (useCache && x %in% names(wfo_cache$names) ) {
    # If name found, use cached version
    match <- wfo_cache$names[[x]]
    cat(sprintf("Using cached data for: %s\n", x))
  } else if (useAPI == TRUE) {
    # If name not found in cache, send API call
    response <- callAPI(x, query_taxonNameMatch(), 
      fallbackToGenus = fallbackToGenus,
      checkRank = checkRank,
      checkHomonyms = checkHomonyms,
      fuzzyNameParts = fuzzyNameParts,
      delay = delay)

    # If interactive and name not found
    cand_roles <- unlist(lapply(response$data$taxonNameMatch$candidates, "[[", "role")) 
    if (is.null(response$data$taxonNameMatch$match)) {
      if (preferAccepted && sum(cand_roles == "accepted") == 1) {
        match <- response$data$taxonNameMatch$candidates[[which(cand_roles == "accepted")]]
        match$method <- "AUTO ACC"
      } else if (interactive) {
        match <- pickName(x, 
          response$data$taxonNameMatch$candidates,
          delay = delay)
      } else {
        cat(sprintf("No cached name for: %s\n", x))
        match <- list()
        match$method <- "EMPTY"
      }
    } else {
      match <- response$data$taxonNameMatch$match
      match$method <- "AUTO"
    }
  } else {
    cat(sprintf("No cached name for: %s\n", x))
    match <- list()
      match$method <- "EMPTY"
  }

  # Add query parameters
  match$fallbackToGenus <- fallbackToGenus
  match$checkRank <- checkRank
  match$checkHomonyms <- checkHomonyms
  match$fuzzyNameParts <- fuzzyNameParts 
  match$preferAccepted <- preferAccepted 

  # Store result in cache
  if (match$method %in% c("AUTO", "MANUAL") & 
      !x %in% names(wfo_cache$names) &
      !is.null(match$id) && !is.na(match$id)) {
    wfo_cache$names[[x]] <- match
  }

  # Return
  return(match)
}


