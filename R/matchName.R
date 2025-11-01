#' Match a taxonomic name against the WFO GraphQL API
#'
#' @param x taxonomic name to be searched 
#' @param genus_fallback logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#'
#' @return list of data returned by API calls
#' 
#' @export
#' 
#' @examples
#' matchName("Burkea africana")
#' 
matchName <- function(x, genus_fallback = FALSE, interactive = TRUE) {

  # Construct cache search string
  xc <- paste0(x, "_gf", as.character(genus_fallback))

  # Search cache for name 
  if (xc %in% names(wfo_cache$names)) {
    # If name found, use cached version
    match <- wfo_cache$names[[xc]]
    cat(sprintf("Using cached data for: %s\n", x))
  } else {
    # If name not found in cache, send API call
    response <- callAPI(x, query_NameMatch(), genus_fallback = genus_fallback)

    # If interactive and name not found
    if (is.null(response$data$taxonNameMatch$match) & interactive) {
      match <- pickName(x, response$data$taxonNameMatch$candidates)
    } else {
      match <- response$data$taxonNameMatch$match
      match$method <- "AUTO"
    }
  }

  match$genus_fallback <- genus_fallback

  # Store result in cache
  if (match$method %in% c("AUTO", "MANUAL") & 
      !x %in% names(wfo_cache$names) &
      !is.null(match$id) && !is.na(match$id)) {
    wfo_cache$names[[xc]] <- match
  }

  # Return
  return(match)
}


