#' Get taxonomic information from the World Flora Online database using a WFO ID 
#'
#' @description
#' This function retrieves taxonomic information from the World Flora Online 
#'     database, via their GraphQL API
#'
#' @param x vector of WFO IDs, e.g. "wfo-0000214110"
#' @param useCache logical, if TRUE use cached values in
#'     `wfo_cache_get()` preferentially, to reduce the number of API calls
#' @param useAPI logical, if TRUE (default) allow API calls
#' @param raw logical, if TRUE raw a nested list is returned, otherwise a
#'     dataframe
#' @param capacity maximum number of API calls which can accumulate over the 
#'     duration of `fill_time_s`. See documentation for `httr2::req_throttle()`
#' @param fill_time_s time in seconds to refill the capacity for repeated API 
#'     calls. See documentation for `httr2::req_throttle()`
#' @param timeout time in seconds to wait before disconnecting from an
#'     unresponsive request
#'
#' @return dataframe with taxonomic name information for each submitted WFO ID
#' \describe{
#'   \item{taxon_wfo_syn}{WFO ID as in `x`}
#'   \item{taxon_name_syn}{Taxonomic name}
#'   \item{taxon_auth_syn}{Authority}
#'   \item{taxon_stat_syn}{Taxonomy status, e.g. "conserved", "deprecated", 
#'   "illegitimate", etc} 
#'   \item{taxon_role_syn}{Taxonomic role, e.g. "accepted", "synonym", 
#'   "unplaced", etc}
#'   \item{taxon_rank_syn}{Taxonomic rank, e.g. "species", "genus", 
#'   "family", etc} 
#'   \item{taxon_wfo_acc}{WFO ID of accepted name}
#'   \item{taxon_name_acc}{Taxonomic name of accepted name}
#'   \item{taxon_auth_acc}{Authority of accepted name}
#'   \item{taxon_stat_acc}{Taxonomy status of accepted name, e.g. "conserved",
#'   "deprecated", "illegitimate", etc} 
#'   \item{taxon_role_acc}{Taxonomic role of accepted name, e.g. "accepted",
#'   "synonym", "unplaced", etc}
#'   \item{taxon_rank_acc}{Taxonomic rank of accepted name, e.g. "species",
#'   "genus", "family", etc}
#' }
#'
#' @references 
#' Borsch, T. et al. (2020). _World Flora Online: Placing taxonomists at the
#' heart of a definitive and comprehensive global resource on the world's
#' plants_. TAXON, 69, 6. doi10.1002/tax.12373:
#'
#' @author John L. Godlee
#' 
#' @export
#' 
#' @examples
#' x <- c("wfo-0000187086", "wfo-0000620977", "wfo-4000033298")
#' getName(x)
#' getName(x, raw = TRUE)
#'
getName <- function(x, useCache = FALSE, useAPI = TRUE, raw = FALSE, 
  capacity = 60, fill_time_s = 60, timeout = 10) {

  # If API throttling arguments are empty, set to generous values 
  if (is.na(capacity)) { 
    capacity <- length(x)
  }

  if (is.na(fill_time_s)) { 
    fill_time_s <- length(x)
  }

  if (!useCache & !useAPI) {
    stop("Either useCache or useAPI must be TRUE")
  }

  # Check that all WFO IDs are valid
  if (any(!grepl("^wfo-\\d{10}$", x))) {
    stop("x must contain WFO IDs, formatted 'wfo-1234567890'")
  }

  # Check if WFO API is reachable 
  if (useAPI && !checkURL(getOption("wfo.api_uri"))) {
    w <- paste("WFO API unreachable:", getOption("wfo.api_uri"))
    if (useCache) {
      warning(w, "\nOnly cached names will be filled")
      useAPI <- FALSE
    } else {
      stop("\n", w, " and useCache = FALSE, Exiting ...")
    }
  }

  # Extract unique IDs 
  xun <- unique(x)

  # Optionally search cache for names
  match_cache_list <- list()
  if (useCache & length(xun) > 0) {
    # Extract cached names
    match_cache_list <- wfo_cache_get()$getName[xun]
    match_cache_list[sapply(match_cache_list, is.null)] <- NULL

    # Remove names matched in cache from vector of names
    xun <- xun[!xun %in% names(match_cache_list)]

    # Message
    if (length(match_cache_list) > 0) {
      cat(sprintf("Using cached data for %s IDs\n", length(match_cache_list)), "\n")
    }
  }

  # Send message if names not matched in cache and API is off 
  if (!useAPI & length(xun) > 0) {
  cat(sprintf(
      "Some IDs not found in cache and useAPI = FALSE. These IDs will be NA:\n  %s",
      paste(xun, collapse = "\n  ")
    ),
      "\n")
  }

  # For each WFO ID (optionally excluding IDs matched in cache)
  # Construct API calls
  match_api_list <- list()
  if (useAPI & length(xun) > 0) {
    api_call_list <- list()
    for (i in seq_along(xun)) {
      api_vars <- list(searchString = xun[i])
      api_call_list[[i]] <- callAPI(api_vars, 
        query = query_taxonNameById(),
        capacity = capacity,
        fill_time_s = fill_time_s,
        timeout = timeout)
    }

    # Submit API calls
    api_resp_list <- httr2::req_perform_parallel(api_call_list)

    # Convert API responses to JSON
    match_api_list <- lapply(api_resp_list, httr2::resp_body_json)

    # Set WFO version
    wfo_version <- wfoVersion()

    # Collect matched names 
    for (i in seq_along(match_api_list)) {
      match_api_list[[i]] <- match_api_list[[i]]$data$taxonNameById
      match_api_list[[i]]$wfo_version <- wfo_version
    }

    names(match_api_list) <- xun
  }

  # Combine match lists
  match_list <- c(match_cache_list, match_api_list)

  # Store values in cache 
  match_good_list <- match_list[
    !names(match_list) %in% names(wfo_cache_get()$getName)]
  the$wfo_cache$getName <- c(wfo_cache_get()$getName, match_good_list)

  # Add missing values
  # Not matched by cache or API
  match_miss <- x[!x %in% names(match_list)]
  if (length(match_miss) > 0) {
    match_list <- c(match_list, 
      setNames(vector("list", length(match_miss)), match_miss))
  }

  # Order by x
  match_all <- match_list[x]

  if (!raw) {
    # Create formatted dataframe
    out <- do.call(rbind, lapply(match_all, function(i) { 
        data.frame(
          taxon_wfo_syn = null2na(i$id),
          taxon_name_syn = null2na(i$fullNameStringNoAuthorsPlain),
          taxon_auth_syn = null2na(i$authorsString),
          taxon_stat_syn = null2na(i$nomenclaturalStatus),
          taxon_role_syn = null2na(i$role),
          taxon_rank_syn = null2na(i$rank),
          taxon_wfo_acc = null2na(i$currentPreferredUsage$hasName$id),
          taxon_name_acc = null2na(i$currentPreferredUsage$hasName$fullNameStringNoAuthorsPlain),
          taxon_auth_acc = null2na(i$currentPreferredUsage$hasName$authorsString),
          taxon_stat_acc = null2na(i$currentPreferredUsage$hasName$nomenclaturalStatus),
          taxon_role_acc = null2na(i$currentPreferredUsage$hasName$role),
          taxon_rank_acc = null2na(i$currentPreferredUsage$hasName$rank),
          wfo_version = i$wfo_version
        )
    }))

    # Match row order of dataframe to x
    rownames(out) <- NULL
  } else {
    out <- match_all
  }

  # Return
  return(out)
}


