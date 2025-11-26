#' Correct and match taxonomic names to the World Flora Online database
#'
#' @description
#' This function matches taxonomic names using the World Flora Online database,
#'     via their GraphQL API
#'
#' @param x vector of taxonomic names
#' @param fallbackToGenus logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param checkRank logical, if TRUE consider matches to be ambiguous if it is
#'     possible to estimate taxonomic rank from the search string and the rank
#'     does not match that in the name record
#' @param checkHomonyms logical, if TRUE (default) consider matches to be
#'     ambiguous if there are other names with the same words but different
#'     author strings
#' @param fuzzyNameParts integer value of 3 (default) or greater. The maximum
#'     Levenshtein distance used for server-side fuzzy matching of words in `x`
#'     with taxonomic names in the WFO database. This does not affect further
#'     client-side fuzzy matching of multiple ambiguous matches if 
#'     `preferFuzzy = TRUE`
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#' @param preferAccepted logical, if TRUE, if multiple ambiguous matches are
#'     found, and if only one candidate is an "accepted" name,
#'     automatically choose that name
#' @param preferFuzzy logical, if TRUE, if multiple ambiguous matches are 
#'     found, the accepted matched name with the lowest Levenshtein distance to
#'     the submitted name will be returned
#' @param useCache logical, if TRUE use cached values in
#'     `.GlobalEnv : wfo_cache` preferentially, to reduce the number of API
#'     calls
#' @param useAPI logical, if TRUE (default) allow API calls
#' @param raw logical, if TRUE raw a nested list is returned, otherwise a
#'     dataframe
#' @param capacity maximum number of API calls which can accumulate over the 
#'     duration of `fill_time_s`. See documentation for `httr2::req_throttle()`
#' @param fill_time_s time in seconds to refill the capacity for repeated API 
#'     calls. See documentation for `httr2::req_throttle()`
#'
#' @return data.frame containing taxonomic name information with rows matching
#'     names in `x`, or a list containing unique values in `x` if raw = TRUE
#'
#' @references Borsch, T. et al. (2020).
#' _World Flora Online: Placing taxonomists at the heart of a definitive and
#' comprehensive global resource on the world's plants_. TAXON, 69, 6.
#' doi10.1002/tax.12373:
#'
#' @author John L. Godlee
#' 
#' @export
#' 
#' @importFrom stringdist stringsim
#'
#' @examples
#' x <- c("Burkea africana", "Julbernardia paniculata", "Fabaceae", 
#'   "Indet indet", "Brachystegia")
#' matchNames(x)
#' matchNames(x, raw = TRUE)
#' matchNames(x, fallbackToGenus = TRUE)
#' matchNames(x, interactive = FALSE)
#'
matchNames <- function(x, fallbackToGenus = FALSE, checkRank = FALSE, 
  checkHomonyms = TRUE, fuzzyNameParts = 3, interactive = TRUE, 
  preferAccepted = FALSE, preferFuzzy = FALSE, useCache = FALSE, 
  useAPI = TRUE, raw = FALSE, capacity = 60, fill_time_s = 60) {

  if (!useCache & !useAPI) {
    stop("Either useCache or useAPI must be TRUE")
  }

  if (preferFuzzy & interactive) { 
    warning(
      "'preferFuzzy' and 'interactive' are both TRUE, defaulting to interactive matching", 
      immediate. = TRUE)
    preferFuzzy <- FALSE
  }

  # Define function to check URL
  checkURL <- function(url) {
    tryCatch(
      {
        req <- httr2::request(url)
        req <- httr2::req_method(req, "HEAD")
        httr2::req_perform(req)
        TRUE
      },
      error = function(e) {
        FALSE
      }
    )
  }

  # Check if WFO API is reachable 
  if (!checkURL(getOption("wfo.api_uri"))) {
    w <- paste("WFO API unreachable:", getOption("wfo.api_uri"))
    if (useCache) {
      warning(w, "\nOnly cached names will be filled")
      useAPI <- FALSE
    } else {
      stop("\n", w, " and useCache = FALSE, Exiting ...")
    }
  }

  # Extract unique names 
  xun <- sort(unique(x))
  xlen <- length(xun)

  # Optionally search cache for names
  match_cache_list <- list()
  if (useCache) {
    # Extract cached names
    match_cache_list <- the$wfo_cache[xun]

    # Remove names matched in cache from vector of names
    xun <- xun[!xun %in% names(match_cache_list)]
    xlen <- length(xun)

    # Message
    cat(sprintf("Using cached data for %s species\n", length(match_cache_list)))
  }

  # For each taxonomic name (optionally excluding names matched in cache)
  # Construct API calls
  if (useAPI) {
    api_call_list <- list()
    for (i in seq_along(xun)) {
      api_call_list[[i]] <- callAPI(xun[i], 
          query = query_taxonNameMatch(), 
          fallbackToGenus = fallbackToGenus, 
          checkRank = checkRank,
          checkHomonyms = checkHomonyms,
          fuzzyNameParts = fuzzyNameParts,
          capacity = capacity,
          fill_time_s = fill_time_s)
    }

    # Submit API calls
    api_resp_list <- httr2::req_perform_parallel(api_call_list)

    # Convert API responses to JSON
    api_json_list <- lapply(api_resp_list, httr2::resp_body_json)

    # Collect matched names 
    match_api_list <- list()
    for (i in seq_along(api_json_list)) {

      # Find Levenshtein distance between submitted name and candidate names
      cand_dist <- 1 - stringdist::stringsim(x,
        unlist(lapply(
          api_json_list[[i]]$data$taxonNameMatch$candidates, 
          "[[", "fullNameStringNoAuthorsPlain")) )

      # List role of candidate matches (accepted, synonym, unplaced, etc.) 
      role_lev <- c("accepted", "synonym", "unplaced", "deprecated")
      cand_roles <- factor(unlist(lapply(
          api_json_list[[i]]$data$taxonNameMatch$candidates, 
          "[[", "role")), levels = role_lev)

      # Reorder candidate matches according to Levenshtein distance and role
      cand_names <- unlist(lapply(
          api_json_list[[i]]$data$taxonNameMatch$candidates, 
          "[[", "fullNameStringNoAuthorsPlain")) 

      # Sort candidate matches first by Levenshtein distance, then by role
      cand_sort <- order(cand_dist, cand_roles, cand_names)
      api_json_list[[i]]$data$taxonNameMatch$candidates <- 
        api_json_list[[i]]$data$taxonNameMatch$candidates[cand_sort]

      # If unambiguous match found
      if (!is.null(api_json_list[[i]]$data$taxonNameMatch$match)) {
        # Singular accepted name
        match_api_list[[i]] <- api_json_list[[i]]$data$taxonNameMatch$match
        match_api_list[[i]]$method <- "AUTO"
      } else {
        if (preferAccepted && sum(cand_roles == "accepted") == 1) {
          # Auto-accept singular accepted name
          match_api_list[[i]] <- api_json_list[[i]]$data$taxonNameMatch$candidates[[
            which(cand_roles == "accepted")]]
          match_api_list[[i]]$method <- "AUTO ACC"
        } else if (preferFuzzy) {
          # Auto-accept singular accepted name
          match_api_list[[i]] <- api_json_list[[i]]$data$taxonNameMatch$candidates[[1]]
          match_api_list[[i]]$method <- "AUTO FUZZY"
        } else if (interactive) {
          # Interactive name picking
          match_api_list[[i]] <- pickName(x, api_json_list[[i]]$data$taxonNameMatch$candidates)
        } else {
          # No successful match
          cat(sprintf("No cached name for: %s\n", x))
          match_api_list[[i]] <- list()
          match_api_list[[i]]$method <- "EMPTY"
        }
      }

      # Add query parameters
      match_api_list[[i]]$submitted_name <- xun[i]
      match_api_list[[i]]$fallbackToGenus <- fallbackToGenus
      match_api_list[[i]]$checkRank <- checkRank
      match_api_list[[i]]$checkHomonyms <- checkHomonyms
      match_api_list[[i]]$fuzzyNameParts <- fuzzyNameParts 
      match_api_list[[i]]$preferAccepted <- preferAccepted 
      match_api_list[[i]]$preferFuzzy <- preferFuzzy 
    }
    names(match_api_list) <- xun

  }

  # Combine match lists
  match_list <- c(match_cache_list, match_api_list)

  if (raw) {
    # Return raw list output
    out <- match_list
  } else {
    # Create formatted dataframe
    out <- do.call(rbind, lapply(match_list, function(i) { 
      if ("id" %in% names(i)) {
        data.frame(
          taxon_name_subm = null2na(i$submitted_name),
          method = null2na(i$method),
          fallbackToGenus = null2na(i$fallbackToGenus),
          checkRank = null2na(i$checkRank),
          checkHomonyms = null2na(i$checkHomonyms),
          fuzzyNameParts = null2na(i$fuzzyNameParts),
          preferAccepted = null2na(i$preferAccepted),
          preferFuzzy = null2na(i$preferFuzzy),
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
          taxon_rank_acc = null2na(i$currentPreferredUsage$hasName$rank))
      } else { 
        data.frame(
          taxon_name_subm = i$submitted_name,
          method = i$method,
          fallbackToGenus = i$fallbackToGenus,
          checkRank = i$checkRank,
          checkHomonyms = i$checkHomonyms,
          fuzzyNameParts = i$fuzzyNameParts,
          preferAccepted = i$preferAccepted,
          preferFuzzy = i$preferFuzzy,
          taxon_wfo_syn = NA_character_,
          taxon_name_syn = NA_character_,
          taxon_auth_syn = NA_character_,
          taxon_stat_syn = NA_character_,
          taxon_role_syn = NA_character_,
          taxon_rank_syn = NA_character_,
          taxon_wfo_acc = NA_character_,
          taxon_name_acc = NA_character_,
          taxon_auth_acc = NA_character_,
          taxon_stat_acc = NA_character_,
          taxon_role_acc = NA_character_,
          taxon_rank_acc = NA_character_)
      }
    }))

    # Match row order of dataframe to x
    out <- out[match(x, out$taxon_name_subm),]
  }

  # Store good API results in cache
  match_api_good_list <- match_api_list[
    unlist(lapply(match_api_list, "[[", "method")) %in% c("AUTO", "MANUAL") &
    !names(match_api_list) %in% names(the$wfo_cache)]
  the$wfo_cache <- c(the$wfo_cache, match_api_good_list)

  # Return
  return(out)
}

