#' Correct and match taxonomic names to the World Flora Online database
#'
#' @description
#' This function matches taxonomic names using the World Flora Online database,
#'     via their GraphQL API
#'
#' @param x vector of taxonomic names
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#' @param sub_pattern character vector of regex patterns which will be removed
#'     from names in `x` using `gsub()`. The order of this vector matters,
#'     substitutions are applied sequentially. Sensible defaults are provided
#'     by `subPattern()`
#' @param tolower logical, if TRUE names are converted to all lower case
#'     characters 
#' @param nonumber logical, if TRUE names containing numbers will be
#'     interpreted as genera, only matching the first word
#' @param nobracket logical If TRUE characters occurring after "(" or ")" will
#'     be removed. Note this could also remove authorities.
#' @param preferAccepted logical, if TRUE, if multiple ambiguous matches are
#'     found, and if only one candidate is an "accepted" name,
#'     automatically choose that name
#' @param preferFuzzy logical, if TRUE, if multiple ambiguous matches are 
#'     found, the accepted matched name with the lowest Levenshtein distance to
#'     the submitted name will be returned
#' @param useCache logical, if TRUE use cached values in
#'     `wfo_cache_get()` preferentially, to reduce the number of API
#'     calls
#' @param useAPI logical, if TRUE (default) allow API calls
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
#' @param raw logical, if TRUE raw a nested list is returned, otherwise a
#'     dataframe
#' @param capacity maximum number of API calls which can accumulate over the 
#'     duration of `fill_time_s`. See documentation for `httr2::req_throttle()`
#' @param fill_time_s time in seconds to refill the capacity for repeated API 
#'     calls. See documentation for `httr2::req_throttle()`
#' @param timeout time in seconds to wait before disconnecting from an
#'     unresponsive request
#'
#' @return data.frame containing taxonomic name information with rows matching
#'     names in `x`, or a list containing unique values in `x` if raw = TRUE
#' \describe{
#'   \item{taxon_name_orig}{Original name as in `x`}
#'   \item{taxon_name_subm}{Name after optional sanitisation according to
#'   `sub_pattern`, `tolower`, `nonumber`, and `nobracket`}
#'   \item{method}{The method by which the name was matched. Either: "AUTO" if
#'   a single non-ambiguous accepted name was matched, "AUTO ACC" if
#'   preferAccepted = TRUE and a single accepted name was found among the
#'   possible candidate names, "AUTO FUZZY" if preferFuzzy = TRUE and at least
#'   one accepted name was found among the possible candidate names, "MANUAL"
#'   if interactive = TRUE and the user picked a name, or "EMPTY" if no matches
#'   were found}
#'   \item{fallbackToGenus}{Value of argument in function call}
#'   \item{checkRank}{Value of argument in function call}
#'   \item{checkHomonyms}{Value of argument in function call}
#'   \item{fuzzyNameParts}{Value of argument in function call}
#'   \item{preferAccepted}{Value of argument in function call}
#'   \item{preferFuzzy}{Value of argument in function call}
#'   \item{tolower}{Value of argument in function call}
#'   \item{nonumber}{Value of argument in function call}
#'   \item{nobracket}{Value of argument in function call}
#'   \item{taxon_wfo_syn}{WFO ID of matched name}
#'   \item{taxon_name_syn}{Taxonomic name of matched name}
#'   \item{taxon_auth_syn}{Authority of matched name}
#'   \item{taxon_stat_syn}{Taxonomy status of matched name, e.g. "conserved",
#'   "deprecated", "illegitimate", etc} 
#'   \item{taxon_role_syn}{Taxonomic role of matched name, e.g. "accepted",
#'   "synonym", "unplaced", etc}
#'   \item{taxon_rank_syn}{Taxonomic rank of matched name, e.g. "species",
#'   "genus", "family", etc} 
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
matchNames <- function(x, interactive = TRUE, sub_pattern = subPattern(), 
  tolower = FALSE, nonumber = FALSE, nobracket = FALSE, 
  preferAccepted = FALSE, preferFuzzy = FALSE, useCache = FALSE, useAPI = TRUE, 
  fallbackToGenus = FALSE, checkRank = FALSE, checkHomonyms = TRUE, fuzzyNameParts = 3, 
  raw = FALSE, capacity = 60, fill_time_s = 60, timeout = 10) {

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

  if (preferFuzzy & interactive) { 
    warning(
      "'preferFuzzy' and 'interactive' are both TRUE, defaulting to interactive matching", 
      immediate. = TRUE)
    preferFuzzy <- FALSE
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

  # Create duplicate names to prepare for sanitising
  xsub <- x

  # Optionally convert characters to lowercase
  if (tolower) {
    xsub <- tolower(xsub)
  }

  # Optionally remove substrings matched by sub_pattern
  sub_pattern <- na.omit(sub_pattern)
  if (length(sub_pattern) > 0) {
    for (i in seq_along(sub_pattern)) {
      xsub <- gsub(sub_pattern[i], "", xsub)
    }
  } 

  # Optionally converts names with a number to genera (first word only)
  if (nonumber) { 
    has_num <- grepl("[0-9]", xsub)
    xsub[has_num] <- gsub("\\s.*", "", xsub[has_num])
  }

  # Optionally remove characters after parentheses
  if (nobracket) {
    xsub <- gsub("\\(.*|\\).*", "", xsub)
  }

  # Remove leading and trailing whitespace and multiple spaces
  xsub <- trimws(gsub("\\s+", " ", xsub))

  # Extract unique names 
  xun <- sort(unique(xsub))

  # Optionally search cache for names
  match_cache_list <- list()
  if (useCache) {
    # Extract cached names
    match_cache_list <- wfo_cache_get()$matchNames[xun]
    match_cache_list[sapply(match_cache_list, is.null)] <- NULL

    # Remove names matched in cache from vector of names
    xun <- xun[!xun %in% names(match_cache_list)]

    # Message
    if (length(match_cache_list) > 0) {
      cat(sprintf("Using cached data for %s names\n", length(match_cache_list)), "\n")
    }
  }

  # Send message if names not matched in cache and API is off 
  if (!useAPI & length(xun) > 0) {
  cat(sprintf(
      "Some names not found in cache and useAPI = FALSE. These names will be NA:\n  %s",
      paste(xun, collapse = "\n  ")
    ),
      "\n")
  }

  # For each taxonomic name (optionally excluding names matched in cache)
  # Construct API calls
  match_api_list <- list()
  if (useAPI & length(xun) > 0) {
    api_call_list <- list()
    for (i in seq_along(xun)) {
      api_vars <- list(
        searchString = xun[i],
        fallbackToGenus = fallbackToGenus, 
        checkRank = checkRank,
        checkHomonyms = checkHomonyms,
        fuzzyNameParts = fuzzyNameParts)

      api_call_list[[i]] <- callAPI(api_vars, 
          query = query_taxonNameMatch(), 
          capacity = capacity,
          fill_time_s = fill_time_s,
          timeout = timeout)
    }

    # Submit API calls
    api_resp_list <- httr2::req_perform_parallel(api_call_list)

    # Convert API responses to JSON
    api_json_list <- lapply(api_resp_list, httr2::resp_body_json)

    # Collect matched names 
    for (i in seq_along(api_json_list)) {

      # If unambiguous match found
      if (!is.null(api_json_list[[i]]$data$taxonNameMatch$match)) {
        # Singular accepted name
        match_api_list[[i]] <- api_json_list[[i]]$data$taxonNameMatch$match
        match_api_list[[i]]$method <- "AUTO"
      } else if (length(api_json_list[[i]]$data$taxonNameMatch$candidates) != 0) {
        # Find Levenshtein distance between submitted name and candidate names
        cand_dist <- 1 - stringdist::stringsim(xun[i],
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
          match_api_list[[i]] <- pickName(xun[i], api_json_list[[i]]$data$taxonNameMatch$candidates)
        } else {
          # No successful match
          cat(sprintf("No match for: %s\n", xun[i]))
          match_api_list[[i]] <- list()
          match_api_list[[i]]$method <- "EMPTY"
        }
      } else {
        # No candidates 
        cat(sprintf("No candidates for: %s\n", xun[i]))
        match_api_list[[i]] <- list()
        match_api_list[[i]]$method <- "EMPTY"
      }

      # Add query parameters
      match_api_list[[i]]$submitted_name <- xun[i]
      match_api_list[[i]]$fallbackToGenus <- fallbackToGenus
      match_api_list[[i]]$checkRank <- checkRank
      match_api_list[[i]]$checkHomonyms <- checkHomonyms
      match_api_list[[i]]$fuzzyNameParts <- fuzzyNameParts 
      match_api_list[[i]]$preferAccepted <- preferAccepted 
      match_api_list[[i]]$preferFuzzy <- preferFuzzy 
      match_api_list[[i]]$tolower <- tolower 
      match_api_list[[i]]$nonumber <- nonumber 
      match_api_list[[i]]$nobracket <- nobracket 
    }
    names(match_api_list) <- xun

  }

  # Combine match lists
  match_list <- c(match_cache_list, match_api_list)

  if (raw) {

    # Add missing matches
    match_miss <- xsub[!xsub %in% names(match_list)]
    if (length(match_miss) > 0) {
      match_list <- c(match_list, 
        setNames(vector("list", length(match_miss)), match_miss))
    }

    # Match row order of list to x
    out <- match_list[match(xsub, names(match_list))]

  } else {
    # Create formatted dataframe
    match_df <- do.call(rbind, lapply(match_list, function(i) { 
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
          tolower = null2na(i$tolower),
          nonumber = null2na(i$nonumber),
          nobracket = null2na(i$nobracket),
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
          tolower = i$tolower,
          nonumber = i$nonumber,
          nobracket = i$nobracket,
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
    out <- cbind(taxon_name_orig = x, match_df[match(xsub, match_df$taxon_name_subm),])
    rownames(out) <- NULL
  }

  # Store good API results in cache
  # Non-ambiguous automatic matches and manual assertions only
  match_good_list <- match_list[
    unlist(lapply(match_list, "[[", "method")) %in% c("AUTO", "MANUAL") &
    !names(match_list) %in% names(wfo_cache_get()$matchNames)]
  the$wfo_cache$matchNames <- c(wfo_cache_get()$matchNames, match_good_list)

  # Return
  return(out)
}

