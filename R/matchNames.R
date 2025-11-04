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
#' @param checkHomonyms logical, if TRUE consider matches to be ambiguous if
#'     there are other names with the same words but different author strings
#' @param fuzzyNameParts integer value of 0 (default) or greater. The maximum
#'     Levenshtein distance used for fuzzy matching words in `x`
#' @param preferAccepted logical, if TRUE, if multiple ambiguous matches are
#'     found, and if there is only one candidate is an "accepted" name,
#'     automatically choose that name
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#' @param useCache logical, if TRUE use cached values in
#'     `options("wfo.api_uri")` preferentially, to reduce the number of API
#'     calls
#' @param useAPI logical, if TRUE (default) allow API calls
#' @param raw logical, if TRUE raw a nested list is returned, otherwise a
#'     dataframe
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
#' @importFrom data.table rbindlist
#' @importFrom httr2 request req_method req_perform
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
  checkHomonyms = FALSE, fuzzyNameParts = 0, preferAccepted = FALSE,
   interactive = TRUE, useCache = FALSE, useAPI = TRUE, raw = FALSE) {

  if (!useCache & !useAPI) {
    stop("Either useCache or useAPI must be TRUE")
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
  if (!checkURL(options("wfo.api_uri")$wfo.api_uri)) {
    w <- paste("WFO API unreachable:", options("wfo.api_uri")$wfo.api_uri)
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

  # For each unique taxonomic name 
  resp_list <- lapply(seq_along(xun), function(i) {
    taxon <- xun[i]

    cat(sprintf("%i of %i:\t%s\n", i, xlen, taxon))

    # Submit API query
    c(list(submitted_name = taxon), 
      matchName(taxon, 
        fallbackToGenus = fallbackToGenus, 
        checkRank = checkRank,
        checkHomonyms = checkHomonyms,
        fuzzyNameParts = fuzzyNameParts,
        preferAccepted = preferAccepted,
        useCache = useCache,
        useAPI = useAPI,
        interactive = interactive))
  })

  # Define helper function to convert NULL values to NA
  null2na <- function(x) {
    if (is.null(x)) {
      NA_character_ 
    } else { 
      x
    }
  }

  if (raw) {
    # Return raw list output
    out <- resp_list
  } else {
    # Create formatted dataframe
    out <- data.table::rbindlist(lapply(resp_list, function(i) { 
      if ("id" %in% names(i)) {
        data.frame(
          taxon_name_subm = null2na(i$submitted_name),
          method = null2na(i$method),
          fallbackToGenus = null2na(i$fallbackToGenus),
          checkRank = null2na(i$checkRank),
          checkHomonyms = null2na(i$checkHomonyms),
          fuzzyNameParts = null2na(i$fuzzyNameParts),
          taxon_wfo_syn = null2na(i$id),
          taxon_name_syn = null2na(i$fullNameStringNoAuthorsPlain),
          taxon_auth_syn = null2na(i$authorsString),
          taxon_stat_syn = null2na(i$nomenclaturalStatus),
          taxon_role_syn = null2na(i$role),
          taxon_rank_syn = null2na(i$rank),
          taxon_path_syn = null2na(i$wfoPath),
          taxon_wfo_acc = null2na(i$currentPreferredUsage$hasName$id),
          taxon_name_acc = null2na(i$currentPreferredUsage$hasName$fullNameStringNoAuthorsPlain),
          taxon_auth_acc = null2na(i$currentPreferredUsage$hasName$authorsString),
          taxon_stat_acc = null2na(i$currentPreferredUsage$hasName$nomenclaturalStatus),
          taxon_role_acc = null2na(i$currentPreferredUsage$hasName$role),
          taxon_rank_acc = null2na(i$currentPreferredUsage$hasName$rank),
          taxon_path_acc = null2na(i$currentPreferredUsage$hasName$wfoPath))
      } else { 
        data.frame(
          taxon_name_subm = i$submitted_name,
          method = i$method,
          fallbackToGenus = i$fallbackToGenus,
          checkRank = i$checkRank,
          checkHomonyms = i$checkHomonyms,
          fuzzyNameParts = i$fuzzyNameParts,
          taxon_wfo_syn = NA_character_,
          taxon_name_syn = NA_character_,
          taxon_auth_syn = NA_character_,
          taxon_stat_syn = NA_character_,
          taxon_role_syn = NA_character_,
          taxon_rank_syn = NA_character_,
          taxon_path_syn = NA_character_,
          taxon_wfo_acc = NA_character_,
          taxon_name_acc = NA_character_,
          taxon_auth_acc = NA_character_,
          taxon_stat_acc = NA_character_,
          taxon_role_acc = NA_character_,
          taxon_rank_acc = NA_character_,
          taxon_path_acc = NA_character_)
      }
    }))

    # Match row order of dataframe to x
    out <- out[match(x, out$taxon_name_subm),]
  }

  # Return
  return(out)
}

