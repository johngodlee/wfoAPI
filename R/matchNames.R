#' Match a vector of names and return formatted output
#'
#' @param x vector of taxonomic names
#'     If the column values do not include the author strings for the plant names then a authors_col should be
#'     specified.
#' @param genus_fallback logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#' @param raw logical, if TRUE raw a nested list is returned, otherwise a
#'     dataframe
#'
#' @return data.frame containing taxonomic name information with rows matching
#'     names in `x`, or a list containing unique values in `x`
#' 
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#' x <- c("Burkea africana", "Julbernardia paniculata", "Fabaceae", 
#'   "Indet indet", "Brachystegia")
#' matchNames(x)
#' matchNames(x, raw = TRUE)
#' matchNames(x, genus_fallback = TRUE)
#' matchNames(x, interactive = FALSE)
#'
matchNames <- function(x, genus_fallback = FALSE, interactive = TRUE, raw = FALSE) {

  # Extract unique names 
  xun <- sort(unique(x))
  xlen <- length(xun)

  # For each unique taxonomic name 
  resp_list <- lapply(seq_along(xun), function(i) {
    taxon <- xun[i]

    cat(sprintf("%i of %i:\t%s\n", i, xlen, taxon))

    # Submit API query
    c(list(submitted_name = taxon), 
      matchName(taxon, genus_fallback, interactive))
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
          genus_fallback = null2na(i$genus_fallback),
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
          genus_fallback = i$genus_fallback,
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

