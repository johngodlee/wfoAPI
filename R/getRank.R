#' Get higher order taxonomic information for WFO IDs
#'
#' @param x vector of WFO IDs, e.g. "wfo-0000214110". Note IDs must be of
#'     accepted names. Non-accepted names will not return any higher order
#'     taxonomic information.
#' @param rank optional character vector of desired ranks. If NULL (default),
#'     all higher order taxonomic information is returned. Return all available
#'     ranks with `listRanks()`
#' @param useCache logical, if TRUE use cached values in
#'     `wfo_cache_get()` preferentially, to reduce the number of API calls
#' @param useAPI logical, if TRUE (default) allow API calls
#' @param capacity maximum number of API calls which can accumulate over the 
#'     duration of `fill_time_s`. See documentation for `httr2::req_throttle()`
#' @param fill_time_s time in seconds to refill the capacity for repeated API 
#'     calls. See documentation for `httr2::req_throttle()`
#' @param timeout time in seconds to wait before disconnecting from an
#'     unresponsive request
#'
#' @return list of dataframes with the higher order taxonomic ranks of each of
#'     the submitted WFO IDs
#' 
#' @export
#' 
getRank <- function(x, rank = NULL, useCache = FALSE, useAPI = TRUE, 
  capacity = 60, fill_time_s = 60, timeout = 10) {

  if (!useCache & !useAPI) {
    stop("Either useCache or useAPI must be TRUE")
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

  # Optionally search cache for names
  match_cache_list <- list()
  if (useCache) {
    # Extract cached names
    match_cache_list <- wfo_cache_get()$getRank[x]
    match_cache_list[sapply(match_cache_list, is.null)] <- NULL

    # Remove names matched in cache from vector of names
    xsub <- x[!x %in% names(match_cache_list)]

    # Message
    if (length(match_cache_list) > 0) {
      cat(sprintf("Using cached data for %s IDs\n", length(match_cache_list)), "\n")
    }
  } else {
    xsub <- x
  }

  match_api_list <- list()
  if (useAPI & length(xsub) > 0) {
    # Create request 
    req <- httr2::request(getOption("wfo.api_uri"))

    # Get the most recent taxonomic backbone classification
    q <- "
      query {
        classifications(classificationId: \"DEFAULT\") {
          id
        }
      }
    "

    classif_payload <- list(query = q)
    classif_req <- httr2::req_body_json(req, classif_payload, auto_unbox = TRUE)
    classif_resp <- httr2::req_perform(classif_req)
    classif_json <- httr2::resp_body_json(classif_resp)
    classif <- unlist(classif_json)

    # Create WFO ID string with classification
    xc <- paste0(xsub, "-", classif)

    # Construct API calls for taxon concept
    api_call_list <- list()
    for (i in seq_along(xc)) {
      api_call_list[[i]] <- callAPI(xc[i], 
        query = query_taxonConceptById(),
        capacity = capacity,
        fill_time_s = fill_time_s,
        timeout = timeout)
    }

    # Submit API calls
    api_req_list <- httr2::req_perform_parallel(api_call_list)

    # Convert API responses to JSON
    api_resp_list <- lapply(api_req_list, httr2::resp_body_json)

    # For each species 
    match_api_list <- lapply(seq_along(api_resp_list), function(y) { 
      do.call(rbind, lapply(api_resp_list[[y]]$data$taxonConceptById$path, function(z) {
        data.frame(
          taxon_wfo_acc = null2na(z$hasName$id),
          taxon_name_acc = null2na(z$hasName$fullNameStringNoAuthorsPlain),
          taxon_auth_acc = null2na(z$hasName$authorsString),
          taxon_stat_acc = null2na(z$hasName$nomenclaturalStatus),
          taxon_role_acc = null2na(z$hasName$role),
          taxon_rank_acc = null2na(z$hasName$rank),
          taxon_path_acc = null2na(z$hasName$wfoPath)
        )
      }))
    })
    names(match_api_list) <- xsub
  }

  # Combine API and cached matches
  match_list <- c(match_api_list, match_cache_list)

  # Store values in cache 
  match_good_list <- match_list[
    !names(match_list) %in% names(wfo_cache_get()$getRank)]
  the$wfo_cache$getRank <- c(wfo_cache_get()$getRank, match_good_list)

  # Optionally filter to named ranks
  if (!is.null(rank)) { 
    match_list_sel <- lapply(match_list, function(y) {
      y[y$taxon_rank_acc %in% rank,]
    })
  } else {
    match_list_sel <- match_list
  }

  # Add missing values
  # Not matched by cache or API
  match_miss <- x[!x %in% names(match_list_sel)]
  if (length(match_miss) > 0) {
    match_list_sel <- c(match_list_sel, 
      setNames(vector("list", length(match_miss)), match_miss))
  }

  # Order by x
  out <- match_list_sel[x]

  # Return
  return(out)
}

#' Return all valid ranks in the World Flora Online database
#'
#' @return character vector, e.g. for use in `getRank(rank = )`
#' 
#' @export
#' 
listRanks <- function() {
  # Construct query
  q <- "
    query ranks {
      ranks {
        id
      }
    }
  "

  # Run query
  req <- callAPI("", query = q)
  req <- httr2::req_perform(req)
  out <- unname(unlist(httr2::resp_body_json(req)))

  # Return
  return(out)
}

