#' Get higher order taxonomic information for WFO IDs
#'
#' @param x vector of WFO IDs, e.g. "wfo-0000214110"
#' @param capacity maximum number of API calls which can accumulate over the 
#'     duration of `fill_time_s`. See documentation for `httr2::req_throttle()`
#' @param fill_time_s time in seconds to refill the capacity for repeated API 
#'     calls. See documentation for `httr2::req_throttle()`
#'
#' @return list of dataframes with the higher order taxonomic ranks of each of
#'     the submitted WFO IDs
#' 
#' @export
#' 
getRank <- function(x, capacity = 60, fill_time_s = 60) {

  # Create request 
  req <- httr2::request(getOption("wfo.api_uri"))

  # Get the most recent taxonomic backbone classification
  query <- "
    query {
      classifications(classificationId: \"DEFAULT\") {
        id
      }
    }
  "

  classif_payload <- list(query = query)
  classif_req <- httr2::req_body_json(req, classif_payload, auto_unbox = TRUE)
  classif_resp <- httr2::req_perform(classif_req)
  classif_json <- httr2::resp_body_json(classif_resp)
  classif <- unlist(classif_json)

  # Create WFO ID string with classification
  xc <- paste0(x, "-", classif)

  # Construct API calls for taxon concept
  api_call_list <- list()
  for (i in seq_along(xc)) {
    api_call_list[[i]] <- callAPI(xc[i], 
      query = query_taxonConceptById(),
      capacity = capacity,
      fill_time_s = fill_time_s)
  }

  # Submit API calls
  api_req_list <- httr2::req_perform_parallel(api_call_list)

  # Convert API responses to JSON
  api_resp_list <- lapply(api_req_list, httr2::resp_body_json)

  # For each species 
  out <- lapply(seq_along(api_resp_list), function(y) { 
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
  names(out) <- x

  return(out)
}

