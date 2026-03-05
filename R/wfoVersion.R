#' Get the most recent WFO taxonomic backbone classification
#'
#' @return character string with WFO taxonomic backbone classification. A 
#'     YYYY-MM date
#' 
#' @export
#' 
wfoVersion <- function() { 
  req <- httr2::request(getOption("wfo.api_uri"))
  classif_payload <- list(query = query_classifications())
  classif_req <- httr2::req_body_json(req, classif_payload, auto_unbox = TRUE)
  classif_resp <- httr2::req_perform(classif_req)
  classif_json <- httr2::resp_body_json(classif_resp)
  classif <- unname(unlist(classif_json))
  return(classif)
}
