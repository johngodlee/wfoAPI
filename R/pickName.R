#' Manually pick a taxonomic name from a list returned by the WFO GraphQL API
#'
#' @param x original taxonomic name searched by `callAPI()`
#' @param cand list of candidate taxa returned by `callAPI()`
#' @param offset initial index value used internally by pager, controls index
#'     of page start
#' @param page_size index value used internally by pager, controls page length
#'
#' @return list containing information of matched taxonomic name
#' 
#' @examples
#' x <- "Burkea af"
#' resp <- callAPI(x, query_taxonNameMatch())
#' pickName(x, resp$data$taxonNameMatch$candidates)
#' 
#' @export
#' 
pickName <- function(x, cand, offset = 0, page_size = 10) {

  # If no candidates, SKIP
  if (length(cand) == 0) {
    cat(sprintf("No candidates, skipping: %s\n", x))
    match <- list(method = "EMPTY")
    return(match)
  }

  # Set up number of candidates per page
  start_page <- offset + 1
  end_page <- start_page + page_size -1
  if (end_page > length(cand)) {
    end_page <- length(cand)
  }

  # Create header
  cat("\n\n--- Pick a name ---")
  cat(sprintf("\nMatching string:\t%s\n", x))

  # List candidates
  for (i in start_page:end_page) {
    cat(
      sprintf(
        "%-4s%s\t%s\t%s\t%s\t%s\n",
        i,
        cand[[i]]$id,
        cand[[i]]$fullNameStringNoAuthorsPlain,
        cand[[i]]$authorsString,
        cand[[i]]$role,
        cand[[i]]$wfoPath
      )
    )
  }

  valid <- FALSE
  while (!valid) {

    # Create footer
    prompt <- paste(
      "Enter a number to pick a row from the list,",
      "a valid WFO ID,", 
      "'N' for the next page,", 
      "'P' for the previous page,",
      "'S' to skip this name: ")

    # Prompt the user for input
    input <- trimws(readline(prompt))
    input_num <- suppressWarnings(as.numeric(input))

    # Valid numeric selection
    if (!is.na(input_num) && input_num > 0 && input_num <= length(cand)) {
      match <- cand[[input_num]]
      match$method <- "MANUAL"
      valid <- TRUE
    } else if (grepl("^wfo-[0-9]{10}$", tolower(trimws(input)))) {
      input <- tolower(trimws(input))
      api_call <- callAPI(input, 
        query_taxonNameById())
      api_resp <- httr2::req_perform(api_call)
      api_json <- httr2::resp_body_json(api_resp)
      match <- api_json$data$taxonNameById
      match$method <- "MANUAL"
      valid <- TRUE
    } else if (tolower(input) == "n") {
      if (end_page < length(cand)) {
        return(pickName(x, cand, offset + page_size, page_size))
      } else {
        cat("Already on last page.\n")
      }
    } else if (tolower(input) == "p") {
      if (start_page > 1) {
        return(pickName(x, cand, offset - page_size, page_size))
      } else {
        cat("Already on first page.\n")
      }
    } else if (tolower(input) %in% c("s", "")) {
      match <- list(method = "SKIP")
      valid <- TRUE
    } else {
      cat("Invalid input. Enter an integer, a valid WFO ID, 'N', 'P', 'S', or press Enter to skip.\n" )
    }
  }

  # Return
  return(match)
}
