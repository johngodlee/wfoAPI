#' Manually pick a taxonomic name from a list returned by the WFO GraphQL API
#'
#' @param x original taxonomic name 
#' @param cand list of candidate taxa 
#' @param offset initial index value used internally by pager, controls index
#'     of page start
#' @param page_size index value used internally by pager, controls page length
#' @param timeout time in seconds to wait before disconnecting from an
#'     unresponsive request
#'
#' @return list containing information of matched taxonomic name
#' 
#' @noRd
#' 
pickName <- function(x, cand, offset = 0, page_size = 10, timeout = 10) {

  # Initialise while loops
  valid <- FALSE

  # If no candidates, prompt for WFO ID
  if (length(cand) == 0) {
    cat(sprintf("\n\nNo candidates for: %s\n", x))
    while (!valid) {
      # Prompt user for input
      prompt <- "Enter a valid WFO ID, 'S', or press Enter to skip. "
      input <- tolower(trimws(readline(prompt)))

      # If a valid WFO ID, search for that
      if (grepl("^wfo-[0-9]{10}$", tolower(trimws(input)))) {
        api_vars <- list(searchString = input)
        api_call <- callAPI(api_vars, 
          query_taxonNameById(), timeout = timeout)
        api_resp <- httr2::req_perform(api_call)
        api_json <- httr2::resp_body_json(api_resp)
        match <- api_json$data$taxonNameById
        match$method <- "MANUAL"
        valid <- TRUE
      # If skipped deliberately by user
      } else if (tolower(input) %in% c("s", "")) {
        match <- list(method = "SKIP")
        valid <- TRUE
      # If invalid input
      } else {
        cat("Invalid input.\n")
      }
    }

    # Return
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
        null2na(cand[[i]]$id),
        null2na(cand[[i]]$fullNameStringNoAuthorsPlain),
        null2na(cand[[i]]$authorsString),
        null2na(cand[[i]]$role),
        null2na(cand[[i]]$wfoPath)
      )
    )
  }

  while (!valid) {

    # Create footer
    prompt <- paste(
      "Enter a number to pick a row from the list,",
      "a valid WFO ID,", 
      "'N' for the next page,", 
      "'P' for the previous page,",
      "'S' or press Enter to skip: ")

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
      api_vars <- list(searchString = input)
      api_call <- callAPI(api_vars, 
        query_taxonNameById(), timeout = timeout)
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
      cat("Invalid input.\n")
    }
  }

  # Return
  return(match)
}
