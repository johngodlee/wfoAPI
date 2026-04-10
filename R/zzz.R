.onLoad <- function(libname, pkgname) {
  .set_endpoint()
}

.onUnload <- function(libname, pkgname){
  options("wfo.api_uri" = NULL)
}

.onAttach <- function(libname, pkgname) {
  # Send startup messages
  packageStartupMessage(
    "WFO API URI set: ", getOption("wfo.api_uri")
  )
  wfo_cache_status()
}

.wfo_default_endpoints <- function() {
  c(
    "https://list.worldfloraonline.org/gql.php",
    "https://rhakhis.rbge.info/gql.php"
  )
}

.set_endpoint <- function() {
  # Capture user-defined input if it exists
  env_url <- Sys.getenv("WFO_API_URI")
  opt_url <- getOption("wfo.api_uri")
  
  # Determine if custom URL was provided - Sys.getenv takes precedence
  if (env_url != "") {
    user_url <- env_url 
  } else {
    user_url <- opt_url
  }
  
  # Define list of URLs to test
  # If user URL exists, put it first, then add defaults
  defaults <- .wfo_default_endpoints()
  
  if (!is.null(user_url) && user_url != "") {
    # Test user choice first, then fall back to defaults
    api_urls_to_test <- unique(c(user_url, defaults))
    is_user <- TRUE
  } else {
    api_urls_to_test <- defaults
    is_user <- FALSE
  }

  active_url <- NULL

  # Test endpoints in order
  for (url in api_urls_to_test) {
    is_active <- tryCatch({
      req <- httr2::request(url)
      req <- httr2::req_timeout(req, 2)
      req <- httr2::req_error(req, is_error = function(req) FALSE) 
      resp <- httr2::req_perform(req)
      TRUE
    }, error = function(e) { FALSE })
    
    if (is_active) {
      active_url <- url
      break 
    } else {
      # If user URL failed, send warning 
      if (is_user && url == user_url) {
        packageStartupMessage(sprintf(
          "Warning: User-specified API endpoint '%s' is unreachable.", url
        ))
      }
    }
  }
  
  # Final assignment
  if (!is.null(active_url)) {
    # If active URL isn't a user-defined URL, send message
    if (is_user && active_url != user_url) {
      packageStartupMessage(sprintf("Falling back to default API endpoint: %s", active_url))
    }
    options("wfo.api_uri" = active_url)
  } else {
    # Total failure (offline)
    options("wfo.api_uri" = defaults[1])
    packageStartupMessage(
      "Warning: Could not connect to any WFO API endpoint.\n", 
      "Package functions that require the API may fail."
    )
  }
  
  # Return
  return(invisible())
}

.get_api_uri <- function() {
  # Env var.
  env <- Sys.getenv("WFO_API_URI")
  if (env != "") {
    return(env)
  }
  
  # R option
  opt <- getOption("wfo.api_uri")
  if (!is.null(opt) && opt != "") {
    return(opt)
  }
  
  # Package default
  return(.wfo_default_endpoints())
}
