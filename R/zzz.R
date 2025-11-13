.onLoad <- function(libname, pkgname) {
  # Set WFO GraphQL API URI if not already set
  if( !"wfo.api_uri" %in% names(options()) ){
    options("wfo.api_uri" = c("https://list.worldfloraonline.org/gql.php"))
  }
}

.onUnload <- function(libname, pkgname){
  options("wfo.api_uri" = NULL)
}

.onAttach <- function(libname, pkgname) {
  # Initiate WFO names cache 
  wfo_cache_init()
  
  # Send startup messages
  packageStartupMessage(
    "WFO API URI set: ", getOption("wfo.api_uri")
  )
  wfo_cache_status()
}


