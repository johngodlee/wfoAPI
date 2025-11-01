#' Initiate a cache for API results from WFO
#' 
#' @export
#' 
wfo_cache_init <- function() { 
  if (!(exists("wfo_cache", envir = .GlobalEnv) && 
    is.environment(get("wfo_cache", envir = .GlobalEnv)))) {
    # Create wfo_cache in the global environment
    assign("wfo_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
    wfo_cache$names <- list()
  }
}

#' Return a loaded WFO cache
#'
#' @export
#' 
wfo_cache_get <- function() {
  return(wfo_cache$names)
}

#' Save a WFO cache to file
#'
#' @param file filepath for the cache 
#'
#' @export
#' 
wfo_cache_save <- function(file = "./wfo_cache.rds") {
  saveRDS(wfo_cache, file)
  message(sprintf("WFO cache saved to '%s'", file))
}

#' Load an existing WFO cache object
#'
#' @param file filepath for the cache 
#'
#' @export
#' 
wfo_cache_load <- function(file = "./wfo_cache.rds") {
  wfo_cache$names <- readRDS(file)
  message(sprintf("WFO cache loaded from '%s'", file))
}

#' Report the status of the loaded WFO cache
#'
#' @export
#' 
wfo_cache_status <- function() {
  n <- length(wfo_cache$names)
  message(
    "\n--- WFO name cache status ---\n",
    sprintf("Cache contains %s name strings.\n", format(n, big.mark=",")),
    "Return the cache object:\n",
    "\twfo_cache_get()\n",
    "Save the cache for future sessions:\n",
    "\twfo_cache_save('wfo_cache.rds')\n",
    "Initiate a new cache:\n",
    "\twfo_cache_init('wfo_cache.rds')\n",
    "Load an existing cache:\n",
    "\twfo_cache_load('wfo_cache.rds')\n"
  )
}

