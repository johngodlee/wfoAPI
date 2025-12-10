#' Clear the cache for API results from WFO
#' 
#' @export
#' 
wfo_cache_clear <- function() { 
  rm(wfo_cache, envir = the)
  the$wfo_cache <- list()
}

#' Return a loaded WFO cache
#'
#' @export
#' 
wfo_cache_get <- function() {
  return(the$wfo_cache)
}

#' Save a WFO cache to file
#'
#' @param file filepath for the cache 
#'
#' @export
#' 
wfo_cache_save <- function(file = "./wfo_cache.rds") {
  saveRDS(wfo_cache_get(), file)
  message(sprintf("WFO cache saved to '%s'", file))
}

#' Load an existing WFO cache object
#'
#' @param file filepath for the cache 
#'
#' @export
#' 
wfo_cache_load <- function(file = "./wfo_cache.rds") {
  the$wfo_cache <- readRDS(file)
  message(sprintf("WFO cache loaded from '%s'", file))
}

#' Report the status of the loaded WFO cache
#'
#' @export
#' 
wfo_cache_status <- function() {
  n_matchNames <- length(the$wfo_cache$matchNames)
  n_getRank <- length(the$wfo_cache$getRank)
  message(
    "\n--- WFO name cache status ---\n",
    "Cache contains:\n",
    sprintf("  %s matched names\n", format(n_matchNames, big.mark=",")),
    sprintf("  %s taxon hierarchies\n", format(n_getRank, big.mark=",")),
    "Return the cache object:\n",
    "  wfo_cache_get()\n",
    "Save the cache for future sessions:\n",
    "  wfo_cache_save('./wfo_cache.rds')\n",
    "Load an existing cache:\n",
    "  wfo_cache_load('./wfo_cache.rds')\n",
    "Clear the current cache:\n",
    "  wfo_cache_clear()\n"
  )
}

