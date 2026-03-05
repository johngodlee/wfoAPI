# wfoAPI

The `wfoAPI` R package uses the World Flora Online GraphQL API to search for taxonomic names and their synonyms.

Unlike the [WorldFlora](https://cran.r-project.org/web/packages/WorldFlora/index.html) R package, this package does not require the user to download a static copy of the World Flora Online database. Compared to the [wfor](https://github.com/worldflora/wfor) R package, this package implementation is more mature and user-ready.

Use the `matchName()` function with a vector of taxonomic names to return matched names from the World Flora Online database.

Use the `getRank()` function with a vector of WFO IDs for accepted names to return higher order taxonomic ranks from the World Flora Online database.

Use the `getName()` function with a vector of WFO IDs to return taxonomic information from the World Flora Online database.

Features:

* Server-side fuzzy matching of names
* Interactive selection when multiple names are matched
* Retrieve accepted names for synonyms 
* Retrieve higher order taxonomic information
* Optionally use cached data to reduce number of API calls 
