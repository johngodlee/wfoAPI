#' Define WFO GraphQL API query for name matching
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_NameMatch <- function() {
  "query NameMatch(
    $searchString: String, 
    $fallbackToGenus: Boolean)
    {
      taxonNameMatch(
        inputString: $searchString
        checkHomonyms: false
        checkRank: false
        fallbackToGenus: $fallbackToGenus
      ) {
        inputString
        searchString
        match {
          id
          fullNameStringPlain
          fullNameStringNoAuthorsPlain
          genusString
          nameString
          authorsString
          nomenclaturalStatus
          role
          rank
          wfoPath
          currentPreferredUsage {
            hasName {
              id
              fullNameStringPlain
              fullNameStringNoAuthorsPlain
              genusString
              nameString
              authorsString
              nomenclaturalStatus
              role
              rank
              wfoPath
            }
          }
        }
        candidates {
          id
          fullNameStringPlain
          fullNameStringNoAuthorsPlain
          genusString
          nameString
          authorsString
          nomenclaturalStatus
          role
          rank
          wfoPath
          currentPreferredUsage {
            hasName {
              id
              fullNameStringPlain
              fullNameStringNoAuthorsPlain
              genusString
              nameString
              authorsString
              nomenclaturalStatus
              role
              rank
              wfoPath
            }
          }
        }
        error
        errorMessage
        method
        narrative
      }
    }"
}


#' Define WFO GraphQL API query for WFO ID matching
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_taxonNameById <- function() {
  "query NameByID(
    $searchString: String)
    {
      taxonNameById(
        nameId: $searchString
      ) {
        id
        fullNameStringPlain
        fullNameStringNoAuthorsPlain
        genusString
        nameString
        authorsString
        nomenclaturalStatus
        role
        rank
        wfoPath
        currentPreferredUsage {
          hasName {
            id
            fullNameStringPlain
            fullNameStringNoAuthorsPlain
            genusString
            nameString
            authorsString
            nomenclaturalStatus
            role
            rank
            wfoPath
          }
        }
      }
    }"
}

