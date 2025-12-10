#' Define WFO GraphQL API query for name matching
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_taxonNameMatch <- function() {
  "query NameMatch(
    $searchString: String, 
    $checkHomonyms: Boolean,
    $checkRank: Boolean,
    $fallbackToGenus: Boolean
    $fuzzyNameParts: Int
  )
    {
      taxonNameMatch(
        inputString: $searchString
        checkHomonyms: $checkHomonyms
        checkRank: $checkRank
        fallbackToGenus: $fallbackToGenus
        fuzzyNameParts: $fuzzyNameParts
      ) {
        inputString
        searchString
        match {
          id
          fullNameStringNoAuthorsPlain
          authorsString
          nomenclaturalStatus
          role
          rank
          wfoPath
          currentPreferredUsage {
            hasName {
              id
              fullNameStringNoAuthorsPlain
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
          fullNameStringNoAuthorsPlain
          authorsString
          nomenclaturalStatus
          role
          rank
          wfoPath
          currentPreferredUsage {
            hasName {
              id
              fullNameStringNoAuthorsPlain
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
        fullNameStringNoAuthorsPlain
        authorsString
        nomenclaturalStatus
        role
        rank
        wfoPath
        currentPreferredUsage {
          hasName {
            id
            fullNameStringNoAuthorsPlain
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

#' Define WFO GraphQL API query for WFO ID concept matching
#' 
#' Used to return higher order taxonomic rank information
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_taxonConceptById <- function() {
  "query ConceptByID(
    $searchString: String)
  {
    taxonConceptById(
      taxonId: $searchString
    ) {
      path {
        hasName {
          id
          fullNameStringNoAuthorsPlain
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


#' Define WFO GraphQL API query for current backbone version
#' 
#' Used to return higher order taxonomic rank information
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_classifications <- function() { 
  "query {
      classifications(classificationId: \"DEFAULT\") {
        id
      }
    }
  "
}

