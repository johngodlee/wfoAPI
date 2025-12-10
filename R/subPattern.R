#' Common name substitutions
#' 
#' @return character vector with common name substitutions. Used in `gsub()`
#' 
#' @export
#' 
subPattern <- function() {
  c(" sp[.]", " spp[.]", " pl[.]", " indet[.]", " ind[.]", " gen[.]", " g[.]",
    " fam[.]", " nov[.]", " prox[.]", " cf[.]", " aff[.]", " s[.]s[.]", 
    " s[.]l[.]", " p[.]p[.]", " p[.] p[.]", "[?]", " inc[.]", " stet[.]",
    "Ca[.]", "nom[.] cons[.]", "nom[.] dub[.]", " nom[.] err[.]", " nom[.] illeg[.]", 
    " nom[.] inval[.]", " nom[.] nov[.]", " nom[.] nud[.]", " nom[.] obl[.]", 
    " nom[.] prot[.]", " nom[.] rej[.]", " nom[.] supp[.]", " sensu auct[.]", 
    "\\bsp\\b", "\\bspp\\b", "\\bpl\\b", "\\bindet\\b", "\\bind\\b", "\\bgen\\b", "\\bg\\b",
    "\\bfam\\b", "\\bnov\\b", "\\bprox\\b", "\\bcf\\b", "\\baff\\b")
}

