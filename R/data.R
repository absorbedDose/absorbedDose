#' Example data set for calculating the absorbed dose
#'
#' A data set with 100 fictional Swedish inhabitants.
#'
#' @format A data set in the data.table format with 2283 rows and 10 variables:
#' \describe{
#' \item{id}{ID number (unique for each person in the database)}
#' \item{date_birth}{date of birth}
#' \item{sex}{values: W, M}
#' \item{household}{hunter status of household, values: Hunter, Non-hunter}
#' \item{event_type}{values: Cancer, Emigrated, Dead}
#' \item{date_event}{date of cancer diagnosis, emigration or death}
#' \item{year}{year of the address (of county, municipality and cesium values)}
#' \item{county}{code for county where the person was living}
#' \item{municipality}{code for municipality where the person was living}
#' \item{cesium}{Cs-137 surface deposition at the dwelling coordinates, in kBq/m^2}
#' }
"dose_data"


#' County dependent parameters
#'
#' A table with values of A_{esd}, F_{snow}, c_{milk} and E_{inh} per county.
#'
#' @format A data set in data.table format with 24 rows and 6 variables:
#' \describe{
#' \item{Code}{codes of Swedish counties, values: 1--25, without 2}
#' \item{County}{names of the counties}
#' \item{A_esd}{kBq/m^2 }
#' \item{F_snow}{dimensionless}
#' \item{c_milk}{time-integrated activity concentrations in milk, Bq d/kg}
#' \item{E_inh}{µSv}
#' }
#' @encoding UTF-8
#' @source `r citation("absorbedDose")$textVersion`
"tab_county"

#' Shielding factor
#'
#' A table with values of shielding factor f_shield per municipality.
#'
#' @format A data set in data.table format with 340 rows and 3 variables:
#' \describe{
#' \item{Code}{code of Swedish municipalities}
#' \item{Municipality}{names of the municipalities}
#' \item{f_shield}{dimensionless}
#' }
#' @encoding UTF-8
#' @source `r citation("absorbedDose")$textVersion`
"tab_fshield"

#' Cancer site dependent parameters
#'
#' A table with values of k_SEQ, coefficients of organ dependent polynomials,
#' \eqn{\epsilon}_Cs-134
#'  and
#' \eqn{\epsilon}_Cs_137,
#' per cancer site (organ) and per sex.
#'
#'@encoding UTF-8
#' @format A data set in data.table format with 23 rows and 15 variables:
#' \describe{
#' \item{Organ}{names of the cancer sites}
#' \item{k_SEQ_organ_f, k_SEQ_organ_m}{k_SEQ values for females and males, respectively}
#' \item{a0_f, a1_f, a2_f, a3_f}{coefficients of the polynomial for females}
#' \item{a0_m, a1_m, a2_m, a3_m}{coefficients of the polynomial for males}
#' \item{Cs134_f, Cs134_m}{\eqn{\epsilon}
#'  values for Cs-134 for
#' females and males, respectively}
#' \item{Cs137_f, Cs137_m}{\eqn{\epsilon}
#' values for Cs-137 for
#' females and males, respectively}
#' }
#' @encoding UTF-8
#' @source `r citation("absorbedDose")$textVersion`
"tab_organ"
