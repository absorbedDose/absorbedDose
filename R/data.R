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
#' @format A data set in data.table format with 21 rows and 6 variables:
#' \describe{
#' \item{Code}{codes of Swedish counties, values: 1--25, without 2, 11, 15, 16}
#' \item{County}{names of the counties}
#' \item{A_esd}{average deposition of Cs-137, kBq/m^2 }
#' \item{F_snow}{dimensionless}
#' \item{c_milk}{time-integrated activity concentrations in milk, Bq d/kg}
#' \item{E_inh}{µSv}
#' }
#' @encoding UTF-8
#' @source Tondel M, Gabrysch K, Rääf C, Isaksson M. (2025)
#' A model for estimating radiation doses and population cancer risk in Sweden
#' after the Chernobyl Nuclear Power Plant accident in 1986. Uppsala University.
#' doi:10.33063/diva-544175.
"tab_county"

#' County dependent parameters used in 2023
#'
#' A table with values of A_{esd}, F_{snow}, c_{milk} and E_{inh} per county.
#'
#' `r lifecycle::badge("superseded")`
#' Replaced with \code{\link{tab_county}}.
#'
#' @format A data set in data.table format with 24 rows and 6 variables:
#' \describe{
#' \item{Code}{codes of Swedish counties, values: 1--25, without 2}
#' \item{County}{names of the counties}
#' \item{A_esd}{average deposition of Cs-137, kBq/m^2 }
#' \item{F_snow}{dimensionless}
#' \item{c_milk}{time-integrated activity concentrations in milk, Bq d/kg}
#' \item{E_inh}{µSv}
#' }
#' @encoding UTF-8
#' @source `r citation("absorbedDose")$textVersion`
"tab_county_2023"

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

#' Municipality dependent parameters
#'
#' A table with values of the average deposition, shielding factor f_shield and
#' percentage of hunter households per municipality.
#'
#' @format A data set in data.table format with 290 rows and 7 variables:
#' \describe{
#' \item{Code}{code of Swedish municipalities}
#' \item{municipality_name}{names of the municipalities}
#' \item{county}{codes of Swedish counties}
#' \item{county_name}{names of the counties}
#' \item{cesium}{average deposition of Cs-137, kBq/m^2}
#' \item{f_shield}{dimensionless, same as in \code{\link{tab_fshield}}.}
#' \item{hunter_perc}{percentage of hunter households}
#' }
#' @encoding UTF-8
#' @source Tondel M, Gabrysch K, Rääf C, Isaksson M. (2025)
#' A model for estimating radiation doses and population cancer risk in Sweden
#' after the Chernobyl Nuclear Power Plant accident in 1986. Uppsala University.
#' doi:10.33063/diva-544175.
"tab_municip"

#' Cancer site dependent parameters
#'
#' A table with values of k_SEQ, coefficients of organ dependent polynomials,
#' \eqn{\epsilon}_Cs-134
#'  and
#' \eqn{\epsilon}_Cs_137,
#' per cancer site (organ) and per sex.
#'
#'`r lifecycle::badge("superseded")`
#' Replaced with \code{\link{tab_organ}}.
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
"tab_organ_2023"

#' Cancer site dependent parameters
#'
#' A table with values of k_SEQ, coefficients of organ dependent polynomials,
#' \eqn{\epsilon}_Cs-134
#'  and
#' \eqn{\epsilon}_Cs_137,
#' per cancer site (organ) and per sex.
#'
#'@encoding UTF-8
#' @format A data set in data.table format with 33 rows and 17 variables:
#' \describe{
#' \item{Organ}{names of the organs}
#' \item{EPA_cancer_site}{organ/tissues included by EPA (2011) that are used
#' for computation of lifetime attributable risk (LAR)}
#' \item{ICRP_risk_organ}{organ/tissues included by ICRP that are used for computation of effective dose}
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
#' @source Tondel M, Gabrysch K, Rääf C, Isaksson M. (2025)
#' A model for estimating radiation doses and population cancer risk in Sweden
#' after the Chernobyl Nuclear Power Plant accident in 1986. Uppsala University.
#' doi:10.33063/diva-544175.
"tab_organ"

#' Tissue weighting factors
#'
#' A table with tissue weighting factors.
#'
#'@encoding UTF-8
#' @format A data set in data.table format with 17 rows and 2 variables:
#' \describe{
#' \item{Organ}{names of the tissue}
#' \item{w_T}{tissue weighting factor}
#' }
#' @encoding UTF-8
#' @source ICRP (2007). ICRP Publication 103: The 2007 recommendations of the
#' international commission on radiological protection. Ann. ICRP 37.2–4, pp. 1–332.
#' doi: 10.1016/j.icrp.2007.10.003.
"tab_wT"

#' LAR coefficients for males
#'
#' Lifetime attributable risk (LAR) for cancer incidence (cases per 10 000 person-Gy)
#' by age at exposure for males. The coefficients are taken from EPA (2011).
#' In addition, we set coefficients to 0 at age 100.
#'
#'@encoding UTF-8
#' @format A data set in data.table format with 12 rows and 15 variables:
#' \describe{
#' \item{Age}{age at exposure}
#' \item{cancer sites}{LAR for cancer incidnce of the organ by age}
#' }
#' @encoding UTF-8
#' @source EPA (2011). Radiogenic Cancer Risk Models and Projections for the U.S. Population.
#' EPA 402-R-11-001. Washington, DC: U.S. Environmental Protection Agency.
"tab_LAR_M"

#' LAR coefficients for females
#'
#' Lifetime attributable risk (LAR) for cancer incidence (cases per 10 000 person-Gy)
#' by age at exposure for females. The coefficients are taken from EPA (2011).
#' In addition, we set coefficients to 0 at age 100.
#'
#'@encoding UTF-8
#' @format A data set in data.table format with 12 rows and 17 variables:
#' \describe{
#' \item{Age}{age at exposure}
#' \item{cancer sites}{LAR for cancer incidnce of the organ by age}
#' }
#' @encoding UTF-8
#' @source EPA (2011). Radiogenic Cancer Risk Models and Projections for the U.S. Population.
#' EPA 402-R-11-001. Washington, DC: U.S. Environmental Protection Agency.
"tab_LAR_W"
