#' Calculation of the effective dose
#'
#' Calculates the effective dose for every row in the data set.
#' The function calculates dose components
#' (\code{\link{dose_external}},
#' \code{\link{dose_internal_ing}},
#' \code{\link{dose_internal_milk}},
#' \code{\link{dose_inhalation}}, \code{\link{dose_total}}) for
#' all tissues for the weighted sum.
#'
#' @param data data table with all variables as in the data set returned from
#'  \code{\link{dose_new_variables}}.
#' @param tb_county table with county dependent parameters,
#' it should have column names like \code{\link{tab_county}}.
#' @param tb_fshield table with municipality dependent parameter f_shield,
#' it should have columns Code and `f_shield`, like \code{\link{tab_fshield}}.
#' @param tb_organ coefficients per tissue (organ),
#' table with columns like \code{\link{tab_organ}}.
#' @param organ_column name of the column in `tb_organ` with tissue names.
#' Tissue names need to be the same as in `tb_wT`.
#' @param tb_wT Tissue weighting factors,
#' table with columns  Organ and w_T.
#' @param ...
#'
#' @return data set with original variables and effective dose per row,
#' the effective dose variables have names `Dext_Effective`, `Dint_Effective`,
#' `Dmilk_Effective`, `Dinh_Effective`, `Dtot_Effective`.
#' @export
#'
#' @seealso \code{\link{dose_dm}},
#' \code{\link{dose_new_variables}},
#' \code{\link{dose_external}},
#' \code{\link{dose_internal_ing}},
#' \code{\link{dose_internal_milk}},
#' \code{\link{dose_inhalation}}
#'
#' @examples dose_effective(data = dose_new_variables(dose_dm(dose_data)))

dose_effective <- function(data,
                           tb_county = tab_county,
                           tb_fshield = tab_fshield,
                           tb_organ = tab_organ,
                           organ_column = "ICRP_risk_organ",
                           tb_wT = tab_wT, ...)
{
  data.copy <- copy(data)
  tb_organ_eff <- copy(tb_organ)
  tb_organ_eff[, Organ := get(organ_column)]

  organ_ls <- tab_wT$Organ

  if (length(setdiff(organ_ls, tb_organ_eff$Organ)) > 0) stop("Some organs missing for the calculation of the effective dose.")
  #if (length(setdiff(organ_ls, tb_wT$Organ)) > 0) stop("Weighting function for some organ is missing.")

  # remove previous dose columns from data.copy
  old_dose_col <- names(data.copy)[grep("^Dext|^Dint|^Dinh|^Dmilk|^Dtot", names(data.copy))]
  if (length(old_dose_col) > 0) data.copy[,(old_dose_col) := NULL]
  data.copy <- dose_external(data.copy,
                             tb_Fsnow = tb_county,
                             tb_fshield = tb_fshield,
                             tb_organ = tb_organ_eff[Organ %in% organ_ls], ...)

  data.copy <- dose_internal_ing(data.copy,
                                 tb_Aesd = tb_county,
                                 tb_organ = tb_organ_eff[Organ %in% organ_ls,], ...)

  data.copy <- dose_inhalation(data.copy,
                               tb_E_inh = tb_county, ...)
  data.copy <- dose_internal_milk(data.copy,
                                  tb_c_milk = tb_county, ...)

  data.copy[, Dint_Effective := 0]
  data.copy[, Dext_Effective := 0]
  for (organ in organ_ls) {
    w <- tb_wT[Organ == organ, w_T]
    # NA for breast and testes
    tmp_dose <- gsub(" ", "_", paste0("Dint_", organ))
    data.copy[!is.na(get(tmp_dose)), Dint_Effective := Dint_Effective + w * get(tmp_dose)]
    tmp_dose <- gsub(" ", "_", paste0("Dext_", organ))
    data.copy[!is.na(get(tmp_dose)), Dext_Effective := Dext_Effective + w * get(tmp_dose)]
  }

  w <- tb_wT[Organ == "Thyroid", w_T]
  data.copy[, Dmilk_Effective := w * Dmilk_Thyroid]
  data.copy[, Dinh_Effective := w * Dinh_Thyroid]

  data.copy[, Dtot_Effective := Dint_Effective + Dext_Effective +
              Dinh_Effective + Dmilk_Effective]

  # remove old columns with effective dose
  if ("Dint_Effective" %in% names(data)) data[, Dint_Effective := NULL]
  if ("Dext_Effective" %in% names(data)) data[, Dext_Effective := NULL]
  if ("Dmilk_Effective" %in% names(data)) data[, Dmilk_Effective := NULL]
  if ("Dinh_Effective" %in% names(data)) data[, Dinh_Effective := NULL]
  if ("Dtot_Effective" %in% names(data)) data[, Dtot_Effective := NULL]

  data2 <- data[data.copy[, .(municipality, start_date, age, sex,
                              Dint_Effective, Dext_Effective, Dinh_Effective, Dmilk_Effective, Dtot_Effective)],
                on = .(municipality, start_date, age, sex)]
  return(data2)
}
