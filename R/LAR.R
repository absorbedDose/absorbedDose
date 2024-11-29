#' Calculation of the age dependent value of the Lifetime attributable cancer risk (LAR)
#'
#' For given age, gives a value of the LAR coefficients.
#'
#' @param age a numerical vector
#' @param tab, LAR coefficients per cancer site, table with columns age, organ names, e.g. `tab_LAR_M` and `tab_LAR_W`
#'
#' @return data set with LAR coefficients for given age
#' @export
#'
#' @seealso \code{\link{LAR}}
#'
#' @examples LAR_coef_lin_int(age = c(2, 8.5, 54, 63), tab = tab_LAR_M)
LAR_coef_lin_int <- function(age, tab) {
  age <- sort(age)
  tab_new <- matrix(ncol = ncol(tab) - 1, nrow = length(age))
  colnames(tab_new) <- paste0("LAR_coef_", colnames(tab)[2:ncol(tab)])

  for (organ in colnames(tab)[2:ncol(tab)]) {
    for (i in 1:length(age)) {
      a <- age[i]
      if (a > 100) {
        tab_new[i, paste0("LAR_coef_", organ)] <- 0
      } else {
        from <- ifelse(a < 20, floor(a/5)*5,
                       ifelse(a < 80, floor(a/10)*10, 80))
        to <- ifelse(a < 20, from + 5,
                     ifelse(a < 80, from + 10, 100))
        L_from <- tab[tab$Age == from, organ]
        L_to <- tab[tab$Age == to, organ]
        tab_new[i, paste0("LAR_coef_", organ)] <- (L_to - L_from)/(to - from)*(a - from) + L_from
      }
    }
  }
  return(data.frame(tab_new, "age" = age))
}

#' Calculation of the Lifetime attributable cancer risk (LAR)
#'
#' Calculates LAR for every row in the data set.
#'
#' @param data data table with variables as in the data set returned from
#'  \code{\link{calculate_dose}}.
#' @param dose_comp vector of dose components to calculate LAR for
#' @param organ_ls vector of cancer sites (organs) to calculate LAR for,
#' the doses to the organs should be in `data`, and LAR coefficients in `tb_LAR_M` and `tb_LAR_W`
#' @param tb_LAR_M LAR for cancer incidence by age at exposure for males, in cases per 10 000 person-Gy.
#' @param tb_LAR_W LAR for cancer incidence by age at exposure for females, in cases per 10 000 person-Gy.
#' @param ...
#'
#' @return data set with original variables and LAR per row,
#' the effective dose variables have names `LAR_comp_organ_name`.
#' @export
#'
#' @seealso \code{\link{calculate_dose}}
#'
#' @examples LAR(data = calculate_dose(dose_data), dose_comp = "tot", organ_ls = c("Colon", "Thyroid"))
LAR <- function(data,
                dose_comp = c("ext", "int", "milk", "inh", "tot"),
                organ_ls = absorbedDose::tab_organ[!is.na(EPA_cancer_site), EPA_cancer_site],
                tb_LAR_M = absorbedDose::tab_LAR_M,
                tb_LAR_W = absorbedDose::tab_LAR_W, ...)
{
  data.copy <- copy(data)

  organ_ls <- gsub(" ", "_", organ_ls)

  if (length(setdiff(organ_ls, c(names(tb_LAR_M), names(tb_LAR_W)))) > 0) {
    stop(sprintf("LAR coefficients for %s are not in the tab_LAR_M or tab_LAR_W table.",
                 paste(setdiff(organ_ls, c(names(tb_LAR_M), names(tb_LAR_F))), collapse = ", ")))
  }

  dose_organ_ls <- NULL
  for (organ in organ_ls) {
    if (organ  == "Thyroid") {
      dose_organ_ls <- c(dose_organ_ls,
                         paste0("D",
                                intersect(dose_comp, c("ext", "int", "milk", "inh", "tot")),
                                "_", organ))
    } else {
      dose_organ_ls <- c(dose_organ_ls,
                         paste0("D",
                                intersect(dose_comp, c("ext", "int", "tot")),
                                "_", organ))
    }
  }

  if (length(setdiff(dose_organ_ls, names(data.copy))) > 0) {
    stop(sprintf("Doses %s are not in the dataset.
                 The doses should be calculated before calling LAR function.",
                 paste(setdiff(dose_organ_ls, names(data.copy)), collapse = ", ")))
  }

  if (nrow(dose_data[sex == "M"]) > 0) {
    # find LAR coefficients per age
    LAR_M_age <- LAR_coef_lin_int(unique(data.copy[sex == "M", age_middle]), tb_LAR_M)
    setDT(LAR_M_age)
    data.LAR_M <- data.copy[sex == "M"][LAR_M_age, on = .(age_middle = age)]
    for (D in dose_organ_ls) {
      organ <- sub("^[^_]*_", "", D)
      if (paste0("LAR_coef_", organ) %in% names(data.LAR_M)) {
        data.LAR_M[, gsub("^D", "LAR_", D) := get(D) * get(paste0("LAR_coef_", organ))/10^7]
      }
    }
    cM <- setdiff(names(data.LAR_M), grep("^LAR_coef", names(data.LAR_M), value = T))
    data.LAR_M <- data.LAR_M[, ..cM]
  } else {
    data.LAR_M <- NULL
  }

  if (nrow(dose_data[sex == "W"]) > 0) {
    # find LAR coefficients per age
    LAR_W_age <- LAR_coef_lin_int(unique(data.copy[sex == "W", age_middle]), tb_LAR_W)
    setDT(LAR_W_age)
    data.LAR_W <- data.copy[sex == "W"][LAR_W_age, on = .(age_middle = age)]
    for (D in dose_organ_ls) {
      organ <- sub("^[^_]*_", "", D)
      if (paste0("LAR_coef_", organ) %in% names(data.LAR_W)) {
        data.LAR_W[, gsub("^D", "LAR_", D) := get(D) * get(paste0("LAR_coef_", organ))/10^7]
      }
    }
    cW <- setdiff(names(data.LAR_W), grep("^LAR_coef", names(data.LAR_W), value = T))
    data.LAR_W <- data.LAR_W[, ..cW]
  } else {
    data.LAR_W <- NULL
  }

  l <- list(data.LAR_M,
            data.LAR_W)
  data.LAR <- rbindlist(l, use.names = T, fill = TRUE)

  return(data.LAR)
}

#' Sum of the variables by given groups
#'
#' Calculates sum of given variables in all rows per group.
#'
#' @param data data table
#' @param by.var vector of variables to group the data, NULL if no groups
#' @param prefix prefix of the variables to calculate sum for, e.g. "LAR_", "Dtot", ...
#' If NULL, parameter `cols` is used.
#' @param cols vector with names of the variables to calculate sum for, e.g. c("LAR_tot_Colon", "LAR_tot_Thyroid").
#'   If NULL, parameter `prefix` is used.
#' @return data set with `by.var` variables and sum of the variables defined by `prefix` or `cols`.
#' @export
#'
#' @seealso \code{\link{calculate_dose}}, \code{\link{LAR}}
#'
#' @examples dose_collective(LAR(data = calculate_dose(dose_data),
#'                               dose_comp = "tot",
#'                               organ_ls = c("Colon", "Thyroid")),
#'                           prefix = "LAR_")
#'          dose_collective(LAR(data = calculate_dose(dose_data),
#'                              dose_comp = "tot",
#'                              organ_ls = c("Colon", "Thyroid")),
#'                         prefix = NULL,
#'                         cols = c("LAR_tot_Colon", "LAR_tot_Thyroid"))
dose_collective <- function(data,
                            by.var = c("year", "sex"),
                            prefix = "LAR_",
                            cols = NULL)
{
  if (!is.null(cols) & !is.null(prefix)) {
  stop("One of cols and prefix should be set to NULL.")
  }
  if (!is.null(cols) & !is.null(prefix)) {
    stop("Both cols and prefix are NULL.")
  }
  if (is.null(cols) & !is.null(prefix)) {
    cols <- grep(prefix, names(data), value = T)
  }
  if (length(setdiff(by.var, names(data))) > 0) {
    stop(sprintf("Variable %s in by.var is not in the data.",
                 paste(setdiff(by.var, names(data)), collapse = ", ")))
  }

  data2 <-  data[, lapply(.SD, sum, na.rm = T),
                           by = by.var,
                           .SDcols = cols]
  return(data2)
}

