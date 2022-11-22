#' @import data.table
NULL

# helper function for product
dose_times <-  function(y,x){x*y}

#' Data management of the original data -- data preparation
#'
#' Deletes the rows with empty entries,
#' divides the row when a person moved to the two rows (with change of address on 30 June),
#' creates the variables `start_date` and `stop_date`.
#'
#' @param data data table with variables as in \code{\link{dose_data}}.
#' @param Chernobyl_date starting date for calculations of doses,  in format "%Y-%m-%d".
#' @param last_follow_up_date date until which the dose will be calculated,
#' in format "%Y-%m-%d",
#' NULL if calculated until the last possible entry.
#' @param codes_county vector of county codes,
#' to check if all county codes in the data set are in `codes_county`,
#' the rows with county code not in the `codes_county` are deleted.
#' @param codes_municipality vector of municipality codes,
#' to check if all municipality codes in the data set are in
#' `codes_municipality`,
#' the rows with municipality code not in the `codes_municipality` are deleted.
#' @param ...
#'
#' @return updated data set.
#' @export
#'
#' @seealso \code{\link{dose_new_variables}}
#'
#' @examples dose_dm(data = dose_data)
#' dose_dm(data = dose_data, last_follow_up_date = "1991-04-28")
dose_dm <- function(data,
                    Chernobyl_date = "1986-04-28",
                    last_follow_up_date = NULL,
                    codes_county = tab_county$Code,
                    codes_municipality = tab_fshield$Code, ...)
{
  ### copy the data set
  ### otherwise the original data would change
  data.copy <- data

  ### date format
  data.copy[, `:=`(date_birth = as.Date(date_birth, format = c("%Y-%m-%d")),
                   date_event = as.Date(date_event, format = c("%Y-%m-%d")))]
  Chernobyl_date <- as.Date(Chernobyl_date, format = "%Y-%m-%d")

  ### remove rows with missing values
  data.copy <- data.copy[!is.na(date_birth), ]
  data.copy <- data.copy[!is.na(year), ]
  data.copy <- data.copy[!is.na(county), ]
  data.copy <- data.copy[!is.na(municipality), ]
  data.copy <- data.copy[!is.na(cesium), ]
  data.copy <- data.copy[!is.na(household), ]

  ### change municipality code 2020 (not existing) to 2026
  if (!(2020 %in% codes_municipality)) {
    data.copy[municipality==2020, municipality:=2026]
  }

  ### check that municipality and county are in the tables
  data.copy <- data.copy[data.copy$municipality %in% codes_municipality, ]
  data.copy <- data.copy[data.copy$county %in% codes_county, ]

  ### remove persons with events before or at the Chernobyl date
  data.copy <- data.copy[date_event > Chernobyl_date | is.na(date_event), ]

  ### start date for each row is 1 January except for 1986 when it is Chernobyl_date
  data.copy[ , start_date := as.Date(paste(year, "-01-01", sep=""), format = c("%Y-%m-%d"))]
  data.copy[year==1986, start_date := Chernobyl_date]

  ### stop date is 1 January next year
  data.copy[ , stop_date := as.Date(paste(year+1, "-01-01", sep=""), format = c("%Y-%m-%d"))]

  ### remove rows after the end of follow up
  ### change stop_date to the end of follow up
  if (!is.null(last_follow_up_date)){
    if (!is.na(last_follow_up_date)){
      if (is.character(last_follow_up_date)) {
        last_follow_up_date <- as.Date(last_follow_up_date, format = "%Y-%m-%d")
      }
      data.copy <- data.copy[start_date <= last_follow_up_date, ]
      data.copy[stop_date > last_follow_up_date, stop_date := last_follow_up_date]
    }
  }

  ### for each person remove rows after the event because of censoring
  ### remove also the year of the event
  data.copy <- data.copy[is.na(date_event) | stop_date <= date_event, ]

  ### remove rows where stop_date is before start_date
  data.copy <- data.copy[stop_date >= start_date, ]

  ### order the data.copy by id and start year
  data.copy <- data.copy[order(id, year), ]

  ### if a person is missing one year in the data.copy, delete the rows afterwards
  data.copy <- data.copy[, Nr := .I]
  data.copyt <- data.copy[data.copy[, .I[1], by = id]$V1, .(id, year, Nr)]
  setnames(data.copyt, "year", "first_year")
  setnames(data.copyt, "Nr", "first_Nr")
  data.copy3 <- data.copyt[data.copy, on=.(id = id)]
  data.copy3[, year_test := first_year + Nr - first_Nr]
  data.copy3 <- data.copy3[year_test - year == 0, ]
  data.copy3 <- data.copy3[, c("Nr", "first_Nr", "first_year", "year_test") := NULL]
  data.copy <- data.copy3
  remove(data.copyt, data.copy3)

  data.copy <- data.copy[order(id, start_date), ]

  ### if a person has moved, that is,
  ### the current row and next row have the same id,
  ### start=start+1 and cesium values are different
  ### then add a new row with 1/1 year +1 - 30/6 year+ 1
  ### and cesium/county/municipality values from the previous year
  ### if start_date is after>30/6 do not add a new row
  ### if stop_date is before 30/6 then just change the cesium/county/municipality values

  new_rows <- data.copy[(as.Date(paste(year+1, "-06-30", sep = ""),
                                 format = c("%Y-%m-%d")) < shift(stop_date, fill = 1, type = "lead")) &
                          id == shift(id, fill = 1, type = "lead") &
                          (cesium != shift(cesium, fill = 1, type = "lead") |
                             county != shift(county, fill = 1, type = "lead") |
                             municipality != shift(municipality, fill = 1, type = "lead")) &
                          year+1 == shift(year, fill = 1, type = "lead"), ]
  new_rows[, `:=`(year = year+1,
                  start_date = as.Date(paste(year+1, "-01-01", sep = ""),
                                       format = c("%Y-%m-%d")),
                  stop_date = as.Date(paste(year+1, "-06-30", sep = ""),
                                      format = c("%Y-%m-%d")))]
  data.copy[shift((as.Date(paste(year+1, "-06-30", sep=""),
                           format = c("%Y-%m-%d")) < shift(stop_date, fill = 1, type = "lead")) &
                    id == shift(id, fill = 1, type = "lead") &
                    (cesium != shift(cesium, fill = 1, type = "lead") |
                       county != shift(county, fill = 1, type = "lead") |
                       municipality != shift(municipality, fill = 1, type = "lead")) &
                    year+1 == shift(year, fill = 1, type = "lead"), type = "lag"),
            start_date := as.Date(paste(year, "-06-30", sep=""),
                                  format = c("%Y-%m-%d"))]

  if (!is.null(last_follow_up_date)){
    if (!is.na(last_follow_up_date)){
      data.copy[shift((as.Date(paste(year+1, "-06-30", sep = ""),
                               format = c("%Y-%m-%d")) > shift(stop_date, fill = 1, type = "lead")) &
                        id == shift(id, fill = 1, type = "lead") &
                        (cesium != shift(cesium, fill = 1, type = "lead") |
                           county != shift(county, fill = 1, type = "lead") |
                           municipality != shift(municipality, fill = 1, type = "lead")) &
                        year+1 == shift(year, fill = 1, type = "lead"), type = "lag"),
                `:=` (cesium = NA, county = NA, municipality = NA)]
    }
  }

  ### add new_rows
  data.copy <- rbindlist(list(data.copy, new_rows), use.names=TRUE)
  data.copy <- data.copy[order(id, start_date), ]

  ### change NA values to the values of the previous row
  if (!is.null(last_follow_up_date)){
    if (!is.na(last_follow_up_date)){
      data.copy [, `:=` (cesium = fifelse(is.na(cesium),
                                          shift(cesium, fill = 1, type = "lag"), cesium),
                         county = fifelse(is.na(county),
                                          shift(county, fill = 1, type = "lag"), county),
                         municipality = fifelse(is.na(municipality),
                                                shift(municipality, fill = 1, type = "lag"), municipality))]
    }
  }

  remove(new_rows)

  return(data.copy)
}

# function of weight for female children (age 0-20)
dose_weight20f <- function(age)
{
  return(-0.0000057*age^6 + 0.000552*age^5 - 0.0199*age^4 + 0.3191*age^3 -
           2.1579*age^2 + 7.4423*age + 3.9529)
}
# function of weight for male children (age 0-20)
dose_weight20m <- function(age)
{
  return(-0.0000021*age^6 + 0.0002623*age^5 - 0.011799*age^4 + 0.2305*age^3 -
           1.8759*age^2 + 8.0766*age + 3.8872)
}

#' Data management of the original data -- creates new variables
#'
#' Adds variables that are used in calculations of the organ absorbed dose.
#' The added variables are `start_time`, `stop_time`, `age`, `age_middle`,
#' `stop_date_50d`, `age_50d`, `weight`.
#'
#' @param data data table with variables as in \code{\link{dose_data}} and
#' with variables `start_date`, `stop_date`.
#' @param Chernobyl_date starting date for calculations of doses, in format "%Y-%m-%d".
#' @param ...
#'
#' @return updated data set with the new variables.
#' @export
#'
#' @seealso \code{\link{dose_dm}}
#'
#' @examples dose_new_variables(dose_dm(data = dose_data))
dose_new_variables <- function(data,
                               Chernobyl_date = as.Date("1986-04-28", format = c("%Y-%m-%d")),
                               ...){

  data.copy <- data
  ### calculate the time in years from Chernobyl date to start/stop time
  data.copy[, `:=`( start_time = as.numeric(start_date - Chernobyl_date)/365.25,
                    stop_time = as.numeric(stop_date - Chernobyl_date)/365.25)]

  ### milk and inhalation dose is included just the first 50 days
  ### set stop date for these to 50 days after Chernobyl date
  data.copy[, stop_date_50d := fifelse(stop_date > Chernobyl_date + 50,
                                       Chernobyl_date + 50,
                                       stop_date)]

  ### calculate exact age at start_date
  data.copy[, age := as.numeric(start_date-date_birth)/365.25]

  ### calculate exact age in the middle of the interval (start_date, stop_date)
  data.copy[, age_middle := age + (as.numeric(stop_date - start_date)/2)/365.25]

  ### for milk and inhalation and weight - we want age in the middle of the
  ### interval (start_date, stop_date_milk)
  data.copy[, age_50d := as.numeric(NA)]
  data.copy[start_date < (Chernobyl_date+50), age_50d := age +
              as.numeric(stop_date_50d - start_date)/2/365.25]

  data.copy[, weight := 78]
  data.copy[age_middle >= 20 & sex == "W", weight := 63]
  data.copy[age_middle < 20 & sex == "M", weight := dose_weight20m(age_middle)]
  data.copy[age_middle < 20 & sex == "W", weight := dose_weight20f(age_middle)]

  return(data.copy)
}

# integral of r(t) for adults
dose_int_r_t <- function(t)
{
  # coefficients are the same as in the function int_r_t_unga
  r <- -0.96/36.89*exp(-36.89*t)-
    0.10823/2.447*exp(-2.447*t)-
    0.0796/0.6684*exp(-0.6684*t)-
    0.0314/0.125646*exp(-0.125646*t)
  return(r)
}

# integral of k_{organ,K}*r(t) for children (age<20 at the start_time)
dose_int_r_t_child <- function(age, t, a0, a1, a2, a3) ### age is age at the Chernobyl date
{
  ### coefficients for kerma a0+a1*(age+t)+a2*(age+t)^2+a3*(age+t)^3

  ### coefficients for polynom of t:
  ### b0+b1*t+b2*t^2+b3*t^3=a0+a1*(age+t)+a2*(age+t)^2+a3*(age+t)^3
  b0 <- a0 + a1*age + a2*age^2 + a3*age^3 #t^0
  b1 <- a1 + a2*2*age + a3*3*age^2 #t^1
  b2 <- a2 + a3*3*age #t^2
  b3 <- a3 #t^3

  ### coefficients in function r(t) - same as in the function int_r_t
  c1 <- 0.96
  d1 <- -36.89
  c2 <- 0.10823
  d2 <- -2.447
  c3 <- 0.0796
  d3 <- -0.6684
  c4 <- 0.0314
  d4 <- -0.125646

  ### integral of kerma*r(t)=(b0+b1*t+b2*t^2+b3*t^3)*r(t)
  return(c1/d1*exp(d1*t)*(b0+b1*(t-1/d1) + b2*(t^2-2*t/d1+2/d1^2) +
                              b3*(t^3-3*t^2/d1+6*t/d1^2-6/d1^3)) +
           c2/d2*exp(d2*t)*(b0+b1*(t-1/d2) + b2*(t^2-2*t/d2+2/d2^2) +
                              b3*(t^3-3*t^2/d2+6*t/d2^2-6/d2^3))+
           c3/d3*exp(d3*t)*(b0+b1*(t-1/d3) + b2*(t^2-2*t/d3+2/d3^2) +
                              b3*(t^3-3*t^2/d3+6*t/d3^2-6/d3^3))+
           c4/d4*exp(d4*t)*(b0+b1*(t-1/d4) + b2*(t^2-2*t/d4+2/d4^2) +
                              b3*(t^3-3*t^2/d4+6*t/d4^2-6/d4^3)))
}

#' Calculation of the external dose
#'
#' Calculates the external dose for every row in the data set and for
#' all organs given in `organ_ls`.
#'
#' @param data data table with all variables as in the data set returned from
#'  \code{\link{dose_new_variables}}.
#' @param d_Cs value in (mSv/y)/(kBq/m^2).
#' @param phi_kerma value in mGy/mSv.
#' @param f_out dimensionless value.
#' @param tb_Fsnow F_snow values per county, table with columns
#' Code and F_snow.
#' @param tb_fshield f_shield values per municipality,
#' table with columns Code and f_shield.
#' @param tb_organ coefficients per cancer site (organ),
#' table with columns
#' Organ, k_SEQ_organ_f, a0_f, a1_f, a2_f, a3_f,
#' k_SEQ_organ_m, a0_m, a1_m, a2_m, a3_m.
#' @param organ_ls list of cancer sites (organs) to calculate the dose for,
#' the organs should have coefficients in `tb_organ`.
#' @param ...
#'
#' @return updated data set with external dose values per row,
#' the external dose variables have names of the form `Dext_organ_name`.
#' @export
#'
#' @seealso \code{\link{dose_internal_ing}},
#' \code{\link{dose_internal_milk}}, \code{\link{dose_inhalation}}
#'
#' @examples dose_external(data = dose_new_variables(dose_dm(dose_data)),
#' organ_ls = c("Thyroid", "Colon"))
dose_external <- function(data,
                          d_Cs = 1.016856,
                          phi_kerma = 0.83,
                          f_out = 0.2,
                          tb_Fsnow = tab_county[,.(Code, F_snow)],
                          tb_fshield = tab_fshield,
                          tb_organ = tab_organ,
                          organ_ls = tb_organ$Organ, ...)
{
  data.copy <- data

  if (length(setdiff(organ_ls, tb_organ$Organ)) > 0) {
    stop(sprintf("Coefficients for %s are not in the tb_organ",
                 paste(setdiff(organ_ls, tb_organ$Organ), collapse = ", ")))
  }

  names(organ_ls) <- organ_ls
  organ_ls <- gsub(" ", "_", organ_ls)

  ### coefficient F_snow dependent on county
  data.copy <- tb_Fsnow[data.copy, on=.(Code=county)]
  setnames(data.copy, "Code", "county")

  ### coefficient f_shield dependent on municipality
  data.copy <- tb_fshield[, .(Code, f_shield)][data.copy, on=.(Code=municipality)]
  setnames(data.copy, "Code", "municipality")

  k_SEQ_organ_M <- tb_organ$k_SEQ_organ_m
  names(k_SEQ_organ_M) <- gsub(" ", "_", tab_organ$Organ)
  k_SEQ_organ_M <- k_SEQ_organ_M[names(k_SEQ_organ_M) %in% organ_ls]

  k_SEQ_organ_F <- tb_organ$k_SEQ_organ_f
  names(k_SEQ_organ_F) <- gsub(" ", "_", tab_organ$Organ)
  k_SEQ_organ_F <- k_SEQ_organ_F[names(k_SEQ_organ_F) %in% organ_ls]

  ### organ dose for adults
  data.copy[, k_r_int := 1]
  data.copy[age >= 20, k_r_int := dose_int_r_t(stop_time) - dose_int_r_t(start_time)]
  data.copy[, tmp_ext := cesium * d_Cs * phi_kerma * F_snow * (f_out + (1 - f_out) * f_shield) *  k_r_int]
  data.copy[sex == "W" & age >= 20, (paste0("Dext_", names(k_SEQ_organ_F))) := lapply(k_SEQ_organ_F, dose_times, tmp_ext)]
  data.copy[sex == "M" & age >= 20, (paste0("Dext_", names(k_SEQ_organ_M))) := lapply(k_SEQ_organ_M, dose_times, tmp_ext)]

  ### fix separately for each organ for age<20
  for (organ.n in names(organ_ls))
  {
    organ <- organ_ls[organ.n]
    ### read k_seq coefficients for children - gender dependent
    coef_f <- as.matrix(tab_organ[Organ == organ.n, paste0("a", 0:3, "_f")])
    coef_m <- as.matrix(tab_organ[Organ == organ.n, paste0("a", 0:3, "_m")])
    data.copy[age < 20 & sex == "W",
              k_r_int := dose_int_r_t_child(age = age - start_time, t = stop_time,
                                            a0 = coef_f[1], a1 = coef_f[2],
                                            a2 = coef_f[3], a3 = coef_f[4]) -
                dose_int_r_t_child(age = age - start_time, t = start_time,
                                   a0 = coef_f[1], a1 = coef_f[2],
                                   a2 = coef_f[3], a3 = coef_f[4])]
    data.copy[age < 20 & sex == "M",
              k_r_int := dose_int_r_t_child(age = age-start_time, t = stop_time,
                                            a0 = coef_m[1], a1 = coef_m[2],
                                            a2 = coef_m[3], a3 = coef_m[4]) -
                dose_int_r_t_child(age = age - start_time, t = start_time,
                                   a0 = coef_m[1], a1 = coef_m[2],
                                   a2 = coef_m[3], a3 = coef_m[4])]
    k_seqF <- tab_organ[Organ == organ.n, k_SEQ_organ_f]
    k_seqM <- tab_organ[Organ == organ.n, k_SEQ_organ_m]
    data.copy[sex == "W" & age < 20, (paste0("Dext_", organ)) := tmp_ext * k_r_int * k_seqF]
    data.copy[sex == "M" & age < 20, (paste0("Dext_", organ)) := tmp_ext * k_r_int * k_seqM]
  }

  ### delete columns generated to calculate the dose
  data.copy[, `:=`(tmp_ext = NULL,
                   k_r_int = NULL,
                   F_snow = NULL,
                   f_shield = NULL)]

  return(data.copy)
}

# integral of Cs-137 component
int_Cs_137 <- function(t, t1, t2, t3, c1, c2)
{
  d1 <- -log(2)/t2
  d2 <- -log(2)/t3
  c3 <- c1
  d3 <- -(log(2)/t1 + log(2)/t2)
  c4 <- c2
  d4 <- -(log(2)/t1 + log(2)/t3)

  int <- c1/d1*exp(d1*t) + c2/d2*exp(d2*t) - c3/d3*exp(d3*t) - c4/d4*exp(d4*t)
  return(int)
}

# integral of Cs-134 component
int_Cs_134 <- function(t, t1, t2, t3, c1, c2, T_Cs_137=2.06, T_Cs_134=30)
{
  Cs_diff <- log(2)*(1/T_Cs_134 - 1/T_Cs_137)
  d1 <- -(log(2)/t2 + Cs_diff)
  d2 <- -(log(2)/t3 + Cs_diff)
  c3 <- c1
  d3 <- -(log(2)/t1 + log(2)/t2 + Cs_diff)
  c4 <- c2
  d4 <- -(log(2)/t1 + log(2)/t3 + Cs_diff)

  int <- c1/d1*exp(d1*t) + c2/d2*exp(d2*t) - c3/d3*exp(d3*t) - c4/d4*exp(d4*t)
  return(int)
}

#' Calculation of the internal dose from the ingestion
#'
#' Calculates the internal dose from the ingestion for every row in the data set
#' and for all organs given in `organ_ls`.
#'
#' @param data data table with all variables as in the data set returned from
#'  \code{\link{dose_new_variables}}.
#' @param S_aliment dimensionless value.
#' @param FR dimensionless value.
#' @param f_sex coefficients for women of age>20, men and children have
#' `f_sex=1`, dimensionless.
#' @param tb_Aesd A_esd values per county, table with columns
#' Code and A_esd.
#' @param tb_organ coefficients per cancer site (organ),
#' table with columns
#' Organ, Cs134_f, Cs137_f, Cs134_m,  Cs137_m.
#' @param organ_ls list of cancer sites (organs) to calculate the dose for,
#' all organs should have coefficients in `tb_organ`.
#' @param ...
#'
#' @return updated data set with internal dose from the ingestion
#' values per row,
#' the internal dose variables have names of the form `Dint_organ_name`.
#' @export
#'
#' @seealso \code{\link{dose_external}}, \code{\link{dose_internal_milk}}
#' \code{\link{dose_inhalation}}
#'
#' @examples dose_internal_ing(data = dose_new_variables(dose_dm(dose_data)),
#' organ_ls = c("Thyroid", "Colon"))
dose_internal_ing <- function(data,
                              S_aliment = 1,
                              FR = 0.56,
                              f_sex = 0.61,
                              tb_Aesd = tab_county[,.(Code, A_esd)],
                              tb_organ = tab_organ,
                              organ_ls = tb_organ$Organ, ...)
{
  data.copy <- data

  if (length(setdiff(organ_ls, tb_organ$Organ)) > 0) {
    stop(sprintf("Coefficients for %s are not in the tb_organ",
                 paste(setdiff(organ_ls, tb_organ$Organ), collapse = ", ")))
  }

  names(organ_ls) <- organ_ls
  organ_ls <- gsub(" ", "_", organ_ls)

  T_Cs_137 <- 30.2
  T_Cs_134 <- 2.06

  ### coefficient A_esd dependent on county
  data.copy <- tb_Aesd[data.copy, on=.(Code=county)]
  setnames(data.copy, "Code", "county")

  ### coefficient f_sex depend on gender
  data.copy[, fsex := fifelse(age_middle>=20 & sex=="W", f_sex, 1)]

  ### coefficients dependend on sub_popopulation
  data.copy[household == "Non-hunter", `:=`(T_ag=11, t1=1, t2=0.75, t3=15, c1=1, c2=0.1)]
  data.copy[household == "Hunter", `:=`(T_ag=29.3, t1=1.1, t2=1.2, t3=30, c1=0.9, c2=0.11)]

  data.copy[ , `:=`( int_137_start = int_Cs_137(start_time, t1, t2, t3, c1, c2),
                     int_137_stop = int_Cs_137(stop_time, t1, t2, t3, c1, c2),
                     int_134_start = int_Cs_134(start_time, t1, t2, t3, c1, c2, T_Cs_137, T_Cs_134),
                     int_134_stop = int_Cs_134(stop_time, t1, t2, t3, c1, c2, T_Cs_137, T_Cs_134)
  )]

  weight_adultF <- 63
  weight_adultM <- 78

  k_organ_Cs_137F <- tab_organ$Cs137_f * 3600*24*365.25*1000 * weight_adultF^0.889
  names(k_organ_Cs_137F) <- gsub(" ", "_", tab_organ$Organ)
  k_organ_Cs_137F <- k_organ_Cs_137F[names(k_organ_Cs_137F) %in% organ_ls]
  k_organ_Cs_137M <- tab_organ$Cs137_m * 3600*24*365.25*1000 * weight_adultM^0.889
  names(k_organ_Cs_137M) <- gsub(" ", "_", tab_organ$Organ)
  k_organ_Cs_137M <- k_organ_Cs_137M[names(k_organ_Cs_137M) %in% organ_ls]

  k_organ_Cs_134F <- tab_organ$Cs134_f * 3600*24*365.25*1000 * weight_adultF^0.812
  names(k_organ_Cs_134F) <- gsub(" ", "_", tab_organ$Organ)
  k_organ_Cs_134F <- k_organ_Cs_134F[names(k_organ_Cs_134F) %in% organ_ls]
  k_organ_Cs_134M <- tab_organ$Cs134_m * 3600*24*365.25*1000 * weight_adultM^0.812
  names(k_organ_Cs_134M) <- gsub(" ", "_", tab_organ$Organ)
  k_organ_Cs_134M <- k_organ_Cs_134M[names(k_organ_Cs_134M) %in% organ_ls]

  data.copy[, tmp137 :=  A_esd * T_ag * S_aliment *  fsex * weight^0.111 * (int_137_stop - int_137_start)]
  data.copy[, tmp134 :=  A_esd * T_ag * S_aliment *  fsex *  FR * weight^0.188 * (int_134_stop - int_134_start)]

  data.copy[sex == "W",
            (paste0(paste0("Dint_", names(k_organ_Cs_137F)), 137)) := lapply(k_organ_Cs_137F, dose_times, tmp137)]
  data.copy[sex == "M",
            (paste0(paste0("Dint_", names(k_organ_Cs_137M)), 137)) := lapply(k_organ_Cs_137M, dose_times, tmp137)]
  data.copy[sex == "W",
            (paste0(paste0("Dint_", names(k_organ_Cs_134F)), 134)) := lapply(k_organ_Cs_134F, dose_times, tmp134)]
  data.copy[sex == "M",
            (paste0(paste0("Dint_", names(k_organ_Cs_134M)), 134)) := lapply(k_organ_Cs_134M, dose_times, tmp134)]

  for (organ in organ_ls){
    data.copy[, paste0("Dint_", organ) := Reduce(`+`, .SD), .SDcols=grep(paste0("Dint_", organ), names(data.copy))]
    data.copy[, (paste0(paste0("Dint_", organ), 137)) := NULL]
    data.copy[, (paste0(paste0("Dint_", organ), 134)) := NULL]
  }

  ### delete columns generated to calculate the dose
  data.copy[, `:=`(tmp137 = NULL,
                   tmp134 = NULL,
                   int_137_start = NULL,
                   int_137_stop = NULL,
                   int_134_start = NULL,
                   int_134_stop = NULL)]

  ### delete columns with coefficients
  data.copy[, `:=`(T_ag = NULL,
                   t1 = NULL,
                   t2 = NULL,
                   t3 = NULL,
                   c1 = NULL,
                   c2 = NULL,
                   fsex = NULL,
                   A_esd = NULL)]

  return(data.copy)
}

#' Calculation of the internal dose via diary milk ingestion
#'
#' Calculates the internal dose via diary milk ingestion for every row in the
#' data set for Thyroid.
#'
#' @param data data table with all variables as in the data set returned from
#'  \code{\link{dose_new_variables}}.
#' @param k_delay dimensionless value.
#' @param tb_c_milk c_milk values per county, table with columns
#' Code and c_milk.
#' @param organ_ls list of cancer sites (organs) to calculate the dose for,
#' calculates only dose for Thyroid if it is in the `organ_ls`.
#' @param ...
#'
#' @return updated data set with internal dose via diary milk ingestion values
#' fo Thyroid per row,
#' the internal dose via diary milk variable has name `Dmilk_Thyroid`
#' @export
#'
#' @seealso \code{\link{dose_external}}, \code{\link{dose_internal_ing}},
#' \code{\link{dose_inhalation}}
#' @examples dose_internal_milk(data = dose_new_variables(dose_dm(dose_data)),
#' organ_ls = c("Thyroid", "Colon"))
dose_internal_milk <- function(data,
                               k_delay = 0.74,
                               tb_c_milk = tab_county[,.(Code, c_milk)],
                               organ_ls = "Thyroid", ...)
{
  data.copy <- data

  if ("Thyroid" %in% organ_ls){

    ### coefficient c_milk dependent on county
    data.copy <- tb_c_milk[data.copy, on=.(Code=county)]
    setnames(data.copy, "Code", "county")

    data.copy[, Dmilk_Thyroid := 0]
    data.copy[start_date < stop_date_50d,
              `:=`( a_age = fifelse(age_50d < 1, 0.75*0.3,
                                    fifelse(age_50d>=20, 0.41,
                                            0.00002*age_50d^3 - 0.0017*age_50d^2 +
                                              0.04*age_50d + 0.102)),
                    d_ing = fifelse(age_50d>=20, 0.43,
                                    4.0459-0.718272*age_50d + 0.072904*age_50d^2 -
                                      0.00441*age_50d^3 + 0.0001524*age_50d^4 -
                                      0.000002717*age_50d^5 +
                                      1.93*10^(-8)*age_50d^6)/1000)]
    data.copy[start_date < stop_date_50d,
              Dmilk_Thyroid := c_milk * a_age * d_ing * k_delay]

    data.copy[, `:=`(a_age = NULL,
                     d_ing = NULL,
                     c_milk = NULL)]
  }

  return(data.copy)
}

#' Calculation of the inhalation dose
#'
#' Calculates the inhalation dose for every row in the data
#' set for Thyroid.
#'
#' @param data data table with all variables as in the data set returned from
#'  \code{\link{dose_new_variables}}.
#' @param k_th_E value in mGy/mSv.
#' @param tb_E_inh E_inh values per county, table with columns
#' Code and E_inh.
#' @param organ_ls list of cancer sites (organs) to calculate the dose for,
#' calculates only dose for Thyroid if it is in the `organ_ls`.
#' @param ...
#'
#' @return updated data set with internal dose via diary milk ingestion values
#' fo Thyroid per row,
#' the internal dose via diary milk variable has name `Dinh_Thyroid`.
#' @export
#'
#' @seealso \code{\link{dose_external}}, \code{\link{dose_internal_ing}},
#' \code{\link{dose_internal_milk}}
#'
#' @examples dose_inhalation(data = dose_new_variables(dose_dm(dose_data)),
#' organ_ls = c("Thyroid", "Colon"))
dose_inhalation <- function(data,
                            k_th_E = 20,
                            tb_E_inh = tab_county[,.(Code, E_inh)],
                            organ_ls = "Thyroid", ...)
{
  data.copy <- data

  if ("Thyroid" %in% organ_ls){
    ### coefficient c_milk dependent on county
    data.copy <- tb_E_inh[data.copy, on=.(Code=county)]
    setnames(data.copy, "Code", "county")

    data.copy[, Dinh_Thyroid := 0]
    data.copy[start_date < stop_date_50d,
              F_age := fifelse(age_50d >= 20, 1,
                               1.7245 + 0.7869*age_50d - 0.1976*age_50d^2 +
                                 0.02309*age_50d^3 - 0.0015207*age_50d^4 +
                                 0.0000521*age_50d^5 - 0.0000007097*age_50d^6)]
    data.copy[start_date < stop_date_50d,
              Dinh_Thyroid := E_inh * F_age * k_th_E/1000]

    data.copy[, `:=`( F_age = NULL,
                      E_inh = NULL)]
  }

  return(data.copy)
}

#' Calculation of the total absorbed dose
#'
#' Calculates the sum of the doses for every row in the data.
#' For each organ in `organ_ls`, the function adds the values of all variables that have
#' this organ in their name.
#'
#' @param data data table with variables for doses, like
#' `Dext_organ_name`, `Dint_organ_name`, `Dmilk_Thyroid`, `Dinh_Thyroid`.
#' @param organ_ls list of cancer sites (organs) to calculate the total dose for.
#' @param ...
#'
#' @return updated data set with total dose per row,
#' the total dose variables have name of the form `Dtot_organ_name`.
#' @export
#'
#' @seealso \code{\link{dose_external}}, \code{\link{dose_internal_ing}},
#' \code{\link{dose_internal_milk}}, \code{\link{dose_inhalation}}
dose_total <- function(data,
                       organ_ls = tab_organ$Organ, ...)
{
  data.copy <- data

  names(organ_ls) <- organ_ls
  organ_ls <- gsub(" ", "_", organ_ls)

  for (organ in organ_ls){
    data.copy[, paste0("Dtot_", organ) := Reduce(`+`, .SD),
              .SDcols=grep(organ, names(data.copy))]
  }

  data.copy <- data.copy[order(id, start_date), ]

  return(data.copy)
}

#' Calculation of the total absorbed dose per person
#'
#' Calculation of the sum of the doses for the entire follow-up period
#' for every id in the data. Should be used for data set returned
#' from \code{\link{calculate_dose}} or
#' after calculating some of the dose components.
#'
#' @param data data table with variables for the doses calculated per row, like
#' `Dext_organ_name`, `Dint_organ_name`, `Dmilk_Thyroid`, `Dinh_Thyroid`,
#' `Dtot_organ_name`.
#'
#' @return data table with one row per id and
#' variables
#' `date_birth`, `sex`, `date_event`, `event_type`, `household`,
#' `start_date`, `stop_date` (the time period for which the dose was calculated),
#' `Dext_name_organ`, `Dint_name_organ`, `Dmilk_Thyroid`, `Dinh_Thyroid`,
#' `Dtot_name_organ` (as sum of all rows for each id).
#'
#' @export
#'
#' @seealso \code{\link{calculate_dose}}
#'
#' @examples dose_total_per_person(calculate_dose(data = dose_data, organ_ls = c("Thyroid", "Colon")))
dose_total_per_person <- function(data)
{
  data.copy <- data
  ### select only the first values for each person
  Dose1a  <- data.copy[data.copy[, .I[1], by = id]$V1,
                       .(id, date_birth, sex, date_event, event_type,
                         household, start_date)]
  ### select only the stop date values for each person
  Dose1b <- data.copy[data.copy[, .I[.N], by = id]$V1, .(id, stop_date)]
  Dose <- Dose1a[Dose1b, on = .(id=id)]
  ### sum the yearly doses to get total dose for each id
  Dose2 <- data.copy[, lapply(.SD, sum, na.rm = F),
                     by = id,
                     .SDcols = c(grep("Dext", names(data.copy)),
                                 grep("Dint", names(data.copy)),
                                 grep("Dmilk", names(data.copy)),
                                 grep("Dinh", names(data.copy)),
                                 grep("Dtot", names(data.copy)))]
  Dose <- Dose[Dose2, , on=.(id = id)]

  return(Dose)
}

#' Calculation of all dose components for every row
#'
#' Calls functions for data management (\code{\link{dose_dm}},
#' \code{\link{dose_new_variables}})
#' and for each dose component (\code{\link{dose_external}},
#' \code{\link{dose_internal_ing}},
#' \code{\link{dose_internal_milk}},
#' \code{\link{dose_inhalation}}, \code{\link{dose_total}}).
#'
#' @param data data table with variables as in \code{\link{dose_data}}.
#' @param Chernobyl_date starting date for calculations of the dose components.
#' @param tab_county table with county dependent parameters,
#' it should have column names like \code{\link{tab_county}}.
#' @param tab_fshield table with municipality dependent parameter f_shield,
#' it should have columns Code and `f_shield`, like \code{\link{tab_fshield}}.
#' @param tab_organ table with organ dependent parameters, it should have
#' columns like \code{\link{tab_organ}}.
#' @param ... further arguments passed to the functions for data
#' management and calculation of the dose components.
#'
#' @return data set with original variables and external dose,
#' internal dose from the ingestion,
#' internal dose via diary milk ingestion, inhalation doses,
#' and total dose per row.
#' @export
#'
#' @examples calculate_dose(data = dose_data, organ_ls = c("Thyroid", "Colon"))
calculate_dose <- function(data,
                           Chernobyl_date = as.Date("1986-04-28", format = c("%Y-%m-%d")),
                           tab_county = tab_county,
                           tab_fshield = tab_fshield,
                           tab_organ = tab_organ, ...)
{
  data.copy <- copy(data)

  data.copy <- dose_dm(data = data.copy, ...)
  data.copy <- dose_new_variables(data = data.copy, ...)
  data.copy <- dose_external(data = data.copy, ...)
  data.copy <- dose_internal_ing(data = data.copy, ...)
  data.copy <- dose_internal_milk(data = data.copy, ...)
  data.copy <- dose_inhalation(data = data.copy, ...)
  data.copy <- dose_total(data = data.copy, ...)

  data.copy[, `:=`(start_time = NULL,
                   stop_time = NULL,
                   stop_date_50d = NULL,
                   age = NULL,
                   age_middle = NULL,
                   age_50d = NULL,
                   weight = NULL)]

  setcolorder(data.copy, neworder = c(names(data),
                                      "start_date", "stop_date",
                                      names(data.copy)[grep("Dext_" ,names(data.copy))],
                                      names(data.copy)[grep("Dint_" ,names(data.copy))],
                                      names(data.copy)[grep("Dmilk_" ,names(data.copy))],
                                      names(data.copy)[grep("Dinh_" ,names(data.copy))],
                                      names(data.copy)[grep("Dtot_" ,names(data.copy))])
              )

  return(data.copy)
}

