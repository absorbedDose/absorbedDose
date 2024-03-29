% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dose_calc.R
\name{calculate_dose}
\alias{calculate_dose}
\title{Calculation of all dose components for every row}
\usage{
calculate_dose(
  data,
  Chernobyl_date = as.Date("1986-04-28", format = c("\%Y-\%m-\%d")),
  tab_county = tab_county,
  tab_fshield = tab_fshield,
  tab_organ = tab_organ,
  ...
)
}
\arguments{
\item{data}{data table with variables as in \code{\link{dose_data}}.}

\item{Chernobyl_date}{starting date for calculations of the dose components.}

\item{tab_county}{table with county dependent parameters,
it should have column names like \code{\link{tab_county}}.}

\item{tab_fshield}{table with municipality dependent parameter f_shield,
it should have columns Code and \code{f_shield}, like \code{\link{tab_fshield}}.}

\item{tab_organ}{table with organ dependent parameters, it should have
columns like \code{\link{tab_organ}}.}

\item{...}{further arguments passed to the functions for data
management and calculation of the dose components.}
}
\value{
data set with original variables and external dose,
internal dose from the ingestion,
internal dose via diary milk ingestion, inhalation doses,
and total dose per row.
}
\description{
Calls functions for data management (\code{\link{dose_dm}},
\code{\link{dose_new_variables}})
and for each dose component (\code{\link{dose_external}},
\code{\link{dose_internal_ing}},
\code{\link{dose_internal_milk}},
\code{\link{dose_inhalation}}, \code{\link{dose_total}}).
}
\examples{
calculate_dose(data = dose_data, organ_ls = c("Thyroid", "Colon"))
}
