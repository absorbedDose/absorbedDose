% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dose_calc.R
\name{dose_internal_milk}
\alias{dose_internal_milk}
\title{Calculation of the internal dose via diary milk ingestion}
\usage{
dose_internal_milk(
  data,
  k_delay = 0.74,
  tb_c_milk = tab_county[, .(Code, c_milk)],
  organ_ls = "Thyroid",
  ...
)
}
\arguments{
\item{data}{data table with all variables as in the data set returned from
\code{\link{dose_new_variables}}.}

\item{k_delay}{dimensionless value.}

\item{tb_c_milk}{c_milk values per county, table with columns
Code and c_milk.}

\item{organ_ls}{list of cancer sites (organs) to calculate the dose for,
calculates only dose for Thyroid if it is in the \code{organ_ls}.}

\item{...}{}
}
\value{
updated data set with internal dose via diary milk ingestion values
fo Thyroid per row,
the internal dose via diary milk variable has name \code{Dmilk_Thyroid}
}
\description{
Calculates the internal dose via diary milk ingestion for every row in the
data set for Thyroid.
}
\examples{
dose_internal_milk(data = dose_new_variables(dose_dm(dose_data)),
organ_ls = c("Thyroid", "Colon"))
}
\seealso{
\code{\link{dose_external}}, \code{\link{dose_internal_ing}},
\code{\link{dose_inhalation}}
}
