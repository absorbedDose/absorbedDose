
<!-- README.md is generated from README.Rmd. Please edit that file -->

# absorbedDose

<!-- badges: start -->
<!-- badges: end -->

The package absorbedDose calculates the estimated organ absorbed dose
following the Chernobyl Nuclear Power Plant accident.

The details about the model can be found in Tondel, M., Gabrysch, K.,
Rääf, C., Isaksson, M. (2022) Estimating the organ absorbed dose in
Swedish inhabitants following the Chernobyl Nuclear Power Plant accident
with the R package absorbedDose. Uppsala University. doi:
10.33063/diva-484911.

## Installation

The package <code>absorbedDose</code> can be installed in the following
way:

``` r
# install.packages("devtools")
devtools::install_github("absorbedDose/absorbedDose")
```

and then call

``` r
library("absorbedDose")
```

## Data set

A data set to calculate the doses should look like the generated sample
data set <code>dose_data</code>:

``` r
print(dose_data, nrow=10)
#>        id year date_birth sex  household event_type date_event   cesium
#>    1:   1 1986 1975-08-19   M     Hunter       <NA>       <NA> 8.926435
#>    2:   1 1987 1975-08-19   M     Hunter       <NA>       <NA> 8.926435
#>    3:   1 1988 1975-08-19   M     Hunter       <NA>       <NA> 8.926435
#>    4:   1 1989 1975-08-19   M     Hunter       <NA>       <NA> 8.897168
#>    5:   1 1990 1975-08-19   M     Hunter       <NA>       <NA> 8.897168
#>   ---                                                                  
#> 2280: 100 2016 1940-12-23   W Non-hunter       <NA>       <NA> 3.498142
#> 2281: 100 2017 1940-12-23   W Non-hunter       <NA>       <NA> 3.498142
#> 2282: 100 2018 1940-12-23   W Non-hunter       <NA>       <NA> 3.498142
#> 2283: 100 2019 1940-12-23   W Non-hunter       <NA>       <NA> 3.498142
#> 2284: 100 2020 1940-12-23   W Non-hunter       <NA>       <NA> 3.498142
#>       municipality county
#>    1:          382      3
#>    2:          382      3
#>    3:          382      3
#>    4:          484      4
#>    5:          484      4
#>   ---                    
#> 2280:          483      4
#> 2281:          483      4
#> 2282:          483      4
#> 2283:          483      4
#> 2284:          483      4
```

## Calculation of the absorbed dose

The function <code>calculate_dose</code> calls functions for data
management (<code>dose_dm</code>, <code>dose_new_variables</code>), for
calculation of each dose component (<code>dose_external</code>,
<code>dose_internal_ing</code>, <code>dose_internal_milk</code>,
<code>dose_inhalation</code>), and for adding the dose components
<code>dose_total</code>). The doses are calculated per row (per year of
follow-up time).

``` r
doses_row <- calculate_dose(dose_data, organ_ls=c("Colon", "Thyroid"))
```

To get the total absorbed dose under the follow-up period for each
person (for each id) call:

``` r
doses_id <- dose_total_per_person(doses_row)
```
