# Test positive COVID-19 cases and sequences in Germany

Test positive COVID-19 cases and sequences stratified by voc variant
status summarised by week for Germany. Data is sourced from the RKI via
the Germany/Poland forecasting hub.

## Usage

``` r
germany_covid19_delta_obs
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 114
rows and 9 columns.

## Value

A `data.table` with the following variables: date, location_name,
location, cases, seq_total, seq_voc, share_voc, cases_available, and
seq_available.

## See also

Package data sets
[`fv_example()`](epiforecasts.io/forecast.vocs/reference/fv_example.md)
