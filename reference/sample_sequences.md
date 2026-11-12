# Sample Sequence Observation Model

Sample Sequence Observation Model

## Usage

``` r
sample_sequences(frac_voc, seq_total, phi)
```

## Arguments

- frac_voc:

  A numeric vector of expected proportions positive for the variant of
  concern.

- seq_total:

  An integer vector of total sequences available.

- phi:

  The overdispersion of the sampling process. If not supplied then no
  overdispersion is used (i.e a binomial observation model vs a beta
  binomial observation model).

## Value

A vector of observed sequences positive for the variant of concern.

## See also

Functions to generate simulated data
[`generate_obs()`](epiforecasts.io/forecast.vocs/reference/generate_obs.md)

## Examples

``` r
# dummy sequence data
frac_voc <- seq(0, 1, by = 0.1)
seq_total <- seq(10, length.out = length(frac_voc), by = 100)

# binomial observation model
sample_sequences(frac_voc, seq_total)
#>  [1]    0   15   44   97  167  277  361  489  627  828 1010

# beta binomial observation model
sample_sequences(frac_voc, seq_total, 0.5)
#>  [1]    0    0  210  254  400    1  610  710  810  910 1010
```
