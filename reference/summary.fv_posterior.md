# Summary method for fv_tidy_posterior

`summary` method for class "fv_tidy_posterior". Can be used to filter
the posterior for variables of interest, to return forecasts only, and
to summarise using the `data.table` method

## Usage

``` r
# S3 method for class 'fv_posterior'
summary(object, type = "model", forecast = FALSE, as_dt = FALSE, ...)
```

## Arguments

- object:

  An object of the class `fv_posterior` as returned by
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md)
  .

- type:

  A character string used to filter the summarised output and defaulting
  to "model". Current options are: "model" which returns a summary of
  key model parameters, "cases" which returns summarised cases,
  "voc_frac" which returns summarised estimates of the fraction of cases
  that have the variant of concern, "voc_advantage" that returns
  summarised estimates of the the transmission advantage of the variant
  of concern, "growth" which returns summarised variant specific and
  overall growth rates, "rt" which returns summarised variant specific
  and overall reproduction number estimates, "raw" which returns a raw
  posterior summary, and "all" which returns all tidied posterior
  estimates.

- forecast:

  Logical defaults to `FALSE`. Should
  [`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md)
  be used to return only forecasts rather than complete posterior.

- as_dt:

  Logical defaults to `FALSE`. Once any filtering has been applied
  should [`summary()`](https://rdrr.io/r/base/summary.html) fall back to
  using the default `data.table` method.

- ...:

  Additional summary arguments.

## Value

A summary data.table table unless type "all" is used in which case the
output is still of type "fv_posterior".

## See also

[fv_tidy_posterior](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior)

Functions used for postprocessing of model fits
[`convert_to_stanfit()`](epiforecasts.io/forecast.vocs/reference/convert_to_stanfit.md),
[`extract_draws()`](epiforecasts.io/forecast.vocs/reference/extract_draws.md),
[`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md),
[`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
[`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md),
[`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md),
[`link_dates_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_dates_with_posterior.md),
[`link_obs_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_obs_with_posterior.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
[`print.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/print.fv_posterior.md),
[`quantiles_to_long()`](epiforecasts.io/forecast.vocs/reference/quantiles_to_long.md),
[`update_voc_label()`](epiforecasts.io/forecast.vocs/reference/update_voc_label.md)

## Examples

``` r
posterior <- fv_example(strains = 2, type = "posterior")

# case summary
summary(posterior, type = "cases")
#>               variable       date     type    obs observed forecast_start
#>                 <char>     <IDat>   <char>  <num>   <lgcl>         <lgcl>
#>  1:       sim_cases[1] 2021-03-20 Combined  87328     TRUE          FALSE
#>  2:       sim_cases[2] 2021-03-27 Combined 109442     TRUE          FALSE
#>  3:       sim_cases[3] 2021-04-03 Combined 117965     TRUE          FALSE
#>  4:       sim_cases[4] 2021-04-10 Combined 107223     TRUE          FALSE
#>  5:       sim_cases[5] 2021-04-17 Combined 142664     TRUE          FALSE
#>  6:       sim_cases[6] 2021-04-24 Combined 145568     TRUE          FALSE
#>  7:       sim_cases[7] 2021-05-01 Combined 131887     TRUE          FALSE
#>  8:       sim_cases[8] 2021-05-08 Combined 107141     TRUE          FALSE
#>  9:       sim_cases[9] 2021-05-15 Combined  77261     TRUE          FALSE
#> 10:      sim_cases[10] 2021-05-22 Combined  57310     TRUE          FALSE
#> 11:      sim_cases[11] 2021-05-29 Combined  33052     TRUE          FALSE
#> 12:      sim_cases[12] 2021-06-05 Combined  22631     TRUE          FALSE
#> 13:      sim_cases[13] 2021-06-12 Combined  15553     TRUE          FALSE
#> 14:      sim_cases[14] 2021-06-19 Combined   7659     TRUE          FALSE
#> 15:      sim_cases[15] 2021-06-26 Combined   5033     TRUE           TRUE
#> 16:      sim_cases[16] 2021-07-03 Combined     NA    FALSE          FALSE
#> 17:      sim_cases[17] 2021-07-10 Combined     NA    FALSE          FALSE
#> 18:      sim_cases[18] 2021-07-17 Combined     NA    FALSE          FALSE
#> 19:      sim_cases[19] 2021-07-24 Combined     NA    FALSE          FALSE
#> 20:   sim_voc_cases[1] 2021-04-17      VOC     NA     TRUE          FALSE
#> 21:   sim_voc_cases[2] 2021-04-24      VOC     NA     TRUE          FALSE
#> 22:   sim_voc_cases[3] 2021-05-01      VOC     NA     TRUE          FALSE
#> 23:   sim_voc_cases[4] 2021-05-08      VOC     NA     TRUE          FALSE
#> 24:   sim_voc_cases[5] 2021-05-15      VOC     NA     TRUE          FALSE
#> 25:   sim_voc_cases[6] 2021-05-22      VOC     NA     TRUE          FALSE
#> 26:   sim_voc_cases[7] 2021-05-29      VOC     NA     TRUE          FALSE
#> 27:   sim_voc_cases[8] 2021-06-05      VOC     NA     TRUE          FALSE
#> 28:   sim_voc_cases[9] 2021-06-12      VOC     NA     TRUE           TRUE
#> 29:  sim_voc_cases[10] 2021-06-19      VOC     NA    FALSE          FALSE
#> 30:  sim_voc_cases[11] 2021-06-26      VOC     NA    FALSE          FALSE
#> 31:  sim_voc_cases[12] 2021-07-03      VOC     NA    FALSE          FALSE
#> 32:  sim_voc_cases[13] 2021-07-10      VOC     NA    FALSE          FALSE
#> 33:  sim_voc_cases[14] 2021-07-17      VOC     NA    FALSE          FALSE
#> 34:  sim_voc_cases[15] 2021-07-24      VOC     NA    FALSE          FALSE
#> 35:  sim_nvoc_cases[1] 2021-03-20  non-VOC     NA     TRUE          FALSE
#> 36:  sim_nvoc_cases[2] 2021-03-27  non-VOC     NA     TRUE          FALSE
#> 37:  sim_nvoc_cases[3] 2021-04-03  non-VOC     NA     TRUE          FALSE
#> 38:  sim_nvoc_cases[4] 2021-04-10  non-VOC     NA     TRUE          FALSE
#> 39:  sim_nvoc_cases[5] 2021-04-17  non-VOC     NA     TRUE          FALSE
#> 40:  sim_nvoc_cases[6] 2021-04-24  non-VOC     NA     TRUE          FALSE
#> 41:  sim_nvoc_cases[7] 2021-05-01  non-VOC     NA     TRUE          FALSE
#> 42:  sim_nvoc_cases[8] 2021-05-08  non-VOC     NA     TRUE          FALSE
#> 43:  sim_nvoc_cases[9] 2021-05-15  non-VOC     NA     TRUE          FALSE
#> 44: sim_nvoc_cases[10] 2021-05-22  non-VOC     NA     TRUE          FALSE
#> 45: sim_nvoc_cases[11] 2021-05-29  non-VOC     NA     TRUE          FALSE
#> 46: sim_nvoc_cases[12] 2021-06-05  non-VOC     NA     TRUE          FALSE
#> 47: sim_nvoc_cases[13] 2021-06-12  non-VOC     NA     TRUE           TRUE
#> 48: sim_nvoc_cases[14] 2021-06-19  non-VOC     NA    FALSE          FALSE
#> 49: sim_nvoc_cases[15] 2021-06-26  non-VOC     NA    FALSE          FALSE
#> 50: sim_nvoc_cases[16] 2021-07-03  non-VOC     NA    FALSE          FALSE
#> 51: sim_nvoc_cases[17] 2021-07-10  non-VOC     NA    FALSE          FALSE
#> 52: sim_nvoc_cases[18] 2021-07-17  non-VOC     NA    FALSE          FALSE
#> 53: sim_nvoc_cases[19] 2021-07-24  non-VOC     NA    FALSE          FALSE
#>               variable       date     type    obs observed forecast_start
#>                 <char>     <IDat>   <char>  <num>   <lgcl>         <lgcl>
#>       mean median      sd     mad     q5    q20    q80    q95  rhat ess_bulk
#>      <num>  <num>   <num>   <num>  <num>  <num>  <num>  <num> <num>    <int>
#>  1:  89700  89100 13800.0 12200.0  68400  79200 100000 112000 1.000     1630
#>  2: 103000 102000 14900.0 13200.0  78900  91100 113000 127000 1.000     2030
#>  3: 114000 113000 17800.0 14700.0  87900 101000 126000 144000 1.000     1810
#>  4: 122000 121000 19200.0 16200.0  92500 107000 135000 154000 1.000     1820
#>  5: 133000 133000 20800.0 18000.0 101000 117000 149000 168000 1.000     1830
#>  6: 138000 137000 21200.0 18800.0 105000 122000 153000 172000 1.000     1860
#>  7: 129000 128000 20000.0 17400.0  97200 114000 143000 161000 1.000     1980
#>  8: 107000 106000 16700.0 13800.0  82300  94100 118000 133000 1.000     2230
#>  9:  79700  78900 12500.0 10700.0  61200  70400  88700 100000 0.999     2100
#> 10:  55400  54900  8090.0  7300.0  42600  49000  61500  69300 1.000     2090
#> 11:  35800  35500  5490.0  4620.0  27700  31800  39600  44700 1.000     2180
#> 12:  22900  22700  3420.0  2880.0  17800  20300  25200  28400 1.000     2050
#> 13:  14200  14100  2050.0  1700.0  11100  12700  15600  17500 1.000     1830
#> 14:   8280   8220  1170.0   971.0   6580   7400   9010  10200 1.000     1950
#> 15:   5130   5040   833.0   678.0   3990   4510   5670   6590 1.000     1960
#> 16:   3470   3320   990.0   830.0   2160   2710   4130   5280 1.000     1800
#> 17:   2620   2360  1310.0  1000.0   1130   1630   3380   5150 1.000     1850
#> 18:   2300   1790  1890.0  1100.0    529   1050   3090   5790 1.000     1970
#> 19:   2330   1450  2930.0  1190.0    259    673   3150   7380 1.000     1970
#> 20:    221    219    40.4    37.1    160    189    253    291 1.000     2000
#> 21:    417    411    82.6    72.6    301    353    475    559 0.999     1600
#> 22:    692    684   125.0   114.0    501    594    785    902 1.000     1780
#> 23:   1030   1010   203.0   177.0    735    871   1170   1380 1.000     2050
#> 24:   1380   1360   266.0   245.0    991   1160   1580   1840 0.999     2340
#> 25:   1710   1670   355.0   320.0   1200   1430   1970   2330 1.000     2230
#> 26:   1960   1910   437.0   372.0   1360   1630   2260   2690 1.000     2100
#> 27:   2150   2110   478.0   434.0   1480   1770   2510   3010 1.000     1770
#> 28:   2230   2180   508.0   442.0   1490   1840   2600   3120 1.000     1680
#> 29:   2100   2050   504.0   450.0   1380   1700   2450   2970 1.000     1790
#> 30:   1950   1880   505.0   429.0   1260   1560   2290   2800 1.000     1650
#> 31:   1810   1720   618.0   524.0    980   1320   2240   2970 1.000     1750
#> 32:   1730   1550   907.0   696.0    687   1030   2290   3400 1.000     1790
#> 33:   1780   1380  1500.0   872.0    395    789   2400   4580 1.000     2010
#> 34:   2010   1240  2520.0  1020.0    220    572   2740   6400 1.000     2000
#> 35:  89700  89100 13800.0 12200.0  68400  79200 100000 112000 1.000     1630
#> 36: 103000 102000 14900.0 13200.0  78900  91100 113000 127000 1.000     2030
#> 37: 114000 113000 17800.0 14700.0  87900 101000 126000 144000 1.000     1810
#> 38: 122000 121000 19200.0 16200.0  92500 107000 135000 154000 1.000     1820
#> 39: 133000 133000 20800.0 18100.0 101000 117000 148000 168000 1.000     1830
#> 40: 137000 137000 21200.0 18800.0 105000 121000 153000 171000 1.000     1860
#> 41: 128000 127000 20000.0 17400.0  96500 113000 142000 160000 1.000     1980
#> 42: 106000 105000 16600.0 13700.0  81300  93100 117000 132000 1.000     2230
#> 43:  78300  77500 12400.0 10700.0  60000  69100  87200  99000 0.999     2100
#> 44:  53600  53200  8020.0  7210.0  41000  47400  59800  67400 1.000     2090
#> 45:  33800  33500  5370.0  4500.0  25700  29800  37600  42500 1.000     2170
#> 46:  20700  20500  3320.0  2780.0  15800  18200  23000  26200 1.000     2080
#> 47:  12000  11800  1950.0  1650.0   9100  10500  13300  15100 1.000     1890
#> 48:   6180   6090  1060.0   941.0   4660   5350   6930   7930 1.000     1910
#> 49:   3180   3120   660.0   549.0   2260   2670   3630   4330 1.000     2010
#> 50:   1660   1590   537.0   440.0    938   1240   2000   2640 1.000     1920
#> 51:    895    790   499.0   355.0    352    531   1170   1790 1.000     1810
#> 52:    514    388   453.0   259.0    117    218    702   1310 1.000     1810
#> 53:    328    194   456.0   170.0     33     85    438   1010 1.000     1750
#>       mean median      sd     mad     q5    q20    q80    q95  rhat ess_bulk
#>      <num>  <num>   <num>   <num>  <num>  <num>  <num>  <num> <num>    <int>
#>     ess_tail
#>        <int>
#>  1:     1660
#>  2:     1750
#>  3:     1630
#>  4:     1580
#>  5:     1610
#>  6:     1520
#>  7:     1660
#>  8:     1980
#>  9:     1860
#> 10:     1810
#> 11:     1820
#> 12:     1820
#> 13:     1710
#> 14:     1720
#> 15:     1440
#> 16:     1540
#> 17:     1300
#> 18:     1460
#> 19:     1490
#> 20:     1760
#> 21:     1480
#> 22:     1660
#> 23:     1880
#> 24:     2000
#> 25:     1700
#> 26:     1820
#> 27:     1320
#> 28:     1190
#> 29:     1570
#> 30:     1270
#> 31:     1660
#> 32:     1500
#> 33:     1350
#> 34:     1480
#> 35:     1660
#> 36:     1750
#> 37:     1630
#> 38:     1580
#> 39:     1610
#> 40:     1520
#> 41:     1660
#> 42:     1970
#> 43:     1860
#> 44:     1820
#> 45:     1870
#> 46:     1790
#> 47:     1750
#> 48:     1670
#> 49:     1530
#> 50:     1290
#> 51:     1590
#> 52:     1700
#> 53:     1730
#>     ess_tail
#>        <int>

# summary of the case summary
summary(posterior, type = "cases", as_dt = TRUE)
#>       variable       date                   type         obs        
#>  Length   :53   Min.   :2021-03-20   Length   :53   Min.   :  5033  
#>  N.unique :53   1st Qu.:2021-04-24   N.unique : 3   1st Qu.: 27842  
#>  N.blank  : 0   Median :2021-05-29   N.blank  : 0   Median : 87328  
#>  Min.nchar:12   Mean   :2021-05-25   Min.nchar: 3   Mean   : 77848  
#>  Max.nchar:18   3rd Qu.:2021-06-26   Max.nchar: 8   3rd Qu.:113704  
#>                 Max.   :2021-07-24                  Max.   :145568  
#>                                                     NAs    :38      
#>   observed       forecast_start       mean            median      
#>  Mode :logical   Mode :logical   Min.   :   221   Min.   :   194  
#>  FALSE:16        FALSE:50        1st Qu.:  1950   1st Qu.:  1670  
#>  TRUE :37        TRUE :3         Median :  8280   Median :  8220  
#>                                  Mean   : 44054   Mean   : 43670  
#>                                  3rd Qu.:103000   3rd Qu.:102000  
#>                                  Max.   :138000   Max.   :137000  
#>                                                                   
#>        sd               mad                q5              q20        
#>  Min.   :   40.4   Min.   :   37.1   Min.   :    33   Min.   :    85  
#>  1st Qu.:  508.0   1st Qu.:  442.0   1st Qu.:   980   1st Qu.:  1240  
#>  Median : 2050.0   Median : 1190.0   Median :  6580   Median :  7400  
#>  Mean   : 7040.5   Mean   : 5987.3   Mean   : 33450   Mean   : 38715  
#>  3rd Qu.:14900.0   3rd Qu.:13200.0   3rd Qu.: 78900   3rd Qu.: 91100  
#>  Max.   :21200.0   Max.   :18800.0   Max.   :105000   Max.   :122000  
#>                                                                       
#>       q80              q95              rhat           ess_bulk   
#>  Min.   :   253   Min.   :   291   Min.   :0.9990   Min.   :1600  
#>  1st Qu.:  2290   1st Qu.:  2970   1st Qu.:1.0000   1st Qu.:1810  
#>  Median :  9010   Median : 10200   Median :1.0000   Median :1920  
#>  Mean   : 48940   Mean   : 55618   Mean   :0.9999   Mean   :1928  
#>  3rd Qu.:113000   3rd Qu.:127000   3rd Qu.:1.0000   3rd Qu.:2050  
#>  Max.   :153000   Max.   :172000   Max.   :1.0000   Max.   :2340  
#>                                                                   
#>     ess_tail   
#>  Min.   :1190  
#>  1st Qu.:1520  
#>  Median :1660  
#>  Mean   :1641  
#>  3rd Qu.:1760  
#>  Max.   :2000  
#>                

# case forecast only
summary(posterior, type = "cases", forecast = TRUE)
#>         type       date horizon forecast_start  mean median    sd   mad    q5
#>       <char>     <IDat>   <int>         <lgcl> <num>  <num> <num> <num> <num>
#>  1: Combined 2021-07-03       1          FALSE  3470   3320   990   830  2160
#>  2: Combined 2021-07-10       2          FALSE  2620   2360  1310  1000  1130
#>  3: Combined 2021-07-17       3          FALSE  2300   1790  1890  1100   529
#>  4: Combined 2021-07-24       4          FALSE  2330   1450  2930  1190   259
#>  5:      VOC 2021-06-19       1          FALSE  2100   2050   504   450  1380
#>  6:      VOC 2021-06-26       2          FALSE  1950   1880   505   429  1260
#>  7:      VOC 2021-07-03       3          FALSE  1810   1720   618   524   980
#>  8:      VOC 2021-07-10       4          FALSE  1730   1550   907   696   687
#>  9:      VOC 2021-07-17       5          FALSE  1780   1380  1500   872   395
#> 10:      VOC 2021-07-24       6          FALSE  2010   1240  2520  1020   220
#> 11:  non-VOC 2021-06-19       1          FALSE  6180   6090  1060   941  4660
#> 12:  non-VOC 2021-06-26       2          FALSE  3180   3120   660   549  2260
#> 13:  non-VOC 2021-07-03       3          FALSE  1660   1590   537   440   938
#> 14:  non-VOC 2021-07-10       4          FALSE   895    790   499   355   352
#> 15:  non-VOC 2021-07-17       5          FALSE   514    388   453   259   117
#> 16:  non-VOC 2021-07-24       6          FALSE   328    194   456   170    33
#>       q20   q80   q95
#>     <num> <num> <num>
#>  1:  2710  4130  5280
#>  2:  1630  3380  5150
#>  3:  1050  3090  5790
#>  4:   673  3150  7380
#>  5:  1700  2450  2970
#>  6:  1560  2290  2800
#>  7:  1320  2240  2970
#>  8:  1030  2290  3400
#>  9:   789  2400  4580
#> 10:   572  2740  6400
#> 11:  5350  6930  7930
#> 12:  2670  3630  4330
#> 13:  1240  2000  2640
#> 14:   531  1170  1790
#> 15:   218   702  1310
#> 16:    85   438  1010

# voc fraction summary
summary(posterior, type = "voc_frac")
#>         variable       date   type         obs observed forecast_start    mean
#>           <char>     <IDat> <char>       <num>   <lgcl>         <lgcl>   <num>
#>  1:  frac_voc[1] 2021-04-17    VOC 0.001465917     TRUE          FALSE 0.00168
#>  2:  frac_voc[2] 2021-04-24    VOC 0.007085917     TRUE          FALSE 0.00302
#>  3:  frac_voc[3] 2021-05-01    VOC 0.015085025     TRUE          FALSE 0.00542
#>  4:  frac_voc[4] 2021-05-08    VOC 0.019443217     TRUE          FALSE 0.00971
#>  5:  frac_voc[5] 2021-05-15    VOC 0.026459592     TRUE          FALSE 0.01740
#>  6:  frac_voc[6] 2021-05-22    VOC 0.032900194     TRUE          FALSE 0.03090
#>  7:  frac_voc[7] 2021-05-29    VOC 0.038084020     TRUE          FALSE 0.05450
#>  8:  frac_voc[8] 2021-06-05    VOC 0.085600907     TRUE          FALSE 0.09430
#>  9:  frac_voc[9] 2021-06-12    VOC 0.166666667     TRUE           TRUE 0.15800
#> 10: frac_voc[10] 2021-06-19    VOC          NA    FALSE          FALSE 0.25300
#> 11: frac_voc[11] 2021-06-26    VOC          NA    FALSE          FALSE 0.37800
#> 12: frac_voc[12] 2021-07-03    VOC          NA    FALSE          FALSE 0.52000
#> 13: frac_voc[13] 2021-07-10    VOC          NA    FALSE          FALSE 0.65800
#> 14: frac_voc[14] 2021-07-17    VOC          NA    FALSE          FALSE 0.77400
#> 15: frac_voc[15] 2021-07-24    VOC          NA    FALSE          FALSE 0.85800
#>      median       sd      mad      q5     q20     q80     q95  rhat ess_bulk
#>       <num>    <num>    <num>   <num>   <num>   <num>   <num> <num>    <int>
#>  1: 0.00166 0.000226 0.000203 0.00136 0.00150 0.00185 0.00207 1.000     1410
#>  2: 0.00298 0.000352 0.000324 0.00250 0.00273 0.00327 0.00362 1.000     1560
#>  3: 0.00538 0.000572 0.000530 0.00457 0.00495 0.00585 0.00637 1.000     1720
#>  4: 0.00965 0.001000 0.000944 0.00821 0.00891 0.01050 0.01140 1.000     1860
#>  5: 0.01730 0.001900 0.001750 0.01460 0.01590 0.01880 0.02050 1.000     1970
#>  6: 0.03070 0.003790 0.003340 0.02530 0.02800 0.03370 0.03740 1.000     1990
#>  7: 0.05420 0.007570 0.006670 0.04350 0.04840 0.05990 0.06750 1.000     2020
#>  8: 0.09360 0.014600 0.012700 0.07300 0.08280 0.10500 0.11900 0.999     1900
#>  9: 0.15700 0.026000 0.022700 0.12000 0.13800 0.17700 0.20300 1.000     1800
#> 10: 0.25000 0.041700 0.036300 0.19100 0.21900 0.28300 0.32500 1.000     1710
#> 11: 0.37600 0.057800 0.051400 0.29000 0.33000 0.42100 0.47500 1.000     1640
#> 12: 0.52000 0.067800 0.060800 0.41200 0.46500 0.57300 0.63100 1.000     1570
#> 13: 0.66200 0.067500 0.060800 0.54800 0.60500 0.71200 0.76300 1.000     1530
#> 14: 0.77800 0.058100 0.051600 0.67600 0.72900 0.82000 0.86000 1.000     1500
#> 15: 0.86300 0.044800 0.038400 0.78100 0.82500 0.89300 0.92100 1.000     1480
#>     ess_tail
#>        <int>
#>  1:     1140
#>  2:     1190
#>  3:     1340
#>  4:     1640
#>  5:     1540
#>  6:     1400
#>  7:     1340
#>  8:     1380
#>  9:     1290
#> 10:     1240
#> 11:     1180
#> 12:     1190
#> 13:     1210
#> 14:     1220
#> 15:     1240

# voc advantage summary
summary(posterior, type = "voc_advantage")
#>              variable       date   type observed forecast_start     mean
#>                <char>     <IDat> <char>   <lgcl>         <lgcl>    <num>
#>  1:  voc_advantage[1] 2021-04-17    VOC     TRUE          FALSE 1.588493
#>  2:  voc_advantage[2] 2021-04-24    VOC     TRUE          FALSE 1.588493
#>  3:  voc_advantage[3] 2021-05-01    VOC     TRUE          FALSE 1.588493
#>  4:  voc_advantage[4] 2021-05-08    VOC     TRUE          FALSE 1.588493
#>  5:  voc_advantage[5] 2021-05-15    VOC     TRUE          FALSE 1.588493
#>  6:  voc_advantage[6] 2021-05-22    VOC     TRUE          FALSE 1.588493
#>  7:  voc_advantage[7] 2021-05-29    VOC     TRUE          FALSE 1.588493
#>  8:  voc_advantage[8] 2021-06-05    VOC     TRUE           TRUE 1.588493
#>  9:  voc_advantage[9] 2021-06-12    VOC    FALSE          FALSE 1.588493
#> 10: voc_advantage[10] 2021-06-19    VOC    FALSE          FALSE 1.588493
#> 11: voc_advantage[11] 2021-06-26    VOC    FALSE          FALSE 1.588493
#> 12: voc_advantage[12] 2021-07-03    VOC    FALSE          FALSE 1.588493
#> 13: voc_advantage[13] 2021-07-10    VOC    FALSE          FALSE 1.588493
#> 14: voc_advantage[14] 2021-07-17    VOC    FALSE          FALSE 1.588493
#>       median     sd    mad     q5      q20      q80     q95  rhat ess_bulk
#>        <num>  <num>  <num>  <num>    <num>    <num>   <num> <num>    <int>
#>  1: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>  2: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>  3: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>  4: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>  5: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>  6: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>  7: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>  8: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>  9: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#> 10: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#> 11: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#> 12: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#> 13: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#> 14: 1.588493 0.0308 0.0297 1.5297 1.557595 1.618732 1.65214     1     1270
#>     ess_tail
#>        <int>
#>  1:     1250
#>  2:     1250
#>  3:     1250
#>  4:     1250
#>  5:     1250
#>  6:     1250
#>  7:     1250
#>  8:     1250
#>  9:     1250
#> 10:     1250
#> 11:     1250
#> 12:     1250
#> 13:     1250
#> 14:     1250

# growth summary
summary(posterior, type = "growth")
#>      variable       date     type observed forecast_start        mean
#>        <char>     <IDat>   <char>   <lgcl>         <lgcl>       <num>
#>  1:  com_r[1] 2021-03-20 Combined     TRUE          FALSE  0.10450000
#>  2:  com_r[2] 2021-03-27 Combined     TRUE          FALSE  0.07935714
#>  3:  com_r[3] 2021-04-03 Combined     TRUE          FALSE  0.05256429
#>  4:  com_r[4] 2021-04-10 Combined     TRUE          FALSE  0.07016429
#>  5:  com_r[5] 2021-04-17 Combined     TRUE          FALSE  0.02867857
#>  6:  com_r[6] 2021-04-24 Combined     TRUE          FALSE -0.05617857
#>  7:  com_r[7] 2021-05-01 Combined     TRUE          FALSE -0.14614286
#>  8:  com_r[8] 2021-05-08 Combined     TRUE          FALSE -0.22628571
#>  9:  com_r[9] 2021-05-15 Combined     TRUE          FALSE -0.28128571
#> 10: com_r[10] 2021-05-22 Combined     TRUE          FALSE -0.33550000
#> 11: com_r[11] 2021-05-29 Combined     TRUE          FALSE -0.34257143
#> 12: com_r[12] 2021-06-05 Combined     TRUE          FALSE -0.35907143
#> 13: com_r[13] 2021-06-12 Combined     TRUE          FALSE -0.39757143
#> 14: com_r[14] 2021-06-19 Combined     TRUE           TRUE -0.34885714
#> 15: com_r[15] 2021-06-26 Combined    FALSE          FALSE -0.29700000
#> 16: com_r[16] 2021-07-03 Combined    FALSE          FALSE -0.24121429
#> 17: com_r[17] 2021-07-10 Combined    FALSE          FALSE -0.19014286
#> 18: com_r[18] 2021-07-17 Combined    FALSE          FALSE -0.15635714
#> 19:  voc_r[1] 2021-04-17      VOC     TRUE          FALSE  0.49028571
#> 20:  voc_r[2] 2021-04-24      VOC     TRUE          FALSE  0.40385714
#> 21:  voc_r[3] 2021-05-01      VOC     TRUE          FALSE  0.31192857
#> 22:  voc_r[4] 2021-05-08      VOC     TRUE          FALSE  0.22864286
#> 23:  voc_r[5] 2021-05-15      VOC     TRUE          FALSE  0.16735714
#> 24:  voc_r[6] 2021-05-22      VOC     TRUE          FALSE  0.10214286
#> 25:  voc_r[7] 2021-05-29      VOC     TRUE          FALSE  0.07613571
#> 26:  voc_r[8] 2021-06-05      VOC     TRUE           TRUE  0.02962143
#> 27:  voc_r[9] 2021-06-12      VOC    FALSE          FALSE -0.05272143
#> 28: voc_r[10] 2021-06-19      VOC    FALSE          FALSE -0.06222857
#> 29: voc_r[11] 2021-06-26      VOC    FALSE          FALSE -0.07613571
#> 30: voc_r[12] 2021-07-03      VOC    FALSE          FALSE -0.08485714
#> 31: voc_r[13] 2021-07-10      VOC    FALSE          FALSE -0.08721429
#> 32: voc_r[14] 2021-07-17      VOC    FALSE          FALSE -0.09192857
#> 33:      r[1] 2021-03-20  non-VOC     TRUE          FALSE  0.10450000
#> 34:      r[2] 2021-03-27  non-VOC     TRUE          FALSE  0.07935714
#> 35:      r[3] 2021-04-03  non-VOC     TRUE          FALSE  0.05256429
#> 36:      r[4] 2021-04-10  non-VOC     TRUE          FALSE  0.07016429
#> 37:      r[5] 2021-04-17  non-VOC     TRUE          FALSE  0.02726429
#> 38:      r[6] 2021-04-24  non-VOC     TRUE          FALSE -0.05869286
#> 39:      r[7] 2021-05-01  non-VOC     TRUE          FALSE -0.15085714
#> 40:      r[8] 2021-05-08  non-VOC     TRUE          FALSE -0.23414286
#> 41:      r[9] 2021-05-15  non-VOC     TRUE          FALSE -0.29542857
#> 42:     r[10] 2021-05-22  non-VOC     TRUE          FALSE -0.36064286
#> 43:     r[11] 2021-05-29  non-VOC     TRUE          FALSE -0.38657143
#> 44:     r[12] 2021-06-05  non-VOC     TRUE           TRUE -0.43292857
#> 45:     r[13] 2021-06-12  non-VOC    FALSE          FALSE -0.51542857
#> 46:     r[14] 2021-06-19  non-VOC    FALSE          FALSE -0.52485714
#> 47:     r[15] 2021-06-26  non-VOC    FALSE          FALSE -0.53900000
#> 48:     r[16] 2021-07-03  non-VOC    FALSE          FALSE -0.54764286
#> 49:     r[17] 2021-07-10  non-VOC    FALSE          FALSE -0.54921429
#> 50:     r[18] 2021-07-17  non-VOC    FALSE          FALSE -0.55471429
#>      variable       date     type observed forecast_start        mean
#>        <char>     <IDat>   <char>   <lgcl>         <lgcl>       <num>
#>          median     sd    mad           q5         q20          q80         q95
#>           <num>  <num>  <num>        <num>       <num>        <num>       <num>
#>  1:  0.10607143 0.0762 0.0721  0.006961429  0.05790714  0.151642857  0.20035714
#>  2:  0.08014286 0.0637 0.0569 -0.001367143  0.04250714  0.117071429  0.15635714
#>  3:  0.05704286 0.0713 0.0600 -0.045728571  0.01304286  0.094285714  0.13514286
#>  4:  0.06733571 0.0763 0.0627 -0.027421429  0.02757857  0.113142857  0.17285714
#>  5:  0.02647857 0.0731 0.0633 -0.057357143 -0.01422143  0.071264286  0.12021429
#>  6: -0.05672857 0.0670 0.0583 -0.141428571 -0.09428571 -0.018228571  0.02915000
#>  7: -0.14535714 0.0687 0.0598 -0.236500000 -0.18621429 -0.106857143 -0.05704286
#>  8: -0.22471429 0.0696 0.0595 -0.316642857 -0.26478571 -0.185428571 -0.13671429
#>  9: -0.28128571 0.0717 0.0586 -0.371642857 -0.32214286 -0.241214286 -0.18778571
#> 10: -0.33392857 0.0690 0.0583 -0.427428571 -0.37478571 -0.295428571 -0.25378571
#> 11: -0.34571429 0.0698 0.0592 -0.428214286 -0.38264286 -0.305642857 -0.24750000
#> 12: -0.36300000 0.0703 0.0615 -0.443928571 -0.40071429 -0.318214286 -0.26557143
#> 13: -0.39442857 0.0803 0.0715 -0.507571429 -0.44550000 -0.348071429 -0.30328571
#> 14: -0.34885714 0.1030 0.0966 -0.476928571 -0.41250000 -0.286785714 -0.21921429
#> 15: -0.29542857 0.1710 0.1600 -0.517785714 -0.40150000 -0.190142857 -0.07542857
#> 16: -0.24200000 0.2350 0.2200 -0.540571429 -0.38028571 -0.091928571  0.04635714
#> 17: -0.18935714 0.2840 0.2660 -0.553142857 -0.36378571 -0.004392143  0.16971429
#> 18: -0.15085714 0.3380 0.3070 -0.591642857 -0.35907143  0.046985714  0.26635714
#> 19:  0.48792857 0.0750 0.0665  0.400714286  0.44785714  0.534285714  0.58850000
#> 20:  0.40385714 0.0687 0.0609  0.315071429  0.36378571  0.446285714  0.48871429
#> 21:  0.31192857 0.0725 0.0663  0.215285714  0.26792857  0.357500000  0.40150000
#> 22:  0.23021429 0.0735 0.0678  0.130428571  0.18307143  0.274214286  0.31900000
#> 23:  0.16814286 0.0784 0.0668  0.067964286  0.12021429  0.210571429  0.26950000
#> 24:  0.10371429 0.0733 0.0658  0.005366429  0.05853571  0.146928571  0.18935714
#> 25:  0.07409286 0.0739 0.0640 -0.012650000  0.03182143  0.118642857  0.17600000
#> 26:  0.02757857 0.0724 0.0638 -0.058300000 -0.01272857  0.070242857  0.12571429
#> 27: -0.04957857 0.0802 0.0729 -0.162642857 -0.09900000 -0.003370714  0.04447143
#> 28: -0.06317143 0.1020 0.0955 -0.189357143 -0.12492857 -0.000181500  0.06859286
#> 29: -0.07637143 0.1700 0.1600 -0.293857143 -0.18071429  0.031664286  0.14535714
#> 30: -0.08485714 0.2340 0.2190 -0.388142857 -0.22314286  0.065057143  0.20664286
#> 31: -0.08642857 0.2840 0.2680 -0.454928571 -0.26085714  0.095071429  0.27185714
#> 32: -0.08485714 0.3380 0.3110 -0.528785714 -0.29542857  0.117071429  0.33707143
#> 33:  0.10607143 0.0762 0.0721  0.006961429  0.05790714  0.151642857  0.20035714
#> 34:  0.08014286 0.0637 0.0569 -0.001367143  0.04250714  0.117071429  0.15635714
#> 35:  0.05704286 0.0713 0.0600 -0.045728571  0.01304286  0.094285714  0.13514286
#> 36:  0.06733571 0.0763 0.0627 -0.027421429  0.02757857  0.113142857  0.17285714
#> 37:  0.02498571 0.0731 0.0633 -0.058614286 -0.01563571  0.069850000  0.11864286
#> 38: -0.05908571 0.0670 0.0582 -0.143785714 -0.09664286 -0.020742857  0.02647857
#> 39: -0.15007143 0.0687 0.0597 -0.241214286 -0.19092857 -0.110785714 -0.06160000
#> 40: -0.23335714 0.0697 0.0593 -0.324500000 -0.27264286 -0.193285714 -0.14378571
#> 41: -0.29542857 0.0717 0.0589 -0.385785714 -0.33707143 -0.254571429 -0.20271429
#> 42: -0.35907143 0.0691 0.0583 -0.451000000 -0.39992857 -0.321357143 -0.27735714
#> 43: -0.38971429 0.0703 0.0603 -0.470642857 -0.42664286 -0.347285714 -0.29071429
#> 44: -0.43607143 0.0723 0.0643 -0.520928571 -0.47692857 -0.390500000 -0.33785714
#> 45: -0.51307143 0.0831 0.0758 -0.627785714 -0.56571429 -0.463571429 -0.41564286
#> 46: -0.52642857 0.1070 0.1010 -0.659214286 -0.59007143 -0.460428571 -0.38185714
#> 47: -0.53900000 0.1730 0.1650 -0.762142857 -0.64585714 -0.430571429 -0.31821429
#> 48: -0.54607143 0.2370 0.2230 -0.856428571 -0.68907143 -0.394428571 -0.25378571
#> 49: -0.55157143 0.2860 0.2730 -0.919285714 -0.72521429 -0.366928571 -0.18857143
#> 50: -0.55000000 0.3400 0.3150 -1.005714286 -0.75978571 -0.344142857 -0.13042857
#>          median     sd    mad           q5         q20          q80         q95
#>           <num>  <num>  <num>        <num>       <num>        <num>       <num>
#>      rhat ess_bulk ess_tail
#>     <num>    <int>    <int>
#>  1: 1.000      950      923
#>  2: 1.000     1530     1550
#>  3: 1.000     1190     1520
#>  4: 1.000     1050     1330
#>  5: 1.000     1540     1530
#>  6: 1.000     1920     1760
#>  7: 1.000     1920     1290
#>  8: 1.000     1640     1330
#>  9: 1.000     1930     1400
#> 10: 1.000     1910     1760
#> 11: 1.000     1760     1560
#> 12: 0.999     1460     1660
#> 13: 1.000     1690     1660
#> 14: 1.000     2140     1790
#> 15: 1.000     1780     1360
#> 16: 1.000     1840     1340
#> 17: 1.000     1930     1400
#> 18: 1.000     1830     1470
#> 19: 1.000     1700     1700
#> 20: 1.000     1880     1820
#> 21: 1.000     1850     1670
#> 22: 1.000     1860     1460
#> 23: 1.000     1660     1480
#> 24: 1.000     1970     1860
#> 25: 1.000     1560     1530
#> 26: 1.000     1420     1660
#> 27: 1.000     1740     1660
#> 28: 1.000     2140     1700
#> 29: 1.000     1790     1460
#> 30: 1.000     1840     1290
#> 31: 1.000     1900     1320
#> 32: 1.000     1800     1540
#> 33: 1.000      950      923
#> 34: 1.000     1530     1550
#> 35: 1.000     1190     1520
#> 36: 1.000     1050     1330
#> 37: 1.000     1540     1590
#> 38: 1.000     1920     1760
#> 39: 1.000     1910     1290
#> 40: 1.000     1640     1350
#> 41: 1.000     1940     1390
#> 42: 1.000     1880     1620
#> 43: 1.000     1830     1540
#> 44: 1.000     1540     1670
#> 45: 1.000     1530     1530
#> 46: 1.000     2110     1640
#> 47: 1.000     1780     1400
#> 48: 1.000     1790     1430
#> 49: 1.000     1850     1390
#> 50: 1.000     1740     1620
#>      rhat ess_bulk ess_tail
#>     <num>    <int>    <int>

# Rt summary
summary(posterior, type = "rt")
#>      variable       date     type observed forecast_start      mean    median
#>        <char>     <IDat>   <char>   <lgcl>         <lgcl>     <num>     <num>
#>  1:  com_r[1] 2021-03-20 Combined     TRUE          FALSE 1.1101554 1.1119013
#>  2:  com_r[2] 2021-03-27 Combined     TRUE          FALSE 1.0825909 1.0834418
#>  3:  com_r[3] 2021-04-03 Combined     TRUE          FALSE 1.0539703 1.0587012
#>  4:  com_r[4] 2021-04-10 Combined     TRUE          FALSE 1.0726844 1.0696545
#>  5:  com_r[5] 2021-04-17 Combined     TRUE          FALSE 1.0290938 1.0268322
#>  6:  com_r[6] 2021-04-24 Combined     TRUE          FALSE 0.9453703 0.9448505
#>  7:  com_r[7] 2021-05-01 Combined     TRUE          FALSE 0.8640343 0.8647134
#>  8:  com_r[8] 2021-05-08 Combined     TRUE          FALSE 0.7974902 0.7987444
#>  9:  com_r[9] 2021-05-15 Combined     TRUE          FALSE 0.7548126 0.7548126
#> 10: com_r[10] 2021-05-22 Combined     TRUE          FALSE 0.7149805 0.7161049
#> 11: com_r[11] 2021-05-29 Combined     TRUE          FALSE 0.7099424 0.7077147
#> 12: com_r[12] 2021-06-05 Combined     TRUE          FALSE 0.6983245 0.6955864
#> 13: com_r[13] 2021-06-12 Combined     TRUE          FALSE 0.6719499 0.6740651
#> 14: com_r[14] 2021-06-19 Combined     TRUE           TRUE 0.7054939 0.7054939
#> 15: com_r[15] 2021-06-26 Combined    FALSE          FALSE 0.7430440 0.7442126
#> 16: com_r[16] 2021-07-03 Combined    FALSE          FALSE 0.7856732 0.7850562
#> 17: com_r[17] 2021-07-10 Combined    FALSE          FALSE 0.8268410 0.8274909
#> 18: com_r[18] 2021-07-17 Combined    FALSE          FALSE 0.8552537 0.8599705
#> 19:  voc_r[1] 2021-04-17      VOC     TRUE          FALSE 1.6327827 1.6289385
#> 20:  voc_r[2] 2021-04-24      VOC     TRUE          FALSE 1.4975900 1.4975900
#> 21:  voc_r[3] 2021-05-01      VOC     TRUE          FALSE 1.3660571 1.3660571
#> 22:  voc_r[4] 2021-05-08      VOC     TRUE          FALSE 1.2568931 1.2588697
#> 23:  voc_r[5] 2021-05-15      VOC     TRUE          FALSE 1.1821764 1.1831056
#> 24:  voc_r[6] 2021-05-22      VOC     TRUE          FALSE 1.1075417 1.1092835
#> 25:  voc_r[7] 2021-05-29      VOC     TRUE          FALSE 1.0791090 1.0769068
#> 26:  voc_r[8] 2021-06-05      VOC     TRUE           TRUE 1.0300645 1.0279624
#> 27:  voc_r[9] 2021-06-12      VOC    FALSE          FALSE 0.9486442 0.9516304
#> 28: voc_r[10] 2021-06-19      VOC    FALSE          FALSE 0.9396681 0.9387825
#> 29: voc_r[11] 2021-06-26      VOC    FALSE          FALSE 0.9266904 0.9264720
#> 30: voc_r[12] 2021-07-03      VOC    FALSE          FALSE 0.9186435 0.9186435
#> 31: voc_r[13] 2021-07-10      VOC    FALSE          FALSE 0.9164807 0.9172011
#> 32: voc_r[14] 2021-07-17      VOC    FALSE          FALSE 0.9121703 0.9186435
#> 33:      r[1] 2021-03-20  non-VOC     TRUE          FALSE 1.1101554 1.1119013
#> 34:      r[2] 2021-03-27  non-VOC     TRUE          FALSE 1.0825909 1.0834418
#> 35:      r[3] 2021-04-03  non-VOC     TRUE          FALSE 1.0539703 1.0587012
#> 36:      r[4] 2021-04-10  non-VOC     TRUE          FALSE 1.0726844 1.0696545
#> 37:      r[5] 2021-04-17  non-VOC     TRUE          FALSE 1.0276394 1.0253005
#> 38:      r[6] 2021-04-24  non-VOC     TRUE          FALSE 0.9429964 0.9426260
#> 39:      r[7] 2021-05-01  non-VOC     TRUE          FALSE 0.8599705 0.8606465
#> 40:      r[8] 2021-05-08  non-VOC     TRUE          FALSE 0.7912488 0.7918707
#> 41:      r[9] 2021-05-15  non-VOC     TRUE          FALSE 0.7442126 0.7442126
#> 42:     r[10] 2021-05-22  non-VOC     TRUE          FALSE 0.6972280 0.6983245
#> 43:     r[11] 2021-05-29  non-VOC     TRUE          FALSE 0.6793822 0.6772503
#> 44:     r[12] 2021-06-05  non-VOC     TRUE           TRUE 0.6486068 0.6465715
#> 45:     r[13] 2021-06-12  non-VOC    FALSE          FALSE 0.5972446 0.5986540
#> 46:     r[14] 2021-06-19  non-VOC    FALSE          FALSE 0.5916399 0.5907109
#> 47:     r[15] 2021-06-26  non-VOC    FALSE          FALSE 0.5833313 0.5833313
#> 48:     r[16] 2021-07-03  non-VOC    FALSE          FALSE 0.5783114 0.5792209
#> 49:     r[17] 2021-07-10  non-VOC    FALSE          FALSE 0.5774033 0.5760439
#> 50:     r[18] 2021-07-17  non-VOC    FALSE          FALSE 0.5742363 0.5769498
#>      variable       date     type observed forecast_start      mean    median
#>        <char>     <IDat>   <char>   <lgcl>         <lgcl>     <num>     <num>
#>         sd    mad        q5       q20       q80       q95  rhat ess_bulk
#>      <num>  <num>     <num>     <num>     <num>     <num> <num>    <int>
#>  1: 0.0762 0.0721 1.0069857 1.0596166 1.1637445 1.2218391 1.000      950
#>  2: 0.0637 0.0569 0.9986338 1.0434235 1.1241997 1.1692437 1.000     1530
#>  3: 0.0713 0.0600 0.9553012 1.0131283 1.0988737 1.1447003 1.000     1190
#>  4: 0.0763 0.0627 0.9729511 1.0279624 1.1197919 1.1886963 1.000     1050
#>  5: 0.0731 0.0633 0.9442568 0.9858792 1.0738650 1.1277385 1.000     1540
#>  6: 0.0670 0.0583 0.8681172 0.9100227 0.9819366 1.0295790 1.000     1920
#>  7: 0.0687 0.0598 0.7893859 0.8300957 0.8986540 0.9445536 1.000     1920
#>  8: 0.0696 0.0595 0.7285909 0.7673704 0.8307482 0.8722194 1.000     1640
#>  9: 0.0717 0.0586 0.6896005 0.7245947 0.7856732 0.8287923 1.000     1930
#> 10: 0.0690 0.0583 0.6521840 0.6874366 0.7442126 0.7758580 1.000     1910
#> 11: 0.0698 0.0592 0.6516718 0.6820564 0.7366497 0.7807502 1.000     1760
#> 12: 0.0703 0.0615 0.6415112 0.6698414 0.7274469 0.7667677 0.999     1460
#> 13: 0.0803 0.0715 0.6019557 0.6405039 0.7060484 0.7383881 1.000     1690
#> 14: 0.1030 0.0966 0.6206869 0.6619932 0.7506726 0.8031496 1.000     2140
#> 15: 0.1710 0.1600 0.5958384 0.6693153 0.8268410 0.9273460 1.000     1780
#> 16: 0.2350 0.2200 0.5824153 0.6836660 0.9121703 1.0474484 1.000     1840
#> 17: 0.2840 0.2660 0.5751394 0.6950401 0.9956175 1.1849662 1.000     1930
#> 18: 0.3380 0.3070 0.5534174 0.6983245 1.0481070 1.3052011 1.000     1830
#> 19: 0.0750 0.0665 1.4928907 1.5649551 1.7062291 1.8012845 1.000     1700
#> 20: 0.0687 0.0609 1.3703572 1.4387659 1.5624978 1.6302189 1.000     1880
#> 21: 0.0725 0.0663 1.2402162 1.3072538 1.4297506 1.4940641 1.000     1850
#> 22: 0.0735 0.0678 1.1393166 1.2009002 1.3154967 1.3757513 1.000     1860
#> 23: 0.0784 0.0668 1.0703271 1.1277385 1.2343832 1.3093096 1.000     1660
#> 24: 0.0733 0.0658 1.0053809 1.0602829 1.1582712 1.2084725 1.000     1970
#> 25: 0.0739 0.0640 0.9874297 1.0323331 1.1259677 1.1924381 1.000     1560
#> 26: 0.0724 0.0638 0.9433669 0.9873521 1.0727687 1.1339581 1.000     1420
#> 27: 0.0802 0.0729 0.8498947 0.9057427 0.9966350 1.0454751 1.000     1740
#> 28: 0.1020 0.0955 0.8274909 0.8825599 0.9998185 1.0710001 1.000     2140
#> 29: 0.1700 0.1600 0.7453830 0.8346738 1.0321709 1.1564525 1.000     1790
#> 30: 0.2340 0.2190 0.6783154 0.8000006 1.0672200 1.2295434 1.000     1840
#> 31: 0.2840 0.2680 0.6344933 0.7703910 1.0997374 1.3123995 1.000     1900
#> 32: 0.3380 0.3110 0.5893201 0.7442126 1.1241997 1.4008391 1.000     1800
#> 33: 0.0762 0.0721 1.0069857 1.0596166 1.1637445 1.2218391 1.000      950
#> 34: 0.0637 0.0569 0.9986338 1.0434235 1.1241997 1.1692437 1.000     1530
#> 35: 0.0713 0.0600 0.9553012 1.0131283 1.0988737 1.1447003 1.000     1190
#> 36: 0.0763 0.0627 0.9729511 1.0279624 1.1197919 1.1886963 1.000     1050
#> 37: 0.0731 0.0633 0.9430705 0.9844859 1.0723473 1.1259677 1.000     1540
#> 38: 0.0670 0.0582 0.8660733 0.9078802 0.9794708 1.0268322 1.000     1920
#> 39: 0.0687 0.0597 0.7856732 0.8261916 0.8951305 0.9402589 1.000     1910
#> 40: 0.0697 0.0593 0.7228887 0.7613647 0.8242464 0.8660733 1.000     1640
#> 41: 0.0717 0.0589 0.6799162 0.7138578 0.7752487 0.8165115 1.000     1940
#> 42: 0.0691 0.0583 0.6369908 0.6703679 0.7251642 0.7577838 1.000     1880
#> 43: 0.0703 0.0603 0.6246006 0.6526966 0.7066034 0.7477293 1.000     1830
#> 44: 0.0723 0.0643 0.5939687 0.6206869 0.6767184 0.7132972 1.000     1540
#> 45: 0.0831 0.0758 0.5337724 0.5679543 0.6290331 0.6599159 1.000     1530
#> 46: 0.1070 0.1010 0.5172576 0.5542877 0.6310132 0.6825926 1.000     2110
#> 47: 0.1730 0.1650 0.4666654 0.5242130 0.6501375 0.7274469 1.000     1780
#> 48: 0.2370 0.2230 0.4246761 0.5020420 0.6740651 0.7758580 1.000     1790
#> 49: 0.2860 0.2730 0.3988038 0.4842208 0.6928591 0.8281413 1.000     1850
#> 50: 0.3400 0.3150 0.3657833 0.4677667 0.7088277 0.8777192 1.000     1740
#>         sd    mad        q5       q20       q80       q95  rhat ess_bulk
#>      <num>  <num>     <num>     <num>     <num>     <num> <num>    <int>
#>     ess_tail
#>        <int>
#>  1:      923
#>  2:     1550
#>  3:     1520
#>  4:     1330
#>  5:     1530
#>  6:     1760
#>  7:     1290
#>  8:     1330
#>  9:     1400
#> 10:     1760
#> 11:     1560
#> 12:     1660
#> 13:     1660
#> 14:     1790
#> 15:     1360
#> 16:     1340
#> 17:     1400
#> 18:     1470
#> 19:     1700
#> 20:     1820
#> 21:     1670
#> 22:     1460
#> 23:     1480
#> 24:     1860
#> 25:     1530
#> 26:     1660
#> 27:     1660
#> 28:     1700
#> 29:     1460
#> 30:     1290
#> 31:     1320
#> 32:     1540
#> 33:      923
#> 34:     1550
#> 35:     1520
#> 36:     1330
#> 37:     1590
#> 38:     1760
#> 39:     1290
#> 40:     1350
#> 41:     1390
#> 42:     1620
#> 43:     1540
#> 44:     1670
#> 45:     1530
#> 46:     1640
#> 47:     1400
#> 48:     1430
#> 49:     1390
#> 50:     1620
#>     ess_tail
#>        <int>

# model parameter summary
summary(posterior, type = "model")
#>             variable                  clean_name exponentiated         mean
#>               <char>                      <char>        <lgcl>        <num>
#> 1: avg_voc_advantage          Average VOC effect         FALSE     0.589000
#> 2:              beta                        Beta         FALSE     0.230000
#> 3:     init_cases[1]               Initial cases         FALSE 89800.000000
#> 4: init_voc_cases[1]           Initial VOC cases         FALSE   222.000000
#> 5:            phi[1] Notification overdispersion         FALSE    95.300000
#> 6:            phi[2]   Sequencing overdispersion         FALSE   191.000000
#> 7:            r_init              Initial growth         FALSE     0.133000
#> 8:           r_scale                 Growth (sd)         FALSE     0.118000
#> 9:           voc_mod          Initial VOC effect          TRUE     1.802185
#>          median       sd      mad           q5          q20          q80
#>           <num>    <num>    <num>        <num>        <num>        <num>
#> 1:     0.589000 3.08e-02 2.97e-02     0.541000     0.564000     0.613000
#> 2:     0.260000 4.08e-01 4.53e-01    -0.466000    -0.151000     0.621000
#> 3: 89600.000000 6.14e+03 5.82e+03 79900.000000 84800.000000 94600.000000
#> 4:   221.000000 2.27e+01 2.16e+01   186.000000   203.000000   240.000000
#> 5:    78.000000 7.42e+01 5.32e+01    21.200000    41.200000   135.000000
#> 6:   160.000000 1.33e+02 9.83e+01    44.500000    92.200000   268.000000
#> 7:     0.135000 7.62e-02 7.21e-02     0.008860     0.073700     0.193000
#> 8:     0.111000 5.58e-02 5.42e-02     0.044000     0.068600     0.161000
#> 9:     1.802185 3.08e-02 2.97e-02     1.717724     1.757689     1.845961
#>             q95  rhat ess_bulk ess_tail
#>           <num> <num>    <int>    <int>
#> 1:     0.639000     1     1270     1250
#> 2:     0.846000     1      779      923
#> 3: 99900.000000     1     1360     1340
#> 4:   261.000000     1     1610     1060
#> 5:   231.000000     1      746     1030
#> 6:   443.000000     1     1760     1150
#> 7:     0.255000     1      950      923
#> 8:     0.218000     1      771     1310
#> 9:     1.894585     1     1270     1250

# raw posterior values
summary(posterior, type = "raw")
#>         variable      mean    median     sd    mad        q5       q20
#>           <char>     <num>     <num>  <num>  <num>     <num>     <num>
#>   1:        lp__ -3360.000 -3360.000 4.6500 4.5500 -3.37e+03 -3.36e+03
#>   2:      r_init     0.133     0.135 0.0762 0.0721  8.86e-03  7.37e-02
#>   3:     r_scale     0.118     0.111 0.0558 0.0542  4.40e-02  6.86e-02
#>   4:        beta     0.230     0.260 0.4080 0.4530 -4.66e-01 -1.51e-01
#>   5:      eta[1]    -0.285    -0.308 0.8170 0.7920 -1.59e+00 -9.71e-01
#>  ---                                                                  
#> 278: log_lik[11]   -14.700   -14.600 0.6930 0.6230 -1.60e+01 -1.52e+01
#> 279: log_lik[12]   -13.800   -13.800 0.5790 0.5440 -1.48e+01 -1.43e+01
#> 280: log_lik[13]   -13.500   -13.400 0.8670 0.7770 -1.51e+01 -1.41e+01
#> 281: log_lik[14]    -8.160    -8.070 0.5980 0.4910 -9.20e+00 -8.56e+00
#> 282: log_lik[15]    -7.730    -7.590 0.6970 0.5320 -9.01e+00 -8.15e+00
#>            q80       q95  rhat ess_bulk ess_tail
#>          <num>     <num> <num>    <int>    <int>
#>   1: -3360.000 -3350.000  1.01      529      816
#>   2:     0.193     0.255  1.00      950      923
#>   3:     0.161     0.218  1.00      771     1310
#>   4:     0.621     0.846  1.00      779      923
#>   5:     0.380     1.050  1.00     1130     1060
#>  ---                                            
#> 278:   -14.100   -13.700  1.00     1300     1500
#> 279:   -13.400   -13.000  1.01      915     1160
#> 280:   -12.700   -12.300  1.00     1220     1490
#> 281:    -7.690    -7.420  1.00     1070     1250
#> 282:    -7.200    -6.880  1.00      910     1320
```
