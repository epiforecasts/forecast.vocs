# Label the Variant of Concern

Assign a custom label to the variant of concern in the output from
[`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md).

## Usage

``` r
update_voc_label(posterior, label, target_label = "VOC")
```

## Arguments

- posterior:

  A dataframe of posterior output as produced by
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md).
  For forecast dates to be extracted data with `value_type == "cases"`
  must be present.

- label:

  Character string indicating the new label to use for the variant of
  concern.

- target_label:

  A character string defaulting to "VOC". Indicates the current label
  for the variant of concern.

## Value

A list of data frames as returned by
\`[`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md)
but with updated labels.

## See also

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
[`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md)

## Examples

``` r
p <- fv_example(strains = 2, type = "posterior")
p <- update_voc_label(p, "Delta")
summary(p, type = "cases")
#>               variable       date      type    obs observed forecast_start
#>                 <char>     <IDat>    <char>  <num>   <lgcl>         <lgcl>
#>  1:       sim_cases[1] 2021-03-20  Combined  87328     TRUE          FALSE
#>  2:       sim_cases[2] 2021-03-27  Combined 109442     TRUE          FALSE
#>  3:       sim_cases[3] 2021-04-03  Combined 117965     TRUE          FALSE
#>  4:       sim_cases[4] 2021-04-10  Combined 107223     TRUE          FALSE
#>  5:       sim_cases[5] 2021-04-17  Combined 142664     TRUE          FALSE
#>  6:       sim_cases[6] 2021-04-24  Combined 145568     TRUE          FALSE
#>  7:       sim_cases[7] 2021-05-01  Combined 131887     TRUE          FALSE
#>  8:       sim_cases[8] 2021-05-08  Combined 107141     TRUE          FALSE
#>  9:       sim_cases[9] 2021-05-15  Combined  77261     TRUE          FALSE
#> 10:      sim_cases[10] 2021-05-22  Combined  57310     TRUE          FALSE
#> 11:      sim_cases[11] 2021-05-29  Combined  33052     TRUE          FALSE
#> 12:      sim_cases[12] 2021-06-05  Combined  22631     TRUE          FALSE
#> 13:      sim_cases[13] 2021-06-12  Combined  15553     TRUE          FALSE
#> 14:      sim_cases[14] 2021-06-19  Combined   7659     TRUE          FALSE
#> 15:      sim_cases[15] 2021-06-26  Combined   5033     TRUE           TRUE
#> 16:      sim_cases[16] 2021-07-03  Combined     NA    FALSE          FALSE
#> 17:      sim_cases[17] 2021-07-10  Combined     NA    FALSE          FALSE
#> 18:      sim_cases[18] 2021-07-17  Combined     NA    FALSE          FALSE
#> 19:      sim_cases[19] 2021-07-24  Combined     NA    FALSE          FALSE
#> 20:   sim_voc_cases[1] 2021-04-17     Delta     NA     TRUE          FALSE
#> 21:   sim_voc_cases[2] 2021-04-24     Delta     NA     TRUE          FALSE
#> 22:   sim_voc_cases[3] 2021-05-01     Delta     NA     TRUE          FALSE
#> 23:   sim_voc_cases[4] 2021-05-08     Delta     NA     TRUE          FALSE
#> 24:   sim_voc_cases[5] 2021-05-15     Delta     NA     TRUE          FALSE
#> 25:   sim_voc_cases[6] 2021-05-22     Delta     NA     TRUE          FALSE
#> 26:   sim_voc_cases[7] 2021-05-29     Delta     NA     TRUE          FALSE
#> 27:   sim_voc_cases[8] 2021-06-05     Delta     NA     TRUE          FALSE
#> 28:   sim_voc_cases[9] 2021-06-12     Delta     NA     TRUE           TRUE
#> 29:  sim_voc_cases[10] 2021-06-19     Delta     NA    FALSE          FALSE
#> 30:  sim_voc_cases[11] 2021-06-26     Delta     NA    FALSE          FALSE
#> 31:  sim_voc_cases[12] 2021-07-03     Delta     NA    FALSE          FALSE
#> 32:  sim_voc_cases[13] 2021-07-10     Delta     NA    FALSE          FALSE
#> 33:  sim_voc_cases[14] 2021-07-17     Delta     NA    FALSE          FALSE
#> 34:  sim_voc_cases[15] 2021-07-24     Delta     NA    FALSE          FALSE
#> 35:  sim_nvoc_cases[1] 2021-03-20 non-Delta     NA     TRUE          FALSE
#> 36:  sim_nvoc_cases[2] 2021-03-27 non-Delta     NA     TRUE          FALSE
#> 37:  sim_nvoc_cases[3] 2021-04-03 non-Delta     NA     TRUE          FALSE
#> 38:  sim_nvoc_cases[4] 2021-04-10 non-Delta     NA     TRUE          FALSE
#> 39:  sim_nvoc_cases[5] 2021-04-17 non-Delta     NA     TRUE          FALSE
#> 40:  sim_nvoc_cases[6] 2021-04-24 non-Delta     NA     TRUE          FALSE
#> 41:  sim_nvoc_cases[7] 2021-05-01 non-Delta     NA     TRUE          FALSE
#> 42:  sim_nvoc_cases[8] 2021-05-08 non-Delta     NA     TRUE          FALSE
#> 43:  sim_nvoc_cases[9] 2021-05-15 non-Delta     NA     TRUE          FALSE
#> 44: sim_nvoc_cases[10] 2021-05-22 non-Delta     NA     TRUE          FALSE
#> 45: sim_nvoc_cases[11] 2021-05-29 non-Delta     NA     TRUE          FALSE
#> 46: sim_nvoc_cases[12] 2021-06-05 non-Delta     NA     TRUE          FALSE
#> 47: sim_nvoc_cases[13] 2021-06-12 non-Delta     NA     TRUE           TRUE
#> 48: sim_nvoc_cases[14] 2021-06-19 non-Delta     NA    FALSE          FALSE
#> 49: sim_nvoc_cases[15] 2021-06-26 non-Delta     NA    FALSE          FALSE
#> 50: sim_nvoc_cases[16] 2021-07-03 non-Delta     NA    FALSE          FALSE
#> 51: sim_nvoc_cases[17] 2021-07-10 non-Delta     NA    FALSE          FALSE
#> 52: sim_nvoc_cases[18] 2021-07-17 non-Delta     NA    FALSE          FALSE
#> 53: sim_nvoc_cases[19] 2021-07-24 non-Delta     NA    FALSE          FALSE
#>               variable       date      type    obs observed forecast_start
#>                 <char>     <IDat>    <char>  <num>   <lgcl>         <lgcl>
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
summary(p, type = "model")
#>             variable                  clean_name exponentiated         mean
#>               <char>                      <char>        <lgcl>        <num>
#> 1: avg_voc_advantage        Average Delta effect         FALSE     0.589000
#> 2:              beta                        Beta         FALSE     0.230000
#> 3:     init_cases[1]               Initial cases         FALSE 89800.000000
#> 4: init_voc_cases[1]         Initial Delta cases         FALSE   222.000000
#> 5:            phi[1] Notification overdispersion         FALSE    95.300000
#> 6:            phi[2]   Sequencing overdispersion         FALSE   191.000000
#> 7:            r_init              Initial growth         FALSE     0.133000
#> 8:           r_scale                 Growth (sd)         FALSE     0.118000
#> 9:           voc_mod        Initial Delta effect          TRUE     1.802185
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
```
