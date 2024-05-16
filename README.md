<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdleran

<!-- badges: start -->

<!-- badges: end -->

-   Author: $$Kentaro Kawato$$
-   Package manual: $$PDF$$

## Installation Instructions

You can install the development version of rdleran from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kkawato/rdlearn")
```

## Example: Cutoff change relative to the baseline for each department (y-axis) under different smoothness multiplicative factors (x-axis)

``` r
## set seed for replication
# set.seed(12345)
result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group_name = "department", data = acces, fold = 20, M = c(0, 1, 2, 4), cost = 0)
#> # weights:  69 (44 variable)
#> initial  value 24541.513228 
#> iter  10 value 24277.297448
#> iter  20 value 24080.375291
#> iter  30 value 24044.989371
#> iter  40 value 24030.849636
#> iter  50 value 24029.786177
#> final  value 24029.783071 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24676.339479 
#> iter  10 value 24413.157427
#> iter  20 value 24218.503901
#> iter  30 value 24183.961836
#> iter  40 value 24173.230406
#> iter  40 value 24173.230223
#> iter  50 value 24169.280529
#> final  value 24169.218356 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24585.410147 
#> iter  10 value 24319.672505
#> iter  20 value 24136.603057
#> iter  30 value 24103.803505
#> iter  40 value 24093.568840
#> iter  50 value 24063.700698
#> iter  50 value 24063.700657
#> iter  60 value 24057.398672
#> iter  70 value 24057.270587
#> iter  70 value 24057.270441
#> iter  70 value 24057.270387
#> final  value 24057.270387 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24409.822471 
#> iter  10 value 24145.525163
#> iter  20 value 23950.469236
#> iter  30 value 23914.766520
#> iter  40 value 23903.289255
#> iter  50 value 23899.932277
#> iter  50 value 23899.932071
#> iter  60 value 23899.029271
#> iter  70 value 23876.059898
#> iter  80 value 23874.809844
#> final  value 23874.795835 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24522.700263 
#> iter  10 value 24262.155831
#> iter  20 value 24062.008369
#> iter  30 value 24027.859091
#> iter  40 value 24016.995935
#> iter  40 value 24016.995798
#> iter  50 value 24013.821064
#> iter  60 value 23992.538155
#> iter  70 value 23991.419837
#> final  value 23991.339887 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24610.494101 
#> iter  10 value 24343.794025
#> iter  20 value 24143.921660
#> iter  30 value 24087.618382
#> iter  40 value 24082.778115
#> final  value 24082.705835 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24516.429274 
#> iter  10 value 24247.509827
#> iter  20 value 24046.911849
#> iter  30 value 24013.244719
#> iter  40 value 24002.883192
#> iter  40 value 24002.883054
#> iter  50 value 23999.011800
#> iter  50 value 23999.011730
#> final  value 23998.971777 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24510.158286 
#> iter  10 value 24252.437799
#> iter  20 value 24057.165134
#> iter  30 value 24003.241179
#> iter  40 value 23999.771612
#> iter  50 value 23998.690015
#> final  value 23998.634722 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24729.642881 
#> iter  10 value 24462.667900
#> iter  20 value 24271.680609
#> iter  30 value 24237.077049
#> iter  40 value 24223.532565
#> iter  40 value 24223.532406
#> iter  50 value 24222.580951
#> iter  50 value 24222.580815
#> final  value 24222.579403 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24569.732676 
#> iter  10 value 24303.328728
#> iter  20 value 24111.102494
#> iter  30 value 24063.950786
#> iter  40 value 24063.866538
#> iter  50 value 24044.780930
#> iter  60 value 24043.134366
#> iter  70 value 24042.979818
#> iter  70 value 24042.979696
#> final  value 24042.978406 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24557.190699 
#> iter  10 value 24295.925195
#> iter  20 value 24094.623160
#> iter  30 value 24060.688699
#> iter  40 value 24049.673035
#> iter  40 value 24049.672864
#> iter  50 value 24045.240355
#> iter  50 value 24045.240274
#> final  value 24045.175570 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24528.971251 
#> iter  10 value 24269.444990
#> iter  20 value 24063.668465
#> iter  30 value 24026.184870
#> iter  40 value 24014.940828
#> iter  50 value 24010.932371
#> final  value 24010.855334 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24579.139159 
#> iter  10 value 24323.429040
#> iter  20 value 24120.709386
#> iter  30 value 24088.004561
#> iter  40 value 24074.805329
#> iter  40 value 24074.805156
#> iter  50 value 24073.785652
#> iter  50 value 24073.785572
#> final  value 24073.783783 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24522.700263 
#> iter  10 value 24267.372474
#> iter  20 value 24060.656013
#> iter  30 value 24027.784407
#> iter  40 value 24014.166668
#> iter  50 value 24012.858259
#> iter  50 value 24012.858209
#> final  value 24012.796508 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24478.803344 
#> iter  10 value 24202.846182
#> iter  20 value 24001.848615
#> iter  30 value 23966.233580
#> iter  40 value 23955.734702
#> iter  50 value 23952.980206
#> iter  60 value 23937.273433
#> iter  60 value 23937.273385
#> iter  70 value 23932.052343
#> iter  70 value 23932.052248
#> iter  80 value 23931.508360
#> iter  80 value 23931.508276
#> final  value 23931.506075 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24638.713549 
#> iter  10 value 24363.748165
#> iter  20 value 24157.943329
#> iter  30 value 24116.802410
#> iter  40 value 24101.157942
#> iter  50 value 24100.129278
#> iter  50 value 24100.129095
#> final  value 24100.117674 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24532.106745 
#> iter  10 value 24274.406826
#> iter  20 value 24073.795708
#> iter  30 value 24030.138588
#> iter  40 value 24027.179008
#> iter  50 value 24026.193760
#> iter  50 value 24026.193672
#> final  value 24026.144029 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24463.125873 
#> iter  10 value 24200.146238
#> iter  20 value 23994.321634
#> iter  30 value 23961.261268
#> iter  40 value 23948.515709
#> iter  50 value 23947.388587
#> iter  50 value 23947.388535
#> final  value 23947.351215 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24582.274653 
#> iter  10 value 24314.400825
#> iter  20 value 24110.171618
#> iter  30 value 24068.255578
#> iter  40 value 24051.052919
#> iter  40 value 24051.052838
#> final  value 24051.034973 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24635.578055 
#> iter  10 value 24364.313398
#> iter  20 value 24150.869979
#> iter  30 value 24106.495347
#> iter  40 value 24092.673243
#> iter  40 value 24092.673198
#> iter  50 value 24087.550496
#> iter  60 value 24087.449582
#> iter  60 value 24087.449341
#> iter  60 value 24087.449334
#> final  value 24087.449334 
#> converged
#>  [1] -828 -824 -786 -779 -774 -764 -758 -755 -754 -753 -732 -729 -723 -719 -716
#> [16] -695 -678 -676 -672 -660 -632 -618 -559
#> [1] "Calculation in progress for M = 0 and C = 0"
#> [1] "Calculation in progress for M = 1 and C = 0"
#> [1] "Calculation in progress for M = 2 and C = 0"
#> [1] "Calculation in progress for M = 4 and C = 0"

# use "plot" to visualize the result
# Figure 2
plot(result)
```

<img src="man/figures/README-example-1.png" width="100%"/> \## Example: Cutoff change relative to the baseline for each department (y-axis) with varying cost of treatment (x-axis)

``` r
# "sens" is for sensitivity analysis.
# This function inherits the cross-fitting data.
sens_result <- sens(result, M = 1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
#> [1] "Calculation in progress for M = 1 and C = 0"
#> [1] "Calculation in progress for M = 1 and C = 0.2"
#> [1] "Calculation in progress for M = 1 and C = 0.4"
#> [1] "Calculation in progress for M = 1 and C = 0.6"
#> [1] "Calculation in progress for M = 1 and C = 0.8"
#> [1] "Calculation in progress for M = 1 and C = 1"
# Figure 3
plot(sens_result)
```

<img src="man/figures/README-example2-1.png" width="100%"/>
