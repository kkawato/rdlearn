<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdleran

<!-- badges: start -->

<!-- badges: end -->

-   Author: Kentaro Kawato
-   Package manual: PDF

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
#> initial  value 24607.358607 
#> iter  10 value 24339.623355
#> iter  20 value 24148.734196
#> iter  30 value 24113.101932
#> iter  40 value 24102.046351
#> iter  50 value 24097.999260
#> iter  60 value 24097.931626
#> iter  70 value 24074.472163
#> iter  80 value 24070.276886
#> iter  90 value 24069.847999
#> iter  90 value 24069.847963
#> final  value 24069.845444 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24544.648722 
#> iter  10 value 24280.785854
#> iter  20 value 24086.402270
#> iter  30 value 24052.134100
#> iter  40 value 24041.518710
#> iter  40 value 24041.518573
#> iter  50 value 24037.863018
#> iter  60 value 24037.611777
#> final  value 24037.565152 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24673.203985 
#> iter  10 value 24400.235127
#> iter  20 value 24194.303405
#> iter  30 value 24157.900416
#> iter  40 value 24146.522170
#> iter  40 value 24146.522008
#> iter  50 value 24142.322002
#> iter  50 value 24142.321881
#> final  value 24142.278704 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24582.274653 
#> iter  10 value 24317.206845
#> iter  20 value 24108.600121
#> iter  30 value 24065.930390
#> iter  40 value 24063.053522
#> iter  50 value 24062.149523
#> iter  50 value 24062.149410
#> final  value 24062.141999 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24538.377734 
#> iter  10 value 24267.486366
#> iter  20 value 24076.444266
#> iter  30 value 24039.942809
#> iter  40 value 24010.016191
#> iter  50 value 23994.598053
#> iter  60 value 23993.214566
#> final  value 23993.201403 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24519.564769 
#> iter  10 value 24258.511535
#> iter  20 value 24067.047011
#> iter  30 value 24032.923577
#> iter  40 value 24022.284741
#> iter  40 value 24022.284551
#> iter  50 value 24018.283620
#> final  value 24018.238161 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24513.293780 
#> iter  10 value 24237.201213
#> iter  20 value 24047.082564
#> iter  30 value 24011.254401
#> iter  40 value 23999.940343
#> iter  40 value 23999.940118
#> iter  50 value 23995.652208
#> iter  50 value 23995.652096
#> final  value 23995.605544 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24613.629595 
#> iter  10 value 24349.796311
#> iter  20 value 24144.072022
#> iter  30 value 24110.949880
#> iter  40 value 24097.897690
#> iter  40 value 24097.897466
#> iter  50 value 24096.960871
#> iter  50 value 24096.960707
#> final  value 24096.959150 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24485.074332 
#> iter  10 value 24222.191588
#> iter  20 value 24022.088269
#> iter  30 value 23987.750717
#> iter  40 value 23973.709534
#> iter  40 value 23973.709312
#> iter  50 value 23972.630587
#> iter  50 value 23972.630487
#> final  value 23972.628495 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24513.293780 
#> iter  10 value 24247.134110
#> iter  20 value 24050.680259
#> iter  30 value 24016.191438
#> iter  40 value 24005.465994
#> iter  40 value 24005.465785
#> iter  50 value 24001.409180
#> final  value 24001.363100 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24601.087618 
#> iter  10 value 24337.123562
#> iter  20 value 24138.472140
#> iter  30 value 24104.408936
#> iter  40 value 24093.701217
#> iter  40 value 24093.701041
#> iter  50 value 24089.828826
#> iter  60 value 24089.588755
#> final  value 24089.574514 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24525.835757 
#> iter  10 value 24264.951328
#> iter  20 value 24065.971602
#> iter  30 value 24031.314195
#> iter  40 value 24020.137238
#> iter  40 value 24020.137112
#> iter  50 value 24015.870568
#> iter  60 value 24015.589480
#> final  value 24015.570738 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24644.984537 
#> iter  10 value 24379.846284
#> iter  20 value 24175.429794
#> iter  30 value 24140.628790
#> iter  40 value 24126.649787
#> iter  50 value 24125.326221
#> iter  50 value 24125.326097
#> final  value 24125.276844 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24510.158286 
#> iter  10 value 24249.615204
#> iter  20 value 24048.154760
#> iter  30 value 24000.486928
#> iter  40 value 23995.892683
#> iter  50 value 23994.865260
#> final  value 23994.862772 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24516.429274 
#> iter  10 value 24253.604830
#> iter  20 value 24057.897844
#> iter  30 value 24005.466418
#> iter  40 value 24000.894737
#> iter  40 value 24000.894653
#> final  value 24000.609139 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24707.694422 
#> iter  10 value 24444.862174
#> iter  20 value 24244.784186
#> iter  30 value 24211.802263
#> iter  40 value 24198.563371
#> iter  50 value 24197.385639
#> iter  50 value 24197.385602
#> final  value 24197.335669 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24431.770931 
#> iter  10 value 24181.468157
#> iter  20 value 23979.631129
#> iter  30 value 23946.999968
#> iter  40 value 23933.343737
#> iter  50 value 23932.217158
#> iter  50 value 23932.216931
#> final  value 23932.204001 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24585.410147 
#> iter  10 value 24318.107626
#> iter  20 value 24139.070320
#> iter  30 value 24113.476907
#> iter  30 value 24113.476843
#> iter  40 value 24074.485781
#> iter  40 value 24074.485647
#> iter  50 value 24060.147923
#> iter  60 value 24059.103865
#> iter  60 value 24059.103749
#> final  value 24059.067555 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24579.139159 
#> iter  10 value 24318.242327
#> iter  20 value 24115.824699
#> iter  30 value 24075.170314
#> iter  40 value 24060.637349
#> iter  50 value 24059.543081
#> final  value 24059.539174 
#> converged
#> # weights:  69 (44 variable)
#> initial  value 24497.616309 
#> iter  10 value 24235.389439
#> iter  20 value 24036.447241
#> iter  30 value 24000.705893
#> iter  40 value 23986.349640
#> iter  50 value 23985.280021
#> final  value 23985.277188 
#> converged
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
