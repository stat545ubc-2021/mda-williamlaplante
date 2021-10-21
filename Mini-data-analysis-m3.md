Mini-data-analysis-m3
================
William Laplante
20/10/2021

## Setup

``` r
library(datateachr)
library(plyr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::arrange()   masks plyr::arrange()
    ## x purrr::compact()   masks plyr::compact()
    ## x dplyr::count()     masks plyr::count()
    ## x dplyr::failwith()  masks plyr::failwith()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::id()        masks plyr::id()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::mutate()    masks plyr::mutate()
    ## x dplyr::rename()    masks plyr::rename()
    ## x dplyr::summarise() masks plyr::summarise()
    ## x dplyr::summarize() masks plyr::summarize()

``` r
library(DiscriMiner)
library(corrr)
library(forcats)
```

In milestone 2, we chose two research questions:

1.  How does the area of a cancer affects the diagnosis?

2.  What is the variable in this dataset that is the best indicator of a
    malignant cancer?

## Exercise 1 : Special Data Types

``` r
cancer_sample_numeric <- cancer_sample %>% replace(cancer_sample=="M","1") %>% replace(cancer_sample=="B","0") %>% transform(diagnosis=as.numeric(diagnosis))
cancer_sample_cormatrix = cor(cancer_sample_numeric)

cormatrix_df <- as_cordf(cancer_sample_cormatrix)
cormatrix_df
```

    ## # A tibble: 32 × 33
    ##    term           ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ##    <chr>       <dbl>     <dbl>       <dbl>        <dbl>          <dbl>     <dbl>
    ##  1 ID       NA          0.0398      0.0746       0.0998         0.0732    0.0969
    ##  2 diagnos…  3.98e-2   NA           0.730        0.415          0.743     0.709 
    ##  3 radius_…  7.46e-2    0.730      NA            0.324          0.998     0.987 
    ##  4 texture…  9.98e-2    0.415       0.324       NA              0.330     0.321 
    ##  5 perimet…  7.32e-2    0.743       0.998        0.330         NA         0.987 
    ##  6 area_me…  9.69e-2    0.709       0.987        0.321          0.987    NA     
    ##  7 smoothn… -1.30e-2    0.359       0.171       -0.0234         0.207     0.177 
    ##  8 compact…  9.57e-5    0.597       0.506        0.237          0.557     0.499 
    ##  9 concavi…  5.01e-2    0.696       0.677        0.302          0.716     0.686 
    ## 10 concave…  4.42e-2    0.777       0.823        0.293          0.851     0.823 
    ## # … with 22 more rows, and 26 more variables: smoothness_mean <dbl>,
    ## #   compactness_mean <dbl>, concavity_mean <dbl>, concave_points_mean <dbl>,
    ## #   symmetry_mean <dbl>, fractal_dimension_mean <dbl>, radius_se <dbl>,
    ## #   texture_se <dbl>, perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>, …
