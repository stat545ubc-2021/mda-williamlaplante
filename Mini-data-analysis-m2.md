Mini-data-analysis-m2
================
William Laplante
12/10/2021

``` r
library(datateachr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## Task 1 : Process and summarize your data

### 1.1 : Research questions

1.  How does the area of a cancer affects the diagnosis?

2.  How strongly do smoothness and compactness correlate, and is it
    dependent on diagnosis?

3.  What is the variable in this dataset that is the best indicator of a
    malignant cancer?

4.  Is it harder to perform radius measurements on malignant cancers
    compared to benign cancers? That is, is the standard error in radius
    measurements bigger for malignant cancers?

### 1.2 : Summarizing and Graphing

#### Research question 1

We compute the summary statistics of the area of a cancer :

``` r
summary(cancer_sample$area_mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   143.5   420.3   551.1   654.9   782.7  2501.0
