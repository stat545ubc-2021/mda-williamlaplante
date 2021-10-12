Mini-data-analysis-m2
================
William Laplante
12/10/2021

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

We compute the summary statistics of the area of a cancer for malignant
cancers and for bening cancers. This gives us insights on the spread of
our data and where it is centered. Also, we can compare the mean and
median to see if there are significant outliers.

##### Malignant cancer - summary statistics

``` r
cancer_sample %>% filter(diagnosis == "M") %>% select(area_mean) %>% summary()
```

    ##    area_mean     
    ##  Min.   : 361.6  
    ##  1st Qu.: 705.3  
    ##  Median : 932.0  
    ##  Mean   : 978.4  
    ##  3rd Qu.:1203.8  
    ##  Max.   :2501.0

##### Benign cancer - summary statistics

``` r
sum_B = cancer_sample %>% filter(diagnosis == "B") %>% select(area_mean) %>% summary()
sum_B
```

    ##    area_mean    
    ##  Min.   :143.5  
    ##  1st Qu.:378.2  
    ##  Median :458.4  
    ##  Mean   :462.8  
    ##  3rd Qu.:551.1  
    ##  Max.   :992.1

We also compute and store the mean of area_mean for each of the two
diagnosis. This will be used later for plotting.

``` r
mu = ddply(cancer_sample, "diagnosis", summarise, grp.mean=mean(area_mean))
mu
```

    ##   diagnosis grp.mean
    ## 1         B 462.7902
    ## 2         M 978.3764

##### Graphing

Now, we can visualize the existing difference in summary statistics
between the benign cancer and malignant cancer by making a histogram for
both cases. We play on the transparency to show the small overlap
between the two histograms.

``` r
p <- ggplot(cancer_sample, aes(x=area_mean, fill=diagnosis, color=diagnosis)) + geom_histogram(position="identity", alpha=0.5)
p +   geom_vline(data=mu, aes(xintercept=grp.mean, color=diagnosis), linetype="dashed")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Mini-data-analysis-m2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### Research question 2

We look into the summary statistics of smoothness and compactness. Also,
we look at the covariance and correlation between the two.

##### smoothness - summary statistics

``` r
cancer_sample %>% select(smoothness_mean) %>% summary()
```

    ##  smoothness_mean  
    ##  Min.   :0.05263  
    ##  1st Qu.:0.08637  
    ##  Median :0.09587  
    ##  Mean   :0.09636  
    ##  3rd Qu.:0.10530  
    ##  Max.   :0.16340

##### compactness - summary statistics

``` r
cancer_sample %>% select(compactness_mean) %>% summary()
```

    ##  compactness_mean 
    ##  Min.   :0.01938  
    ##  1st Qu.:0.06492  
    ##  Median :0.09263  
    ##  Mean   :0.10434  
    ##  3rd Qu.:0.13040  
    ##  Max.   :0.34540

##### Correlation between the variables

``` r
cor(cancer_sample$smoothness_mean, cancer_sample$compactness_mean)
```

    ## [1] 0.6591232

##### Graphing

Now, we make a plot illustrating the correlation between the two
variables. We add a fitted regression line to show the correlation. Note
that for now, we ignore the diagnosis component of the data.

``` r
ggplot(cancer_sample, aes(smoothness_mean, compactness_mean)) + geom_point() + geom_smooth(method=lm)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Mini-data-analysis-m2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

#### Research question 3
