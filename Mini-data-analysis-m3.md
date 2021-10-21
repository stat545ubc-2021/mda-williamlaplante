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

#### Task 1

Here, we take the correlation matrix computed in the previous milestone,
and we divide into categories the correlation values for the diagnosis
column. We then reorder categories using fct_infreq() and plot the bar
graph with this new reordering. This is helpful to visualize which
category is dominant in our data and which category is the least
present. Clearly, we see that the category that is the least present is
“very high”, meaning most of our variables don’t have a strong
correlation with the diagnosis. Also the correlation category that is
most present is “medium”.

``` r
#we compute the correlation matrix for the cancer_sample dataset. 
cancer_sample_numeric <- cancer_sample %>% replace(cancer_sample=="M","1") %>% replace(cancer_sample=="B","0") %>% transform(diagnosis=as.numeric(diagnosis))
cancer_sample_cormatrix = cor(cancer_sample_numeric)

#we store as a dataframe the correlation matrix
cormatrix_df <- as_cordf(cancer_sample_cormatrix)

#we break the correlation values into categories for the diagnosis column.
category_corr <- cormatrix_df %>% mutate(category=cut(diagnosis, breaks=c(-Inf, 0.2, 0.4, 0.6, 0.75, Inf), labels=c("Very Low", "Low", "Medium", "High", "Very High"))) %>% select(term, diagnosis, category) %>% filter(!is.na(diagnosis)) %>% rename(variable=term, corr_with_diagnosis=diagnosis)

#now we plot the bar graph for the correlation categories in order using the forcats package.
ggplot(category_corr, aes(x = fct_infreq(category))) + geom_bar() + coord_flip()
```

![](Mini-data-analysis-m3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

#### Task 2

We now regroup our factor levels into two categories ; one will contain
the variables with a “very high” correlation with the diagnosis and the
other category will be labeled “other” and contain the remaining
variables that won’t be used for research purposes. This is useful to
show how many variables will be discarded vs how many will be kept for
future analyses.

``` r
ggplot(category_corr, aes(x = fct_lump(category, -1))) + geom_bar()
```

![](Mini-data-analysis-m3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Exercise 2 : Modelling

#### 2.0

We now choose our final research question and define the variable of
interest. Note that here, we change a bit the initial research question,
since the original question can already be answered through our previous
analysis.

Research question : If we create two models, one trained with the
variables that have “Very High” correlation with the diagnosis, and
another with the variables falling under the “other” category, how do
these two models compare?

Variable of Interest : Accuracy of model (based on the diagnosis
variable)

#### 2.1

#### 2.2
