
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppscore - a R implementation of the Predictive Power Score (PPS)

<!-- badges: start -->

<!-- badges: end -->

**For the life of me, I cannot figure out how to get the build tools to
work on Windows 10, so this is updated progress thus far. The functions
*should* work, but the mechanics of getting the packing to work has
become frustrating.**

This in R port of the Predictive Power Score (PPS) developed by
[8080Labs](https://github.com/8080labs/ppscore) for Python and described
on the blog post [Toward Data
Science](https://towardsdatascience.com/rip-correlation-introducing-the-predictive-power-score-3d90808b9598?gi=ba32980cd4d4).

This is also my first attempt at building a package… mostly for fun. Its
under development as there would need to be a lot better error handling
to consider making this something for public use.

The goal of ppscore is to …

## Installation

You can install via:

``` r
remotes::install.packages("jlawren67/ppscore")
```

## PPScore

The `ppscore` function returns the Predictive Power Score for any target
and feature. The score represents how well the feature explains the
target.

``` r
library(ppscore)
## basic example code

titanic <- read.csv('https://raw.githubusercontent.com/8080labs/ppscore/master/examples/titanic.csv', stringsAsFactors = F)

ppscore(titanic, "Survived", "Sex")
```

## pps\_matrix

To run comparions on all variables in the data.frame, use the
`pps_matrix` function. This will run all possible target/feature
combinations and return in a long-format data frame.

``` r
mtx_check <- pps_matrix(titanic)

mtx_check
```

The PPS Matrix can then be generated by:

``` r
mtx_check %>%
  filter(task_name %in% c('predict_itself', 'classification', 'regression')) %>%
  add_count(target, name = "appearance") %>%
  filter(appearance > 1) %>%
ggplot(aes(x = feature, y = target, fill = ppscore)) +
  geom_tile() +
  geom_text(aes(label = ppscore %>% round(2))) +
  scale_fill_viridis_c(guide = F)
```
