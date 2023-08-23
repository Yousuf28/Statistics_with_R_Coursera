Probability Distribution
================

### Binomial Distribution

Hospital records show that of patients suffering from a certain disease, 75% die of it. What is the probability that of 6 randomly selected patients,4 will recover?

 $PMF : {{n}\\choose{k}} \\cdot p^k(1-p)^{n-k}$

here, die = 75% so p= .25 n= 6 k=4

``` r
dbinom(4, size = 6, prob = .25)
```

    ## [1] 0.03295898

plot the distribution

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
y <- dbinom(0:6, 6, .25)
y
```

    ## [1] 0.1779785156 0.3559570312 0.2966308594 0.1318359375 0.0329589844
    ## [6] 0.0043945312 0.0002441406

``` r
x <- c(0:6)
x
```

    ## [1] 0 1 2 3 4 5 6

``` r
plot(x,y, type = 'h', col='blue')
```

![](Probability_Distribution_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Bernoulli Distribution

### Geometric Distribution

### Hypergeometric Distribution

### Negative binomial Distribution

### Poisson Distribution

### Normal Distribution

### Exponential Distribution

### Beta Distribution

### Gamma Distribution

### Cauchy Distribution

### chi-squared

### Erlang Distribution

### F distribution

### Student's t distribution

question:
