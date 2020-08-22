<!-- README.md is generated from README.Rmd. Please edit that file -->

dyRank: Dynamic Rating Estimation for Ranked Data
=================================================

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/soichiroy/dyRank.svg?branch=master)](https://travis-ci.org/soichiroy/dyRank)
[![R build
status](https://github.com/soichiroy/dyRank/workflows/R-CMD-check/badge.svg)](https://github.com/soichiroy/dyRank/actions)
<!-- badges: end -->

The goal of `dyRank` is to estimate the dynamic rating of items, players
or students based on rank-ordered data with time-index.

Installation
------------

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("soichiroy/dyRank")

Example: Single Ranking Type
----------------------------

As an example, we use `f1_race` data in the `dyRank` package.

    ## load package 
    require(dyRank)
    require(tidyverse)

    ## load data 
    data("f1_race", package = "dyRank")

    ## see ?f1_race for the details of this dataset.

    ## view data 
    f1_race
    #> [90m# A tibble: 14,037 x 8[39m
    #>      Pos driver            Car            year  race  race_id GP     rank_type  
    #>    [3m[90m<dbl>[39m[23m [3m[90m<chr>[39m[23m             [3m[90m<chr>[39m[23m          [3m[90m<fct>[39m[23m [3m[90m<fct>[39m[23m [3m[90m<fct>[39m[23m   [3m[90m<fct>[39m[23m  [3m[90m<fct>[39m[23m      
    #> [90m 1[39m     1 Alain Prost       McLaren TAG    1984  races 466     brazil race-result
    #> [90m 2[39m     2 Keke Rosberg      Williams Honda 1984  races 466     brazil race-result
    #> [90m 3[39m     3 Elio de Angelis   Lotus Renault  1984  races 466     brazil race-result
    #> [90m 4[39m     4 Eddie Cheever     Alfa Romeo     1984  races 466     brazil race-result
    #> [90m 5[39m     5 Patrick Tambay    Renault        1984  races 466     brazil race-result
    #> [90m 6[39m     6 Thierry Boutsen   Arrows Ford    1984  races 466     brazil race-result
    #> [90m 7[39m     7 Marc Surer        Arrows Ford    1984  races 466     brazil race-result
    #> [90m 8[39m     8 Jonathan Palmer   RAM Hart       1984  races 466     brazil race-result
    #> [90m 9[39m    [31mNA[39m Derek Warwick     Renault        1984  races 466     brazil race-result
    #> [90m10[39m    [31mNA[39m Andrea de Cesaris Ligier Renault 1984  races 466     brazil race-result
    #> [90m# … with 14,027 more rows[39m

### Estimate rating via `dyRank()`

    ## set seed
    set.seed(1234)

    ## estimate via MCMC 
    fit <- dyRank(
        data       = f1_race, 
        var_rank   = "Pos",           
        var_player = "driver", 
        var_match  = "GP",
        var_time   = "year",
        driver_fix = "Timo Glock",
        mcmc = 100, burnin = 10, thin = 1,
        truncation = 3
    )

**Arguments**:

-   `data`: A data frame that contains variables specified in
    `var_rank`, `var_player`, `var_match`, `var_time`. This should be an
    object of either `data.frame` class or `tibble` class.
-   `var_rank`: A variable name (in character) of the outcome that
    records the ranking. The top rank should take `1`, and all ranking
    values within a match (specified in `var_match`) should be
    consecutive. `NA` values is allowed.
-   `var_player`: A variable name (in character) of the players.
-   `var_match`: A variable name (in character) of the matches. The
    package allows for the panel strucutre, which menas that the same
    match can repeat over time.
-   `var_time`: A variable name of time index.
-   `driver_fix`: A name (or index) within `var_player` that is used as
    a refenrece. This should be chosen carefully, otherwise estimates
    will be unstable. Generally, a player who appears in multiple
    matches and years and who can be considered as a “middle player” is
    recommended as a reference.
-   `mcmc`, `burnin` and `thin` correspond to the MCMC parameters that
    specify the iterations, burin periods and the thinning,
    respectively. Default values are set small; in practice longer
    values are recommended.
-   `truncation`: A truncation parameter that takes `1` or larger. This
    parameter specifies if we want to ignore players who ranked `k` or
    lower where `k` is the vlaue of `truncation`. The value of `1`
    corresponds to the original Placket-Luce representation, but the
    estimation might not be stable when observations are dropped due to
    `NA` values.

**Return**

`dyRank()` returns a list of two elements.

-   `lambda`: Estimated parameters stored as a list. Each element of the
    list corresponds to an interation of the MCMC step.

<!-- -->

    ## total number of elements
    length(fit$lambda)
    #> [1] 99

    ## each element of lambda is a list of estimates for all drivers
    length(fit$lambda[[1]])
    #> [1] 209

    ## each element of lambda[[k]] is an estimate for drivers
    length(fit$lambda[[1]][[1]])
    #> [1] 2

-   `data` returns the list of formatted datasets.

<!-- -->

    ## formatted data used for the estimation 
    fit$data$dat_ref
    #> [90m# A tibble: 9,285 x 6[39m
    #>    years races        drivers         rank_type id_time id_driver
    #>    [3m[90m<fct>[39m[23m [3m[90m<fct>[39m[23m        [3m[90m<chr>[39m[23m               [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m     [3m[90m<dbl>[39m[23m
    #> [90m 1[39m 1984  brazil       Alain Prost             1       1         4
    #> [90m 2[39m 1984  brazil       Keke Rosberg            1       1       106
    #> [90m 3[39m 1984  brazil       Elio de Angelis         1       1        43
    #> [90m 4[39m 1984  brazil       Eddie Cheever           1       1        41
    #> [90m 5[39m 1984  brazil       Patrick Tambay          1       1       153
    #> [90m 6[39m 1984  brazil       Thierry Boutsen         1       1       196
    #> [90m 7[39m 1984  brazil       Marc Surer              1       1       118
    #> [90m 8[39m 1984  brazil       Jonathan Palmer         1       1        95
    #> [90m 9[39m 1984  south-africa Niki Lauda              1       1       142
    #> [90m10[39m 1984  south-africa Alain Prost             1       1         4
    #> [90m# … with 9,275 more rows[39m

    ## global information
    fit$data$n_drivers
    #> [1] 209
    fit$data$n_race
    #> [1] 630

### Obtaining the estimated rating via `get_rating()`

    ## get the summary of rating 
    rating <- get_rating(fit)

    ## view the estimates 
    rating
    #> [90m# A tibble: 1,103 x 7[39m
    #>    driver         year  `2.5%`    `5%`   `50%`  `95%` `97.5%`
    #>    [3m[90m<chr>[39m[23m         [3m[90m<int>[39m[23m   [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m  [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m
    #> [90m 1[39m Adrian Campos  [4m1[24m987 -[31m1[39m[31m.[39m[31m67[39m   -[31m1[39m[31m.[39m[31m58[39m   -[31m0[39m[31m.[39m[31m591[39m   0.207   0.437
    #> [90m 2[39m Adrian Campos  [4m1[24m988 -[31m2[39m[31m.[39m[31m28[39m   -[31m1[39m[31m.[39m[31m99[39m   -[31m0[39m[31m.[39m[31m777[39m   0.342   0.423
    #> [90m 3[39m Adrian Sutil   [4m2[24m007 -[31m1[39m[31m.[39m[31m88[39m   -[31m1[39m[31m.[39m[31m63[39m   -[31m0[39m[31m.[39m[31m682[39m  -[31m0[39m[31m.[39m[31m175[39m  -[31m0[39m[31m.[39m[31m113[39m
    #> [90m 4[39m Adrian Sutil   [4m2[24m008 -[31m1[39m[31m.[39m[31m36[39m   -[31m1[39m[31m.[39m[31m28[39m   -[31m0[39m[31m.[39m[31m516[39m   0.203   0.301
    #> [90m 5[39m Adrian Sutil   [4m2[24m009 -[31m0[39m[31m.[39m[31m813[39m  -[31m0[39m[31m.[39m[31m638[39m   0.095[4m9[24m  0.726   0.802
    #> [90m 6[39m Adrian Sutil   [4m2[24m010  0.201   0.332   0.932   1.45    1.49 
    #> [90m 7[39m Adrian Sutil   [4m2[24m011  0.430   0.485   1.11    1.64    1.66 
    #> [90m 8[39m Adrian Sutil   [4m2[24m012 -[31m0[39m[31m.[39m[31m0[39m[31m78[4m4[24m[39m -[31m0[39m[31m.[39m[31m0[39m[31m51[4m9[24m[39m  0.932   1.61    1.69 
    #> [90m 9[39m Adrian Sutil   [4m2[24m013 -[31m0[39m[31m.[39m[31m0[39m[31m91[4m5[24m[39m -[31m0[39m[31m.[39m[31m0[39m[31m61[4m4[24m[39m  0.710   1.27    1.28 
    #> [90m10[39m Adrian Sutil   [4m2[24m014 -[31m1[39m[31m.[39m[31m0[39m[31m8[39m   -[31m0[39m[31m.[39m[31m881[39m  -[31m0[39m[31m.[39m[31m108[39m   0.557   0.589
    #> [90m# … with 1,093 more rows[39m

    ## example visualization 
    drivers_use <- c("Michael Schumacher", "Lewis Hamilton", "Sebastian Vettel", "Kimi Räikkönen")

    ## plot rating with plot_rating()
    gg <- plot_rating(rating, facet = TRUE, ncol = 4, driver_name = drivers_use)
    gg + ylim(-2, 8) + xlim(1984, 2019)

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### Assessing convergence via `get_mcmc()` and `coda` package

    ## load coda package 
    require(coda)
    #> Loading required package: coda

    ## convert estimates to the MCMC object 
    mcmc_obj <- get_mcmc(fit)

    ## who is the first driver in the data?
    names(mcmc_obj)[1]
    #> [1] "Adrian Campos"

    ## plot the first driver's rating estimate 
    plot(mcmc_obj[[1]])

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

#### Checking covergence with Geweke statistics

    ## plot the autocorrelation 
    coda::autocorr.plot(mcmc_obj[[1]])

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />


    ## geweke plot 
    coda::geweke.plot(mcmc_obj[[1]])

<img src="man/figures/README-unnamed-chunk-10-2.png" width="100%" />

### Running with Multiple Chains

`dyRank` package provides a function `bind_chains()` to work with
multiple chains.

-   Store multiple chains as a list
-   `bind_chains()` with `summarize = FALSE` behaves like `get_mcmc()`
-   `bind_chains()` with `summarize = behaves` like `get_rating()`

<!-- -->

    ## load additional package for parallel
    require(furrr)

    ## setup parallel 
    plan(multiprocess)

    ## run with multiple chains and store output as a list
    n_chains <- 3
    set.seed(1234)
    fit_nchains <- future_map(1:n_chains, function(chains) {
        fit_tmp <- dyRank(
            data       = f1_race, 
            var_rank   = "Pos",           
            var_player = "driver", 
            var_match  = "GP",
            var_time   = "year",
            driver_fix = "Timo Glock",
            mcmc = 100, burnin = 10, thin = 1,
            truncation = 3
        )
    }, .options = future_options(seed = TRUE))

#### Gelman-Rubin statistic

Multiple chains are requires to comute the Gelman-Rubin statistics
(`gelman.diag()` and `gelman.plot()` in `coda` package). `bind_chains()`
returns a list of `mcmc.list` object where each element of the returned
list corresponds to a `mcmc.list` object for each player (i.e., each
level of `var_player`).

    ## combine estimates into a single mcmc.list 
    m_list <- bind_chains(fit_nchains)

    ## gelman rubin statistics (for the first driver)
    coda::gelman.plot(m_list[[1]])

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

#### Estimated rating (all chains combined)

We can obtain the summary statistics of the estimated rating by
specifying `summarize = TRUE` option in `bind_chains()` function. As
`bind_chains()` with `summarize = TRUE` option returns an object of
`dyRank.summary` class, a user can use `plot_raing()` function to
visualize the estimates as in the case of a single chain.

    ## summarise estimates by combining chains 
    m_summary <- bind_chains(fit_nchains, summarize = TRUE)

    ## view the output 
    m_summary
    #> [90m# A tibble: 1,103 x 7[39m
    #>    driver         year  `2.5%`    `5%`   `50%`  `95%` `97.5%`
    #>    [3m[90m<chr>[39m[23m         [3m[90m<int>[39m[23m   [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m  [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m
    #> [90m 1[39m Adrian Campos  [4m1[24m987 -[31m1[39m[31m.[39m[31m81[39m   -[31m1[39m[31m.[39m[31m62[39m   -[31m0[39m[31m.[39m[31m518[39m   0.420  0.595 
    #> [90m 2[39m Adrian Campos  [4m1[24m988 -[31m2[39m[31m.[39m[31m14[39m   -[31m1[39m[31m.[39m[31m89[39m   -[31m0[39m[31m.[39m[31m722[39m   0.322  0.516 
    #> [90m 3[39m Adrian Sutil   [4m2[24m007 -[31m1[39m[31m.[39m[31m55[39m   -[31m1[39m[31m.[39m[31m45[39m   -[31m0[39m[31m.[39m[31m807[39m  -[31m0[39m[31m.[39m[31m284[39m -[31m0[39m[31m.[39m[31m213[39m 
    #> [90m 4[39m Adrian Sutil   [4m2[24m008 -[31m1[39m[31m.[39m[31m35[39m   -[31m1[39m[31m.[39m[31m24[39m   -[31m0[39m[31m.[39m[31m670[39m  -[31m0[39m[31m.[39m[31m112[39m  0.027[4m2[24m
    #> [90m 5[39m Adrian Sutil   [4m2[24m009 -[31m0[39m[31m.[39m[31m526[39m  -[31m0[39m[31m.[39m[31m469[39m   0.045[4m2[24m  0.595  0.721 
    #> [90m 6[39m Adrian Sutil   [4m2[24m010  0.252   0.308   0.766   1.35   1.53  
    #> [90m 7[39m Adrian Sutil   [4m2[24m011  0.423   0.493   0.991   1.47   1.55  
    #> [90m 8[39m Adrian Sutil   [4m2[24m012 -[31m0[39m[31m.[39m[31m0[39m[31m52[4m1[24m[39m  0.069[4m7[24m  0.780   1.52   1.61  
    #> [90m 9[39m Adrian Sutil   [4m2[24m013  0.027[4m5[24m  0.070[4m3[24m  0.619   1.08   1.14  
    #> [90m10[39m Adrian Sutil   [4m2[24m014 -[31m0[39m[31m.[39m[31m844[39m  -[31m0[39m[31m.[39m[31m765[39m  -[31m0[39m[31m.[39m[31m164[39m   0.316  0.434 
    #> [90m# … with 1,093 more rows[39m

    ## class 
    class(m_summary)
    #> [1] "tbl_df"         "tbl"            "data.frame"     "dyRank.summary"

Example: Multiple Ranking Types
-------------------------------

    ## load additional data 
    ## see ?f1_grid and ?f1_laptime for the details 
    data("f1_grid", package = "dyRank")
    data("f1_laptime", package = "dyRank")

    ## prepare data 
    f1_all <- bind_rows(f1_race, f1_grid, f1_laptime)

### Estimate rating via `hdyRank()`

    ## fit hierarhcal model
    set.seed(1234)
    fit_hier <- hdyRank(
        data       = f1_all,
        var_rank   = "Pos",           
        var_player = "driver", 
        var_match  = "GP",
        var_time   = "year",
        var_rank_type = "rank_type",
        driver_fix = "Timo Glock",
        mcmc = 100, burnin = 10, thin = 1, 
        truncation = 5
    )

**Arguments**

-   `hdyRank()` inherits all the arguments specified in `dyRank()`.
-   It takes the additional argument `var_rank_type`: A variable name
    (in character) of the rank type. When this variable takes a single
    level (i.e., only one type of ranking), `dyRank()` should be used
    instead.

### Estimated rating

    # obtain rating 
    rating_hier <- get_rating(fit_hier)

    ## example visualization 
    drivers_use <- c("Michael Schumacher", "Lewis Hamilton", "Sebastian Vettel", "Kimi Räikkönen")

    ## plot rating with plot_rating()
    gg <- plot_rating(rating_hier, facet = TRUE, ncol = 4, driver_name = drivers_use)
    gg + ylim(-2, 8) + xlim(1984, 2019)

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />
