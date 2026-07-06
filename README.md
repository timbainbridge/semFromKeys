
<!-- README.md is generated from README.Rmd. Please edit that file -->

# semFromKeys

<!-- badges: start -->

[![R-CMD-check](https://github.com/timbainbridge/semFromKeys/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/timbainbridge/semFromKeys/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The ‘semFromKeys’ package was designed to streamline running ‘lavaan’
models with similar structures using keys lists to generate model code
instead of writing out the code for models manually. Currently, the code
can run confirmatory factor analyses (CFAs), bifactor models,
exploratory factor analyses (EFAs), latent variable correlations, and
exploratory structural equation models (ESEMs). For CFAs and bifactor
models, the code creates and runs a series of models based on keys
indicating each of the factors in the models. For EFAs, keys list are
used to create a target rotation for a single EFA. For latent variable
correlations, the code takes fitted CFA models and runs models with
correlations between all combinations or latent variables or selected
latent variables with other selected latent variables. For ESEMs, the
code takes a fitted EFA model and fitted CFA and/or bifactor models and
runs an ESEM for each CFA or bifactor model input where the EFA factors
predict the series of CFA/bifactor factors.

In the structural models (i.e., the latent variable correlations and the
ESEM), Burt’s (1976) 2-stage procedure is used to prevent
interpretational confounding. The ESEM models were designed to run
analyses equivalent to that of Bainbridge, Ludeke, and Smillie (2022).

For more sets of models that take a long time to run, code has been
included to allow the first run to save outputs that can be checked
against in subsequent runs. If nothing has changed, then the previous
outputs are returned, saving the time (and energy) of running them
again. To get this feature to work, the R version has to be 4.0 or later
and a cache directory will have to be set with the `cache.setup()`
function, which, by default, configures a cache directory in the users’
cache as determined by the operating system. It can alternatively be set
as a subdirectory within the current project or, if not using a project,
the current working directory. Once the cache is set, `save_out = TRUE`
can be included in function calls to save the relevant outputs, and
`check = TRUE` can be included to look for previous outputs and only run
models where something has changed.

Given that the package enables creating files in a cache directory, the
`cache.clean()` function has also been included to help clean up files.
To comply with CRAN policies, the cache directory is set as a temporary
environment variable, so it has to be set each time the global
environment is cleared.

## Installation

You can install the development version of ‘semFromKeys’ from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("timbainbridge/semFromKeys")
```

You can install the stable CRAN version with:

``` r
install.packages("semFromKeys")
```

## Example

The following example generates keys, runs CFAs and an EFA using these
keys, and uses outputs from these to run ESEMs.

### CFAs

In this case, keys can be created from names in the dataset but they can
also be created with simple code to generate a list.

``` r
library(semFromKeys)
keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
keys <- sapply(
  keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
)
```

The lists should look something like this:

``` r
keys
#> $grit_c
#> [1] "grit_c_1" "grit_c_2" "grit_c_3" "grit_c_4" "grit_c_5" "grit_c_6"
#> 
#> $grit_p
#> [1] "grit_p_1" "grit_p_2" "grit_p_3" "grit_p_4" "grit_p_5" "grit_p_6"
#> 
#> $hope_a
#> [1] "hope_a_1" "hope_a_2" "hope_a_3" "hope_a_4"
#> 
#> $hope_p
#> [1] "hope_p_1" "hope_p_2" "hope_p_3" "hope_p_4"
```

Once keys are created, the CFAs can be run. The function produces
messages of progress. These can help identify which models produced
errors or warnings or to keep track of progress for collections of
models with long run times.

``` r
cfa_fit <- cfa.from.keys(keys, BFIGritHope, fit_save = TRUE)
#> Fitting models
#> 1 / 4   grit_c
#> 2 / 4   grit_p
#> 3 / 4   hope_a
#> 4 / 4   hope_p
#> Generating parameter estimates
#> 1 / 4   grit_c
#> 2 / 4   grit_p
#> 3 / 4   hope_a
#> 4 / 4   hope_p
#> Generating model fit statistics
#> 1 / 4   grit_c
#> 2 / 4   grit_p
#> 3 / 4   hope_a
#> 4 / 4   hope_p
```

Results can be examined. For example, standard ‘lavaan’ summaries:

``` r
lavaan::summary(cfa_fit$fit$grit_c)
#> lavaan 0.6-21 ended normally after 12 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        18
#> 
#>   Number of observations                           388
#>   Number of missing patterns                         1
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                64.001
#>   Degrees of freedom                                 9
#>   P-value (Chi-square)                           0.000
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Observed
#>   Observed information based on                Hessian
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   grit_c =~                                           
#>     grit_c_1          0.904    0.053   17.204    0.000
#>     grit_c_2          0.832    0.057   14.560    0.000
#>     grit_c_3          0.685    0.059   11.656    0.000
#>     grit_c_4          0.794    0.056   14.089    0.000
#>     grit_c_5          0.879    0.057   15.317    0.000
#>     grit_c_6          0.762    0.063   12.173    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .grit_c_1          3.235    0.058   55.482    0.000
#>    .grit_c_2          2.856    0.061   47.163    0.000
#>    .grit_c_3          3.088    0.059   52.185    0.000
#>    .grit_c_4          3.219    0.059   54.439    0.000
#>    .grit_c_5          2.938    0.062   47.644    0.000
#>    .grit_c_6          3.376    0.064   52.736    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .grit_c_1          0.501    0.052    9.722    0.000
#>    .grit_c_2          0.731    0.064   11.400    0.000
#>    .grit_c_3          0.889    0.072   12.431    0.000
#>    .grit_c_4          0.726    0.063   11.539    0.000
#>    .grit_c_5          0.704    0.064   11.070    0.000
#>    .grit_c_6          1.010    0.081   12.452    0.000
#>     grit_c            1.000
```

And selected fit measures:

``` r
cfa_fit$fit_measures[, c("cfi", "rmsea")]
#>              cfi      rmsea
#> grit_c 0.9328014 0.12550173
#> grit_p 0.9143581 0.12213711
#> hope_a 0.9796273 0.12148480
#> hope_p 0.9978190 0.03467277
```

These models can be used to examine the measurement characteristics of
the scales or to calculate latent variable model-based reliability
scores (e.g., with
`sapply(cfa_fit$fit, function(x) semTools::compRelSEM(x)[[1]])` for
composite reliability, Jöreskog, 1971).

### Latent variable correlations

It is also possible to examine correlations between the latent variables
calculated above.

``` r
latent_cors <- sem.cor(BFIGritHope, cfa_fit$fit)
#> Fitting models
#> 1 / 6   grit_c.grit_p
#> 2 / 6   grit_c.hope_a
#> 3 / 6   grit_c.hope_p
#> 4 / 6   grit_p.hope_a
#> 5 / 6   grit_p.hope_p
#> 6 / 6   hope_a.hope_p
#> Generating parameter estimates
#> 1 / 6   grit_c.grit_p
#> 2 / 6   grit_c.hope_a
#> 3 / 6   grit_c.hope_p
#> 4 / 6   grit_p.hope_a
#> 5 / 6   grit_p.hope_p
#> 6 / 6   hope_a.hope_p
latent_cors$cor_mat
#>           grit_c    grit_p    hope_a    hope_p
#> grit_c 1.0000000 0.4962176 0.3724859 0.3002280
#> grit_p 0.4962176 1.0000000 0.8993154 0.8325711
#> hope_a 0.3724859 0.8993154 1.0000000 0.9276052
#> hope_p 0.3002280 0.8325711 0.9276052 1.0000000
```

### EFAs

As for CFAs, an EFA can be run from a keys list. In this case, the keys
list indicates factor that items are expected to load on rather than
separate models. These are used to generate a target rotation to help
ensure the EFA matches expectations.

``` r
keys_e0 <- paste0("bfi_", c("e", "a", "c", "n", "o"))
keys_e <- sapply(
  keys_e0,
  function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))],
  simplify = FALSE
)
```

After the keys list has been created, the model can be run similarly to
the CFAs. When running the model, fit measures can be restricted to
speed up estimation if not all are required (as for ‘lavaan’s’
`lavaan::fitMeasures()` function).

``` r
efa_fit <- efa.from.keys(
  keys_e, BFIGritHope, check = FALSE, fit_save = TRUE,
  fit_measures = c("chisq", "df", "pvalue", "bic")
)
#> Fitting models
#> 1 / 1   efa
#> Generating parameter estimates
#> 1 / 1   efa
#> Generating model fit statistics
#> 1 / 1   efa
```

EFA results can be examined in a similar way to the CFAs.

``` r
# Not run due to length
# lavaan::summary(efa_fit$fit$efa)  # Standard lavaan summary
efa_fit$fit_measures                # Fit measures
#>        chisq   df pvalue      bic
#> efa 4808.621 1480      0 62887.71
```

### ESEM

Finally, outputs from these models can be used as inputs into ESEMs
where the scales of the CFAs are regressed on the EFA factors.

``` r
esem_fit <- esem.from.mods(
  efa_fit$fit$efa, cfa_fit$fit, data = BFIGritHope, fit_save = FALSE
)
#> Fitting models
#> 1 / 4   grit_c
#> 2 / 4   grit_p
#> 3 / 4   hope_a
#> 4 / 4   hope_p
#> Generating parameter estimates
#> 1 / 4   grit_c
#> 2 / 4   grit_p
#> 3 / 4   hope_a
#> 4 / 4   hope_p
```

The function provides standard ‘lavaan’ outputs, as well as r-squared
values and regression parameters.

``` r
# Not run due to length
# lavaan::summary(esem_fit$fit$grit_c)  # Standard lavaan summary
round(esem_fit$r2, 3)
#>           R2    se ci.lower ci.upper
#> grit_c 0.508 0.041    0.428    0.588
#> grit_p 0.731 0.035    0.663    0.799
#> hope_a 0.782 0.030    0.724    0.840
#> hope_p 0.610 0.040    0.532    0.688
```

``` r
esem_fit$b$grit_c
#>       rhs est.std    se      z pvalue ci.lower ci.upper
#> 388 bfi_e  -0.155 0.046 -3.369  0.001   -0.244   -0.065
#> 389 bfi_a   0.077 0.048  1.607  0.108   -0.017    0.171
#> 390 bfi_c   0.432 0.048  8.995  0.000    0.338    0.526
#> 391 bfi_n  -0.367 0.048 -7.606  0.000   -0.461   -0.272
#> 392 bfi_o   0.100 0.048  2.112  0.035    0.007    0.193
```

To take advantage of functions’ time-saving `check = TRUE` for
subsequent running of code, a cache directory will need to be set. To
see how to do this, see `?cache.setup`.

<!-- You'll need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->

## References

Bainbridge, T. F., Ludeke, S. G., & Smillie, L. D. (2022). Evaluating
the Big Five as an organizing framework for commonly used psychological
trait scales. Journal of Personality and Social Psychology, 122(4),
749-777. <https://doi.org/10.1037/pspp0000395>.

Burt, R. S. (1976). Interpretational confounding of unobserved variables
in Structural Equation Models. Sociological Methods & Research, 5(1),
3-52. <https://doi.org/10.1177/004912417600500101>.

Jöreskog, K. G. (1971). Statistical Analysis of Sets of Congeneric
Tests. Psychometrika, 36(2), 109-133.
<https://doi.org/10.1007/BF02291393>.
