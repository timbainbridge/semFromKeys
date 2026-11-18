
<!-- README.md is generated from README.Rmd. Please edit that file -->

# semFromKeys

<!-- badges: start -->

[![R-CMD-check](https://github.com/timbainbridge/semFromKeys/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/timbainbridge/semFromKeys/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The ‘semFromKeys’ package was designed to streamline running ‘lavaan’
models with similar structures using keys lists to generate model code
instead of writing the code for models manually. Functions run
confirmatory factor analyses (CFAs; `cfa.from.keys()`), bi-factor models
(`bifactor.from.keys()`), exploratory factor analysis (EFA;
`efa.from.keys()`), latent variable correlations (`sem.cor()`),
incremental validity analysis (`r2.change()`), regular structural
equation path models (`sem.path()`), and exploratory structural equation
models (ESEM; `esem.from.keys()` and `esem.from.mods()`), where ESEM
factors predict CFA or bi-factor latent variables.

Functions take either keys lists or prior model outputs as inputs. A
keys list is simply a list of scales where each list element is a named
vector of scale items. With the keys list (or lists), the function
creates the relevant code for the models and runs the models, avoiding
the necessity of writing out each model’s code manually.

Functions that compute measurement models include `cfa.from.keys()` and
`bifactor.from.keys()`, which create and run a series of models based on
keys indicating each of the factors in the models, and
`efa.from.keys()`, which uses a keys list to create a target rotation
for a single EFA. Structural models include `sem.cor()`, `sem.path()`,
`r2.change()`, `esem.from.keys()`, and `esem.from.mods()`.

`sem.cor()` takes fitted CFA models and runs an SEM for each combination
of scales computing a latent variable correlation for each. These are
then collected into a correlation table similar to the `stats::cor()`
function. `sem.path()` takes fitted CFA or bi-factor models and a path
equation, and produces a single SEM model. `r2.change()` takes keys
lists, and names for the focal variable (X), the outcome variable (Y),
and a vector of variables to test incremental validity against (Z). The
function produces an r-squared change figure and optionally bootstrap
confidence intervals. Finally, for exploratory structural equation
models (ESEM), there are two options. In each case, EFA factors predict
scale factors. The first option (`esem.from.keys()`) takes EFA and CFA
keys as inputs and uses Rosseel and Loh’s (2022)
Structural-After-Measurement (SAM) method to prevent interpretational
confounding (Burt, 1976), and the second (`esem.from.mods()`) takes a
fitted EFA model and fitted CFA or bi-factor models and uses Burt’s
(1976) 2-stage procedure to prevent interpretational confounding. The
ESEM models were designed to run analyses analogous to those of
Bainbridge, Ludeke, and Smillie (2022).

For sets of models that take a long time to run, code has been included
to allow the first run to save outputs that can be checked against in
subsequent runs. If nothing has changed, then the previous outputs are
returned, saving the time (and energy) of running them again. For this
feature to work, the R version has to be 4.0 or later and a cache
directory will have to be set with the `cache.setup()` function, which,
by default, configures a cache directory in the users’ cache as
determined by the operating system. It can alternatively be set as a
subdirectory within the current project or, if not using a project, the
current working directory. Once the cache is set, `save_out = TRUE` can
be included in function calls to save the relevant outputs, and
`check = TRUE` can be included to look for previous outputs and only run
models where something has changed. The feature means that small changes
in data cleaning or an error in one model’s key need not necessitate
re-running every model, only those where something has changed. These
functions are facilitated by the `sem.check()` and `check.cache()`
functions, which are largely used as derivative functions by other
functions rather than for use by regular users.

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

## Examples

The following examples generate keys, run CFAs and an EFA using these
keys, compute correlations between CFA latent variables, check a scale’s
incremental validity, and run ESEMs.

### CFAs

Keys are simply a list of scales and their items.

``` r
library(semFromKeys)
keys <- list(
  grit_p = paste0("grit_p_", 1:6),
  grit_c = paste0("grit_c_", 1:6),
  hope_a = paste0("hope_a_", 1:4),
  hope_p = paste0("hope_p_", 1:4)
)
```

The lists should look something like this:

``` r
keys
#> $grit_p
#> [1] "grit_p_1" "grit_p_2" "grit_p_3" "grit_p_4" "grit_p_5" "grit_p_6"
#> 
#> $grit_c
#> [1] "grit_c_1" "grit_c_2" "grit_c_3" "grit_c_4" "grit_c_5" "grit_c_6"
#> 
#> $hope_a
#> [1] "hope_a_1" "hope_a_2" "hope_a_3" "hope_a_4"
#> 
#> $hope_p
#> [1] "hope_p_1" "hope_p_2" "hope_p_3" "hope_p_4"
```

Once keys are created, the CFAs can be run. By default, the function
produces messages of progress. These can help identify which models
produced errors or warnings or to keep track of progress for collections
of models with long run times.

``` r
cfa_fit <- cfa.from.keys(keys, BFIGritHope, fit_save = TRUE)
#> Fitting models
#> 1 / 4   grit_p
#> 2 / 4   grit_c
#> 3 / 4   hope_a
#> 4 / 4   hope_p
#> Generating parameter estimates
#> 1 / 4   grit_p
#> 2 / 4   grit_c
#> 3 / 4   hope_a
#> 4 / 4   hope_p
#> Generating model fit statistics
#> 1 / 4   grit_p
#> 2 / 4   grit_c
#> 3 / 4   hope_a
#> 4 / 4   hope_p
```

Results can be examined; for example, standard ‘lavaan’ summaries:

``` r
lavaan::summary(cfa_fit$fit$grit_c)
#> lavaan 0.7-3 ended normally after 12 iterations
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
#>   Browne's residual (NT model-based) test             
#>   Test statistic                                58.635
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
#> grit_p 0.9143581 0.12213711
#> grit_c 0.9328014 0.12550173
#> hope_a 0.9796273 0.12148480
#> hope_p 0.9978190 0.03467277
```

These models can be used to examine the measurement characteristics of
the scales or to calculate latent variable model-based reliability
scores (e.g., with
`sapply(cfa_fit$fit, function(x) semTools::compRelSEM(x)[[1]])` for
composite reliability, Jöreskog, 1971). They are also used as input into
other functions (below).

Bifactor models can be run with a similar, albeit more complex, method.
See `?bifactor.from.keys` for details.

### EFAs

As for CFAs, an EFA can be run from a keys list. In this case, the keys
list indicates factor that items are expected to load on rather than
separate models. These are used to generate a target rotation to help
ensure the EFA matches expectations.

``` r
keys_e <- sapply(
  paste0("bfi_", c("e", "a", "c", "n", "o")),
  function(x) {
    as.vector(sapply(1:3, function(y) paste0(x, paste0(y, "_", 1:4))))
  },
  simplify = FALSE
)
keys_e
#> $bfi_e
#>  [1] "bfi_e1_1" "bfi_e1_2" "bfi_e1_3" "bfi_e1_4" "bfi_e2_1" "bfi_e2_2"
#>  [7] "bfi_e2_3" "bfi_e2_4" "bfi_e3_1" "bfi_e3_2" "bfi_e3_3" "bfi_e3_4"
#> 
#> $bfi_a
#>  [1] "bfi_a1_1" "bfi_a1_2" "bfi_a1_3" "bfi_a1_4" "bfi_a2_1" "bfi_a2_2"
#>  [7] "bfi_a2_3" "bfi_a2_4" "bfi_a3_1" "bfi_a3_2" "bfi_a3_3" "bfi_a3_4"
#> 
#> $bfi_c
#>  [1] "bfi_c1_1" "bfi_c1_2" "bfi_c1_3" "bfi_c1_4" "bfi_c2_1" "bfi_c2_2"
#>  [7] "bfi_c2_3" "bfi_c2_4" "bfi_c3_1" "bfi_c3_2" "bfi_c3_3" "bfi_c3_4"
#> 
#> $bfi_n
#>  [1] "bfi_n1_1" "bfi_n1_2" "bfi_n1_3" "bfi_n1_4" "bfi_n2_1" "bfi_n2_2"
#>  [7] "bfi_n2_3" "bfi_n2_4" "bfi_n3_1" "bfi_n3_2" "bfi_n3_3" "bfi_n3_4"
#> 
#> $bfi_o
#>  [1] "bfi_o1_1" "bfi_o1_2" "bfi_o1_3" "bfi_o1_4" "bfi_o2_1" "bfi_o2_2"
#>  [7] "bfi_o2_3" "bfi_o2_4" "bfi_o3_1" "bfi_o3_2" "bfi_o3_3" "bfi_o3_4"
```

After the keys list has been created, the model can be run similarly to
the CFAs. When running the model, fit measures can be restricted to
speed up estimation if not all are required (as for ‘lavaan’s’
`lavaan::fitMeasures()` function).

``` r
efa_fit <- efa.from.keys(
  keys_e, BFIGritHope, check = FALSE, fit_save = TRUE,
  fit_measures = c("chisq", "df", "pvalue", "cfi")
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
# lavaan::summary(efa_fit$fit)  # Standard lavaan summary
efa_fit$fit_measures          # Fit measures
#>        chisq   df pvalue       cfi
#> efa 4808.621 1480      0 0.7811926
```

### Correlations

The fitted CFA models can be used to calculate latent variable
correlations with the `sem.cor()` function. In this example, Burt’s
2-stage procedure is used by setting `nagy = FALSE` to save time, but in
many cases Nagy and colleagues’ (2017) method will be superior and is
the default. See `?sem.cor()` for further details.

``` r
latent_cors <- sem.cor(BFIGritHope, cfa_fit$fit, nagy = FALSE)
#> Fitting models
#> 1 / 6   grit_p.grit_c
#> 2 / 6   grit_p.hope_a
#> 3 / 6   grit_p.hope_p
#> 4 / 6   grit_c.hope_a
#> 5 / 6   grit_c.hope_p
#> 6 / 6   hope_a.hope_p
#> Generating parameter estimates
#> 1 / 6   grit_p.grit_c
#> 2 / 6   grit_p.hope_a
#> 3 / 6   grit_p.hope_p
#> 4 / 6   grit_c.hope_a
#> 5 / 6   grit_c.hope_p
#> 6 / 6   hope_a.hope_p
```

``` r
round(latent_cors$cor_mat, 3)
#>        grit_p grit_c hope_a hope_p
#> grit_p  1.000  0.496  0.899  0.833
#> grit_c  0.496  1.000  0.372  0.300
#> hope_a  0.899  0.372  1.000  0.928
#> hope_p  0.833  0.300  0.928  1.000
```

### Incremental Validity

Incremental validity using aggregate scores is invalid (Westfall &
Yarkoni, 2016). The solution is to use latent variables. This can be
done with the `r2.change()` function. Inputs include keys lists, the
focal variable (X), the outcome variable (Y), and a list of variables to
test incremental validity against (Z). In the following example, the
incremental validity of the consistency facet of grit (“grit_c”) to
incrementally predict the perseverance facet of grit (“grit_p”) over
hope is tested. Hope is measured as a bi-factor model with one of its
pathways facet (“hope_p”) having a group factor.

``` r
keys_g <- list(hope = c(keys$hope_p, paste0("hope_a_", 1:4)))
keys_b <- list(hope = "hope_p")
dr2 <- r2.change(
  BFIGritHope, X = "grit_c", Y = "grit_p", Z = "hope",
  keys = keys, keys_g = keys_g, keys_b = keys_b, fit_save = FALSE
)
#> Running baseline models
#> Fitting models
#> 1 / 2   full
#> 2 / 2   reduced
#> Generating parameter estimates
#> 1 / 2   full
#> 2 / 2   reduced
dr2$dr2
#>         r2        dr2 
#> 0.39996270 0.02033731
```

The primary output includes the r-squared value without `grit_c` and the
incremental change in r-squared by adding `grit_c`. A bootstrap
confidence interval could have been calculated by specifying a value of
`boot` in the `r2.change()` function call.

### ESEM

Two function run ESEM with EFA factors predicting measurement model
latent variables. The first of these—`esem.from.keys()`—uses keys lists
to run the models using Rosseel and Loh’s (2022) “Structure After
Measurement” (SAM) method, and the second—`esem.from.mods()`—uses fitted
CFA, bi-factor, and EFA models to run the models using Burt’s (1976)
2-stage procedure. In general, the SAM method is superior, so
`esem.from.keys()` should be used whenever possible. However,
`esem.from.keys()` currently only supports single-factor measurement
models and `esem.from.mods` should be used for bi-factor measurement
models. See `?esem.from.keys` for details.

``` r
esam_fit <- esem.from.keys(BFIGritHope, keys_e, keys, fit_save = FALSE)
#> Fitting models
#> 1 / 4   grit_p
#> 2 / 4   grit_c
#> 3 / 4   hope_a
#> 4 / 4   hope_p
#> Generating parameter estimates
#> 1 / 4   grit_p
#> 2 / 4   grit_c
#> 3 / 4   hope_a
#> 4 / 4   hope_p
```

The function provides standard ‘lavaan’ outputs, as well as r-squared
values and regression parameters.

``` r
lavaan::summary(esam_fit$fit$grit_c)  # lavaan summary for the SAM method
#> This is lavaan 0.7-3 -- using the SAM approach to SEM
#> 
#>   SAM method                                    GLOBAL
#>   Number of measurement blocks                       2
#>   Estimator measurement part                        ML
#>   Estimator  structural part                        ML
#> 
#>   Number of observations                           388
#>   Number of missing patterns                         1
#> 
#> Summary Information Measurement Part:
#> 
#>   Block                        Latent Nind    Chisq   Df
#>       1 bfi_e,bfi_a,bfi_c,bfi_n,bfi_o   60 4808.621 1480
#>       2                        grit_c    6   64.001    9
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                              5500.324    3876.504
#>   Degrees of freedom                              1824        1824
#>   P-value (Chi-square)                           0.000       0.000
#>   Scaling correction factor                                  1.419
#>     Yuan-Chan (2002) correction                                   
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                              Twostep
#>   Information                                 Observed
#>   Observed information based on                Hessian
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   grit_c ~                                            
#>     bfi_e            -0.155    0.049   -3.150    0.002
#>     bfi_a             0.077    0.050    1.548    0.122
#>     bfi_c             0.432    0.059    7.374    0.000
#>     bfi_n            -0.367    0.056   -6.548    0.000
#>     bfi_o             0.100    0.049    2.029    0.042
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   bfi_e ~~                                            
#>     bfi_a             0.144    0.058    2.458    0.014
#>     bfi_c             0.172    0.057    3.020    0.003
#>     bfi_n            -0.259    0.056   -4.658    0.000
#>     bfi_o             0.161    0.057    2.810    0.005
#>   bfi_a ~~                                            
#>     bfi_c             0.276    0.054    5.110    0.000
#>     bfi_n            -0.244    0.056   -4.371    0.000
#>     bfi_o             0.233    0.056    4.195    0.000
#>   bfi_c ~~                                            
#>     bfi_n            -0.421    0.048   -8.709    0.000
#>     bfi_o             0.285    0.053    5.358    0.000
#>   bfi_n ~~                                            
#>     bfi_o            -0.194    0.056   -3.495    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .grit_c            0.492    0.085    5.793    0.000
```

``` r
round(esam_fit$r2, 3)
#>           R2    se ci.lower ci.upper
#> grit_p 0.731 0.029    0.675    0.788
#> grit_c 0.508 0.042    0.426    0.590
#> hope_a 0.782 0.024    0.736    0.828
#> hope_p 0.610 0.038    0.535    0.685
```

``` r
esam_fit$b$grit_c
#>       rhs est.std    se      z pvalue ci.lower ci.upper
#> 307 bfi_e  -0.155 0.048 -3.219  0.001   -0.249   -0.060
#> 308 bfi_a   0.077 0.049  1.556  0.120   -0.020    0.174
#> 309 bfi_c   0.432 0.050  8.675  0.000    0.334    0.530
#> 310 bfi_n  -0.367 0.050 -7.328  0.000   -0.465   -0.269
#> 311 bfi_o   0.100 0.049  2.049  0.040    0.004    0.196
```

The `esem.from.mods()` function provides the same outputs as
`esem.from.keys()`, only with somewhat biased standard error and fit
statistic estimates.

``` r
esem_fit <- esem.from.mods(
  BFIGritHope, efa_fit$fit, cfa_fit$fit, fit_save = FALSE
)
#> Fitting models
#> 1 / 4   grit_p
#> 2 / 4   grit_c
#> 3 / 4   hope_a
#> 4 / 4   hope_p
#> Generating parameter estimates
#> 1 / 4   grit_p
#> 2 / 4   grit_c
#> 3 / 4   hope_a
#> 4 / 4   hope_p
```

``` r
# Not run due to length
# lavaan::summary(esem_fit$fit$grit_c)  # Standard lavaan summary
round(esem_fit$r2, 3)
#>           R2    se ci.lower ci.upper
#> grit_p 0.731 0.035    0.663    0.799
#> grit_c 0.508 0.041    0.428    0.588
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

### SEM

Finally, the `sem.path()` function runs standard structural equation
models (SEM). The function takes fitted CFA models for the measurement
models and lavaan code for the structural path(s) and uses Rosseel and
Loh’s (2022) method to run the model.

``` r
path <- "grit_p ~ grit_c + hope_p\nhope_p ~ hope_a"
sem_fit <- sem.path(path, BFIGritHope, cfa_fit$fit, fit_save = TRUE)
#> Fitting models
#> 1 / 1   sam
#> Generating parameter estimates
#> 1 / 1   sam
#> Generating model fit statistics
#> 1 / 1   sam
#> lavaan NOTE:  
#>    the fit measures are computed for the structural part only, conditional on 
#>    the (fixed) measurement model of step 1.
```

The function outputs include a standard lavaan summary, r-squared
values, regression path coefficients and correlations, and fit measures.

``` r
print(lavaan::summary(sem_fit$fit))      # Standard lavaan summary
#> This is lavaan 0.7-3 -- using the SAM approach to SEM
#> 
#>   SAM method                                     LOCAL
#>   Mapping matrix M method                           ML
#>   Number of measurement blocks                       4
#>   Estimator measurement part                        ML
#>   Estimator  structural part                        ML
#> 
#>   Number of observations                           388
#>   Number of missing patterns                         1
#> 
#> Summary Information Measurement + Structural:
#> 
#>   Block Latent Nind  Chisq Df
#>       1 grit_c    6 64.001  9
#>       2 grit_p    6 61.092  9
#>       3 hope_a    4 13.453  2
#>       4 hope_p    4  2.933  2
#> 
#>   Model-based reliability latent variables:
#> 
#>   grit_p grit_c hope_a hope_p
#>    0.809   0.85  0.832  0.794
#> 
#>   Summary Information Structural part:
#> 
#>   chisq.scaled df.scaled pvalue.scaled cfi.robust rmsea.robust  srmr
#>             NA         2            NA         NA           NA 0.032
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                              Twostep
#>   Information                                 Observed
#>   Observed information based on                Hessian
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   grit_p ~                                            
#>     grit_c            0.271    0.048    5.582    0.000
#>     hope_p            0.751    0.066   11.425    0.000
#>   hope_p ~                                            
#>     hope_a            0.928    0.065   14.358    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   grit_c ~~                                           
#>     hope_a            0.372    0.052    7.180    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .grit_p            0.240    0.114    2.103    0.035
#>     grit_c            1.000                           
#>     hope_a            1.000                           
#>    .hope_p            0.140    0.120    1.164    0.244
```

``` r
sem_fit$b      # Standardised regression path coefficients
#>     y_var  x_var est.std    se       z pvalue ci.lower ci.upper
#> 22 grit_p grit_c   0.268 0.045   5.938      0    0.180    0.357
#> 23 grit_p hope_p   0.744 0.032  23.196      0    0.682    0.807
#> 24 hope_p hope_a   0.928 0.009 102.887      0    0.910    0.945
```

``` r
sem_fit$r2        # R^2 values
#>    y_var        R2         se  ci.lower  ci.upper
#> 1 grit_p 0.7641686 0.02644630 0.7123348 0.8160024
#> 2 hope_p 0.8604515 0.01672613 0.8276689 0.8932341
```

``` r
sem_fit$cors          # Correlations
#>       lhs op    rhs est.std    se    z pvalue ci.lower ci.upper
#> 21 grit_c ~~ hope_a   0.372 0.052 7.18      0    0.271    0.474
```

``` r
# Fit measures
round(sem_fit$fit_measures[c("chisq", "df", "pvalue", "cfi", "rmsea")], 3)
#>   chisq      df  pvalue     cfi   rmsea 
#> 162.576   2.000   0.000   0.895   0.455
```

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

Nagy, G., Brunner, M., Lüdtke, O., and Greiff, S. (2017). Extension
Procedures for Confirmatory Factor Analysis. Journal of Experimental
Education, 85(4), 574-596.
<https://doi.org/10.1080/00220973.2016.1260524>.

Rosseel, Y. & Loh, W. W. (2022). A structural after measurement approach
to structural equation modeling. Psychological Methods, 29(3), 561-588.
<https://doi.org/10.1037/met0000503>.

Westfall, J., and Yarkoni, T. (2016). Statistically controlling for
confounding constructs is harder than you think. PLoS ONE, 11(3), 1-23.
<https://doi.org/10.1371/journal.pone.0152719>.

<!-- You'll need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
