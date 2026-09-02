# semFromKeys 0.5.4

* Removed the rstudioapi suggestion as it is no longer used.
* Fixed a couple of small issues in the README file.
* Fixed an issue whereby correlations could be inverted in `sem.cor` when `nagy = TRUE`.
* Fixed a bug where correlations were not correctly selected into the matrix when `fit_x` was specified in `sem.cor`.

# semFromKeys 0.5.3

* Added p-values to `sem.cor` output.
* Added `name` as an option for finding files to clean from the cache in `cache.clean`.
* Fixed a bug where correlated residuals in `sem.cor` input where causing an unexpected error.
* Fixed a bug whereby pattern matching to select correlations for cells in `sem.cor` could match more than one correlation, resulting in an error.
* Switched `cache.clean` from using `unlink` to using `remove.files` to negate a bug in `cache.clean` whereby directories were sometimes not removed.
* Cache directories should now match when running `cache.setup()` in interactive and non-interactive sessions.

# semFromKeys 0.5.2

* Added item residual correlations to `sem.cor` output.
* Simplified how `items` works in `sem.cor`.
* Changed `efa.from.keys` output to not have the unnecessary third layer (e.g., `fit$fit$efa` became `fit$fit`).

# semFromKeys 0.5.1

* Added `ordered` as an argument for measurement model functions.

# semFromKeys 0.5.0

* Added the `esem.from.keys` function, which takes EFA and CFA keys to run ESEM.

# semFromKeys 0.4.0

* Added the `sem.path` function, which takes CFA outputs and SEM path code to run an SEM model.

# semFromKeys 0.3.3

* Fixed a bug in `sem.cor()` whereby variable names were not correctly extracted when '.'s were included in the name.
* Added a missing error check, such that cryptic, "missing variables" errors would not appear in `sem.check()` due to mismatched `mods` and `keys_s` names.

# semFromKeys 0.3.2

* Fixed a bug for length 1 `cfa_fit` and `bif_fit` in `esem.from.mods`.
* Updated `esem.from.mods` documentation.

# semFromKeys 0.3.1

* Fixed minor bugs in two esem.from.mods error messages.

# semFromKeys 0.3.0

* Added the `sem.cor()` function, which takes CFA outputs and produces a correlation matrix between latent variables and, optionally, single items.

# semFromKeys 0.2.4

* Updated `cache.clean()` to only work in non-interactive sessions when `interactive = FALSE`.
* Changed `cache.setup()` and `cache.clean()` examples to `\dontrun` to prevent saved elements from remaining after CRAN checks of `\donttest`.

# semFromKeys 0.2.3

* Fixed esem.from.mods returning an empty `fit_measures` entry when `fit_save = FALSE`.

# semFromKeys 0.2.2

* Fixed the option issue that remained at the last CRAN submission (in esem.from.mods.R).
* Default cache directory now includes a subdirectory of the project name if run within an RStudio project.

# semFromKeys 0.2.1

* Fixed issues in initial CRAN submission.
* Changed setting an option for cache to using an environment variable.
* Added a reference to Description.

# semFromKeys 0.2.0

* Initial CRAN version.

# semFromKeys 0.1.1

* Added caching with `cache.setup()` and `cache.clean()` for use with
`save_out = TRUE` and `check = TRUE`.

# semFromKeys 0.1.0

* Initial github version.
