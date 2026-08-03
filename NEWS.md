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
