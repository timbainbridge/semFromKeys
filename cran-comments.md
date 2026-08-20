## Latest submission

### Notes

* Fixed an important bug whereby the `sem.cor` function would fail if a factor name included a '.'.
* Added two functions (i.e., `sem.path` and `esem.from.keys`).
* Various updates to other functions (see NEWS.md since v0.3.0).

## 28-7-2026 submission

### Issue in \donttest 

* From https://cran.r-project.org/web/checks/check_results_semFromKeys.html.
* Found the following files/directories:
  '\~/.cache/R/semFromKeys' '\~/.cache/R/semFromKeys/cfa'
  '\~/.cache/R/semFromKeys/cfa/cfa_fit.rds'
  '\~/.cache/R/semFromKeys/cfa/cfa_fit_m.rds'
  '\~/.cache/R/semFromKeys/cfa/cfa_hash.rds'
  '\~/.cache/R/semFromKeys/cfa/cfa_mod.rds'
  '\~/.cache/R/semFromKeys/cfa/cfa_par.rds'
  '\~/.cache/R/semFromKeys/cfa/cfa_params.rds'

### R CMD check results

0 errors | 0 warnings | 0 notes

### Note

`setup.cache()` and `cache.clean()` create and set up, and clean a cache directory. The `\donttest` examples did not aggressively clean the cache, so model outputs remained when the examples were run. I could have fixed it by including an aggressive version of `cache.clean()` in the examples but, if a user has set up the same cache directory as the example, they may delete results stored there unintentionally because `interactive = FALSE` would have to be set to work in testing. Therefore, I have chosen to wrap the examples in `\dontrun` instead of `\donttest`, so the examples remain useful to users without risking deleting their data, while also passing CRAN's automated tests.

## 17-7-2026 submission

### R CMD check results

0 errors | 0 warnings | 1 note

* Possibly misspelled words in DESCRIPTION:
  Bainbridge (17:5)
  Ludeke (17:24)
  Smillie (17:41)

### Notes

* Possibly misspelled words are correctly spelled names.
* Removed the code that set an option from esem.from.mods.R.

## 3-7-2026 submission

### R CMD check results

0 errors | 0 warnings | 0 note

### Notes

* Changed setting an option for cache to using an environment variable.
* Added a reference to Description.

## First submission (21-6-2026)

### R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
