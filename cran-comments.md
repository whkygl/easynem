## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Resubmission: easynem 1.0.1

This is a resubmission following feedback from Benjamin Altmann. 
Package version has been updated from 1.0.0 to 1.0.1.

I have addressed all points raised in the review:

1.  **Redundant Title/Description:**
    * Omitted the redundant "An R package" from the `Title` field and "This R package" from the `Description` field in the `DESCRIPTION` file, as requested.

2.  **References in DESCRIPTION:**
    * Added references for the methods used in the package to the `DESCRIPTION` file. These are formatted using `<doi:...>` and `<https:...>` tags as specified in the CRAN cookbook.

3.  **Missing `\value` tag for `nem_plot.Rd`:**
    * Added an `@return` tag (which generates `\value`) to the Roxygen comments for the generic `nem_plot` function located in `R/nem_plot.R`.
    * I then ran `devtools::document()` to correctly regenerate the `man/nem_plot.Rd` file with the new `\value` section.

4.  **Global environment modification (`<<-`):**
    * Removed the global assignment operator (`<<-`) found in `R/calc_lm2.R`.
    * The function has been refactored to use `base::split()` and `base::lapply()` to generate the list of results, which avoids writing to the global environment and complies with CRAN policies.

Thank you for your time and for reviewing my package.

