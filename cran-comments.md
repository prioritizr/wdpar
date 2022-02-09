Dear CRAN volunteers,

Thank you for reviewing this submission. The main updates in this version include (i) fixing compatibility issues with the Protected Planet website to enable users to download data, (ii) improving data cleaning procedures, (iii) updating code and documentation to reflect the recent addition of the World Database on Other Effective Area-Based Conservation Measures (WDOECM) to the Protected Planet website, and (iv) removing unused package dependencies. The package now also includes the prepr R package as an optional dependency to augment data processing (https://github.com/dickoa/prepr).

Cheers,

Jeff

# Test environments

* [Ubuntu 20.04, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/prioritizr/wdpar/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3A%22Mac+OSX%22)
* [macOS 11.5.2 (arm64), R-release (macOS builder)](https://mac.r-project.org/macbuilder/submit.html)
* [Windows Server 2019, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3AWindows)
* [Windows Server 2008 (x64), R-devel (Win-Builder)](https://win-builder.r-project.org/)

# R CMD check results

0 errors | 0 warnings | 2 note

# CRAN check notes for new submission

* Possibly misspelled words in DESCRIPTION:
  WDOECM (7:28)

  **This term is an acronym for the World Database on Other Effective Area-Based Conservation Measures (as noted in the package Description).**

* Suggests or Enhances not in mainstream repositories:
  prepr

  **The prepr R package is an optional dependency that is available on GitHub (<https://github.com/dickoa/prepr>). Instructions for installing the prepr R package are provided in the DESCRIPTION file and the package README file.**

# CRAN check notes for current version on CRAN

* Namespace in Imports field not imported from: ‘withr’
  All declared Imports should be used.

  **This note has been addressed by removing the withr R package from the Imports field.**

# Downstream dependencies

There are no existing packages that depend on this package.
