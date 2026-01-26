Dear CRAN volunteers,

Thank you for reviewing this submission. It contains an update to the _wdpar_ package that has been on CRAN since 2019. The main changes in this new version include updates to maintain compatibility with the Protected Planet website and the World Database on Protected Areas and assorted improvements to the documentation.

Cheers,

Jeff

# Test environments

* [Ubuntu 22.04, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3AUbuntu)
* [Ubuntu 22.04, R-devel](https://github.com/prioritizr/wdpar/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3A%22Mac+OSX%22)
* [macOS 11.5.2 (arm64), R-release (macOS builder)](https://mac.r-project.org/macbuilder/submit.html)
* [Windows Server 2019, R-release](https://github.com/prioritizr/wdpar/actions?query=workflow%3AWindows)
* [Windows Server 2008 (x64), R-devel (Win-Builder)](https://win-builder.r-project.org/)

# R CMD check results

0 errors | 0 warnings | 3 notes

# Package check notes

* Possibly misspelled words in DESCRIPTION:
  WDOECM (7:28)

  **This term is an acronym for the World Database on Other Effective Area-Based Conservation Measures (as noted in the package Description).**

* Suggests or Enhances not in mainstream repositories:
  prepr

  **The prepr R package is an optional dependency that is available on GitHub (<https://github.com/prioritizr/prepr>). Instructions for installing the prepr R package are provided in the DESCRIPTION file and the package README file.**

* Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1111/conl.12158
      FROM: README.md
      Status: Service Unavailable
      Message: 503
    URL: https://doi.org/10.1126/science.aac9180
      FROM: README.md
      Status: Service Unavailable
      Message: 503

  **I have manually checked and can confirm that these URLs are correct.**

# Downstream dependencies

There are no existing packages that depend on this package.
