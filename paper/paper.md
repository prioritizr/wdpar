---
title: "wdpar: Interface to the World Database on Protected Areas"
tags:
  - R
  - conservation planning
  - protected areas
  - reserves
  - marine protected areas
  - other effective area-based conservation measures
  - spatial prioritization
authors:
  - name: Jeffrey O. Hanson
    orcid: 0000-0002-4716-6134
    affiliation: 1
affiliations:
 - name: Department of Biology, Carleton University, Ottawa, Canada
   index: 1
date: "14 May 2022"
bibliography: paper.bib
output: rticles::joss_article
journal: JOSS
link-citations: yes
---

# Summary

The \texttt{wdpar R} package provides an interface to data available on Protected Planet (https://www.protectedplanet.net). It can be used to access the World Database on Protected Areas (WDPA) and the World Database on Other Effective Area-Based Conservation Measures (WDOECM). Additionally, it provides data cleaning procedures to prepare these databases for analysis. These data cleaning procedures are essential for ensuring correct results when using the databases. As a software package for the \texttt{R} statistical computing environment, it can easily be integrated into work flows and spatial analyses. The package has applications for conservation research. For example, it has been used to help assess the performance of existing protected area systems and account for existing protected areas when identifying priority areas for conservation efforts.

# Statement of need

Area-based conservation measures are crucial for safeguarding biodiversity [@r5; @r6]. Examples of such measures include protected areas, marine reserves, and other effective area-based conservation measures (OECMs). Protected Planet is a key resource for area-based conservation measures, providing the World Database on Protected Areas (WDPA) and the World Database on Other Effective Area-Based Conservation Measures (WDOECM) [@r9]. These publicly available databases contain standardized data for over 270,000 protected areas and over 700 OECMs worldwide [@r9]. They are regularly updated by the UN Environment Programme World Conservation Monitoring Centre (UNEP-WCMC), in collaboration with governments, non-governmental organizations, and other data providers [@r21]. By providing data on the designation, establishment, management, and spatial boundaries of area-based conservation measures [@r21], these databases play a vital role in monitoring biodiversity conservation and prioritizing future conservation efforts [@r4; @r8].

The WDPA and WDOECM require data cleaning procedures to prepare them for analysis [@r4; @r10]. For example, these procedures include repairing invalid geometries in protected area boundaries, excluding areas that have yet to be fully implemented, excluding areas that are no longer designated, excluding UNESCO Biosphere Reserves [@r1], accommodating areas represented by point localities [@r3], and removing overlapping areas [@r2]. Although these procedures are critical to ensure correctness when calculating coverage of area-based conservation measures [@r10], they can be challenging to implement---especially given the size of the databases. By providing an interface to the databases and automated data cleaning procedures, the \texttt{wdpar R} package helps facilitate their use without specialized knowledge.

# Research applications

The \texttt{wdpar R} package has applications for conservation research. For example, it has been used to assess the performance of existing protected areas in Colombia, Greece, and South Asia [@r11; @r16; @r13; @r17]. It has also been used to examine the potential implications of climate change on conservation efforts [@r12; @r14]. Additionally, it has been used to account for existing protected areas when identifying priority areas for biodiversity conservation [@r15]. Furthermore, it has been used to help understand how protected area management by Indigenous Peoples can reduce deforestation [@r18].

# Availability

The \texttt{wdpar R} package is available on the Comprehensive R Archive Network (CRAN) [@r20]. Developmental versions of the package are available through an online code repository (<https://github.com/prioritizr/wdpar>). Documentation for the package is also available online (<https://prioritizr.github.io/wdpar>).

# Acknowledgments

The author thanks the many people that have contributed to the package by submitting bug reports and providing suggestions to improve functionality. The author is also grateful to Joseph Bennett for feedback on a draft of the manuscript. JOH was supported by Environment and Climate Change Canada (ECCC) and Nature Conservancy of Canada (NCC).

# Conflict of interest

The author declares no conflict of interest.

# References
