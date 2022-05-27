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
date: "27 May 2022"
bibliography: paper.bib
output: rticles::joss_article
journal: JOSS
link-citations: yes
---

# Summary

The \texttt{wdpar} \texttt{R} package provides an interface to data available on Protected Planet (https://www.protectedplanet.net). It can be used to access the World Database on Protected Areas (WDPA) and the World Database on Other Effective Area-Based Conservation Measures (WDOECM). Additionally, it provides data cleaning procedures to prepare these databases for analysis. These data cleaning procedures are essential for ensuring correct results when using the databases. As a software package for the \texttt{R} statistical computing environment, it can easily be integrated into workflows and spatial analyses. The package has applications for conservation research. It has been used to assess performance of existing protected areas and account for such areas when identifying priority areas for conservation efforts.

# Statement of need

Area-based conservation measures are crucial for safeguarding biodiversity [@r5; @r6]. Examples of such measures include protected areas, marine reserves, and other effective area-based conservation measures (OECMs). Protected Planet is a key resource for area-based conservation measures, providing the World Database on Protected Areas (WDPA) and the World Database on Other Effective Area-Based Conservation Measures (WDOECM) [@r9]. These publicly available databases contain standardized data for over 270,000 protected areas and over 700 OECMs worldwide [@r9]. By detailing the designation, establishment, management, and spatial boundaries of area-based conservation measures [@r21], these databases play a vital role in monitoring and prioritizing conservation efforts [@r4; @r8].

The WDPA and WDOECM require data cleaning procedures to prepare them for analysis [@r4; @r10]. These procedures include repairing invalid geometries in spatial boundaries, excluding areas that have yet to be fully implemented, excluding areas that are no longer designated, excluding UNESCO Biosphere Reserves [@r1], buffering areas represented by point localities [@r3], and removing spatial overlaps [@r2]. These procedures are critical to ensure that assessments of area-based conservation measures do not overestimate the spatial extent of such measures and their ability to conserve biodiversity [@r1; @r2]. Although these procedures are critical, they can be technically challenging to implement. The \texttt{wdpar} \texttt{R} package provides automated methods to complete these procedures following best practices [@r4; @r10]. Using the package, data cleaning procedures can applied without specialized knowledge, customized to particular use cases, and across the entire WDPA and WDOECM. As such, the \texttt{wdpar} \texttt{R} package helps increase accessibility to the databases.

# Research applications

The \texttt{wdpar} \texttt{R} package has several applications for conservation research. For example, it has been used to assess the performance of existing protected areas in Colombia, Greece, and South Asia [@r11; @r16; @r13; @r17]. It has also been used to examine the potential implications of climate change on conservation efforts [@r12; @r14]. Additionally, it has been used to account for existing protected areas when identifying priority areas for biodiversity conservation [@r15]. Furthermore, it has been used to help understand how protected area management by Indigenous Peoples can reduce deforestation [@r18].

# Availability

The \texttt{wdpar} \texttt{R} package is implemented as a software package for \texttt{R} statistical computing environment [@r19]. It is available on the Comprehensive R Archive Network (CRAN) [@r20]. Developmental versions are available on an online code repository (<https://github.com/prioritizr/wdpar>). Documentation for the package is also available online (<https://prioritizr.github.io/wdpar>).

# Acknowledgments

The author thanks all of the individuals that have contributed to the package by reporting software defects and providing suggestions to improve functionality. The author is also grateful to Joseph Bennett for feedback on a draft of the manuscript. The author was supported by Environment and Climate Change Canada (ECCC) and Nature Conservancy of Canada (NCC).

# Conflict of interest

The author declares no conflict of interest.

# References
