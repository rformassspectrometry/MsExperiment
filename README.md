# Managing mass spectrometry experiments

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/MsExperiment/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/MsExperiment/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov](https://codecov.io/gh/rformassspectrometry/MsExperiment/branch/devel/graph/badge.svg?token=IEBQZJAJVI)](https://codecov.io/gh/rformassspectrometry/MsExperiment)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)
[![years in bioc](http://bioconductor.org/shields/years-in-bioc/MsExperiment.svg)](https://bioconductor.org/packages/release/bioc/html/MsExperiment.html)
[![Ranking by downloads](http://bioconductor.org/shields/downloads/release/MsExperiment.svg)](https://bioconductor.org/packages/stats/bioc/MsExperiment/)
[![build release](http://bioconductor.org/shields/build/release/bioc/MsExperiment.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/MsExperiment/)
[![build devel](http://bioconductor.org/shields/build/devel/bioc/MsExperiment.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/MsExperiment/)


The `MsExperiment` package provides the `MsExperiment` class that can
be used to store and manage all aspects related to a complete
proteomics or metabolomics mass spectrometry experiment. This includes

- experimental design
- data files
- raw data: spectra and chromatograms
- proteomics identification results and protein database
- quantitative features

The respective data handling functionality is handled by dedicated
packages such as `Spectra` for spectra data, `QFeatures` for
quantitative features, `Biostrings` for fasta files, ...

See the package [homepage](https://rformassspectrometry.github.io/MsExperiment)
for more information.

This package is part of the RforMassSpectrometry initiative:
https://www.rformassspectrometry.org/


# Contributions

Contributions are highly welcome and should follow the [contribution
guidelines](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions).
Also, please check the coding style guidelines in the [RforMassSpectrometry
vignette](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html).
