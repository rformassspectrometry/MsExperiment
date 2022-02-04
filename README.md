# Managing mass spectrometry experiments

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/MsExperiment/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/MsExperiment/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov.io](https://codecov.io/github/rformassspectrometry/MsExperiment/coverage.svg?branch=main)](https://codecov.io/github/rformassspectrometry/MsExperiment?branch=main)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)


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
