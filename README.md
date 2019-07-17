# Managing full mass spectrometry experiments

The `MsExperiment` package provides the `MsExperiment` class that can
be used to store and manage all aspects related to a complete
proteomics or metabolomics mass spectrometry experiment. This includes

- experimental desing
- raw data: spectra
- raw data: chromatograms
- proteomics identification results and protein database
- quantitative features

The respective data handling functionality is handled by dedicated
packages such as `Spectra` for spectra data, `Features` for
quantitative features, `Biostrings` for fasta files, ...


This package is part of the RforMassSpectrometry initiative:
https://www.rformassspectrometry.org/