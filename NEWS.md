# MsExperiment 1.10

## Changes in version 1.10.1

- Fix in `readMsExperiment()` to ensure that, if `sampleData` has rownames, they
  match the names of the data files.

# MsExperiment 1.9

## MsExperiment 1.9.1

- Small fixes and update to *MsBackendSql* version >= 1.7.3.

# MsExperiment 1.5

## MsExperiment 1.5.5

- Add `spectraSampleIndex()` function.

## MsExperiment 1.5.4

- Fix missing export of `filterSpectra`.

## MsExperiment 1.5.3

- Add `filterSpectra` method to allow filtering of `Spectra` within an
  `MsExperiment` while keeping possibly present relationships between samples
  and spectra consistent.

## MsExperiment 1.5.2

- Add support to read/write sample data from/to a *MsBackendSql* database
  ([issue #39](https://github.com/rformassspectrometry/MsExperiment/issues/39)).

## MsExperiment 1.5.1

- Fix subset with negative indices ([issue #37](https://github.com/rformassspectrometry/MsExperiment/issues/37).)

# MsExperiment 1.1

## MsExperiment 1.1.4

- Fix and improve show,MSnExperiment.
- New `otherData` setter and getter functions.

## MsExperiment 1.1.3

- Fix problem in unit test.

## MsExperiment 1.1.2

- Add `readMsExperiment` function ([issue #32](https://github.com/rformassspectrometry/MsExperiment/issues/32)).

## MsExperiment 1.1.1

- Use `S4Vectors::findMatches` in `linkSampleData` which improves performance,
  especially for larger data sets.

## MsExperiment 1.1.0

- Bioconductor release 3.17 (devel).


# MsExperiment 0.99

## MsExperiment 0.99.4

- Import `spectra<-` from `ProtGenerics`.

## MsExperiment 0.99.3

- Fix assignment of default value for `@experimentFiles` slot.

## MsExperiment 0.99.2

- Fix imports.
- Inherit from the `S4Vectors::Annotated` class.
- Rename the `@assay` slot into `@qdata`.
- Fix class unions.

## MsExperiment 0.99.0

- Prepare for Bioconductor submission.
