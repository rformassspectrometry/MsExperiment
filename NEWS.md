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
