# A class to store experiment files

The `MsExperimentFiles` class stores files that are part of a mass
spectrometry experiment. The objects are created with the
`MsExperimentFiles()` function.

The files encoded in a `MsExperimentFiles` instance don't need to exist
on the current filesystem - sometimes, these might be created in
anticipation of their creation. The `existMsExperimentFiles()` function
can be used to verify which ones currently exist: it returns a list of
logicals (formally an instance of
[`IRanges::LogicalList()`](https://rdrr.io/pkg/IRanges/man/AtomicList-class.html)
of lenghts equal to the `MsExperimentFiles` used as input.

## Usage

``` r
MsExperimentFiles(..., metadata = list())

# S4 method for class 'MsExperimentFiles'
show(object)

existMsExperimentFiles(object)
```

## Arguments

- ...:

  Either a named list or a set of named vectors. All elements are
  coerced to characters.

- metadata:

  [`list()`](https://rdrr.io/r/base/list.html) holding arbitrary R
  objects as annotations.

- object:

  The `existMsExperimentFiles()` function works with either an instance
  of `MsExperimentFiles` or `MsExperiment`.

## Value

`MsExperimentFiles` returns an instance of `MsExperimentFiles`.

## Author

Laurent Gatto

## Examples

``` r
fls <- MsExperimentFiles(mzmls = c("/path/to/f1.mzML", "/path/to/f2.mzML"),
                         mzids = "/another/path/to/id1.mzid",
                         fasta = "file.fas")
fls
#> MsExperimentFiles of length  3 
#> [["mzmls"]] f1.mzML f2.mzML
#> [["mzids"]] id1.mzid
#> [["fasta"]] file.fas

## A new MsExperimentFiles containing mzML or mzid files
fls[1]
#> MsExperimentFiles of length  1 
#> [["mzmls"]] f1.mzML f2.mzML
fls["mzids"]
#> MsExperimentFiles of length  1 
#> [["mzids"]] id1.mzid

## The actual file names
fls[[1]]
#> [1] "/path/to/f1.mzML" "/path/to/f2.mzML"
fls[[2]]
#> [1] "/another/path/to/id1.mzid"
fls[["fasta"]]
#> [1] "file.fas"

## None of the files used in this example actually exist
existMsExperimentFiles(fls)
#> mzmls: 0 out of 2 exist(s)
#> mzids: 0 out of 1 exist(s)
#> fasta: 0 out of 1 exist(s)
```
