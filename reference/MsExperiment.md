# Managing Mass Spectrometry Experiments

The `MsExperiment` class allows the storage and management of all
aspects related to a complete proteomics or metabolomics mass
spectrometry experiment. This includes experimantal design (i.e. a table
with samples), raw mass spectromtry data as spectra and chromatograms,
quantitative features, and identification data or any other relevant
data files.

For details, see https://rformassspectrometry.github.io/MsExperiment

This package is part of the RforMassSpectrometry initiative:
https://www.rformassspectrometry.org/

## Usage

``` r
experimentFiles(object)

experimentFiles(object) <- value

sampleData(object)

sampleData(object) <- value

qdata(object)

qdata(object) <- value

spectraSampleIndex(x, duplicates = c("first", "keep"))

MsExperiment(
  experimentFiles = MsExperimentFiles(),
  otherData = List(),
  qdata = NULL,
  sampleData = DataFrame(),
  spectra = NULL
)

# S4 method for class 'MsExperiment'
show(object)

# S4 method for class 'MsExperiment'
length(x)

# S4 method for class 'MsExperiment'
spectra(object)

# S4 method for class 'MsExperiment'
spectra(object) <- value

otherData(object)

otherData(object) <- value

linkSampleData(
  object,
  with = character(),
  sampleIndex = seq_len(nrow(sampleData(object))),
  withIndex = integer(),
  subsetBy = 1L
)

# S4 method for class 'MsExperiment,ANY,ANY,ANY'
x[i, j, ..., drop = FALSE]

# S4 method for class 'MsExperiment,function'
filterSpectra(object, filter, ...)
```

## Arguments

- object:

  An instance of class `MsExperiment`.

- value:

  An object of the appropriate class for the slot to be populated.

- x:

  an `MsExperiment`.

- duplicates:

  for `spectraSampleIndex()`: `character(1)` defining the type of result
  returned by `spectraSampleIndex()`. With `duplicates = "first"` an
  `integer` vector is returned with the first match while
  `duplicates = "keep"` returns a `list` of `integer` with the index of
  all matches.

- experimentFiles:

  [`MsExperimentFiles()`](https://rformassspectrometry.github.io/MsExperiment/reference/MsExperimentFiles.md)
  defining (external) files to data or annotation.

- otherData:

  `List` with arbitrary additional (*other*) information or data.

- qdata:

  `QFeatures` or `SummarizedExperiment` with the quantification data.

- sampleData:

  `DataFrame` (or `data.frame`) with information on individual samples
  of the experiment.

- spectra:

  [`Spectra::Spectra()`](https://rdrr.io/pkg/Spectra/man/Spectra.html)
  object with the MS spectra data of the experiment.

- with:

  for `linkSampleData()`: `character(1)` defining the data to which
  samples should be linked. See section *Linking sample data to other
  experimental data* for details.

- sampleIndex:

  for `linkSampleData()`: `integer` with the indices of the samples in
  `sampleData(object)` that should be linked.

- withIndex:

  for `linkSampleData()`: `integer` with the indices of the elements in
  `with` to which the samples (specified by `sampleIndex`) should be
  linked to.

- subsetBy:

  for `linkSampleData()`: optional `integer(1)` defining the dimension
  on which the subsetting will occurr on the linked data. Defaults to
  `subsetBy = 1L` thus subsetting will happen on the first dimension
  (rows or elements).

- i:

  for `[`: an `integer`, `character` or `logical` referring to the
  indices or names (rowname of `sampleData`) of the samples to subset.

- j:

  for `[`: not supported.

- ...:

  optional additional parameters. For `filterSpectra()`: parameters to
  be passed to the filter function (parameter `filter`).

- drop:

  for `[`: ignored.

- filter:

  for `filterSpectra()`: any filter function supported by
  [`Spectra::Spectra()`](https://rdrr.io/pkg/Spectra/man/Spectra.html)
  to filter the spectra object (such as `filterRt` or `filterMsLevel`).
  Parameters for the filter function can be passed through `...`.

## Value

See help of the individual functions.

## Slots

- `experimentFiles`:

  An instance of class `MsExperimentFiles` or `NULL`.

- `spectra`:

  An instance of class `Spectra` or `NULL`.

- `qdata`:

  An instance of class `QFeatures`, `SummarizedExperiment` or `NULL`.

- `otherData`:

  A `List` to store any additional data objects.

- `sampleData`:

  A `DataFrame` documenting the experimental design.

- `sampleDataLinks`:

  A `List` with link definitions between samples and data elements.
  Should not be directly accessed or modified by the user.

- `metadata`:

  A `list` to store additional metadata.

## General information

An experiment is typically composed of several items

- Description and information (covariates etc) of each sample from the
  experiment. These are stored in the `sampleData` slot as a
  `DataFrame`, each row describing a sample with columns containing all
  relevant information on that sample.

- Files to data or annotations. These are stored in the
  `@experimentFiles` slot as an instance of class `MsExperimentFiles`.

- General metadata about the experiment, stored as a `list` in the
  `@metadata` slot.

- Mass spectrometry data. Sectra and their metadata are stored as an
  `[Spectra::Spectra()]` object in the `spectra` slot. Chromatographic
  data is not yet supported but will be stored as a `Chromatograms()`
  object in the `@chromatorgrams` slot.

- Quantification data is stored as `QFeatures` or `SummarizedExperiment`
  objects in the `@qdata` slot and can be accessed or replaced with the
  `qdata()` or `qdata<-` functions, respectively.

- Any additional data, be it other spectra data, or proteomics
  identification data (i.e peptide-spectrum matches defined as `PSM`
  objects) can be added as elements to the list stored in the
  `otherData` slot.

The *length* of a `MsExperiment` is defined by the number of samples
(i.e. the number of rows of the object's `sampleData`). A `MsExperiment`
with two samples will thus have a length of two, independently of the
number of files or length of raw data in the object. This also defines
the subsetting of the object using the `[` function which will always
subset by samples. See the section for filtering and subsetting below
for more information.

`MsExperiment` objects can be created using the `MsExperiment()`
function providing the data with the parameters listed below. If the
[`Spectra::Spectra()`](https://rdrr.io/pkg/Spectra/man/Spectra.html)
object provided with the `spectra` param uses a `MsBackendSql` backend,
sample data could be retrieved from the associated SQL database (see
section *Using `MsExperiment` with `MsBackendSql`* in the vignette for
details). Alternatively, it is also possible to subsequently add data
and information to an existing `MsExperiment`. Finally, with the
[`readMsExperiment()`](https://rformassspectrometry.github.io/MsExperiment/reference/readMsExperiment.md)
function it is possible to create a `MsExperiment` by importing MS
spectra data directly from provided data files. See examples below or
the package vignette for more information.

## Accessing data

Data from an `MsExperiment` object can be accessed with the dedicated
accessor functions:

- `experimentFiles()`, `experimentFiles<-`: gets or sets experiment
  files.

- [`length()`](https://rdrr.io/r/base/length.html): get the *length* of
  the object which represents the number of samples availble in the
  object's `sampleData`.

- [`metadata()`](https://rdrr.io/pkg/S4Vectors/man/Annotated-class.html),
  `metadata<-`: gets or sets the object's metadata.

- `sampleData()`, `sampleData<-`: gets or sets the object's sample data
  (i.e. a `DataFrame` containing sample descriptions).

- `spectra()`, `spectra<-`: gets or sets spectra data. `spectra()`
  returns a
  [`Spectra::Spectra()`](https://rdrr.io/pkg/Spectra/man/Spectra.html)
  object, `spectra<-` takes a `Spectra` data as input and returns the
  updated `MsExperiment`.

- `spectraSampleIndex()`: depending on parameter `duplicates` it returns
  either an `integer` (`duplicates = "first"`, the default) or a `list`
  (`duplicates = "keep"`) of length equal to the number of spectra
  within the object with the indices of the sample(s) (in
  `sampleData()`) a spectrum is assigned to. With
  `duplicates = "first"`, an `integer` with the index is returned for
  each spectrum. If a spectrum was assigned to more than one sample a
  warning is shown and only the first sample index is returned for that
  spectrum. For `duplicates = "keep"`, assignments are returned as a
  `list` of `integer` vectors, each element being the index(es) of the
  sample(s) a spectrum is assigned to. For spectra that are not linked
  to any sample an `NA_integer_` is returned as index for
  `duplicates = "first"` and an empty integer
  ([`integer()`](https://rdrr.io/r/base/integer.html)) for
  `duplicates = "keep"`. Note that the default `duplicates = "first"`
  will work in almost all use cases, as generally, a spectrum will be
  assigned to a single sample.

- `qdata()`, `qdata<-`: gets or sets the quantification data, which can
  be a `QFeatures` or `SummarizedExperiment`.

- `otherData()` , `otherData<-`: gets or sets the addition data types,
  stored as a `List` in the object's `otherData` slot.

## Linking sample data to other experimental data

To start with, an `MsExperiment` is just a loose collection of files and
data related to an experiment, no explicit links or associactions are
present between the samples and related data. Such links can however be
created with the `linkSampleData()` function. This function can
establish links between individual (or all) samples within the object's
`sampleData` to individual, or multiple, data elements or files, such as
`Spectra` or raw data files.

The presence of such links enables a (consistent) subsetting of an
`MsExperiment` by samples. Thus, once the link is defined, any
subsetting by sample will also correctly subset the linked data. All
other, not linked, data elements are always retained as in the original
`MsExperiment`.

To be able to link different elements within an `MsExperiment` it is
also required to *identify* them with a consistent naming scheme. The
naming scheme of slots and data elements within follows an SQL-like
scheme, in which the variable (element) is identified by the name of the
database table, followed by a `"."` and the name of the database table
column. For `MsExperiment`, the naming scheme is defined as
`"<slot name>.<element name>"`. A column called `"sample_name"` within
the `sampleData` data frame can thus be addressed with
`"sampleData.sample_name"`, while `spectra.msLevel` would represent the
spectra variable called `msLevel` within the `Spectra` stored in the
`spectra` slot.

Links between sample data rows and any other data element are stored as
`integer` matrices within the `@sampleDataLinks` slot of the object (see
also the vignette for examples and illustrations). The first column of a
matrix is always the index of the sample, and the second column the
index of the element that is linked to that sample, with one row per
element. Links can be defined/added with the `linkSampleData()` function
which adds a relationship between rows in `sampleData` to elements in
any other data within the `MsExperiment` that are specified with
parameter `with`. `linkSampleData()` supports two different ways to
define the link:

- Parameter `with` defines the data to which the link should be
  established. To link samples to raw data files that would for example
  be available as a `character` in an element called `"raw_files"`
  within the object's `experimentFiles`,
  `with = experimentFiles.raw_files` would have to be used. Next it is
  required to specify which samples should be linked with which elements
  in `with`. This needs to be defined with the parameters `sampleIndex`
  and `withIndex`, both are expected to be `integer` vectors specifying
  which sample in `sampleData` should be linked to which element in
  `with` (see examples below or vignette for examples and details).

- As an alternative way, a link could be defined with an SQL-like syntax
  that relates a column in `sampleData` to a column/element in the data
  to which the link should be established. To link for example
  individual spectra to the corresponding samples
  `with = "sampleData.raw_file = spectra.dataOrigin"` could be used
  assuming that `sampleData` contains a column named `"raw_file"` with
  the (full path) of the raw data file for each sample from which the
  spectra were imported. In this case both `sampleIndex` and `withIndex`
  can be omitted, but it is expected/required that the columns/elements
  from `sampleData` and the data element to which the link should be
  established contain matching values.

Note that `linkSampleData` will **replace** a previously existing link
to the same data element.

- `spectraSampleIndex()` is a convenience function that extracts for
  each spectrum in the object's `spectra()` the index of the sample it
  is associated with (see function's help above for more information).

## Subsetting and filtering

- `[`: `MsExperiment` objects can be subset **by samples** with `[i]`
  where `i` is the index or a logical defining to which samples the data
  should be subset. Subsetting by sample will (correctly) subset all
  linked data to the respective samples. If multiple samples are linked
  to the same data element, subsetting might duplicate that data
  element. This duplication of *n:m* relationships between samples to
  elements does however not affect data consistency (see examples below
  for more information). Not linked data (slots) will be returned as
  they are. Subsetting in arbitrary order is supported. See the vignette
  for details and examples.

- `filterSpectra()`: subsets the `Spectra` within an `MsExperiment`
  using a provided filter function (parameter `filter`). Parameters for
  the filter function can be passed with parameter `...`. Any of the
  filter functions of a
  [`Spectra::Spectra()`](https://rdrr.io/pkg/Spectra/man/Spectra.html)
  object can be passed with parameter `filter`. Possibly present
  relationships between samples and spectra (*links*, see also
  `linkSampleData()`) are updated. Filtering affects only the spectra
  data of the object, none of the other slots and data (e.g.
  `sampleData`) are modified. The function returns an `MsExperiment`
  with the filtered `Spectra` object.

## Author

Laurent Gatto, Johannes Rainer

## Examples

``` r

## An empty MsExperiment object
msexp <- MsExperiment()
msexp
#> Empty object of class MsExperiment 

example(MsExperimentFiles)
#> 
#> MsExpF> fls <- MsExperimentFiles(mzmls = c("/path/to/f1.mzML", "/path/to/f2.mzML"),
#> MsExpF+                          mzids = "/another/path/to/id1.mzid",
#> MsExpF+                          fasta = "file.fas")
#> 
#> MsExpF> fls
#> MsExperimentFiles of length  3 
#> [["mzmls"]] f1.mzML f2.mzML
#> [["mzids"]] id1.mzid
#> [["fasta"]] file.fas
#> 
#> MsExpF> ## A new MsExperimentFiles containing mzML or mzid files
#> MsExpF> fls[1]
#> MsExperimentFiles of length  1 
#> [["mzmls"]] f1.mzML f2.mzML
#> 
#> MsExpF> fls["mzids"]
#> MsExperimentFiles of length  1 
#> [["mzids"]] id1.mzid
#> 
#> MsExpF> ## The actual file names
#> MsExpF> fls[[1]]
#> [1] "/path/to/f1.mzML" "/path/to/f2.mzML"
#> 
#> MsExpF> fls[[2]]
#> [1] "/another/path/to/id1.mzid"
#> 
#> MsExpF> fls[["fasta"]]
#> [1] "file.fas"
#> 
#> MsExpF> ## None of the files used in this example actually exist
#> MsExpF> existMsExperimentFiles(fls)
#> mzmls: 0 out of 2 exist(s)
#> mzids: 0 out of 1 exist(s)
#> fasta: 0 out of 1 exist(s)
experimentFiles(msexp) <- fls
msexp
#> Object of class MsExperiment 
#>  Files: mzmls, mzids, fasta 

## Linking samples to data elements

## Create a small experiment
library(S4Vectors)
#> Loading required package: stats4
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: ‘BiocGenerics’
#> The following objects are masked from ‘package:stats’:
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from ‘package:base’:
#> 
#>     Filter, Find, Map, Position, Reduce, anyDuplicated, aperm, append,
#>     as.data.frame, basename, cbind, colnames, dirname, do.call,
#>     duplicated, eval, evalq, get, grep, grepl, is.unsorted, lapply,
#>     mapply, match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     rank, rbind, rownames, sapply, saveRDS, table, tapply, unique,
#>     unsplit, which.max, which.min
#> 
#> Attaching package: ‘S4Vectors’
#> The following object is masked from ‘package:utils’:
#> 
#>     findMatches
#> The following objects are masked from ‘package:base’:
#> 
#>     I, expand.grid, unname
mse <- MsExperiment()
sd <- DataFrame(sample_id = c("QC1", "QC2"),
                sample_name = c("QC Pool", "QC Pool"),
                injection_idx = c(1, 3))
sampleData(mse) <- sd

## define file names containing spectra data for the samples and
## add them, along with other arbitrary files to the experiment
fls <- c(MsDataHub::X20171016_POOL_POS_1_105.134.mzML(),
         MsDataHub::X20171016_POOL_POS_3_105.134.mzML())
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache
experimentFiles(mse) <- MsExperimentFiles(
    mzML_files = fls,
    annotations = "internal_standards.txt")

## Link samples to data files: first sample to first file in "mzML_files",
## second sample to second file in "mzML_files"
mse <- linkSampleData(mse, with = "experimentFiles.mzML_files",
    sampleIndex = c(1, 2), withIndex = c(1, 2))

## Link all samples to the one file in "annotations"
mse <- linkSampleData(mse, with = "experimentFiles.annotations",
    sampleIndex = c(1, 2), withIndex = c(1, 1))
mse
#> Object of class MsExperiment 
#>  Files: mzML_files, annotations 
#>  Experiment data: 2 sample(s)
#>  Sample data links:
#>   - experimentFiles.mzML_files: 2 sample(s) to 2 element(s).
#>   - experimentFiles.annotations: 2 sample(s) to 1 element(s).

## Import the spectra data and add it to the experiment
library(Spectra)
#> Loading required package: BiocParallel
spectra(mse) <- Spectra(fls, backend = MsBackendMzR())

## Link each spectrum to the respective sample. We use the alternative
## link definition that does not require sampleIndex and withIndex but
## links elements based on matching values in the specified data elements.
## We need to add the full file name as an additional column to sampleData
## in order to allow matching this file names with the value in
## spectra(mse)$dataOrigin which contains the original file names from which
## the spectra were imported.
sampleData(mse)$raw_file <- normalizePath(fls)

## The links can be added using the short notation below
mse <- linkSampleData(mse, with = "sampleData.raw_file = spectra.dataOrigin")
mse
#> Object of class MsExperiment 
#>  Files: mzML_files, annotations 
#>  Spectra: MS1 (1862) 
#>  Experiment data: 2 sample(s)
#>  Sample data links:
#>   - experimentFiles.mzML_files: 2 sample(s) to 2 element(s).
#>   - experimentFiles.annotations: 2 sample(s) to 1 element(s).
#>   - spectra: 2 sample(s) to 1862 element(s).

## With sampleData links present, any subsetting of the experiment by sample
## will ensure that all linked elements are subset accordingly
b <- mse[2]
b
#> Object of class MsExperiment 
#>  Files: mzML_files, annotations 
#>  Spectra: MS1 (931) 
#>  Experiment data: 1 sample(s)
#>  Sample data links:
#>   - experimentFiles.mzML_files: 1 sample(s) to 1 element(s).
#>   - experimentFiles.annotations: 1 sample(s) to 1 element(s).
#>   - spectra: 1 sample(s) to 931 element(s).
sampleData(b)
#> DataFrame with 1 row and 4 columns
#>     sample_id sample_name injection_idx      raw_file
#>   <character> <character>     <numeric>   <character>
#> 1         QC2     QC Pool             3 /github/ho...
experimentFiles(b)$mzML_files
#>                                                  EH7810 
#> "/github/home/.cache/R/ExperimentHub/17391674d18a_7860" 

## The `spectraSampleIndex()` function returns, for each spectrum, the
## index in the object's `sampleData` to which it is linked/assigned
spectraSampleIndex(mse)
#>    [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>   [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>   [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [112] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [149] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [186] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [223] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [260] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [297] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [334] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [371] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [408] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [445] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [482] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [519] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [556] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [593] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [630] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [667] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [704] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [741] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [778] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [815] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [852] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [889] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [926] 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [963] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1000] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1037] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1074] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1111] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1148] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1185] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1222] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1259] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1296] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1333] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1370] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1407] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1444] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1481] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1518] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1555] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1592] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1629] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1666] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1703] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1740] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1777] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1814] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [1851] 2 2 2 2 2 2 2 2 2 2 2 2

## Subsetting with duplication of n:m sample to data relationships
##
## Both samples were assigned above to one "annotation" file in
## `experimentFiles`:
experimentFiles(mse[1])[["annotations"]]
#> [1] "internal_standards.txt"
experimentFiles(mse[2])[["annotations"]]
#> [1] "internal_standards.txt"

## Subsetting will always keep the relationship between samples and linked
## data elements. Subsetting will however possibly duplicate data elements
## that are shared among samples. Thus, while in the original object the
## element "annotations" has a single entry, subsetting with [1:2] will
## result in an MsExperiment with duplicated entries in "annotations"
experimentFiles(mse)[["annotations"]]
#> [1] "internal_standards.txt"
experimentFiles(mse[1:2])[["annotations"]]
#> [1] "internal_standards.txt" "internal_standards.txt"

## Spectra within an MsExperiment can be filtered/subset with the
## `filterSpectra` function and any of the filter functions supported
## by `Spectra` objects. Below we restrict the spectra data to spectra
## with a retention time between 200 and 210 seconds.
res <- filterSpectra(mse, filterRt, rt = c(200, 210))
res
#> Object of class MsExperiment 
#>  Files: mzML_files, annotations 
#>  Spectra: MS1 (72) 
#>  Experiment data: 2 sample(s)
#>  Sample data links:
#>   - experimentFiles.mzML_files: 2 sample(s) to 2 element(s).
#>   - experimentFiles.annotations: 2 sample(s) to 1 element(s).
#>   - spectra: 2 sample(s) to 72 element(s).

## The object contains now much less spectra. The retention times for these
rtime(spectra(res))
#>  [1] 200.049 200.328 200.607 200.886 201.165 201.444 201.723 202.002 202.281
#> [10] 202.560 202.839 203.118 203.397 203.676 203.955 204.234 204.513 204.792
#> [19] 205.071 205.350 205.629 205.908 206.187 206.466 206.745 207.024 207.303
#> [28] 207.582 207.861 208.140 208.419 208.698 208.977 209.256 209.535 209.814
#> [37] 200.044 200.323 200.602 200.881 201.160 201.439 201.718 201.997 202.276
#> [46] 202.555 202.834 203.113 203.392 203.671 203.950 204.229 204.508 204.787
#> [55] 205.066 205.345 205.624 205.903 206.182 206.461 206.740 207.019 207.298
#> [64] 207.577 207.856 208.135 208.414 208.693 208.972 209.251 209.530 209.809

## Relationship between samples and spectra was preserved by the filtering
a <- res[1L]
spectra(a)
#> MSn data (Spectra) with 36 spectra in a MsBackendMzR backend:
#>       msLevel     rtime scanIndex
#>     <integer> <numeric> <integer>
#> 1           1   200.049       717
#> 2           1   200.328       718
#> 3           1   200.607       719
#> 4           1   200.886       720
#> 5           1   201.165       721
#> ...       ...       ...       ...
#> 32          1   208.698       748
#> 33          1   208.977       749
#> 34          1   209.256       750
#> 35          1   209.535       751
#> 36          1   209.814       752
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> 173941d49f52_7859
#> Processing:
#>  Filter: select retention time [200..210] on MS level(s)  [Fri Feb  6 13:24:37 2026] 
```
