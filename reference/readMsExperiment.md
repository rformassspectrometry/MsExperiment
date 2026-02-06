# Import MS spectra data of an experiment

Read/import MS spectra data of an experiment from the respective (raw)
data files into an
[`MsExperiment()`](https://rformassspectrometry.github.io/MsExperiment/reference/MsExperiment.md)
object. Files provided with the `spectraFiles` parameter are imported as
a `Spectra` object and each file is automatically *linked* to rows
(samples) of a `sampleData` data frame (if provided).

## Usage

``` r
readMsExperiment(spectraFiles = character(), sampleData = data.frame(), ...)
```

## Arguments

- spectraFiles:

  `character` with the (absolute) file names of the MS data files that
  should be imported as a
  [`Spectra::Spectra()`](https://rdrr.io/pkg/Spectra/man/Spectra.html)
  object.

- sampleData:

  `data.frame` or `DataFrame` with the sample annotations. Each row is
  expected to contain annotations for one file (sample). The order of
  the data frame's rows is expected to match the order of the provided
  files (with parameter `spectraFiles`).

- ...:

  additional parameters for the
  [`Spectra::Spectra()`](https://rdrr.io/pkg/Spectra/man/Spectra.html)
  call to import the data.

## Value

`MsExperiment`.

## Author

Johannes Rainer

## Examples

``` r

## Define the files of the experiment to import. For our example we get
## the files from the *MsDataHub*
library(MsDataHub)
fls <- c(MsDataHub::X20171016_POOL_POS_1_105.134.mzML(),
         MsDataHub::X20171016_POOL_POS_3_105.134.mzML())
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache

## Define a data frame with some sample annotations
ann <- data.frame(
    injection_index = c(1, 9),
    sample_id = c("QC_1", "QC_3"))

## Import the data
library(MsExperiment)
mse <- readMsExperiment(spectraFiles = fls, ann)
mse
#> Object of class MsExperiment 
#>  Spectra: MS1 (1862) 
#>  Experiment data: 2 sample(s)
#>  Sample data links:
#>   - spectra: 2 sample(s) to 1862 element(s).

## Access the spectra data
spectra(mse)
#> MSn data (Spectra) with 1862 spectra in a MsBackendMzR backend:
#>        msLevel     rtime scanIndex
#>      <integer> <numeric> <integer>
#> 1            1     0.280         1
#> 2            1     0.559         2
#> 3            1     0.838         3
#> 4            1     1.117         4
#> 5            1     1.396         5
#> ...        ...       ...       ...
#> 1858         1   258.636       927
#> 1859         1   258.915       928
#> 1860         1   259.194       929
#> 1861         1   259.473       930
#> 1862         1   259.752       931
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> 173941d49f52_7859
#> 17391674d18a_7860

## Access the sample annotations
sampleData(mse)
#> DataFrame with 2 rows and 3 columns
#>                   injection_index   sample_id spectraOrigin
#>                         <numeric> <character>   <character>
#> 173941d49f52_7859               1        QC_1 /github/ho...
#> 17391674d18a_7860               9        QC_3 /github/ho...

## Import the data reading all MS spectra directly into memory
mse <- readMsExperiment(spectraFiles = fls, ann,
    backend = Spectra::MsBackendMemory())
mse
#> Object of class MsExperiment 
#>  Spectra: MS1 (1862) 
#>  Experiment data: 2 sample(s)
#>  Sample data links:
#>   - spectra: 2 sample(s) to 1862 element(s).
```
