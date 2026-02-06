# Write sample annotations to a MsBackendSql SQL database

For `MsExperiment` objects with their MS data represented by a `Spectra`
object that use a `MsBackendSql` backend, its sample annotations can be
written to the backend's SQL database with the `dbWriteSampleData()`
function. The content of the object's `[sampleData()]` (as well as
eventually present *linking* between samples and spectra) will be stored
in two separate database tables *sample_data* and
*sample_to_msms_spectrum* in the same database.

This requires that the MS data of the experiment is *represented* by a
`MsBackendSql` backend (see help on the `createMsBackendSqlDatabase` or
the *MsBackendSql* package vignette for more information on how to
create or use such SQL databases).

## Usage

``` r
dbWriteSampleData(x)
```

## Arguments

- x:

  `MsExperiment` from which sample annotations should be written to the
  database.

## Author

Johannes Rainer, Laurent Gatto

## Examples

``` r

library(MsExperiment)

## Create a MsBackendSql database from two mzML files.
## Connect first to an empty SQLite database (for the example we create
## a database in a temporary file).
library(RSQLite)
sqlite_db <- tempfile()
con <- dbConnect(SQLite(), sqlite_db)

## Define the files from which we import the data. Test files provided from
## the *MsDataHub*
fls <- c(MsDataHub::X20171016_POOL_POS_1_105.134.mzML(),
    MsDataHub::X20171016_POOL_POS_3_105.134.mzML())
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache

## Create a MsBackendSql database containing the full MS data
library(MsBackendSql)
createMsBackendSqlDatabase(con, fls)
#> Importing data ... 
#> 
#> [==========================================================] 1/1 (100%) in  5s
#> 
#> Creating indices 
#> .
#> .
#> .
#> .
#>  Done
#> [1] TRUE

## Note: alternatively it would be possible to first import the MS data
## to a `Spectra` object and then change the backend to a `MsBackendSql`
## using the `setBackend` function.

## Load this data as a `Spectra` object (using a `MsBackendOfflineSql`
## backend)
library(Spectra)
sps <- Spectra(sqlite_db, source = MsBackendOfflineSql(),
    drv = SQLite())
sps
#> MSn data (Spectra) with 1862 spectra in a MsBackendOfflineSql backend:
#>        msLevel precursorMz  polarity
#>      <integer>   <numeric> <integer>
#> 1            1          NA         1
#> 2            1          NA         1
#> 3            1          NA         1
#> 4            1          NA         1
#> 5            1          NA         1
#> ...        ...         ...       ...
#> 1858         1          NA         1
#> 1859         1          NA         1
#> 1860         1          NA         1
#> 1861         1          NA         1
#> 1862         1          NA         1
#>  ... 35 more variables/columns.
#>  Use  'spectraVariables' to list all of them.
#> Database: /tmp/RtmpGRl2PS/file1d7c4f82b9e3

## Define sample annotations for the two data files. Adding one column
## `"file"` that contains the file name of the data files.
df <- data.frame(sample = c("QC1", "QC2"), file = basename(fls))

## Add a spectra variable `"file"` to the `Spectra` object with
## the raw data files' file names to simplify the linking between
## samples and spectra performed later.
sps$file <- basename(dataOrigin(sps))

## Create a MsExperiment with the spectra and sample data.
mse <- MsExperiment(spectra = sps, sampleData = df)

## Establish the link (mapping) between samples and spectra
## using the column `"file"` in the `sampleData` and the spectra
## variable `"file"`.
mse <- linkSampleData(mse, with = "sampleData.file = spectra.file")
mse
#> Object of class MsExperiment 
#>  Spectra: MS1 (1862) 
#>  Experiment data: 2 sample(s)
#>  Sample data links:
#>   - spectra: 2 sample(s) to 1862 element(s).

## Write sample data (and the sample to spectra mapping) to the
## *MsBackendSql* database.
dbWriteSampleData(mse)

## List the tables in the database
dbListTables(con)
#> [1] "msms_spectrum"            "msms_spectrum_peak_blob2"
#> [3] "sample_data"              "sample_to_msms_spectrum" 

## Sample data was thus stored to the database.
dbGetQuery(con, "select * from sample_data;")
#>   sample              file sample_id_
#> 1    QC1 173941d49f52_7859          1
#> 2    QC2 17391674d18a_7860          2
```
