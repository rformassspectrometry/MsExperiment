#' Functionality to store/retrieve MsExperiment data to/from databases.
#'
#' @noRd
NULL

#' @title Write sample annotations to a MsBackendSql SQL database
#'
#' @description
#'
#' For `MsExperiment` objects with their MS data represented by a `Spectra`
#' object that use a `MsBackendSql` backend its sample annotations can be
#' written to that SQL database with the `dbWriteSampleData` function. The
#' content of the object's `[sampleData()]` (as well as eventually present
#' *linking* between samples and spectra) will be stored in two database
#' tables *sample_data* and *sample_to_msms_spectrum* in the same database
#' that contains all the MS data of the experiment.
#'
#' This requires that the MS data of the experiment is *represented* by a
#' `MsBackendSql` backend (see help on the `createMsBackendSqlDatabase` or the
#' *MsBackendSql* package vignette for more information on how to create or
#' use such SQL databases).
#'
#' @param x `MsExperiment` from which sample annotations should be written
#'     to the database.
#'
#' @author Johannes Rainer, Laurent Gatto
#'
#' @importFrom DBI dbExistsTable dbRemoveTable dbWriteTable dbDisconnect
#'
#' @export
#'
#' @examples
#'
#' library(MsExperiment)
#'
#' ## Create a MsBackendSql database from two mzML files.
#' ## Connect first to an empty SQLite database (for the example we create
#' ## a database in a temporary file).
#' library(RSQLite)
#' sqlite_db <- tempfile()
#' con <- dbConnect(SQLite(), sqlite_db)
#'
#' ## Define the files from which we import the data
#' fls <- dir(system.file("sciex", package = "msdata"), pattern = "mzML",
#'     full.names = TRUE)
#'
#' ## Create a MsBackendSql database containing the full MS data
#' library(MsBackendSql)
#' createMsBackendSqlDatabase(con, fls)
#'
#' ## Note: alternatively it would be possible to first import the MS data
#' ## to a `Spectra` object and then change the backend to a `MsBackendSql`
#' ## using the `setBackend` function.
#'
#' ## Load this data as a `Spectra` object (using a `MsBackendOfflineSql`
#' ## backend)
#' library(Spectra)
#' sps <- Spectra(sqlite_db, source = MsBackendOfflineSql(),
#'     drv = SQLite())
#' sps
#'
#' ## Define sample annotations for the two data files. Adding one column
#' ## `"file"` that contains the file name of the data files.
#' df <- data.frame(sample = c("QC1", "QC2"), file = basename(fls))
#'
#' ## Add a spectra variable `"file"` to the `Spectra` object with
#' ## the raw data files' file names to simplify the linking between
#' ## samples and spectra performed later.
#' sps$file <- basename(dataOrigin(sps))
#'
#' ## Create a MsExperiment with the spectra and sample data.
#' mse <- MsExperiment(spectra = sps, sampleData = df)
#'
#' ## Establish the link (mapping) between samples and spectra
#' ## using the column `"file"` in the `sampleData` and the spectra
#' ## variable `"file"`.
#' mse <- linkSampleData(mse, with = "sampleData.file = spectra.file")
#' mse
#'
#' ## Write sample data (and the sample to spectra mapping) to the
#' ## *MsBackendSql* database.
#' dbWriteSampleData(mse)
#'
#' ## List the tables in the database
#' dbListTables(con)
#'
#' ## Sample data was thus stored to the database.
#' dbGetQuery(con, "select * from sample_data;")
dbWriteSampleData <- function(x) {
    if (!inherits(x, "MsExperiment"))
        stop("'x' is supposed to be an 'MsExperiment'")
    if (!length(spectra(x))) return()
    if (inherits(spectra(x)@backend, "MsBackendSql")) {
        be <- spectra(x)@backend
        if (inherits(be, "MsBackendOfflineSql")) {
            flags <- be@flags
            be@flags <- 2L # fix to ensure getting a writebale connection
            con <- dbconn(be)
            be@flags <- flags
            on.exit(dbDisconnect(con))
        } else con <- dbconn(be)
        if (dbExistsTable(con, "sample_data")) {
            message("Sample data already available in the database. ",
                    "Will overwrite.")
            dbRemoveTable(con, "sample_data")
        }
        if (dbExistsTable(con, "sample_to_msms_spectrum"))
            dbRemoveTable(con, "sample_to_msms_spectrum")
        sdata <- as.data.frame(sampleData(x))
        if (any(colnames(sdata) == "sample_id_"))
            warning("Replacing already present column \"sample_id_\" ",
                    "in 'sampleData'.")
        sdata$sample_id_ <- seq_len(nrow(sdata))
        dbWriteTable(con, name = "sample_data", sdata)
        sdata_links <- x@sampleDataLinks[["spectra"]]
        if (length(sdata_links)) {
            sdata_links <- as.data.frame(sdata_links)
            colnames(sdata_links) <- c("sample_index", "spectrum_index")
            dbWriteTable(con, name = "sample_to_msms_spectrum", sdata_links)
        }
    } else stop("The object's 'Spectra' object does not use a ",
                "'MsBackendSql' backend.")
}

#' @title Get sample data from a MsBackendSql database
#'
#' @description
#'
#' Retrieve sample data and sample to spectra links from a MsBackendSql
#' database, if present. This requires that the `Spectra` `x` uses a
#' `MsBackendSql` or `MsBackendOfflineSql` backend and that the database
#' contains database tables *sample_data* and eventually
#' *sample_to_msms_spectrum*.
#'
#' @param x `Spectra`
#'
#' @author Johannes Rainer
#'
#' @return information from the database or an empty array.
#'
#' @importFrom DBI dbGetQuery dbListTables
#'
#' @importMethodsFrom BiocGenerics dbconn
#'
#' @noRd
.db_get_sample_data <- function(x) {
    if (inherits(x@backend, "MsBackendSql")) {
        con <- dbconn(x@backend)
        if (inherits(x@backend, "MsBackendOfflineSql"))
            on.exit(dbDisconnect(con))
        if (any(dbListTables(con) == "sample_data"))
            dbGetQuery(con, "select * from sample_data")
        else data.frame()
    } else data.frame()
}
.db_get_sample_spectra_link <- function(x) {
    if (inherits(x@backend, "MsBackendSql")) {
        con <- dbconn(x@backend)
        if (inherits(x@backend, "MsBackendOfflineSql"))
            on.exit(dbDisconnect(con))
        if (any(dbListTables(con) == "sample_to_msms_spectrum"))
            unname(as.matrix(
                dbGetQuery(con, "select * from sample_to_msms_spectrum")))
        else matrix(ncol = 2, nrow = 0)
    } else matrix(ncol = 2, nrow = 0)
}
