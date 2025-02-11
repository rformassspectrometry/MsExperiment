library(MsBackendSql)
library(RSQLite)
ref_sql <- tempfile()
ref_con <- dbConnect(SQLite(), ref_sql)
mse_db <- mse
spectra(mse_db) <- setBackend(spectra(mse_db), MsBackendSql(), dbcon = ref_con)
dbWriteSampleData(mse_db)

test_that("dbWriteSampleData works", {
    expect_error(dbWriteSampleData("a"), "MsExperiment")
    m <- MsExperiment()
    expect_null(dbWriteSampleData(m))
    ## MsExperiment with Spectra but not using MsBackendSql
    expect_error(dbWriteSampleData(mse), "MsBackendSql")

    s <- spectra(mse)
    sql_file <- tempfile()
    con <- dbConnect(SQLite(), sql_file)
    s <- setBackend(s, MsBackendSql(), dbcon = con)
    s$f <- basename(dataOrigin(s))
    spectra(m) <- s
    ## Empty sampleData
    dbWriteSampleData(m)
    expect_equal(dbListTables(con),
                 c("msms_spectrum", "msms_spectrum_peak_blob2", "sample_data"))
    expect_true(nrow(dbGetQuery(con, "select * from sample_data")) == 0L)

    ## Only sampleData
    sd <- data.frame(fl = basename(fls), sample_name = c("A", "B"),
                     sample_index = c(1, 4))
    sampleData(m) <- DataFrame(sd)
    expect_message(dbWriteSampleData(m), "overwrite")
    res <- dbGetQuery(con, "select * from sample_data")
    expect_equal(res[, colnames(sd)], sd)

    ## sampleData and sample to spectra mapping
    m <- linkSampleData(m, with = "sampleData.fl = spectra.f")
    expect_message(dbWriteSampleData(m), "overwrite")
    res <- dbGetQuery(con, "select * from sample_data")
    expect_equal(res[, colnames(sd)], sd)
    expect_equal(dbListTables(con),
                 c("msms_spectrum", "msms_spectrum_peak_blob2",
                   "sample_data", "sample_to_msms_spectrum"))
    res <- dbGetQuery(con, "select * from sample_to_msms_spectrum")
    res <- unname(as.matrix(res))
    expect_equal(res, m@sampleDataLinks[["spectra"]])
    dbDisconnect(con)

    ## Same with a MsBackendOfflineSql
    sql_file <- tempfile()
    s <- setBackend(spectra(mse), MsBackendOfflineSql(),
                    dbname = sql_file, drv = SQLite(),
                    BPPARAM = SerialParam())
    s$f <- basename(dataOrigin(s))
    spectra(m) <- s

    dbWriteSampleData(m)
    con <- dbConnect(SQLite(), dbname = sql_file)
    expect_equal(dbListTables(con),
                 c("msms_spectrum", "msms_spectrum_peak_blob2",
                   "sample_data", "sample_to_msms_spectrum"))
    res <- dbGetQuery(con, "select * from sample_to_msms_spectrum")
    res <- unname(as.matrix(res))
    expect_equal(res, m@sampleDataLinks[["spectra"]])
    dbDisconnect(con)
})

test_that(".db_get_sample_data works", {
    res <- .db_get_sample_data(spectra(mse))
    expect_equal(res, data.frame())

    res <- .db_get_sample_data(spectra(mse_db))
    expect_equal(res[, 1:3], as.data.frame(sampleData(mse_db)))
})

test_that(".db_get_sample_spectra_link works", {
    res <- .db_get_sample_spectra_link(spectra(mse))
    expect_equal(res, matrix(ncol = 2, nrow = 0))

    res <- .db_get_sample_spectra_link(spectra(mse_db))
    expect_equal(res, matrix(ncol = 2, nrow = 0))

    spectra(mse_db)$file <- basename(dataOrigin(spectra(mse_db)))
    mse_db <- linkSampleData(
        mse_db, with = "sampleData.mzML_file = spectra.file")
    dbWriteSampleData(mse_db)

    res <- .db_get_sample_spectra_link(spectra(mse_db))
    expect_equal(res, mse_db@sampleDataLinks[["spectra"]])
})

dbDisconnect(ref_con)