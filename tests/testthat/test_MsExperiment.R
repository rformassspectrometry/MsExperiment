test_that("linkSampleData,MsExperiment works", {
    res <- linkSampleData(mse)
    expect_true(length(res@sampleDataLinks) == 0)

    expect_error(linkSampleData(mse, with = "experimentFile.fls"),
                 "unsupported")
    expect_error(
        linkSampleData(mse, with = "experimentFiles.mzML_file", withIndex = 1),
        "Length")
    expect_error(linkSampleData(
        mse, with = "experimentFile.mzML_file", withIndex = c(2, 1)), "No slot")
    expect_error(linkSampleData(
        mse, with = "experimentFiles.mzML_files", withIndex = c(2, 1)), "empty")

    res <- linkSampleData(mse, with = "experimentFiles.mzML_file",
                          withIndex = c(2, 1))
    expect_true(length(res@sampleDataLinks) == 1)
    expect_equal(res@sampleDataLinks[["experimentFiles.mzML_file"]],
                 cbind(1:2, 2:1))
    expect_warning(linkSampleData(
        res, with = "experimentFiles.mzML_file", withIndex = 1:2),
        "Overwriting")

    ## link all sample to one file.
    res <- linkSampleData(mse, with = "experimentFiles.mzML_file",
                          withIndex = c(1, 1))
    expect_equal(res@sampleDataLinks[["experimentFiles.mzML_file"]],
                 cbind(1:2, c(1L, 1L)))

    ## link one sample to one file.
    res <- linkSampleData(mse, with = "experimentFiles.mzML_file",
                          sampleIndex = 1L, withIndex = 2L)
    expect_equal(res@sampleDataLinks[["experimentFiles.mzML_file"]],
                 cbind(1L, 2L))

    ## link using a SQL-like expression
    expect_error(linkSampleData(mse, with = "bla.blu = ble.blo"),
                 "one of the slot")

    sampleData(mse)$orgfile <- unique(spectra(mse)$dataOrigin)
    res <- linkSampleData(mse, with = "sampleData.orgfile = spectra.dataOrigin")
    res2 <- linkSampleData(mse, with = "spectra.dataOrigin = sampleData.orgfile")
    expect_equal(res@sampleDataLinks[[1L]], res2@sampleDataLinks[[1L]])

    res3 <- linkSampleData(
        mse, with = "spectra",
        sampleIndex = match(basename(spectra(mse)$dataOrigin),
                            sampleData(mse)$mzML_file),
        withIndex = seq_along(spectra(mse)))
    expect_equal(unname(res@sampleDataLinks[[1L]]),
                 unname(res3@sampleDataLinks[[1L]]))
})

test_that("[,LinkedMsExperiment works", {
    tmp <- mse
    expect_error(res <- tmp[4], "out-of-bound")
    expect_error(res <- tmp[1, 2], "is supported")
    expect_error(res <- tmp[c(TRUE, FALSE, TRUE)], "number of")
    expect_warning(tmp[c("b")], "rownames")

    res <- tmp[c(TRUE, FALSE)]
    expect_equal(sampleData(res), sampleData(tmp)[1, ])

    ## Subset including qdata
    library(SummarizedExperiment)
    sd <- DataFrame(sample = c("QC1", "QC2", "QC3"), idx = c(1, 3, 5))
    se <- SummarizedExperiment(colData = sd, assay = cbind(1:10, 11:20, 21:30))

    mse2 <- mse
    qdata(mse2) <- se

    res <- mse2[2]
    expect_equal(length(res), 1L)
    expect_equal(sampleData(res), sampleData(mse2)[2, ])
    expect_equal(spectra(res), spectra(mse2))
    expect_equal(qdata(res), qdata(mse2))

    ## Link spectra
    spectra(mse2)$mzML_file <- basename(spectra(mse2)$dataOrigin)
    mse2 <- linkSampleData(
        mse2, with = "sampleData.mzML_file = spectra.mzML_file")
    res <- mse2[2]
    expect_equal(
        spectra(res),
        spectra(mse2)[spectra(mse2)$mzML_file == "20171016_POOL_POS_3_105-134.mzML"])
    res <- mse2[1]
    expect_equal(
        spectra(res),
        spectra(mse2)[spectra(mse2)$mzML_file == "20171016_POOL_POS_1_105-134.mzML"])

    ## Link experiment files
    mse2 <- linkSampleData(mse2, with = "experimentFiles.mzML_file",
                           sampleIndex = c(1, 2), withIndex = c(1, 2))
    res <- mse2[2]
    expect_equal(experimentFiles(res)[["mzML_file"]],
                 experimentFiles(mse2)[["mzML_file"]][2L])
    res <- mse2[1]
    expect_equal(experimentFiles(res)[["mzML_file"]],
                 experimentFiles(mse2)[["mzML_file"]][1L])

    ## Link qdata
    mse2 <- linkSampleData(mse2, with = "sampleData.sample = qdata.sample")
    res <- mse2[2]
    expect_equal(qdata(res), qdata(mse2)[, 2L])
    res <- mse2[1]
    expect_equal(qdata(res), qdata(mse2)[, 1L])

    ##
    qdata(mse2) <- se[, 1:2]
    res <- mse2[1:2]
    expect_equal(res, mse2)

    ## Subsetting with negative indices
    msel <- mse
    spectra(msel)$mzML_file <- basename(spectra(msel)$dataOrigin)
    msel <- linkSampleData(
        msel, with = "sampleData.mzML_file = spectra.mzML_file")
    res <- msel[-1]
    expect_equal(length(res), (length(msel) - 1))
    expect_equal(sampleData(res)$sample, sampleData(msel)$sample[-1])
    expect_equal(unique(basename(spectra(res)$dataOrigin)),
                 unique(basename(spectra(msel)$dataOrigin))[2])

    expect_error(msel[c(1, -2)], "not supported")

    ## Multiple files/samples.
    sd <- data.frame(sample = c("A", "B", "C"))
    f <- c(fls, system.file("microtofq", "MM14.mzML", package = "msdata"))
    msel <- readMsExperiment(spectraFiles = f, sampleData = sd)
    res <- msel[-2]
    expect_true(length(res) == 2L)
    expect_equal(sampleData(res)$sample, c("A", "C"))
    expect_equal(unique(basename(spectra(res)$dataOrigin)),
                 unique(basename(spectra(msel)$dataOrigin))[c(1, 3)])

    ref <- msel[2]
    res <- msel[c(-1, -3)]
    expect_equal(sampleData(ref), sampleData(res))
    expect_equal(spectra(ref), spectra(res))
    expect_equal(ref@sampleDataLinks, res@sampleDataLinks)
})

test_that("MsExperiment works", {
    m <- MsExperiment()
    expect_s4_class(m, "MsExperiment")

    sdata <- data.frame(sample_name = c("A", "B"), sample_index = c(4, 12),
                        sample_file = basename(fls))
    s <- Spectra(fls)
    s$file <- basename(dataOrigin(s))

    m <- MsExperiment(spectra = s)
    expect_equal(spectra(m), s)

    m <- MsExperiment(spectra = s, sampleData = sdata)
    expect_equal(spectra(m), s)
    expect_equal(sampleData(m), DataFrame(sdata))

    s_sql <- tempfile()
    s <- setBackend(s, MsBackendOfflineSql(), drv = RSQLite::SQLite(),
                    dbname = s_sql, BPPARAM = SerialParam())

    m <- MsExperiment(spectra = s)
    expect_equal(spectra(m), s)
    expect_equal(sampleData(m), DataFrame())

    m <- MsExperiment(spectra = s, sampleData = sdata)
    dbWriteSampleData(m)

    ## Read sample data from database
    m <- MsExperiment(spectra = s)
    expect_equal(spectra(m), s)
    expect_equal(sampleData(m)[, 1:3], DataFrame(sdata))

    m <- MsExperiment(spectra = s, sampleData = sdata)
    expect_equal(sampleData(m), DataFrame(sdata))

    m <- linkSampleData(m, with = "sampleData.sample_file = spectra.file")
    dbWriteSampleData(m)
    m2 <- MsExperiment(spectra = s)
    expect_equal(sampleData(m2)[, 1:3], DataFrame(sdata))
    expect_equal(m@sampleDataLinks[["spectra"]],
                 m2@sampleDataLinks[["spectra"]])
})

test_that("show,MsExperiment works", {
    expect_output(show(MsExperiment()), "MsExperiment")
    expect_output(show(mse), "Experiment data")
    expect_output(show(MsExperiment()), "Empty object")
    otherData(mse)$X <- 1
    expect_output(show(mse), "Other data: X")
})

test_that("metadata<-,metadata,MsExperiment works", {
    m <- MsExperiment()
    metadata(m) <- list(version = "1.2", data = "1900")
    res <- metadata(m)
    expect_equal(res$version, "1.2")
    expect_equal(res$data, "1900")
})

test_that("spectra<-,spectra,MsExperiment works", {
    m <- MsExperiment()
    expect_null(spectra(m))

    res <- spectra(mse)
    expect_s4_class(res, "Spectra")
    expect_true(length(res) > 0)

    spectra(m) <- res
    expect_equal(spectra(m), res)

    expect_error(spectra(m) <- "b")
})


test_that("otherData<-,otherData,MsExperiment works", {
    m <- MsExperiment()
    expect_identical(length(otherData(m)), 0L)
    otherData(m)$NUM <- 1
    expect_identical(length(otherData(m)), 1L)
    expect_identical(names(otherData(m)), "NUM")
    expect_identical(otherData(m)[["NUM"]], 1)
    otherData(m)[["NUM"]] <- NULL
    expect_identical(length(otherData(m)), 0L)
})
