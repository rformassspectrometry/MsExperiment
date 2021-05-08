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
                 cbind(sampleIndex = 1:2, withIndex = 2:1))
    expect_warning(linkSampleData(
        res, with = "experimentFiles.mzML_file", withIndex = 1:2),
        "Overwriting")

    ## link all sample to one file.
    res <- linkSampleData(mse, with = "experimentFiles.mzML_file",
                          withIndex = c(1, 1))
    expect_equal(res@sampleDataLinks[["experimentFiles.mzML_file"]],
                 cbind(sampleIndex = 1:2, withIndex = c(1L, 1L)))

    ## link one sample to one file.
    res <- linkSampleData(mse, with = "experimentFiles.mzML_file",
                          sampleIndex = 1L, withIndex = 2L)
    expect_equal(res@sampleDataLinks[["experimentFiles.mzML_file"]],
                 cbind(sampleIndex = 1L, withIndex = 2L))

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
    expect_error(res <- tmp[, 4], "out-of-bound")
    expect_error(res <- tmp[1, 2], "is supported")
    expect_error(res <- tmp[, c(TRUE, FALSE, TRUE)], "number of")
    expect_warning(tmp[, c("b")], "rownames")

    res <- tmp[, c(TRUE, FALSE)]
    expect_equal(sampleData(res), sampleData(tmp)[1, ])
})
