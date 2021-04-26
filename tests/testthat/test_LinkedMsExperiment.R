test_that(".valid_link works", {
    expect_error(.valid_link(3), "needs to be")
    expect_error(.valid_link(matrix(ncol = 2, nrow = 4, "a")), "needs to be")
    expect_error(.valid_link(matrix(ncol = 3, nrow = 2, 1L)), "2 columns")
    mat <- cbind(1:3, 1L)
    expect_error(.valid_link(mat, nfrom = 1), " <= 1")
    mat[, 2] <- -1L
    expect_error(.valid_link(mat, nfrom = 4), "smaller than 1")
})

test_that(".get_element works", {
    res <- .get_element(mse, "sampleData.mzML_file")
    expect_equal(res, sampleData(mse)$mzML_file)
    res <- .get_element(mse, "sampleData.dont_exist")
    expect_equal(res, NULL)

    expect_error(.get_element(mse, "noslot"), "No slot named")

    mse@metadata$new_entry.dot <- "value"
    res <- .get_element(mse, "metadata.new_entry.dot")
    expect_equal(res, "value")

    res <- .get_element(mse, "sampleData")
    expect_equal(res, sampleData(mse))

    res <- .get_element(mse, "experimentFiles.other_file")
    expect_equal(res, "other_file.txt")
})

test_that(".add_sample_data_link works", {
    x <- new("LinkedMsExperiment")
    mat <- cbind(1:3, 1L)
    expect_equal(.add_sample_data_link(x, mat), x)

    sampleData(x) <- DataFrame(df)
    x@metadata$test <- 1:5
    expect_error(.add_sample_data_link(x, mat, with = "metadata.test"), "<= 2")
    expect_error(.add_sample_data_link(x, mat[1:2, ], "spectra"), "empty")
    
    x@metadata$test_char <- c("a", "b", "c")
    mat <- mat[1:2, ]
    mat[, 2] <- c(3L, 1L)
    res <- .add_sample_data_link(x, mat, with = "metadata.test_char")
    expect_true(names(res@sampleDataLinks) == "metadata.test_char")
})

test_that(".sample_data_links works", {
    x <- new("LinkedMsExperiment")
    expect_equal(.sample_data_links(x), List())

    x@sampleDataLinks$a <- cbind(1:2, 2:3)
    x@sampleDataLinks$b <- cbind(1:4, 1:4)

    res <- .sample_data_links(x, "a")
    expect_equal(res[[1L]], cbind(1:2, 2:3))
    
    res <- .sample_data_links(x, "d")
    expect_true(length(res) == 0)
})

test_that(".parse_join_string works", {
    res <- .parse_join_string("spectra.dataOrigin =  otherData.something")
    expect_equal(res, c("spectra", "dataOrigin", "otherData", "something"))
    expect_error(.parse_join_string("does not work"), "unsupported format")    
})

test_that(".link_matrix works", {
    res <- .link_matrix()
    expect_true(is.matrix(res))
    expect_true(nrow(res) == 0)

    res <- .link_matrix(c("a", "a", "b", "d", "b", "c"), c("g", "a", "b", "e"))
    expect_equal(res, cbind(c(1, 2, 3, 5), c(2, 2, 3, 3)))
})

test_that("linkSampleData,MsExperiment works", {
    res <- linkSampleData(mse)
    expect_true(length(res@sampleDataLinks) == 0)

    expect_error(linkSampleData(mse, with = "experimentFile.fls"),
                 "unsupported")
    expect_error(
        linkSampleData(mse, with = "experimentFiles.mzML_file", toIndex = 1),
        "Length")
    expect_error(linkSampleData(
        mse, with = "experimentFile.mzML_file", toIndex = c(2, 1)), "No slot")
    expect_error(linkSampleData(
        mse, with = "experimentFiles.mzML_files", toIndex = c(2, 1)), "empty")

    res <- linkSampleData(mse, with = "experimentFiles.mzML_file",
                          toIndex = c(2, 1))
    expect_true(length(res@sampleDataLinks) == 1)
    expect_equal(res@sampleDataLinks[["experimentFiles.mzML_file"]],
                 cbind(fromIndex = 1:2, toIndex = 2:1))
    expect_warning(res <- linkSampleData(
                       res, with = "experimentFiles.mzML_file", toIndex = 1:2),
                   "Overwriting")

    ## link all sample to one file.
    res <- linkSampleData(mse, with = "experimentFiles.mzML_file",
                          toIndex = c(1, 1))
    expect_equal(res@sampleDataLinks[["experimentFiles.mzML_file"]],
                 cbind(fromIndex = 1:2, toIndex = c(1L, 1L)))
    
    ## link one sample to one file.
    res <- linkSampleData(mse, with = "experimentFiles.mzML_file",
                          fromIndex = 1L, toIndex = 2L)
    expect_equal(res@sampleDataLinks[["experimentFiles.mzML_file"]],
                 cbind(fromIndex = 1L, toIndex = 2L))

    ## link using a SQL-like expression
    expect_error(linkSampleData(mse, with = "bla.blu = ble.blo"),
                 "one of the slot")

    sampleData(mse)$orgfile <- unique(spectra(mse)$dataOrigin)
    res <- linkSampleData(mse, with = "sampleData.orgfile = spectra.dataOrigin")
    res2 <- linkSampleData(mse, with = "spectra.dataOrigin = sampleData.orgfile")
    expect_equal(res@sampleDataLinks[[1L]], res2@sampleDataLinks[[1L]])

    res3 <- linkSampleData(mse, with = "spectra",
                           fromIndex = match(basename(spectra(mse)$dataOrigin),
                                             sampleData(mse)$mzML_file),
                           toIndex = seq_along(spectra(mse)))
    expect_equal(unname(res@sampleDataLinks[[1L]]),
                 unname(res3@sampleDataLinks[[1L]]))
})
