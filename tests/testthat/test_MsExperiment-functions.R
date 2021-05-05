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

test_that(".valid_link works", {
    expect_error(.valid_link(3), "needs to be")
    expect_error(.valid_link(matrix(ncol = 2, nrow = 4, "a")), "needs to be")
    expect_error(.valid_link(matrix(ncol = 3, nrow = 2, 1L)), "2 columns")
    mat <- cbind(1:3, 1L)
    expect_error(.valid_link(mat, nfrom = 1), " <= 1")
    mat[, 2] <- -1L
    expect_error(.valid_link(mat, nfrom = 4), "smaller than 1")
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
    expect_equal(mcols(res@sampleDataLinks)["metadata.test_char", "subsetBy"],
                 1L)

    res@metadata$other_var = c("b")
    mat <- cbind(1L, 1L)
    res <- .add_sample_data_link(x, mat, with = "metadata.other_var",
                                 subsetBy = 2L)
    expect_equal(mcols(res@sampleDataLinks)["metadata.other_var", "subsetBy"],
                 2L)
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

test_that(".set_element works", {
    tmp <- new("LinkedMsExperiment")
    expect_error(.set_element(tmp, "does_not_exist", 4), "No slot")
    res <- .set_element(tmp, "metadata.new_val", data.frame(id = 1:3))
    expect_true(names(metadata(res)) == "new_val")
    expect_equal(metadata(res)$new_val, data.frame(id = 1:3))

    res <- .set_element(tmp, "spectra", spectra(mse)[1:3])
    expect_equal(length(spectra(res)), 3)
    res <- .set_element(res, "spectra.new_val", "a")
    expect_true(all(spectra(res)$new_val == "a"))
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

test_that(".extractSamples works", {
    tmp <- as(mse, "LinkedMsExperiment")
    res <- MsExperiment:::.extractSamples(tmp, j = 2)
    expect_equal(spectra(res), spectra(tmp))
    expect_equal(sampleData(res), sampleData(tmp)[2, ])

    ## Establish links.
    ## n:1 mapping
    tmp <- linkSampleData(tmp, "experimentFiles.other_file",
                          fromIndex = c(1, 2), toIndex = c(1, 1))
    ## 1:1 mapping
    tmp <- linkSampleData(tmp, "experimentFiles.mzML_file",
                          fromIndex = c(1, 2), toIndex = c(1, 2))
    ## 1:n mapping
    tmp <- linkSampleData(tmp, "spectra",
                          fromIndex = match(basename(spectra(mse)$dataOrigin),
                                            sampleData(mse)$mzML_file),
                          toIndex = seq_along(spectra(mse)))
    ## n:m mapping
    metadata(tmp)[["multivals"]] <- c("AB", "A", "B")
    tmp <- linkSampleData(tmp, "metadata.multivals", fromIndex = c(1, 1, 2, 2),
                          toIndex = c(1, 2, 1, 3))
    ## link a data.frame
    metadata(tmp)$df <- data.frame(a = 1:3, b = 5:7)
    tmp <- linkSampleData(tmp, "metadata.df", fromIndex = c(1, 2), toIndex = c(1, 2))
    ## Add a non-linked element
    metadata(tmp)$not_linked <- "not linked"

    ## Extract a single sample
    res <- MsExperiment:::.extractSamples(tmp, j = 2)
    expect_equal(sampleData(res), sampleData(tmp)[2, ])
    expect_equal(experimentFiles(res)$other_file, "other_file.txt")
    expect_equal(experimentFiles(res)$mzML_file, experimentFiles(tmp)$mzML_file[2])
    expect_equal(spectra(res), spectra(tmp)[932:1862])
    expect_equal(metadata(res)$multivals, c("AB", "B"))
    expect_equal(metadata(res)$df, metadata(tmp)$df[2, , drop = FALSE])
    expect_equal(metadata(res)$not_linked, "not linked")

    ## Re-ordering of samples
    res <- MsExperiment:::.extractSamples(tmp, j = c(2, 1))
    expect_equal(sampleData(res), sampleData(tmp)[2:1, ])
    ## ! n:1 mapping gets duplicated!
    expect_equal(experimentFiles(res)$other_file, rep("other_file.txt", 2))
    ## 1:n mapping are fine
    expect_equal(experimentFiles(res)$mzML_file, experimentFiles(tmp)$mzML_file[2:1])
    expect_equal(rtime(spectra(res))[932:1862], rtime(spectra(tmp))[1:931])
    expect_equal(rtime(spectra(res))[1:931], rtime(spectra(tmp))[932:1862])
    expect_equal(metadata(res)$df, metadata(tmp)$df[2:1, , drop = FALSE])
    ## ! n:m mapping gets duplicated!
    expect_equal(metadata(res)$multivals, c("AB", "B", "AB", "A"))

    ## Duplication of samples
    res <- MsExperiment:::.extractSamples(tmp, j = c(2, 2))
    expect_equal(sampleData(res), sampleData(tmp)[c(2, 2), ])
    expect_equal(experimentFiles(res)$other_file, rep("other_file.txt", 2))
    expect_equal(experimentFiles(res)$mzML_file, experimentFiles(tmp)$mzML_file[c(2, 2)])
    expect_equal(spectra(res)[1:931], spectra(tmp)[932:1862])
    expect_equal(spectra(res)[932:1862], spectra(tmp)[932:1862])
    expect_equal(metadata(res)$multivals, c("AB", "B", "AB", "B"))
    expect_equal(metadata(res)$df, metadata(tmp)$df[c(2, 2), , drop = FALSE])
    expect_equal(metadata(res)$not_linked, "not linked")
})
