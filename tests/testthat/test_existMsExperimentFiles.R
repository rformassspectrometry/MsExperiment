test_that("existMsExperimentFiles works", {
    res <- existMsExperimentFiles(MsExperimentFiles())
    expect_true(length(res) == 0)

    x <- MsExperimentFiles(a = "some_file", b = "other_file", d = tempfile())
    res <- existMsExperimentFiles(x)
    expect_true(length(res) == length(x))
    expect_true(all(all(!res)))

    expect_message(res <- existMsExperimentFiles(mse), "mzML_file: 2 out")
    expect_true(all(res[[1L]]))
    expect_false(res[[2L]])
})
