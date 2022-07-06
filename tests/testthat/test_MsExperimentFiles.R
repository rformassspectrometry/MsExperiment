test_that(".valid_names_non_null works", {
    x <- 1:3
    names(x) <- c("a", "b", "c")
    expect_null(MsExperiment:::.valid_names_non_null(x))
    expect_null(MsExperiment:::.valid_names_non_null(list()))
    expect_match(MsExperiment:::.valid_names_non_null(1:2), "named")
})

test_that(".valid_names_non_empty works", {
    x <- 1:3
    names(x) <- c("a", "", "b")
    expect_match(MsExperiment:::.valid_names_non_empty(x), "empty")

    names(x) <- c("a", "x", "b")
    expect_null(MsExperiment:::.valid_names_non_empty(x))
})

test_that("MsExperimentFiles constructor works", {
    res <- MsExperimentFiles()
    expect_s4_class(res, "MsExperimentFiles")
    expect_true(length(res) == 0)

    res <- MsExperimentFiles(a = 4, b = 5)
    expect_true(length(res) == 2)
    expect_equal(names(res), c("a", "b"))
    expect_true(validObject(res))
})

test_that("show,MsExperimentFiles works", {
    expect_output(show(MsExperimentFiles()), "MsExperimentFiles")
})
