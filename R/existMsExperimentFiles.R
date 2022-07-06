#' @param object The `existMsExperimentFiles()` fonction works with
#'     either an instance of `MsExperimentFiles` or `MsExperiment`.
#'
#' @export
#'
#' @importFrom IRanges LogicalList
#'
#' @rdname MsExperimentFiles
existMsExperimentFiles <- function(object) {
    if (inherits(object, "MsExperiment"))
        object <- experimentFiles(object)
    stopifnot(inherits(object, "MsExperimentFiles"))
    res <- LogicalList(lapply(object, file.exists))
    message(paste0(names(res), ": ", vapply(res, sum, integer(1)),
                   " out of ", lengths(res), " exist(s)\n"))
    invisible(res)
}
