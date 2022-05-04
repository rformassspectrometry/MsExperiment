#' @title A class to store experiment files
#'
#' @aliases MsExperimentFiles-class MsExperimentFiles
#'
#' @name MsExperimentFiles
#'
#' @description
#'
#' The `MsExperimentFiles` class stores files that are part of a mass
#' spectrometry experiment. The objects are created with the
#' `MsExperimentFiles()` function.
#'
#' The files encoded in a `MsExperimentFiles` instance don't need to
#' exist on the current filesystem - sometimes, these might be created
#' in anticipation of their creation. The `existMsExperimentFiles()`
#' function can be used to verify which ones currently exist: it
#' returns a list of logicals (formally an instance of
#' [IRanges::LogicalList()] of lenghts equal to the
#' `MsExperimentFiles` used as input.
#'
#' @author Laurent Gatto
#'
#' @return `MsExperimentFiles` returns an instance of `MsExperimentFiles`.
#'
#' @examples
#' fls <- MsExperimentFiles(mzmls = c("/path/to/f1.mzML", "/path/to/f2.mzML"),
#'                          mzids = "/another/path/to/id1.mzid",
#'                          fasta = "file.fas")
#' fls
#'
#' ## A new MsExperimentFiles containing mzML or mzid files
#' fls[1]
#' fls["mzids"]
#'
#' ## The actual file names
#' fls[[1]]
#' fls[[2]]
#' fls[["fasta"]]
#'
#' ## None of the files used in this example actually exist
#' existMsExperimentFiles(fls)
NULL

#' @name MsExperimentFiles-class
#'
#' @docType class
#'
#' @exportClass MsExperimentFiles
#'
#' @importClassesFrom IRanges SimpleCharacterList
#'
#' @noRd
setClass("MsExperimentFiles",
         contains = "SimpleCharacterList")

#' @export
#'
#' @rdname MsExperimentFiles
#'
#' @importFrom IRanges CharacterList
#'
#' @importFrom S4Vectors metadata metadata<-
#'
#' @importFrom methods new as validObject
#'
#' @param ... Either a named list or a set of named vectors. All
#'     elements are coerced to characters.
#'
#' @param metadata `list()` holding arbitrary R objects as
#'     annotations.
MsExperimentFiles <- function(..., metadata = list()) {
    ans <- new("MsExperimentFiles")
    ans@listData <- CharacterList(..., compress = FALSE)@listData
    if (length(metadata)) metadata(ans) <- metadata
    if (validObject(ans)) ans
}


.valid_names_non_null <- function(object) {
    if (length(object) & is.null(names(object)))
        return("List of file names must be named.")
    NULL
}

.valid_names_non_empty <- function(object) {
    if (any(names(object) == ""))
        return("Names musn't be empty")
    NULL
}

## .valid_files_exist  <- function(object) {
## }

setValidity("MsExperimentFiles",
            function(object) {
                msg <- c(.valid_names_non_null(object),
                         .valid_names_non_empty(object))
                if (is.null(msg)) TRUE
                else msg
            })


#' @rdname MsExperimentFiles
#'
#' @param object An instance of class `MsExperimentFiles`.
#'
#' @importMethodsFrom methods show
#'
#' @exportMethod show
setMethod("show", "MsExperimentFiles",
          function(object) {
              cat("MsExperimentFiles of length ", length(object), "\n")
              IRanges:::.showAtomicList(lapply(object, basename), 10)
          })
