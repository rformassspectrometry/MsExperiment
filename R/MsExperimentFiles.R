#' @title Experiment files
#'
#' @aliases MsExperimentFiles-class show,MsExperimentFiles MsExperimentFiles
#'
#' @name MsExperimentFiles
#'
#' @description
#'
#' The `MsExperimentFiles` class stores files that are part of a mass
#' spectrometry experiment. The objects are created with the
#' `MsExperimentFiles()` function that takes a **named** list of
#' characters as input.
#'
#'
#' @author Laurent Gatto
#'
#' @examples
#' fls <- MsExperimentFiles(list(mzmls = c("/path/to/f1.mzML", "/path/to/f2.mzML"),
#'                               mzids = "/another/path/to/id1.mzid",
#'                               fasta = "file.fas"))
#' fls
#'
#' ## A new MsExperimentFiles containing mzML files
#' fls[1]
#'
#' ## The actual file names
#' fls[[1]]
#' fls[[2]]
setClass("MsExperimentFiles",
         slots = c(files = "list"))


#' @export
#'
#' @rdname MsExperimentFiles
#'
#' @param x A named `list()` of characters containing file names.
MsExperimentFiles <- function(x) {
    new("MsExperimentFiles", files = x)
}



.valid_all_characters <- function(object) {
    if (!all(sapply(object@files, is.character)))
        return("File names must be characters.")
    NULL
}

.valid_names_non_null <- function(object) {
    if (length(object@files) & is.null(names(object@files)))
        return("List of file names must be named.")
    NULL
}

.valid_names_non_empty <- function(object) {
    if (any(names(object@files) == ""))
        return("Names musn't be empty")
    NULL
}

.valid_files_exist  <- function(object) {
}

setValidity("MsExperimentFiles",
            function(object) {
                msg <- c(.valid_names_non_null(object),
                         .valid_names_non_empty(object),
                         .valid_all_characters(object))
                if (is.null(msg)) TRUE
                else msg
            })


#' @importMethodsFrom methods show
#'
#' @rdname MsExperimentFiles
#'
#' @param object An instance of class `MsExperimentFiles`.
#'
#' @exportMethod show
setMethod("show", "MsExperimentFiles",
          function(object) {
              if (!length(object@files))
                  cat("Empty object of class", class(object), "\n")
              else {
                  cat("Object of class", class(object), "\n")
                  for (i in seq_along(object@files)) {
                      fls <- basename(object@files[[i]])
                      n <- length(fls)
                      nm <- names(object@files)[i]
                      cat(" ", nm, "(", n, ")", ": ", sep = "")
                      if (n <= 4)
                          cat(paste(fls, collapse = ", "), "\n")
                      else
                          cat(paste(fls[1:2], collapse = ", "),
                              "...",
                              paste(fls[(n-1):n], collapse = ", "),
                              "\n")
                  }
              }
          })



#' @param i index specifying filename elements to extract. Indices are
#'     numeric or character vectors or empty (missing) or `NULL`, as
#'     in lists. See `?[` for details.
#'
#' @rdname MsExperimentFiles
setMethod("[", "MsExperimentFiles",
          function(x, i) x@files[i])


#' @param x An instance of class `MsExperimentFiles`.
#'
#' @rdname MsExperimentFiles
setMethod("[[", "MsExperimentFiles",
          function(x, i) x@files[[i]])
