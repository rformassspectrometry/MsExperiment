setClassUnion("MsExperimentFilesOrNull", c("NULL", "MsExperimentFiles"))
setClassUnion("SpectraOrNull", c("NULL", "Spectra"))

#' @title Managing Mass Spectrometry Experiments
#'
#' @aliases MsExperiment-class MsExperiment
#'
#' @description
#'
#' The `MsExperiment` class allows the storage, management and processing of all
#' aspects related to a complete proteomics or metabolomics mass spectrometry
#' experiment. This includes experimantal design, raw mass spectromtry data as
#' spectra and chromatograms, quantitative features, and identification data.
#'
#' For details, see https://rformassspectrometry.github.io/MsExperiment
#'
#' This package is part of the RforMassSpectrometry initiative:
#' https://www.rformassspectrometry.org/
#'
#' @details
#'
#' - Data files stored in a [MsExperimentFiles()] object.
#'
#' - Metadata: ...
#'
#' - Mass spectrometry data, i.e. spectra and their metadata are
#'   stored as `Spectra()` objects.
#'
#' - Chromatographic data will be stored as `Chromatograms()` objects.
#'
#' - Proteomics identification data, i.e peptide-spectrum matches is
#'   defined as `PSM()` objects. They are generally joined with the
#'   spectra data.
#'
#' - Quantification data is stored as `QFeatures()` objects.
#'
#' @name MsExperiment
#'
#' @import methods
#'
#' @import Spectra
#'
#' @import QFeatures
#'
#' @import ProtGenerics
#'
#' @author Laurent Gatto
#'
#' @examples
#'
#' ## An empty MsExperiment object
#' msexp <- MsExperiment()
#' msexp
#'
#' example(MsExperimentFiles)
#' experimentFiles(msexp) <- fls
#' msexp
NULL

#' @name MsExperiment-class
#'
#' @docType class
#'
#' @exportClass MsExperiment
#'
#' @noRd
setClass("MsExperiment",
         slots = c(
             experimentFiles = "MsExperimentFilesOrNull",
             spectra = "SpectraOrNull",
             qfeatures = "QFeatures",
             ## chromatograms = "Chromatograms",
             colData = "DataFrame",
             metadata = "list"))


#' @rdname MsExperiment
#'
#' @export
MsExperiment <- function()
    new("MsExperiment")

#' @rdname MsExperiment
#'
#' @param object An instance of class `MsExperiment`.
#'
#' @exportMethod show
setMethod("show", "MsExperiment",
          function(object) {
              cat("Object of class", class(object), "\n")
              if (!is.null(experimentFiles(object)))
                  cat(" Files:", paste(names(experimentFiles(object)),
                                       collapse = ", "), "\n")
              if (!is.null(object@spectra)) {
                  mstab <- table(msLevel(object@spectra))
                  cat(" Spectra:", paste0(names(mstab), " (", mstab, ")"),
                      "\n")
              }
          })


## ------------------------------##
##     Getters and setters       ##
## ------------------------------##

#' @export
#'
#' @param object An instance of class `MsExperiment`
#'
#' @rdname MsExperiment
experimentFiles  <- function(object) {
    stopifnot(inherits(object, "MsExperiment"))
    object@experimentFiles
}

#' @export
#'
#' @param value An object of the appropriate class for the slot to be
#'     populated.
#'
#' @rdname MsExperiment
"experimentFiles<-" <- function(object, value) {
    stopifnot(inherits(value, "MsExperimentFiles"))
    stopifnot(inherits(object, "MsExperiment"))
    object@experimentFiles <- value
    object
}


#' @export
#'
#' @importFrom ProtGenerics spectra
#'
#' @rdname MsExperiment
setMethod("spectra", "MsExperiment", function(object) object@spectra)


#' @export
#'
#' @rdname MsExperiment
"spectra<-" <- function(object, value) {
    stopifnot(inherits(value, "Spectra"))
    stopifnot(inherits(object, "MsExperiment"))
    object@spectra <- value
    object
}
