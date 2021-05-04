#' @importClassesFrom Spectra Spectra
#'
#' @importClassesFrom QFeatures QFeatures
#'
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @noRd
setClassUnion("MsExperimentFilesOrNull", c("NULL", "MsExperimentFiles"))
setClassUnion("SpectraOrNull", c("NULL", "Spectra"))

setClassUnion("QFeaturesOrSummarizedExperiment", c("SummarizedExperiment", "QFeatures"))
setClassUnion("QFeaturesOrSummarizedExperimentOrNull", c("NULL", "QFeaturesOrSummarizedExperiment"))


#' @title Managing Mass Spectrometry Experiments
#'
#' @aliases MsExperiment-class MsExperiment metadata,MsExperiment
#'
#' @description
#'
#' The `MsExperiment` class allows the storage and management of all
#' aspects related to a complete proteomics or metabolomics mass
#' spectrometry experiment. This includes experimantal design, raw
#' mass spectromtry data as spectra and chromatograms, quantitative
#' features, and identification data.
#'
#' For details, see https://rformassspectrometry.github.io/MsExperiment
#'
#' This package is part of the RforMassSpectrometry initiative:
#' https://www.rformassspectrometry.org/
#'
#' @details
#'
#' An experiment is typically composed of several items
#'
#' - Files to data or annotations. There are stored in the
#'   `experimentFiles` slot as an instance of class `MsExperimentFiles`.
#'
#' - General metadata about the experiment, stored as a `list` in the
#'   `metadata` slot.
#'
#' - Mass spectrometry data. Sectra and their metadata are stored as
#'   `Spectra()` objects in the `spectra` object. Chromatographic is
#'   noy yet supported but will be stored as `Chromatograms()` objects
#'   in the `chromatorgrams` slot.
#'
#' - Quantification data is stored as `QFeatures()` objects in the
#'   `qfeatures` slot.
#'
#' - Any additional data, be it other spectra data, or proteomics
#'   identification data, i.e peptide-spectrum matches is defined as
#'   `PSM()` objects can be added as elements to the list stored in
#'   the `otherData` slot.
#'
#' @name MsExperiment
#'
#' @import methods
#'
#' @importFrom S4Vectors List DataFrame
#'
#' @importClassesFrom S4Vectors List
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
#' @slot experimentFiles An instance of class `MsExperimentFiles` or `NULL`.
#'
#' @slot spectra An instance of class `Spectra` or `NULL`.
#'
#' @slot qfeatures An instance of class `QFeatures` or `NULL`.
#'
#' @slot otherData A `List` to store any additional data objects.
#'
#' @slot sampleData A `DataFrame` documenting the experimental design.
#'
#' @slot metadata A `list` to store additional metadata.
#'
#' @rdname MsExperiment
setClass("MsExperiment",
         slots = c(
             experimentFiles = "MsExperimentFilesOrNull",
             spectra = "SpectraOrNull",
             qfeatures = "QFeaturesOrSummarizedExperimentOrNull",
             ## chromatograms = "Chromatograms",
             otherData = "List",
             sampleData = "DataFrame",
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
                  cat(" Spectra:", paste0("MS", names(mstab), " (", mstab, ")"),
                      "\n")
              }
              if (nrow(object@sampleData)) {
                  cat(" Experiment data:",
                      nrow(object@sampleData), "sample(s)\n")
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
#' @param object An instance of class `MsExperiment`
#'
#' @rdname MsExperiment
sampleData  <- function(object) {
    stopifnot(inherits(object, "MsExperiment"))
    object@sampleData
}

#' @export
#'
#' @param value An object of the appropriate class for the slot to be
#'     populated.
#'
#' @rdname MsExperiment
"sampleData<-" <- function(object, value) {
    stopifnot(inherits(value, "DataFrame"))
    stopifnot(inherits(object, "MsExperiment"))
    object@sampleData <- value
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

#' @export
#'
#' @importFrom S4Vectors metadata
#'
#' @param x An instance of `MsExperiment`.
#'
#' @rdname MsExperiment
setMethod("metadata", "MsExperiment", function(x) x@metadata)

#' @export
#'
#' @importFrom S4Vectors metadata<-
#'
#' @param value A `list()` to replace the `MsExperiment`'s metadata.
#'
#' @rdname MsExperiment
setReplaceMethod("metadata", "MsExperiment",
                 function(x, value) {
                     if (!is.list(value))
                         stop("replacement 'metadata' value must be a list")
                     if (!length(value))
                         names(value) <- NULL
                     x@metadata <- value
                     x
                 })
