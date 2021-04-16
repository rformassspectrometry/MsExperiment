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
#' - Chromatographic data is stored as `Chromatograms()` objects.
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
#' @import ProtGenerics
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
             ExperimentFiles = "MsExperimentFiles",
             ## Spectra = "Spectra",
             ## QFeatures = "QFeatures",
             ## Chromatograms = "Chromatograms",
             colData = "DataFrame",
             metadata = "list"))


#' @rdname MsExperiment
#'
#' @param object An instance of class `MsExperiment`.
#'
#' @exportMethod show
setMethod("show", "MsExperiment",
          function(object) cat("Object of class", class(object), "\n"))
