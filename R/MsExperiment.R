#' @title Managing Mass Spectrometry Experiments
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
#' - Metadata:
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
#' @docType package
#' @name MsExperiment
#'
#' @md
#'
#' @import Spectra
#' @import methods
#' @import QFeatures
#' @import ProtGenerics
NULL

setClass("MsExperiment",
         slots = c(Spectra = "Spectra",
                   QFeatures = "QFeatures",
                   ## Chromatograms = "Chromatograms",
                   colData = "DataFrame",
                   metadata = "list"))

setMethod("show", "MsExperiment",
          function(object) cat("Object of class", class(object), "\n"))
