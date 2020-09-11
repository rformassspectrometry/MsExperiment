#' Managing Mass Spectrometry Experiments
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
#' @docType package
#' @name MsExperiment
#'
#' @md
#'
#' @import Spectra
#' @import methods
#' @import Features
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
