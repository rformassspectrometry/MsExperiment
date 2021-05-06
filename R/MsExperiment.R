#' @include MsExperimentFiles.R

#' @importClassesFrom Spectra Spectra
#'
#' @importClassesFrom QFeatures QFeatures
#'
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @noRd
setClassUnion("MsExperimentFilesOrNull", c("NULL", "MsExperimentFiles"))
setClassUnion("SpectraOrNull", c("NULL", "Spectra"))

setClassUnion("QFeaturesOrSummarizedExperiment",
              c("SummarizedExperiment", "QFeatures"))
setClassUnion("QFeaturesOrSummarizedExperimentOrNull",
              c("NULL", "QFeaturesOrSummarizedExperiment"))


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
#' features, and identification data or any other relevant data files.
#'
#' For details, see https://rformassspectrometry.github.io/MsExperiment
#'
#' This package is part of the RforMassSpectrometry initiative:
#' https://www.rformassspectrometry.org/
#'
#' @section General information:
#'
#' An experiment is typically composed of several items
#'
#' - Description and information (covariates etc) of each sample from the
#'   experiment. These are stored in the `sampleData` slot as a `DataFrame`,
#'   each row being a sample with columns containing all relevant informatio on
#'   that sample.
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
#' - Quantification data is stored as `QFeatures` or
#'   `SummarizedExperiment` objects in the `assay` slot.
#'
#' - Any additional data, be it other spectra data, or proteomics
#'   identification data, i.e peptide-spectrum matches is defined as
#'   `PSM()` objects can be added as elements to the list stored in
#'   the `otherData` slot.
#'
#' @section Linking sample data to other experimental data:
#'
#' By default, an `MsExperiment` is just a loose collection of files and data
#' related to an experiment, no explicit links or associactions are present
#' between the samples and related data. Such links can however be created with
#' the `linkSampleData` function. This function can establish links between
#' individual (or all) samples within the object's `sampleData` to individual,
#' or multiple, data elements or files, such as `Spectra` or raw data files.
#'
#' The presence of such links enables a (coherent) subsetting of an
#' `MsExperiment` by samples. Thus, once the link is defined, any subsetting by
#' sample will also correctly subset the linked data. All other, not linked,
#' data elements are always retained as in the original `MsExperiment`.
#'
#' To be able to link different elements within an `MsExperiment` it is also
#' required to *identify* them with a consistent naming scheme. The naming
#' scheme of slots and data elements within follows an SQL-like scheme, in which
#' the variable (element) is identified by the name of the database table,
#' followed by a `"."` and the name of the database table column. For
#' `MsExperiment`, the naming scheme is defined as
#' `"<slot name>.<element name>"`. A column called `"sample_name"` within the
#' `sampleData` data frame can thus be addressed with
#' `"sampleData.sample_name"`, while `spectra.msLevel` would represent the
#' spectra variable called `msLevel` within the `Spectra` stored in the
#' `spectra` slot.
#'
#' Links between sample data rows and any other data element are stored as
#' `integer` matrices within the `sampleDataLinks` slot of the object (see also
#' the vignette for examples and illustrations). Such links can be defined/added
#' with the `linkSampleData` function which adds a relationship between rows in
#' `sampleData` to elements in any other data within the `MsExperiment` that
#' are specified with parameter `with`. `linkSampleData` supports two different
#' ways to define the link:
#'
#' - Parameter `with` defines the data to which the link should be established.
#'   To link samples to raw data files that would for example be available as a
#'   `character` in an element called `"raw_files"` within the object's
#'   `experimentFiles`, `with = experimentFiles.raw_files` would have to be
#'   used. Next it is required to specify which samples should be linked with
#'   which elements in `with`. This needs to be defined with the parameters
#'   `sampleIndex` and `withIndex`, both are expected to be `integer` vectors
#'   specifying which sample in `sampleData` should be linked to which element
#'   in `with` (see examples below or vignette for examples and details).
#'
#' - As an alternative way, a link could be defined with an SQL-like syntax
#'   that relates a column in `sampleData` to a column/element in the data to
#'   which the link should be established. To link for example individual
#'   spectra to the corresponding samples
#'   `with = "sampleData.raw_file = spectra.dataOrigin"` could be used assuming
#'   that `sampleData` contains a column named `"raw_file"` with the (full path)
#'   of the raw data file for each sample from which the spectra were imported.
#'   In this case both `sampleIndex` and `withIndex` can be omitted, but it is
#'   expected/required that the columns/elements from `sampleData` and the data
#'   element to which the link should be established contain matching values.
#'
#' Note that `linkSampleData` will **replace** a previously existing link to the
#' same data element.
#'
#' @section Subsetting and filtering:
#'
#' - `[`: `MsExperiment` objects can be subsetted **by samples** with `[, j]`
#'   where `j` is the index or a logical defining to which samples the data
#'   should be subsetted. Subsetting by sample will (correctly) subset all
#'   linked data to the respective samples. Not linked data (slots) will be
#'   returned as they are. Subsetting in arbitrary order is supported.
#'   See the vignette for details and examples.
#'
#' @param drop for `[`: ignored.
#'
#' @param i for `[`: not supported.
#'
#' @param j for `[`: an `integer`, `character` or `logical` being the index, the
#'     name (rowname of `sampleData`) or a `logical` of the samples to subset.
#'
#' @param object an `MsExperiment`.
#'
#' @param sampleIndex for `linkSampleData`: `integer` with the indices of the
#'     samples in `sampleData(object)` that should be linked.
#'
#' @param subsetBy for `linkSampleData`: optional `integer(1)` defining the
#'     dimension on which the subsetting will occurr on the linked data.
#'     Defaults to `subsetBy = 1L` thus subsetting will happen on the first
#'     dimension (rows or elements).
#'
#' @param with for `linkSampleData`: `character(1)` defining the data to which
#'     samples should be linked. See section *Linking sample data to other
#'     experimental data* for details.
#'
#' @param withIndex for `linkSampleData`: `integer` with the indices of the
#'     elements in `with` to which the samples (specified by `sampleIndex`)
#'     should be linked to.
#'
#' @param x an `MsExperiment`.
#'
#' @param ... optional additional parameters.
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
#' @author Laurent Gatto, Johannes Rainer
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
#'
#' ## Linking samples to data elements
#'
#' ## Create a small experiment
#' library(S4Vectors)
#' mse <- MsExperiment()
#' sd <- DataFrame(sample_id = c("QC1", "QC2"),
#'                 sample_name = c("QC Pool", "QC Pool"),
#'                 injection_idx = c(1, 3))
#' sampleData(mse) <- sd
#'
#' ## define file names containing spectra data for the samples and
#' ## add them, along with other arbitrary files to the experiment
#' fls <- dir(system.file("sciex", package = "msdata"), full.names = TRUE)
#' experimentFiles(mse) <- MsExperimentFiles(
#'     mzML_files = fls,
#'     annotations = "internal_standards.txt")
#'
#' ## Link samples to data files: first sample to first file in "mzML_files",
#' ## second sample to second file in "mzML_files"
#' mse <- linkSampleData(mse, with = "experimentFiles.mzML_files",
#'     sampleIndex = c(1, 2), withIndex = c(1, 2))
#'
#' ## Link all samples to the one file in "annotations"
#' mse <- linkSampleData(mse, with = "experimentFiles.annotations",
#'     sampleIndex = c(1, 2), withIndex = c(1, 1))
#' mse
#'
#' ## Import the spectra data and add it to the experiment
#' library(Spectra)
#' spectra(mse) <- Spectra(fls, backend = MsBackendMzR())
#'
#' ## Link each spectrum to the respective sample. We use the alternative
#' ## link definition that does not require sampleIndex and withIndex but
#' ## links elements based on matching values in the specified data elements.
#' ## We need to add the full file name as an additional column to sampleData
#' ## in order to allow matching this file names with the value in
#' ## spectra(mse)$dataOrigin which contains the original file names from which
#' ## the spectra were imported.
#' sampleData(mse)$raw_file <- normalizePath(fls)
#'
#' ## The links can be added using the short notation below
#' mse <- linkSampleData(mse, with = "sampleData.raw_file = spectra.dataOrigin")
#' mse
#'
#' ## With sampleData links present, any subsetting of the experiment by sample
#' ## will ensure that all linked elements are subsetted accordingly
#' b <- mse[, 2]
#' b
#' sampleData(b)
#' experimentFiles(b)$mzML_files
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
#' @slot assay An instance of class `QFeatures`,
#'     `SummarizedExperiment` or `NULL`.
#'
#' @slot otherData A `List` to store any additional data objects.
#'
#' @slot sampleData A `DataFrame` documenting the experimental design.
#'
#' @slot sampleDataLinks A `List` with link definitions between samples and
#'     data elements. Should not be directly accessed or modified by the user.
#'
#' @slot metadata A `list` to store additional metadata.
#'
#' @rdname MsExperiment
#'
#' @importClassesFrom S4Vectors SimpleList
#'
#' @importFrom S4Vectors DataFrame
setClass("MsExperiment",
         slots = c(
             experimentFiles = "MsExperimentFilesOrNull",
             spectra = "SpectraOrNull",
             assay = "QFeaturesOrSummarizedExperimentOrNull",
             ## chromatograms = "Chromatograms",
             otherData = "List",
             sampleData = "DataFrame",
             sampleDataLinks = "List",
             metadata = "list"),
         prototype = prototype(
             experimentFiles = NULL,
             spectra = NULL,
             qfeatures = NULL,
             otherData = List(),
             sampleData = DataFrame(),
             sampleDataLinks = new(
                 "SimpleList", elementMetadata =
                                   DataFrame(subsetBy = integer())),
             metadata = list())
         )

#' @rdname MsExperiment
#'
#' @export
MsExperiment <- function()
    new("MsExperiment")

#' @rdname MsExperiment
#'
#' @param object An instance of class `MsExperiment`.
#'
#' @importFrom Spectra msLevel
#'
#' @exportMethod show
setMethod("show", "MsExperiment", function(object) {
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
    lnks <- object@sampleDataLinks
    if (length(lnks)) {
        cat(" Sample data links:\n")
        for (i in seq_along(lnks)) {
            if (mcols(lnks)$subsetBy[i] == 2)
                cols <- " column(s).\n"
            else cols <- " element(s).\n"
            cat("  - ", names(lnks)[i], ": ", length(unique(lnks[[i]][, 1L])),
                " sample(s) to ", length(unique(lnks[[i]][, 2L])), cols,
                sep = "")
        }
    }
})

## ------------------------------##
##     Getters and setters       ##
## ------------------------------##

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

#' @rdname MsExperiment
setGeneric("linkSampleData", function(object, ...)
    standardGeneric("linkSampleData"))
#' @rdname MsExperiment
#'
#' @export
setMethod(
    "linkSampleData", "MsExperiment",
    function(object, with = character(),
             sampleIndex = seq_len(nrow(sampleData(object))),
             withIndex = integer(), subsetBy = 1L) {
        if (!length(with))
            return(object)
        subsetBy <- as.integer(subsetBy[1L])
        if (is.na(subsetBy))
            stop("'subsetBy' needs to be an integer of length 1")
        if (!length(withIndex)) {
            link_string <- .parse_join_string(with)
            if (link_string[1L] == "sampleData") {
                from <- paste0(link_string[1:2], collapse = ".")
                to_slot <- link_string[3L]
                with <- paste0(link_string[3:4], collapse = ".")
            } else if (link_string[3L] == "sampleData") {
                from <- paste0(link_string[3:4], collapse = ".")
                to_slot <- link_string[1L]
                with <- paste0(link_string[1:2], collapse = ".")
            } else stop("one of the slot names has to be 'sampleData'.")
            link <- .link_matrix(.get_element(object, from),
                                 .get_element(object, with))
            if (nrow(link) == 0)
                warning("no matches found for '", with, "'")
        } else {
            sampleIndex <- as.integer(sampleIndex)
            withIndex <- as.integer(withIndex)
            sampleIndex <- sampleIndex[!is.na(sampleIndex)]
            withIndex <- withIndex[!is.na(withIndex)]
            if (length(sampleIndex) != length(withIndex))
                stop("Length of 'sampleIndex' and 'withIndex' have to match")
            link <- cbind(sampleIndex, withIndex)
        }
        withl <- unlist(strsplit(with, split = ".", fixed = TRUE))
        if (withl[1L] %in% c("spectra", "qfeatures")) {
            with <- withl[1L]
            if (with == "qfeatures")
                subsetBy <- 2L
        } else if (length(withl) < 2)
            stop("'with' should be a 'character' with the name of the slot and",
                 " the name of element separated by a '.'. ",
                 "See ?linkSampleData for examples")
        .add_sample_data_link(object, link, with = with, subsetBy = subsetBy)
    })

#' @rdname MsExperiment
#'
#' @export
setMethod("[", "MsExperiment", function(x, i, j, ..., drop = FALSE) {
    if (!missing(i))
        stop("Only subsetting with '[, j]' is supported.")
    lj <- length(j)
    if (is.character(j)) {
        j <- match(j, rownames(sampleData(x)))
        if (any(is.na(j)))
            warning(sum(is.na(j)), " of ", lj, " values could not be ",
                    "matched to rownames of 'sampleData(x)'")
        j <- j[!is.na(j)]
    }
    if (is.logical(j)) {
        if (lj != nrow(sampleData(x)))
            stop("if 'j' is logical its length has to match the number of ",
                 "samples in 'x'.")
        j <- which(j)
    }
    .extractSamples(x, j, newx = x)
})
