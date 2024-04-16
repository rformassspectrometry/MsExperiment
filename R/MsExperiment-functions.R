.sample_data_links <- function(x, name = character()) {
    if (length(name))
        x@sampleDataLinks[names(x@sampleDataLinks) %in% name]
    else x@sampleDataLinks
}

#' Check that a *link* matrix is in the correct format:
#'
#' - matrix
#' - integer
#' - two columns
#' - values within 1:length
#'
#' @author Johannes Rainer
#'
#' @noRd
.valid_link <- function(x, nfrom = 0L, nto = 0L) {
    if (!is.matrix(x) || !is.integer(x[1, 1]))
        stop("'x' needs to be an 'integer' 'matrix'")
    if (ncol(x) != 2)
        stop("A link matrix is expected to have 2 columns")
    if (any(x[, 1] > nfrom))
        stop("indices in the first column need to be <= ", nfrom)
    if (any(x[, 2] > nto))
        stop("indices in the second column need to be <= ", nto)
    if (any(x < 1))
        stop("indices can not be smaller than 1")
}

#' The validity of the link matrix is evaluated only when adding the link. Also,
#' possibly existing links between the same entities will be **overwritten**.
#'
#' @param x `LinkedMsExperiment`.
#'
#' @param link two-column `integer` `matrix` with the link.
#'
#' @param with `character(1)` with the definition of the element to which the
#'     link was established (e.g. `"spectra.dataOrigin"` or
#'     `experimentFiles.raw_files`. See also `.get_element` for details.
#'
#' @param subsetBy `integer(1)` defining on which dimension (for `with` with
#'     dimensions > 0) the subsetting should be done.
#'
#' @author Johannes Rainer
#'
#' @importMethodsFrom S4Vectors mcols<- mcols
#'
#' @noRd
.add_sample_data_link <- function(x, link = matrix(), with, subsetBy = 1L) {
    nfrom <- nrow(sampleData(x))
    if (nrow(link) == 0 || nfrom == 0)
        return(x)
    nto <- .nelements(.get_element(x, with), subsetBy) # support link to column
    if (nto == 0)
        stop("'", with, "' is empty. Can not link to empty data")
    .valid_link(link, nfrom, nto)
    if (any(names(.sample_data_links(x)) == with))
        warning("Overwriting previously present link '", with, "'")
    x@sampleDataLinks[[with]] <- unname(link)
    mcols(x@sampleDataLinks)[with, "subsetBy"] <- subsetBy
    x
}

#' Helper function to return an element (slot or
#'
#' The convention to name an element in a slot is `<slot name>.<field name>`. If
#' field name is missing the slot is returned.
#' Note that this fails if the object stored in slot is not a `matrix` or does
#' not have the `$` method implemented. It will work for `data.frame`,
#' `DataFrame`, `list`, `Spectra`.
#'
#' @param x any `S4Object` with slots.
#'
#' @param name `character(1)` defining the element name.
#'
#' @return the requested element which can be a slot or a column in slot.
#'
#' @author Johannes Rainer
#'
#' @importFrom methods slotNames slot slot<-
#'
#' @noRd
.get_element <- function(x, name = "sampleData") {
    name <- unlist(strsplit(name[1L], split = ".", fixed = TRUE))
    slt <- name[1L]
    if (!any(slotNames(x) == slt))
        stop("No slot named '", slt, "' available in 'x'")
    res <- slot(x, slt)
    if (length(name) > 1) {
        el <- paste0(name[-1L], collapse = ".")
        if (is.matrix(res))
            res <- res[, el]
        else
            res <- do.call("$", args = list(res, el))
    }
    res
}

#' Setting/replacing an element within a slot of an `LinkedMsExperiment`. This
#' function does **not** perform any checks except for the presence of a slot.
#'
#' @importFrom methods slot slot<-
#'
#' @author Johannes Rainer
#'
#' @noRd
#'
#' @examples
#'
#' ## Add a new spectra variable to a Spectra
#' .set_element(mse, "spectra.new.value", 1:3)
#'
#' ## Add a new metadata element
#' .set_element(mse, "metadata.new_entry", data.frame(1:3))
.set_element <- function(x, name = character(), value = NULL) {
    if (length(name) == 0)
        return(x)
    name <- unlist(strsplit(name[1L], split = ".", fixed = TRUE))
    slt <- name[1L]
    if (!any(slotNames(x) == slt))
        stop("No slot named '", slt, "' available in 'x'")
    if (length(name) == 1)
        slot(x, slt, check = FALSE) <- value
    else {
        el <- paste0(name[-1L], collapse = ".")
        if (length(dim(slot(x, slt))))
            slot(x, slt)[, el] <- value
        else
            slot(x, slt) <- do.call("$<-", list(slot(x, slt), el, value))
    }
    x
}

#' @param x `character` similar to a join statement, e.g.
#'     `"sampleData.mzML = spectra.dataOrigin"`
#'
#' @return `character` with slot from, column from, slot to, column to.
#'
#' @author Johannes Rainer
#'
#' @noRd
#'
#' @examples
#'
#' x <- "sampleData.mzml_file = spectra.dataOrigin"
#' .parse_join_string(x)
#'
#' .parse_join_string("sampleData.mzml.file= spectra.dataOrigin")
.parse_join_string <- function(x) {
    parts <- unlist(strsplit(gsub(" ", "", x, fixed = TRUE), "="))
    if (length(parts) != 2)
        stop("unsupported format for the link description. ",
             "Please see ?linkSampleData")
    a <- unlist(strsplit(parts[1L], ".", fixed = TRUE))
    b <- unlist(strsplit(parts[2L], ".", fixed = TRUE))
    if (length(a) < 2 | length(b) < 2)
        stop("unsupported format for the link desciption. ",
             "Please see ?linkSampleData")
    c(a[1L], paste(a[-1L], collapse = "."),
      b[1L], paste0(b[-1L], collapse = "."))
}

#' Create a link matrix for specified vectors.
#'
#' @return `integer` `matrix` with the indices of the matches.
#'
#' @author Johannes Rainer
#'
#' @importFrom S4Vectors findMatches from to
#'
#' @noRd
#'
#' @examples
#'
#' .link_matrix(1:3, 2:20)
#' .link_matrix(c("a", "a", "b", "d", "b", "c"), c("g", "a", "b", "e"))
.link_matrix <- function(from = integer(), to = integer()) {
    res <- findMatches(from, to)
    cbind(from(res), to(res))
}

#' @description
#'
#' Subset a `LinkedMsExperiment` by sample also subsetting and updating all
#' linked data:
#'
#' Subsetting with `[i, j]`:
#' - support re-ordering (`j = c(4, 2, 3)`).
#' - support duplication (`j = c(1, 1, 2)`). -> make that a special case?
#' - keep only elements matching a sample after subsetting - and any unlinked
#'   element.
#'
#' @note
#'
#' how could we improve the performance of this subsetting? Copying over the
#' whole object sounds like not an ideal thing to do.
#'
#' Maybe have helper functions `splitBySample` to avoid repeatedly copying the
#' original data. Parameter `newx` might help here - but not sure if that's
#' indeed the case.
#'
#' @param x `LinkedMsExperiment`.
#'
#' @param i `integer`
#'
#' @param newx `LinkedMsExperiment`. Result objects. Might help avoiding
#'     repeatedly copying the object if `.extractSamples` is called within
#'     a loop. Also, providing `newx = new("LinkedMsExperiment")` would perform
#'     a *lightweight* extraction, dropping anything which is not linked.
#'
#' @author Johannes Rainer
#'
#' @importFrom methods slot<- callNextMethod
#'
#' @noRd
.extractSamples <- function(x, i, newx = x) {
    if (!nrow(sampleData(x)))
        return(x)
    slot(newx, "sampleData", check = FALSE) <- x@sampleData[i, , drop = FALSE]
    for (link in names(slot(x, "sampleDataLinks"))) {
        lmat <- slot(x, "sampleDataLinks")[[link]]
        subsetBy <- mcols(slot(x, "sampleDataLinks"))[link, "subsetBy"]
        idxs <- split(lmat[, 2], as.factor(lmat[, 1]))
        idxs <- idxs[as.character(i)]
        ls <- lengths(idxs)
        idxs <- unlist(idxs, use.names = FALSE)
        element <- .get_element(x, link)
        newx <- .set_element(newx, link, .subset_dim(element, idxs, subsetBy))
        newx@sampleDataLinks[[link]] <- cbind(rep(seq_along(ls), ls),
                                              seq_len(sum(ls)))
        ## Note: keeping also empty lmat - to keep info that there was a link
    }
    newx
}

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

#' Simple helper function that returns the number of elements (for the selected
#' dimension).
#'
#' @noRd
.nelements <- function(x, dim = 1L) {
    dims <- dim(x)
    if (length(dims))
        dims[dim]
    else length(x)
}

#' Helper to subset `x` to elements `i` on dimension `subsetBy`. If `x` has
#' no dimensions `x[i]` is returned, if it has dimensions and `subsetBy == 1L`
#' `x[i, ]` is returned and if `subsetBy == 2L` `x[, i]` is returned.
#'
#' @author Johannes Rainer
#'
#' @noRd
.subset_dim <- function(x, i, subsetBy = 1L) {
    if (length(dim(x))) {
        if (subsetBy == 1L)
            x <- x[i, , drop = FALSE]
        if (subsetBy == 2L)
            x <- x[, i, drop = FALSE]
    } else x <- x[i]
    x
}

#' @export
#'
#' @rdname MsExperiment
qdata <- function(object) {
    stopifnot(inherits(object, "MsExperiment"))
    object@qdata
}

#' @export
#'
#' @rdname MsExperiment
"qdata<-" <- function(object, value) {
    stopifnot(inherits(object, "MsExperiment"))
    object@qdata <- value
    object
}

.ms_experiment_is_empty <- function(object) {
    if (length(object@experimentFiles)) return(FALSE)
    if (length(object@spectra)) return(FALSE)
    if (length(object@qdata)) return(FALSE)
    if (length(object@otherData)) return(FALSE)
    if (length(object@sampleData)) return(FALSE)
    TRUE
}

#' @title Import MS spectra data of an experiment
#'
#' @description
#'
#' Read/import MS spectra data of an experiment from the respective (raw)
#' data files into an [MsExperiment()] object. Files provided with the
#' `spectraFiles` parameter are imported as a `Spectra` object and each
#' file is automatically *linked* to rows (samples) of a `sampleData`
#' data frame (if provided).
#'
#' @param spectraFiles `character` with the (absolute) file names of the MS
#'     data files that should be imported as a [Spectra()] object.
#'
#' @param sampleData `data.frame` or `DataFrame` with the sample annotations.
#'     Each row is expected to contain annotations for one file (sample). The
#'     order of the data frame's rows is expected to match the order of the
#'     provided files (with parameter `spectraFiles`).
#'
#' @param ... additional parameters for the [Spectra()] call to import the
#'     data.
#'
#' @return `MsExperiment`.
#'
#' @author Johannes Rainer
#'
#' @importMethodsFrom Spectra Spectra
#'
#' @export
#'
#' @examples
#'
#' ## Define the files of the experiment to import
#' fls <- c(system.file("microtofq/MM14.mzML", package = "msdata"),
#'          system.file("microtofq/MM8.mzML", package = "msdata"))
#'
#' ## Define a data frame with some sample annotations
#' ann <- data.frame(
#'     injection_index = 1:2,
#'     sample_id = c("MM14", "MM8"))
#'
#' ## Import the data
#' library(MsExperiment)
#' mse <- readMsExperiment(spectraFiles = fls, ann)
#' mse
#'
#' ## Access the spectra data
#' spectra(mse)
#'
#' ## Access the sample annotations
#' sampleData(mse)
#'
#' ## Import the data reading all MS spectra directly into memory
#' mse <- readMsExperiment(spectraFiles = fls, ann,
#'     backend = Spectra::MsBackendMemory())
#' mse
readMsExperiment <- function(spectraFiles = character(),
                             sampleData = data.frame(), ...) {
    spectraFiles <- normalizePath(spectraFiles)
    if (!nrow(sampleData))
        sampleData <- data.frame(sample_index = seq_along(spectraFiles))
    if (nrow(sampleData) != length(spectraFiles))
        stop("Number of rows in 'sampleData' have to match the number of ",
             "files in parameter 'spectraFiles'")
    sampleData$spectraOrigin <- spectraFiles
    if (!inherits(sampleData, "DataFrame"))
        sampleData <- DataFrame(sampleData)
    x <- MsExperiment()
    sampleData(x) <- sampleData
    spectra(x) <- Spectra(spectraFiles, ...)
    linkSampleData(x, with = "sampleData.spectraOrigin = spectra.dataOrigin")
}

#' @title Consolidate links between samples and spectra after filtering
#'
#' @description
#'
#' If @spectra got filtered possibly present *links* between them and samples
#' will no longer be valid and need to be updated/fixed. This function
#' consolidates these links using a spectra variable `"._SPECTRA_IDX"` in
#' `@spectra` that needs to represent/contain the index of the spectra
#' **before** filtering.
#'
#' @param x `MsExperiment`
#'
#' @author Johannes Rainer
#' @noRd
.update_sample_data_links_spectra <- function(x) {
    sdl <- .sample_data_links(x, "spectra")[[1L]]
    idx <- match(sdl[, 2L], x@spectra$._SPECTRA_IDX)
    keep <- !is.na(idx)
    sdl <- sdl[keep, , drop = FALSE]
    sdl[, 2L] <- idx[keep]
    x@sampleDataLinks[["spectra"]] <- sdl
    x
}

#' @export
#'
#' @rdname MsExperiment
spectraSampleIndex <- function(x, duplicates = c("first", "keep")) {
    duplicates <- match.arg(duplicates)
    if (duplicates == "first") {
        .spectra_sample_index_first(x@sampleDataLinks[["spectra"]],
                                    length(x@spectra))
    } else {
        .spectra_sample_index_all(x@sampleDataLinks[["spectra"]],
                                  length(x@spectra))
    }
}

#' Return an `integer` vector with the sample index for each spectrum. If
#' a spectrum is assigned to more than one sample, the index of the first
#' is returned and a warning shown. For spectra without a sample assignment
#' `NA_integer_` is returned.
#'
#' @param x 2 column `matrix`, first column being sample index, second
#'     spectra index.
#'
#' @param nspectra length of the object's `Spectra` object
#'
#' @return `integer` of length equal to the number of spectra.
#'
#' @noRd
.spectra_sample_index_first <- function(x, nspectra) {
    if (length(x)) {
        if (anyDuplicated(x[, 2L])) {
            warning("Found at least one spectrum that is assigned to more ",
                    "than one sample. Will return the first sample for these. ",
                    "Consider using 'duplicates = \"all\"' to retrieve all ",
                    "mappings.")
            res <- x[match(seq_len(nspectra), x[, 2L]), 1L]
        } else {
            res <- rep(NA_integer_, nspectra)
            res[x[, 2L]] <- x[, 1L]
        }
    } else res <- rep(NA_integer_, nspectra)
    res
}

#' Return an `list` of integer vectors with the sample indices for each
#' spectrum. For spectra without a sample assignment `integer()` is returned.
#'
#' @param x 2 column `matrix`, first column being sample index, second
#'     spectra index.
#'
#' @param nspectra length of the object's `Spectra` object
#'
#' @return `list` of length equal to the number of spectra.
#'
#' @noRd
.spectra_sample_index_all <- function(x, nspectra) {
    if (length(x)) {
        res <- split(x[, 1L], f = factor(x[, 2L], levels = seq_len(nspectra)))
    } else res <- replicate(nspectra, integer())
    unname(res)
}
