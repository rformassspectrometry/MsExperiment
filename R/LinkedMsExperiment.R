#' @include MsExperiment.R

#' @title Mass spectrometry experiments with explicit links between data entities
#'
#' @name LinkedMsExperiment
#' 
#' @description
#'
#' The `linkSampleData` function allows to add or define *explicit* links
#' between samples (i.e. rows in the `sampleData` `DataFrame`) and other data
#' elements in the `MsExperiment` object. Links can be added between samples
#' and spectra (in the `@spectra` slot) but also between individual samples and
#' elements in any other data slot of the `MsExperiment`, such as
#' `experimentFiles` or `otherData`. Links allow also a n:m mapping between
#' samples and other data elements.
#'
#' Importantly, the presence of such links enables a (coherent) subsetting of an
#' `MsExperiment` by samples. Thus, once the link is defined, any subsetting by
#' sample will also correctly subset the linked data. All other, not linked,
#' data elements are always retained as in the original `MsExperiment`. 
#'
#' @section Implementation notes:
#' 
#' Links from samples to any other element are stored as an `integer` `matrix`
#' as a list element of the `@sampleDataLinks` slot. The link name (i.e.
#' the name of the list element) needs to be the name of the slot that is
#' linked (for slots `@spectra` and `@qfeatures`) or an element within that
#' slot (for `@experimentFiles`, `@otherData`, `@metadata`). For the latter the
#' name needs to be in the format `<slot name>.<element name>`, e.g.
#' `"experimentFiles.mzML_files"` to link samples to an element called
#' `"mzML_files"` in `@experimentFiles`.
#'
#' @author Johannes Rainer
#'
#' @exportMethod linkSampleData
#'
#' @importFrom S4Vectors List
NULL

setClass("LinkedMsExperiment",
         contains = "MsExperiment",
         slots = c(sampleDataLinks = "List"),
         prototype = prototype(sampleDataLinks = List())
         )

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
#' eventually existing links between the same entities will be **overwritten**.
#' 
#' @param x `LinkedMsExperiment`.
#'
#' @param link two-column `integer` `matrix` with the link.
#'
#' @param with `character(1)` with the definition of the element to which the
#'     link was established (e.g. `"spectra.dataOrigin"` or
#'     `experimentFiles.raw_files`. See also `.get_element` for details.
#'
#' @author Johannes Rainer
#'
#' @noRd
.add_sample_data_link <- function(x, link = matrix(), with) {
    nfrom <- nrow(sampleData(x))
    if (nrow(link) == 0 || nfrom == 0)
        return(x)
    nto <- length(.get_element(x, with))
    if (nto == 0)
        stop("'", with, "' is empty. Can not link to empty data")
    .valid_link(link, nfrom, nto)
    if (any(names(.sample_data_links(x)) == with))
        warning("Overwriting previously present link '", with, "'")
    x@sampleDataLinks[[with]] <- link
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
#' @importFrom methods slotNames slot
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
#' @noRd
#'
#' @examples
#'
#' .link_matrix(1:3, 2:20)
#' .link_matrix(c("a", "a", "b", "d", "b", "c"), c("g", "a", "b", "e"))
.link_matrix <- function(from = integer(), to = integer()) {
    res <- lapply(from, function(x) which(to == x))
    ls <- lengths(res)
    cbind(rep(seq_along(from), ls), unlist(res[ls > 0], use.names = FALSE))
}

#' @rdname LinkedMsExperiment
setGeneric("linkSampleData", function(object, ...)
    standardGeneric("linkSampleData"))
#' @rdname LinkedMsExperiment
setMethod("linkSampleData", "MsExperiment",
          function(object, with = character(),
                   fromIndex = seq_len(nrow(sampleData(object))),
                   toIndex = integer()) {
              object <- as(object, "LinkedMsExperiment")
              if (!length(with))
                  return(object)
              if (!length(toIndex)) {
                  link_string <- .parse_join_string(with)
                  if (link_string[1L] == "sampleData") {
                      from <- paste0(link_string[1:2], collapse = ".")
                      to_slot <- link_string[3L]
                      to <- paste0(link_string[3:4], collapse = ".")
                  } else if (link_string[3L] == "sampleData") {
                      from <- paste0(link_string[3:4], collapse = ".")
                      to_slot <- link_string[1L]
                      to <- paste0(link_string[1:2], collapse = ".")
                  } else stop("one of the slot names has to be 'sampleData'.")
                  link <- .link_matrix(.get_element(object, from),
                                       .get_element(object, to))
                  if (nrow(link) == 0)
                      warning("no matches found for '", with, "'")
                  if (to_slot %in% c("spectra", "qfeatures"))
                      to <- to_slot
                  object <- .add_sample_data_link(object, link, with = to)
              } else {
                  fromIndex <- as.integer(fromIndex, na.rm = TRUE)
                  toIndex <- as.integer(toIndex, na.rm = TRUE)
                  if (length(fromIndex) != length(toIndex))
                      stop("Length of 'fromIndex' and 'toIndex' have to match")
                  withl <- unlist(strsplit(with, split = ".", fixed = TRUE))
                  if (withl[1L] %in% c("spectra", "qfeatures"))
                      with <- withl[1L]
                  else if (length(withl) < 2)
                      stop("'with' should be a 'character' with the name of ",
                           "the slot and the name of element separated by a ",
                           "'.'. See ?linkSampleData for examples")
                  object <- .add_sample_data_link(
                      object, cbind(fromIndex, toIndex), with = with)
              }
              object
})

setMethod("show", "LinkedMsExperiment", function(object) {
    callNextMethod()
    lnks <- object@sampleDataLinks
    if (length(lnks)) {
        cat(" Sample data links:\n")
        for (i in seq_along(lnks))
            cat("  - ", names(lnks)[i], ": ", length(unique(lnks[[i]][, 1L])),
                " sample(s) to ", length(unique(lnks[[i]][, 2L])),
                " element(s).\n", sep = "")
    }
})

## establish explicit links between files and samples:
## original raw files and samples
## mse <- linkSampleData(mse, with = "experimentFiles.original_files",
##                      toIndex = 1:3)
## optionally link also the backend file
## mse <- linkSampleData(mse, with = "experimentFiles.sql_backend", toIndex = 1)

## Link the individual spectra to samples.
## mse <- linkSampleData(mse, with = "spectra",
##                      from = sampleData(mse)$original_files,
##                      to = basename(spectra(mse)$dataOrigin))

## subsetting will subset:
## - sampleData
## - any other slot for which a link exists
##
## Example use cases:
## Re-order experiment by time
## mse <- mse[, order(mse$time)]

## extract samples/files from one group (or exclude QC samples etc)
## group_1 <- mse[, mse$group == 1]

## What would also be possible:
## establish link as a join statement (similar to SQL join).
## mse <- linkSampleData(mse, with = "spectra.dataOrigin = sampleData.file_name")

## n:m mappings
## mse <- linkSampleData(mse, with = "experimentfiles.annotations",
##                      fromIndex = c(1, 2, 3), toIndex = c(1, 2, 1))

