#' An S4 class to store models with descriptive metadata.
#' @rdname amModel
#' @slot model A model fit object, e.g. from \code{lm} or any single object containing the means to predict values from data.
#' @slot metadata A named list of length 1 character vectors that form name:value pairs, e.g. the analyst, the project, the data used, etc. The metadata name \code{data} can be used to link the model with the name of a data object, and when manipulated in the \code{\link{amModelLib}} an effort will be made to keep the model and data together. When embedded in an \code{\link{amModelLib}}, model metadata may be retrieved or set with \code{\link{modelMeta}}.
#' @keywords classes
setClass('amModel', slots = c(
        model = 'ANY',
        metadata = 'list'
    ),

    validity = function(object) {
        meta <- object@metadata 
        err <- NULL
        if (!length(meta)) { 
            err <- c(err, 'Must provide metadata about models. We recommend source, species, and comments.')
        }
        if (is.null(names(meta)) || any(names(meta) == "")) {
            err <- c(err, 'All metadata elements must be named.')
        }
        if (length(unique(names(meta))) != length(names(meta))) {
            err <- c(err, 'Metadata names must be unique.')
        }
        if (is.null(err)) err <- TRUE
        err
    }
)

#' An S4 class to store data with descriptive metadata.
#' @rdname amData
#' @slot data A single object containing data relevant to a model. There are no restrictions on how the data are used in the model; for example, they may be covariate data (for use on the right side of the equation) or observed data (for use on the left side of the equation).
#' @slot metadata A named list of length 1 character vectors that form name:value pairs, e.g the source, the collection method, etc. When embedded in an \code{\link{amModelLib}}, data metadata may be retrieved or set with \code{\link{dataMeta}}.
#' @keywords classes
setClass('amData', slots = c(
        data = 'ANY',
        metadata = 'list'
    ),
    validity = function(object) {
        dat <- object@data
        meta <- object@metadata
        err <- NULL
        if (!length(meta)) { 
            err <- c(err, 'Must provide metadata about dataset. We recommend source, covariate data name (data), and comments.')
        }  
        if (is.null(names(meta)) || any(names(meta) == "")) {
            err <- c(err, 'All metadata elements must be named.')
        }
        if (length(unique(names(meta))) != length(names(meta))) {
            err <- c(err, 'Metadata names must be unique.')
        }
        if (is.null(err)) err <- TRUE
        err
    }
)

#' An S4 class to hold model and data, each with descriptive metadata.
#' @rdname amModelLib
#' @slot models A named list of \code{\link{amModel}} objects. Models are added with \code{\link{insertAMModelLib}} and removed with \code{\link{rmModel}}.
#' @slot data A named list of \code{\link{amData}} objects. Data are added with \code{\link{insertAMModelLib}} and removed with \code{\link{rmData}}.
#' @slot info A named list of length 1 character strings that forms name:value metadata pairs describing the \code{amModelLib}, e.g. the project, the owner, etc. Name:value pairs may be retrieved or set using \code{\link{ammlInfo}}.
#' @slot description Length 1 character vector describing the \code{amModelLib}, intended to offer a summary of the \code{amModelLib} and quickly refresh the user to its contents and purpose without poring over the detailed metadata. The description may be retrieved or set with \code{\link{ammlDesc}}.
#' @keywords classes
setClass('amModelLib', 
    slots = c(
        models = 'list',
        data = 'list',
        info = 'list',
        description = 'character'
    ),
    validity = function(object) {
        models <- object@models
        dat <- object@data
        mod.err <- dat.err <- NULL
        if (length(models)) {
            out.mod <- all(sapply(models, is, class2 = 'amModel'))
            if (!out.mod) mod.err <- '"models" must be a named list of objects of class "amModel".'
        } else {
            out.mod <- TRUE
        }
        if (length(dat)) {
            out.dat <- all(sapply(dat, is, class2 = 'amData'))
            if (!out.dat) dat.err <- '"data" must be a named list of objects of class "amData".'
        } else {
            out.dat <- TRUE
        }
#        if(length(info)) {
#            out.info1 <- !(is.null(names(meta)) || any(names(meta) == ""))
#            if(!out.info1) info.err <- c(info.err, 'All info elements must be named.')

#            out.info2 <- length(unique(names(meta))) == length(names(meta)) 
#            if(!out.info2) info.err <- c(info.err, 'Info names must be unique.')
#            out.info <- c(out.info1, out.info2)
#        } else {
#            out.info <- TRUE
#        }
        if (all(out.mod, out.dat)) TRUE
        else c(mod.err, dat.err)
    }
)






