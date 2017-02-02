
#' @name getters
#' @aliases getAMModel getAMData ammlDesc ammlInfo 
#' @rdname getters
#' @title Extract A Single Model or Data Object, Get Or Set Info, Description, Or Metadata.
#' @description  The function \code{getAMData} will extract an \code{\link{amData}} object from an \code{\link{amModelLib}}; the function \code{getAMModel} will extract an \code{\link{amModel}} object from an \code{amModelLib}. The function \code{ammlDesc} can be used to retrieve or set a description of an \code{amModelLib} object. The function \code{ammlInfo} can be used to retrieve or set information about an \code{amModelLib} object. \code{modelMeta} and \code{dataMeta} retrieve and set metadata within \code{amModelLib} objects.
#' @details The objects created by \code{getAMData} and \code{getAMModel} are returned as their original class unless the argument \code{as.list} is set to \code{TRUE}. If \code{as.list}, a list is returned with the original object in the first element and metadata in the second.\cr The setters for \code{ammlInfo}, \code{modelMeta}, and \code{dataMeta} replace individual elements in their respective lists with each call. To remove elements set their value to \code{NULL} in the named replacement list. 
#' @param amml An \code{amModelLib} object. 
#' @param x A name or length 1 integer index to extract from or set within the \code{amModelLib} object.
#' @param as.list Logical; \code{FALSE} to return just the object, \code{TRUE} to return a list with both object and metadata.
#' @param value A named list of metadata to set within \code{x}.
#' @param \dots Additional arguments (not used).
#' @return The model or data object in its original form, or if \code{as.list} a list with \item{models}{The original model} (or \item{data}{The original data} ) and \item{metadata}{metadata}.
#' @family amModelLib
#' @keywords utilities
#' @export getAMModel
#' @examples
#' 
#' # create dataset from lm helpfile
#' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' # notice the models lm.D9 and lm.D90 are of class 'lm'
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' lm.D90 <- lm(weight ~ group - 1) # omitting intercept
#' 
#' 
#' # create two amModel objects with metadata and a soft link to the data
#' full.model <- amModel(
#'     model = lm.D9, 
#'     comment = 'full model', 
#'     source = 'lm helpfile (R).', 
#'     taxa = 'plants', 
#'     data = 'plant.data'
#' )
#' 
#' no.int.model <- amModel(
#'     model = lm.D90, 
#'     comment = 'model without intercept', 
#'     source = 'lm helpfile (R).', 
#'     taxa = 'plants', 
#'     data = 'plant.data'
#' )
#' 
#' # create an amData object that includes metadata
#' # the plant.data is of class data.frame
#' plant.data <- data.frame(group = group, weight = weight)
#' plant.data <- amData(
#'     data = plant.data, 
#'     comment = 'Dataset from lm helpfile.'
#' )
#' 
#' # create a second amData object that includes metadata
#' log.plant.data <- data.frame(group, log.weight=log(weight))
#' log.plant.data <- amData(
#'     data = log.plant.data, 
#'     comment = 'data to fit log model', 
#'     source = 'lm helpfile (R).'
#' )
#' 
#' # create an amModelLib that contains the two amModel objects and two amData objects
#' # the models and data must be supplied as named lists
#' mymodels <- amModelLib(
#'     models = list(
#'         full.model = full.model, 
#'         no.int.model = no.int.model
#'     ), 
#'     data = list(
#'         plant.data = plant.data, 
#'         log.plant.data = log.plant.data
#'     )
#' )
#' 
#' # extract the dataset
#' getAMData(amml = mymodels, 'plant.data', as.list = FALSE)
#' 
#' # you can also extract by index
#' getAMData(amml = mymodels, 1, as.list = FALSE)
#' 
#' # extract the model
#' getAMModel(amml = mymodels, 'full.model', as.list = FALSE)
#' 
#' # you can also extract by index
#' getAMModel(amml = mymodels, 1, as.list = FALSE)
#'  
#' # extraction with '[' and '[[', which are identical here, focus on models
#' mymodels[c(1,2)]
#' mymodels[[1]]
#' 
#' # Add a description to the amModelLib
#' ammlDesc(mymodels) <- "This library demonstrates how to store models 
#' and data in a format that allows for descriptive metadata and easy 
#' retrieval for future reference."
#' 
#' # Extract the description
#' ammlDesc(mymodels)
#' 
#' # Add some metadata 'info' to the amModelLib
#' ammlInfo(mymodels) <- list(owner = 'me', organization = 'My Organization')
#' 
#' # Extract all info for an amModelLib
#' ammlInfo(mymodels) 
#' 
#' # Extract targeted info
#' ammlInfo(amml = mymodels, 'owner')
#' 
#' # Delete metadata by setting to NULL
#' ammlInfo(mymodels) <- list(organization = NULL)
#' 
#' # Extract all model metadata
#' modelMeta(mymodels)
#' 
#' # Extract metadata from specific model
#' modelMeta(amml = mymodels, 'full.model')
#' 
#' # Add metadata to 'full.model'
#' modelMeta(amml = mymodels, 'full.model') <- list(
#'     url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html"
#' )
#' 
#' # remove metadata by setting value to NULL
#' modelMeta(amml = mymodels, 'full.model') <- list(url = NULL)
#' 
#' # Extract all data metadata
#' dataMeta(mymodels)
#' 
#' # Extract metadata from specific data
#' dataMeta(amml = mymodels, 'plant.data')
#' 
#' # Add metadata to 'plant.data'
#' dataMeta(mymodels, 'plant.data') <- list(
#'     url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html"
#' )
#' 
#' # remove metadata by setting value to NULL
#' dataMeta(mymodels, 'plant.data') <- list(url = NULL)
#' 





getAMModel <- function(amml, x, as.list=FALSE, ...) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    if(missing(x)) stop('Must provide a model name or integer index.')
    if(any(is.character(x), is.numeric(x))) {
        model <- amml@models[[x]]@model
        if (!as.list) {
            model
        } else {
            list(model = model, metadata = amml@models[[x]]@metadata)
        }
    } else {
        stop('Model selector must be provided as a character or integer vector.')
    }
}


#' @rdname getters
#' @export

getAMData <- function(amml, x, as.list=FALSE, ...) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    if(missing(x)) stop('Must provide a model name or integer index.')
    if(any(is.character(x), is.numeric(x))) {
        dat <- amml@data[[x]]@data
        if (!as.list) {
            dat
        } else {
            list(data = dat, metadata = amml@data[[x]]@metadata)
        }
    } else {
        stop('Data selector must be provided as a character or integer vector.')
    }
}


#' @rdname getters
#' @export

ammlDesc <- function(amml) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    amml@description
}

#' @rdname getters
#' @export

`ammlDesc<-` <- function(amml, value) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    if (!missing(value)) {
        if (is.character(value)) {
            amml@description <- value
            amml
        } else {
            stop("'value' must be a character string.")
        }
    }
}

#' @rdname getters
#' @export

ammlInfo <- function(amml, x = NULL) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    if (is.null(x)) {
        amml@info
    } else {
        amml@info[x]
    } 
}

#' @rdname getters
#' @export

`ammlInfo<-` <- function(amml, value) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    if (!missing(value)) {
        if (is.list(value) && length(names(value))) {
            for (i in names(value)) amml@info[[i]] <- value[[i]]
            amml@info <- amml@info[!sapply(amml@info, is.null)]
            amml
        } else {
            stop("'value' must be a named list.")
        }
    }
}

#' @rdname getters
#' @export

modelMeta <- function(amml, x=NULL) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    if (is.null(x)) {
        lapply(amml@models, function(y) y@metadata)
    } else if (length(x) == 1) {
        amml@models[[x]]@metadata
    } else if (length(x) > 1) {
        ammods <- amml@models[x]
        lapply(ammods, function(y) y@metadata)
    } 
}

#' @rdname getters
#' @export

`modelMeta<-` <- function(amml, x, value) {
    if (!missing(amml) && !missing(x) && !missing(value)) {
        if (is.list(value) && length(names(value))) {
            for (i in names(value)) {
                if (is.null(value[[i]])) {
                    amml@models[[x]]@metadata <- amml@models[[x]]@metadata[-which(names(amml@models[[x]]@metadata) == i)]
                } else {
                    amml@models[[x]]@metadata[[i]] <- value[[i]]
                }
            }
            amml
        } else if (missing(x)) {
            stop("'x' must identify model as an integer index or character string.")
        } else if (missing(amml)) {
            stop("'amml' must be an amModelLib object.")
        } else if (missing(value)) {
            stop("'value' cannot be missing.")
        }
    }
}


#' @rdname getters
#' @export

dataMeta <- function(amml, x=NULL) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    if (is.null(x)) {
        lapply(amml@data, function(y) y@metadata)
    } else if (length(x) == 1) {
        amml@data[[x]]@metadata
    } else if (length(x) > 1) {
        amdat <- amml@data[x]
        lapply(amdat, function(y) y@metadata)
    } 
}

#' @rdname getters
#' @export

`dataMeta<-` <- function(amml, x, value) {
    if (!missing(amml) && !missing(x) && !missing(value)) {
        if (is.list(value) && length(names(value))) {
            for (i in names(value)) {
                if (is.null(value[[i]])) {
                    amml@data[[x]]@metadata <- amml@data[[x]]@metadata[-which(names(amml@data[[x]]@metadata) == i)]
                } else {
                    amml@data[[x]]@metadata[[i]] <- value[[i]]
                }
            }
            amml
        } else if (missing(x)) {
            stop("'x' must identify data as an integer index or character string.")
        } else if (missing(amml)) {
            stop("'amml' must be an amModelLib object.")
        } else if (missing(value)) {
            stop("'value' cannot be missing.")
        }
    }
}











