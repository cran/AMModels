#' @name rmModel
#' @aliases rmModel rmData
#' @title Remove An \code{amModel} or \code{amData} Object From An \code{amModelLib} Object.
#' @description Remove an object of class \code{amModel} or \code{amData} (a fitted model object or data to fit a model or use as covariate data, with mandatory metadata) from an \code{amModelLib} object.
#' @param x A character vector, numeric vector, or logical vector identifying model(s) or data to remove.
#' @param amml An \code{amModelLib} object.
#' @return An object of class \code{amModelLib}.
#' @family amModelLib
#' @keywords utilities
#' @export rmModel rmData
#' @examples
#' 
#' # create dataset from lm helpfile
#' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' lm.D90 <- lm(weight ~ group - 1) # omitting intercept
#' 
#' # create an amData object that includes metadata
#' plant.data <- data.frame(group = group, weight = weight)
#' plant.data <- amData(
#'     data = plant.data, 
#'     comment = 'Dataset from lm helpfile.'
#' )
#'
#' log.plant.data <- data.frame(group, log.weight=log(weight))
#' log.plant.data <- amData(
#'     data = log.plant.data, 
#'     comment = 'data to fit log model', 
#'     source = 'lm helpfile (R).'
#' )
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
#' # create an amModelLib that contains the two amModel objects and two amData objects
#' # the models and data must be supplied as named lists
#' mymodels <- amModelLib(
#'     models = list(
#'         full.model = full.model, 
#'         no.int.model = no.int.model
#'     ), 
#'     data=list(
#'         plant.data = plant.data, 
#'         log.plant.data = log.plant.data
#'     )
#' )
#' 
#' # show the library
#' mymodels
#' 
#' 
#' # remove just the second model
#' rmModel('no.int.model', amml = mymodels)
#' 
#' # remove the first plant data, has a soft-link from a model, throws warning.
#' rmData('plant.data', amml = mymodels)
#'
#' # show the library
#' mymodels

rmModel <- function(x, amml) {
    if (any(all(is.numeric(x)), all(is.logical(x)))) {
        amml@models <- amml@models[-x]
    } else if (all(is.character(x))) {
        amml@models <- amml@models[-which(names(amml@models) %in% x)]
    } else stop("Invalid class for 'x'.")
    amml
}


#' @rdname rmModel
rmData <- function(x, amml) {
    # identify orphan models to report
    datnames <- unlist(lapply(amml@models, function(y) grep('data', names(y@metadata), ignore.case = TRUE)))
    datnames2 <- unlist(lapply(names(datnames), function(y) amml@models[[y]]@metadata[[datnames[y]]]))
    orphanmods <- NULL
    names(datnames2) <- names(datnames)
    if (any(all(is.numeric(x)), all(is.logical(x)))) {
        rmdat <- names(amml@data)[x]
        orphanmods <- names(datnames2)[datnames2 %in% rmdat]
        amml@data <- amml@data[-x]
    } else if (all(is.character(x))) {
        dn <- names(amml@data)
        names(dn) <- dn
        rmdat <- dn[x]
        orphanmods <- names(datnames2)[datnames2 %in% rmdat]
        amml@data <- amml@data[-which(names(amml@data) %in% x)]
    } else stop("Invalid class for 'x'.")
    if (length(orphanmods)) warning('The following models have dangling data references: ', paste0(orphanmods, collapse = ', '))
    amml
}









