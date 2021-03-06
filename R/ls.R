#' @name lsModels
#' @aliases lsModels lsData
#' @title List Names Of \code{amModel} Or \code{amData} Objects In An \code{amModelLib} Object.
#' @description List names of all objects of class \code{\link{amModel}} (a fitted model with  mandatory metadata) or \code{\link{amData}} (a dataset with mandatory metadata) in an \code{\link{amModelLib}} object.
#' @param x An object of class \code{amModelLib}.
#' @return A vector of names.
#' @family amModelLib
#' @keywords manip
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
#' # create a second amData object with log data
#' log.plant.data <- data.frame(group, log.weight=log(weight))
#' log.plant.data <- amData(
#'     data = log.plant.data, 
#'     comment = 'Dataset that includes the log of plant weight', 
#'     source = 'lm helpfile (R).'
#' )
#' 
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
#' 
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
#' 
#' # list names of models
#' lsModels(mymodels)
#' 
#' # list names of data
#' lsData(mymodels)
#'
NULL


#' @rdname lsModels
#' @export
lsModels <- function(x) {
    if (methods::is(x, 'amModelLib')) {
        names(x@models)
    } else {
        stop('This function only works for "amModelLib" objects.')
    }
}

#' @rdname lsModels
#' @export
lsData <- function(x) {
    if (methods::is(x, 'amModelLib')) {
        names(x@data)
    } else {
        stop('This function only works for "amModelLib" objects.')
    }
}






