#' @name methods-amModel
#' @rdname methods-amModel
#' @docType methods
#' @include classes.R
#' @aliases methods-amModel
#' @title Methods For Displaying, Summarizing, And Manipulating \code{amModel} And \code{amData} Objects 
#' @param object,x An \code{\link{amModel}} or \code{\link{amData}} object.
#' @param i,j indices specifying elements to extract or replace. Indices are numeric or character vectors or empty (missing) or NULL. 
#' @param drop Not used.
#' @param value Replacement value. 
#' @param \dots Additional arguments passed to other functions or methods.
#' @description Getters and setters for models and data.
#' @details Summary assumes some meaningful summary method exists for each object in its home package.
#' @keywords methods
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
#' 
#' # create am amModel object
#' full.model <- amModel(
#'     model = lm.D9, 
#'     comment = 'full model', 
#'     source = 'lm helpfile (R).', 
#'     taxa = 'plants', 
#'     data = 'plant.data'
#' )
#' 
#' 
#' # create an amData object
#' plant.data <- data.frame(group = group, weight = weight)
#' plant.data <- amData(
#'     data = plant.data, 
#'     source = 'lm helpfile (R).',
#'     comment = 'Dataset from lm helpfile.'
#' )
#'
#' summary(full.model)
#' 
#' # [ and [[ index from metadata
#' full.model[c(2,1)]
#' full.model[[1]]
#' full.model[['taxa']]
#'
#' plant.data[c(2,1)]
#' plant.data[[1]]
#' plant.data[['comment']]
#'
NULL


#' @aliases summary,amModel-method
#' @rdname methods-amModel
#' @export
setMethod('summary', signature(object = 'amModel'), function(object, ...) {
    print(summary(object@model, ...))
    cat('\n--- Metadata ---\n')
    print(metaSummary(object@metadata), right = FALSE)
})

#' @aliases [,amModel,ANY,ANY,ANY-method [,amModel-method
#' @rdname methods-amModel
setMethod('[', signature(x='amModel', i = 'ANY', j = 'ANY'), function(x, i, j, ..., drop = TRUE) {
    get <- unlist(c(as.list(i, j), list(...)))
    x@metadata[i]
})

#' @aliases [[,amModel,ANY,ANY-method [[,amModel-method
#' @rdname methods-amModel
setMethod('[[', signature(x = 'amModel'), function(x, i) {
    x@metadata[[i]]
})

#' @aliases [<-,amModel,ANY,ANY,ANY-method [<-,amModel-method
#' @rdname methods-amModel
setMethod('[<-', signature(x = 'amModel'), function(x, i, j, ..., value) {
    get <- unlist(c(as.list(i, j), list(...)))
    x@metadata[get] <- value
})

#' @aliases [[<-,amModel,ANY,ANY-method [[<-,amModel-method
#' @rdname methods-amModel
setMethod('[[<-', signature(x = 'amModel'), function(x, i, value) {
    x@metadata[[i]] <- value
})


##########################
#' @aliases summary,amModel-method
#' @rdname methods-amModel
#' @export
setMethod('summary', 'amData', function(object, ...) {
    print(summary(object@data, ...))
    cat('\n--- Metadata ---\n')
    print(metaSummary(object@metadata), right = FALSE)
})

#' @aliases [,amData,ANY,ANY,ANY-method [,amData-method
#' @rdname methods-amModel
setMethod('[', 'amData', function(x, i, j, ...) {
    get <- unlist(c(as.list(i, j), list(...)))
    x@metadata[get]
})

#' @aliases [[,amData,ANY,ANY-method [[,amData-method
#' @rdname methods-amModel
setMethod('[[', 'amData', function(x, i) {
    x@metadata[[i]]
})

#' @aliases [<-,amData,ANY,ANY,ANY-method [<-,amData-method
#' @rdname methods-amModel
setMethod('[<-', 'amData', function(x, i, j, ..., value) {
    get <- unlist(c(as.list(i, j), list(...)))
    x@metadata[get] <- value
})

#' @aliases [[<-,amData,ANY,ANY-method [[<-,amData-method
#' @rdname methods-amModel
setMethod('[[<-', 'amData', function(x, i, value) {
    x@metadata[[i]] <- value
})






