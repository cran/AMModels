#' @name insertAMModelLib
#' @aliases  insertAMModelLib
#' @title Insert Model Of Class \code{amModel} Or Dataset Of Class \code{amData} Into An \code{amModelLib} Object
#' @description Inserts a model into the model slot of an \code{\link{amModelLib}} object, or inserts a dataset into the data slot of an \code{amModelLib} object. If the \code{amModelLib} is not specified, the function will create an info-less and description-less lib -- or specify these components in the \dots argument.
#' @details If the argument amml is NULL, the function will call \code{amModelLib} to create the object.  The argument info can be passed to this function via the \dots argument.  
#' @param models  A named list of \code{\link{amModel}} objects, each composed of a fitted model object and metadata.
#' @param data An object of class \code{\link{amData}}.
#' @param amml  The name of the object that of class \code{amModelLib}.
#' @param ...  Additional arguments to be passed to the function \code{amModelLib} to be included in the \code{amModelLib} if one is created.
#' @return An object of class \code{amModelLib}
#' @family amModelLib
#' @keywords manip
#' @export 
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
#' plant.data <- amData(data = plant.data, comment = 'Dataset from lm helpfile.')
#' 
#' # create an amModel object with metadata and a soft link to the data
#' full.model <- amModel(
#'     model = lm.D9, 
#'     comment = 'full model', 
#'     source = 'lm helpfile (R).', 
#'     taxa = 'plants', 
#'     data = 'plant.data'
#' )
#' 
#' 
#' # create an amModelLib that contains the amModel object and the amData object
#' # the model and data must be supplied as named lists
#' mymodels <- amModelLib(
#'     description = "An example amModelLib.",
#'     models = list(full.model = full.model), 
#'     data = list(plant.data = plant.data)
#' )
#'  
#' # create second amModel object with metadata and a soft link to the same data
#' no.int.model <- amModel(
#'     model = lm.D90, 
#'     comment = 'model without intercept', 
#'     source = 'lm helpfile (R).', 
#'     taxa = 'plants', 
#'     data = 'plant.data'
#' )
#' 
#' # create a second amData object 
#' log.plant.data <- data.frame(group, log.weight=log(weight))
#' log.plant.data <- amData(
#'     data = log.plant.data, 
#'     comment = 'data to fit log model', 
#'     source = 'lm helpfile (R).'
#' )
#' 
#' # insert the second model and second dataset to the amModelLib
#' mymodels <- insertAMModelLib(
#'     mymodels, 
#'     models = list(no.int.model = no.int.model),
#'     data = list(log.plant.data = log.plant.data)
#' )
#' 

insertAMModelLib <- function(
    models, # a named list of amModels, each containing a fitted model object and metadata
    data,   # an amData object
    amml,   # an existing amModelLib object to merge
    ...     # additional arguments to createAMModelList, such as info list
) {
    if (!missing(models)) {
        if (length(models)) {
            mn <- names(models)
            if (any(is.null(mn))) stop('Model names are required.')
            names(models) <- mn
#            names(models) <- tolower(mn)
            datnames <- unlist(lapply(models, function(y) grep('data', names(y@metadata), ignore.case = TRUE)))
            datnames2 <- unlist(lapply(names(datnames), function(y) models[[y]]@metadata[[datnames[y]]]))
        } else models <- datnames2 <- NULL
    } else models <- NULL
    if (!missing(data)) {
        if (length(data)) {
            dn <- names(data)
            if (any(is.null(dn))) stop('Data names are required.')
            names(data) <- dn
#            names(data) <- tolower(dn)
        } else data <- NULL
    } else data <- NULL
    if (missing(amml)) {
        amml <- amModelLib(...)
        ammlInfo(amml) <- list(date.created = format(Sys.time(), format="%Y-%m-%d %H:%M:%S"))
    }
    amml@data <- c(amml@data, data)
    dn <- names(amml@data)
    names(amml@data) <- make.names(names(amml@data), unique = TRUE)
    if (!all(dn == names(amml@data))) {
        warning('Data names were altered to be made unique.')
        if (!is.null(datnames2)) {
            # If data names are adjusted check the data name reference in the model metadata
            models <- lapply(models, function(y) {
                mmet <- y@metadata
                mmdat <- grepl('data', names(mmet), ignore.case = TRUE)
                if (any(mmdat)) {
                    # Now assuming only one match
                    tochange <- which(mmet %in% dn)
                    newname <- names(amml@data)[which(dn == mmet[[tochange]])]
                    mmet[[tochange]] <- newname[length(newname)]
                }
                y@metadata <- mmet
                y
            })
        }
    }
    amml@models <- c(amml@models, models)
    mn <- names(amml@models)
    names(amml@models) <- make.names(names(amml@models), unique = TRUE)
    if (!all(mn == names(amml@models))) warning('Model names were altered to be made unique.')

    amml
}













