#' @name grepAMModelLib
#' @title Search For A Model In A Model List Using \code{grep}
#' @description Returns an abbreviated amModelLib object that contains models and data that meet search terms.
#' @param pattern Search string or value, typically a model or data name
#' @param amml  An \code{\link{amModelLib}} object
#' @param search Length 1 \code{character} vector indicating whether to search and return models or data that meet the search criteria.
#' @param \dots Additional arguments to \code{grep}.
#' @details \code{grep} is used to search both names, values (models/data), and metadata. An attempt is made to keep data with models if searching for models, or to keep models with data if searching for data, or to keep prior/posterior models together. The relational link between models their data relies on a case-agnostic 'data' element in the model metadata that names the linked data, and the same is true for models that use 'prior' to link to other models. 
#' @return An object of class \code{amModelLib}.
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
#' 
#' # create two amModel objects with metadata and a soft link to the data
#' full.model <- amModel(
#'     lm.D9, 
#'     comment = 'full model', 
#'     source = 'lm helpfile (R).', 
#'     taxa = 'plants', 
#'     data = 'plant.data'
#' )
#' 
#' no.int.model <- amModel(
#'     lm.D90, 
#'     comment = 'model without intercept', 
#'     source = 'lm helpfile (R).', 
#'     taxa = 'plants', 
#'     data = 'plant.data'
#' )
#' 
#' 
#' # create an amData object that includes metadata
#' plant.data <- data.frame(group = group, weight = weight)
#' plant.data <- amData(
#'     plant.data, 
#'     comment = 'Dataset from lm helpfile.'
#' )
#'
#' log.plant.data <- data.frame(group, log.weight=log(weight))
#' log.plant.data <- amData(
#'     log.plant.data, 
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
#'     data=list(
#'         plant.data = plant.data, 
#'         log.plant.data = log.plant.data
#'     )
#' )
#' 
#'
#' # search the entire amModelLib for the word 'intercept'
#' # the dataset associated with the model will be returned
#' grepAMModelLib("intercept", amml = mymodels) 
#' 
#' # the class of returned search is an amModelLib object
#' class(grepAMModelLib("intercept", amml = mymodels))  
#'  
#' # search for data containing the word 'log'
#' grepAMModelLib("log", amml = mymodels, search = "data") 
#' 
#' # search for models containing the word 'full'
#' # Because 'full.model' is soft-linked to a dataset, 
#' # the dataset information will be returned.
#' grepAMModelLib("full", amml = mymodels, search = "model") 
#' 
#'   


grepAMModelLib <- function(
    pattern,        # Either a model name, a data name, or a search string or value
    amml,         # an amModelLib object
    search = c('all', 'model', 'data'), # search model or data names and contents
    ...           # additional arguments to grep such as ignore.case
) {
    if (!methods::is(amml, 'amModelLib') || missing(amml)) stop('Must provide an amModelLib object.')
    search <- match.arg(search)
    
    if (search == 'model') {
        models <- amml@models
        mod.l <- lapply(models, function(x) x@model)
        mmet.l <- lapply(models, function(x) x@metadata)
        mn <- names(models)
        ind.name <- grep(pattern, mn, ...)
        ind.mod <- grep(pattern, mod.l, ...)
        ind.mmet <- grep(pattern, mmet.l, ...)
        models <- models[unique(c(ind.name, ind.mod, ind.mmet))]

        # find priors pointed to by each model
        modprior <- unlist(
            lapply(models, function(x) grep('prior', names(x@metadata), ignore.case = TRUE))
        )
        if (length(modprior)) {
            # pull out the pointers
            modpointers <- unique(sapply(1:length(modprior), function(x) amml@models[[names(modprior)[x]]]@metadata[[modprior[x]]]))
            # identify which pointers match the search results and pull those datasets
            getmodels <- amml@models[modpointers]
            # ignore dead-end pointers
            getmodels <- getmodels[unlist(lapply(getmodels, function(x) as.logical(length(x))))]
            if(length(getmodels)) models <- c(models, getmodels)
        } 
        
        # return an amModelLib, complete with associated data
        if (length(models)) {
            amml@models <- models
        } else {
            amml@models <- list()
        }
        
        # find that data pointed to by each model
        moddata <- unlist(
            lapply(models, function(x) grep('data', names(x@metadata), ignore.case = TRUE))
        )
        if (length(moddata)) {
            # pull out the pointers
            modpointers <- unique(sapply(1:length(moddata), function(x) amml@models[[names(moddata)[x]]]@metadata[[moddata[x]]]))
            # identify which pointers match the search results and pull those datasets
            getdata <- amml@data[modpointers]
            # Ignore dead-end pointers
            getdata <- getdata[unlist(lapply(getdata, function(x) as.logical(length(x))))]
    #        if(!length(moddata)) getdata <- unname(getdata)
            amml@data <- getdata
        } else {
            amml@data <- list()
        }
        
        amml
    } else if (search == 'data') {
        dat <- amml@data
        dat.l <- lapply(dat, function(x) x@data)
        dmet.l <- lapply(dat, function(x) x@metadata)
        dn <- names(dat)
        ind.name <- grep(pattern, dn, ...)
        ind.dat <- grep(pattern, dat.l, ...)
        ind.dmet <- grep(pattern, dmet.l, ...)
        dat <- dat[unique(c(ind.name, ind.dat, ind.dmet))]
        # return an amModelLib, complete with associated models
        if (length(dat)) {
            amml@data <- dat
        } else {
            amml@data <- list()
        }
        # find the models that point to data
        datmod <- unlist(
            lapply(amml@models, function(x) any(grep('data', names(x@metadata), ignore.case = TRUE), grep('prior', names(x@metadata), ignore.case = TRUE)))
        )
        if (length(datmod)) {
            # pull out those pointers
            datpointers <- sapply(1:length(datmod), function(x) amml@models[[names(datmod)[x]]]@metadata[[datmod[x]]])
            # identify which pointers match the search results and pull those models
            getmodel <- amml@models[names(datmod)][datpointers %in% names(dat)]
            amml@models <- getmodel
        } else {
            amml@models <- list()
        }
        amml
    } else if (search == 'all') {
        # search models
        foundmodels <- grepAMModelLib(pattern, amml, search = 'model', ...)
        founddata <- grepAMModelLib(pattern, amml, search = 'data', ...)
        # filter for unique names
        foundmodels_modnames <- lsModels(foundmodels)
        foundmodels_datnames <- lsModels(foundmodels)
        
        founddata_modnames <- lsModels(founddata)
        founddata_datnames <- lsModels(founddata)
        
        doublemodels <- founddata_modnames %in% foundmodels_modnames
        doubledata <- founddata_datnames %in% foundmodels_datnames
        
        mods <- c(foundmodels@models, founddata@models[!doublemodels])
        dat <- c(foundmodels@data, founddata@data[!doubledata])
        
        # return an amModelLib, complete with associated data
        if (length(mods)) {
            amml@models <- mods
        } else {
            amml@models <- list()
        }
        
        if (length(dat)) {
            amml@data <- dat
        } else {
            amml@data <- list()
        }
        
        amml
    }
}






