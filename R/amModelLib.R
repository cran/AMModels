#' @name amModelLib
#' @aliases  amModelLib
#' @title Create An \code{AMModelLib} Object That Stores Lists Of \code{amModel} And \code{amData} Objects  
#' @description Creates an object of class \code{amModelLib}, either empty, or containing \code{\link{amModel}} and/or \code{\link{amData}} objects.
#' @details \code{amModelLib} objects are useful for storing multiple models and datasets of a related theme, along with relevant metadata. Most extraction and manipulation functions will attempt to keep any relevant data with any model for which it is used. Multiple models may refer to a single dataset.
#' @param models  A list of objects of class amModel.  
#' @param data  A list of objects of class amData.    
#' @param info Named list with descriptive metadata about the \code{amModelLib} object. 
#' @param description Text field describing the \code{amModelLib} object. 
#' @return An object of class amModelLib.
#' @family amModelLib
#' @keywords manip
#' @export 
#' @examples
#' 
#' 
#' # code from the lm helpfile
#' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' lm.D90 <- lm(weight ~ group - 1) # omitting intercept
#' 
#' # create a data.frame of the plant data
#' plant.data <- data.frame(group = group, weight = weight)
#' 
#' # create an amData object that includes the data.frame and metadata 
#' plant.data <- amData(
#'     data = plant.data, 
#'     comment = 'Dataset from lm helpfile.'
#' )
#' 
#' # create an amModel model object that includes model lm.D9 and metadata
#' # use the metadata keyword 'data' to link the model with the amData object 
#' # that produced it
#' plant.model1 <- amModel(
#'     model = lm.D9, 
#'     comment = 'Example model produced from from lm helpfile.',
#'     data = 'plant.data'
#' )
#' 
#' # create a second amModel model object that includes model lm.D90 and metadata
#' # use the metadata keyword 'data' to soft link the model with its data
#' plant.model2 <- amModel(
#'     model = lm.D90, 
#'     comment = 'Second model produced from from lm helpfile.',
#'     data = 'plant.data'
#' )
#' 
#' 
#' # use the amModelLib function to create a new amModelLib containing the two
#' # amModel objects and one amData object
#' mymodels <- amModelLib(
#'      models = list(
#'         plant.model1 = plant.model1,
#'         plant.model2 = plant.model2
#'     ), 
#'     data = list(
#'         plant.data = plant.data
#'     ), 
#'     description = "This amModelLib stores models and data from the lm helpfile.",
#'     info = list(
#'         owner = "Me"
#'     )
#' )
#' 
#' # use the amModelLib function amModelLib to create an empty amModelLib called mymodels2
#' mymodels2 <- amModelLib(
#'      description = "A second amModelLib called mymodels2.", 
#'      info = list(
#'          owner = "Me2"
#'      )
#' )
#' 
#' # use the insertAMModelLib function to insert the two amModel objects and one 
#' # amData oject to the existing amModelLib
#' mymodels2 <- insertAMModelLib(
#'      models = list(
#'          plant.model1 = plant.model1, 
#'          plant.model2 = plant.model2), 
#'      data = list(plant.data = plant.data)
#' )
#' 
#' 
#' 
#' 

amModelLib <- function(models = list(), data = list(), info = list(), description = '') {
    if (length(info)) {
        if (!length(names(info))) stop("'info' list must be named.")
        if (!is.list(info)) tryCatch({info <- as.list(info)}, error = function(e) stop("'info' is not a list and cannot be coerced to list."), warning = function(w) stop("'info' is not a list and cannot be coerced to list."))
        info[['date.created']] <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
    }
    if (!is.character(description) || length(description) > 1) stop("'description' must be a length 1 character vector.")
    methods::new('amModelLib', models = models, data = data, info = info, description = description)
}








