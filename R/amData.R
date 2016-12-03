#' @name amData
#' @aliases  amData
#' @title Creates an \code{AMModelData} object that pairs datasets with associated metadata.
#' @description Creates an object of class \code{amData}, which is typically a data frame of covariate data or model fitting data, with mandatory metadata.
#' @param data  A dataset, typically data frame but may be in any structure.  
#' @param \dots Named metadata elements, either supplied individually and coerced to list or supplied as a named list.
#' @return An object of class \code{amData} suitable for inclusion in an \code{amModelLib} object.
#' @family amModelLib
#' @keywords manip
#' @export 
#' @examples
#' 
#' 
#' # create dataset from lm helpfile
#' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' 
#' # create a dataset that is of class data.frame
#' plant.data <- data.frame(weight, group)
#' 
#' # create an amData data object 
#' dat1 <- amData(data = plant.data, comment='Dataset from lm helpfile.', taxa = 'plants')
#' 
#' # the class of dat1 is amData
#' class(dat1)
#' 
#' # the summary function will invoke the summary method for the dataset's original class
#' summary(dat1)
#' 
#' # use the amModelLib function to create a new amModelLib called mymodels that 
#' # includes dat1; data must be supplied in a named list
#' mymodels <- amModelLib(
#'     data=list(dat1 = dat1), 
#'     description = "An example amModelLib called mymodels."
#' )
#' 
#' # use the lsData function to list the amData objects in an amModelLib
#' lsData(mymodels)
#' 
#' # the dataMeta function can be used to retrieve an amData object's metadata
#' dataMeta(amml = mymodels, 'dat1')
#' 
#' #  the dataMeta function can alse be used to set metadata
#' dataMeta(mymodels, 'dat1') <- list(
#'     url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html"
#' )
#' dataMeta(amml = mymodels, 'dat1')
#' 
#' # use the getAMData function to extract the dataset back to its orginal form
#' getAMData(amml = mymodels, 'dat1', as.list = FALSE)
#' 
#' # the retrieved datset is in its original class
#' class(getAMData(amml = mymodels, 'dat1', as.list = FALSE))
#' 
#' # use the amModelLib function to create an empty amModelLib
#' mymodels2 <- amModelLib(description = "An example amModelLib called mymodels2.")
#' 
#' # use the insertAMModelLib function to insert the amData object to an 
#' # existing amModelLib
#' mymodels2 <- insertAMModelLib(data = list(dat1 = dat1))
#'
#' # use rmData to remove an amData object from an amModelLib
#' rmData('dat1', amml = mymodels2)


amData <- function(data, ...) {
  metadata <- list(...)
  if(is.list(metadata[[1]])) metadata <- metadata[[1]]
  metadata$date <- as.character(as.Date(Sys.time()))
  ammd <- methods::new('amData', data = data, metadata = metadata)
  ammd
  
}

