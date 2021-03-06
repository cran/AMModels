% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-amModel.R
\docType{methods}
\name{methods-amModel}
\alias{methods-amModel}
\alias{summary,amModel-method}
\alias{[,amModel,ANY,ANY,ANY-method}
\alias{[,amModel-method}
\alias{[[,amModel,ANY,ANY-method}
\alias{[[,amModel-method}
\alias{[<-,amModel,ANY,ANY-method}
\alias{[<-,amModel,ANY,ANY,ANY-method}
\alias{[<-,amModel-method}
\alias{[[<-,amModel,ANY,ANY-method}
\alias{[[<-,amModel-method}
\alias{summary,amData-method}
\alias{summary,amModel-method}
\alias{[,amData,ANY,ANY,ANY-method}
\alias{[,amData-method}
\alias{[[,amData,ANY,ANY-method}
\alias{[[,amData-method}
\alias{[<-,amData,ANY,ANY-method}
\alias{[<-,amData,ANY,ANY,ANY-method}
\alias{[<-,amData-method}
\alias{[[<-,amData,ANY,ANY-method}
\alias{[[<-,amData-method}
\title{Methods For Displaying, Summarizing, And Manipulating \code{amModel} And \code{amData} Objects}
\usage{
\S4method{summary}{amModel}(object, ...)

\S4method{[}{amModel,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[[}{amModel,ANY,ANY}(x, i)

\S4method{[}{amModel,ANY,ANY}(x, i, j, ...) <- value

\S4method{[[}{amModel,ANY,ANY}(x, i) <- value

\S4method{summary}{amData}(object, ...)

\S4method{[}{amData,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[[}{amData,ANY,ANY}(x, i)

\S4method{[}{amData,ANY,ANY}(x, i, j, ...) <- value

\S4method{[[}{amData,ANY,ANY}(x, i) <- value
}
\arguments{
\item{object, x}{An \code{\link{amModel}} or \code{\link{amData}} object.}

\item{\dots}{Additional arguments passed to other functions or methods.}

\item{i, j}{indices specifying elements to extract or replace. Indices are numeric or character vectors or empty (missing) or NULL.}

\item{drop}{Not used.}

\item{value}{Replacement value.}
}
\description{
Getters and setters for models and data.
}
\details{
Summary assumes some meaningful summary method exists for each object in its home package.
}
\examples{

# create dataset from lm helpfile
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept


# create am amModel object
full.model <- amModel(
    model = lm.D9, 
    comment = 'full model', 
    source = 'lm helpfile (R).', 
    taxa = 'plants', 
    data = 'plant.data'
)


# create an amData object
plant.data <- data.frame(group = group, weight = weight)
plant.data <- amData(
    data = plant.data, 
    source = 'lm helpfile (R).',
    comment = 'Dataset from lm helpfile.'
)

summary(full.model)

# [ and [[ index from metadata
full.model[c(2,1)]
full.model[[1]]
full.model[['taxa']]

plant.data[c(2,1)]
plant.data[[1]]
plant.data[['comment']]

}
\keyword{methods}
