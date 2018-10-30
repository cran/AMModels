#' @name methods-amModelLib
#' @docType methods
#' @include classes.R methods-amModel.R
#' @aliases methods-amModelLib
#' @title Methods For Displaying, Summarizing, And Manipulating \code{amModelLib} Objects
#' @param object An \code{\link{amModelLib}} object.
#' @param x An \code{amModelLib} object.
#' @param recursive Iterate recursively through lists (ignored).
#' @param i,j indices specifying elements to extract or replace. Indices are numeric or character vectors or empty (missing) or NULL. 
#' @param drop Not used.
#' @param value Replacement value.
#' @param name For \code{$} A literal character string or a name (possibly backtick quoted); for \code{summary} an \code{\link{amModel}} or \code{\link{amData}} name as character string.
#' @param \dots Additional arguments passed to other functions or methods.
#' @description Getters and setters for AMModelLib objects.
#' @details Summary adds the metadata to the default show method. If \code{name} is supplied the call is passed on to the \code{amModel} or \code{amData} object with the specified name.
#' @return \code{summary} returns a list with the same elements displayed during the call. Others return an \code{amModelLib} object.
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
#' #' # create an amData object that includes metadata
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
#' 
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
#' summary(mymodels)
#' mymodels <- c(mymodels, mymodels)
#' mymodels[c(2,1)]
#' mymodels[[1]]
#' mymodels[['full.model']]
#' mymodels$full.model
NULL

#' @aliases summary,amModelLib-method
#' @rdname methods-amModelLib
#' @export
setMethod('summary', 'amModelLib', function(object, name, ...) {
    if (missing(name)) {
        mshow <- modmet <- dshow <- datmet <- NULL
        # Print description
        if (object@description != '') {
            cat('\nDescription:\n') 
            catwrap(object@description)
        }
        
        # Print info
        cat('\nInfo:\n')
        info <- object@info
        if (length(info)) {
            for (i in 1:length(info)) {
                cat(' ', names(info)[i], '\n   ')
                catwrap(info[[i]])
            }
        } else {
            cat('\n** no informative metadata **\n')
        }
        
        # Models    
        if (!length(object@models)) {
            cat('\n --- There are no models --- \n')
        } else {
            cat('\n\n--- Model Names and Indices ---\n')
            mshow <- data.frame(name = names(object@models), class=sapply(1:length(object@models), function(x) paste0(class(object@models[[x]]@model), collapse = ', ')))
            rownames(mshow) <- 1:nrow(mshow)
            pkg <- lapply(1:length(object@models), function(x) attr(object@models[[x]]@model, 'class'))
            pkg <- sapply(pkg, function(x) {
                at <- attr(x, 'package')
                if (is.null(at)) at <- NA
                at
            })
            mshow$package <- pkg
            print(mshow)
            # Model metadata
            cat('\n\n--- Model metadata ---\n')
            for (i in 1:length(object@models)) {
                cat(paste0('[[', i, ']] ', names(object@models)[i], '\n'))
                print(metaSummary(object@models[[i]]@metadata), right = FALSE)
                cat('\n')
            }
        }
        
        # Data
        if (!length(object@data)) {
            cat('\n --- There are no datasets --- \n')
        } else {
            cat('\n\n--- Data Names and Indices ---\n  ')
            dclass <- sapply(1:length(object@data), function(x) paste0(class(object@data[[x]]@data), collapse = ', '))
            ddim <- lapply(object@data, function(x) {
                if (is.data.frame(x@data)) {
                    data.frame(rows = nrow(x@data), cols = ncol(x@data))
                } else {
                    data.frame(rows = NA, cols = NA)
                }
            })
            ddim <- do.call(rbind.data.frame, ddim)
            dshow <- data.frame(name = names(object@data), class = dclass, ddim)
            rownames(dshow) <- 1:nrow(dshow)
            pkg <- lapply(1:length(object@data), function(x) attr(object@data[[x]]@data, 'class'))
            pkg <- sapply(pkg, function(x) {
                at <- attr(x, 'package')
                if (is.null(at)) at <- NA
                at
            })
            dshow$package <- pkg
            print(dshow)
            # Data metadata
            cat('\n\n--- Data metadata ---\n')
            for (i in 1:length(object@data)) {
                cat(paste0('[[', i, ']] ', names(object@data)[i], '\n'))
                print(metaSummary(object@data[[i]]@metadata), right = FALSE)
                cat('\n')
            }
        }
        invisible(list(models=list(models = mshow, metadata=modmet), data = list(data = dshow, metadata = datmet)))
    } else {
        tem <- c(object@models, object@data)[[name]]
        if (!length(tem)) stop(paste0('Element "', name, '" not found.'))
        summary(tem)
    }
})

#' @aliases show,amModelLib-method
#' @rdname methods-amModelLib
setMethod('show', 'amModelLib', function(object) {
    # Print description
    if (object@description != '') {
        cat('\nDescription:\n')
        catwrap(object@description)
    }
    
    # Print info
    cat('\nInfo:\n')
    info <- object@info
    if (length(info)) {
        for (i in 1:length(info)) {
            cat(' ', names(info)[i], '\n   ')
            catwrap(info[[i]])
        }
    } else {
        cat('** no informative metadata **\n\n')
    }
    # Models
    cat('\nModels:\n')
    if (!length(object@models)) {
      cat('\n --- There are no models --- \n')
    } else {
      mshow <- data.frame(name = names(object@models), class = sapply(1:length(object@models), function(x) paste0(class(object@models[[x]]@model), collapse = ', ')))
      rownames(mshow) <- 1:nrow(mshow)
      pkg <- lapply(1:length(object@models), function(x) attr(object@models[[x]]@model, 'class'))
      pkg <- sapply(pkg, function(x) {
          at <- attr(x, 'package')
          if (is.null(at)) at <- NA
          at
      })
      
      mshow$package <- pkg
    
    print(mshow)
    }
    
    # Data
    cat('\nData:\n')
    if (!length(object@data)) {
      cat('\n --- There are no datasets --- \n')
    } else {
      dclass <- sapply(1:length(object@data), function(x) paste0(class(object@data[[x]]@data), collapse = ', '))
      ddim <- lapply(object@data, function(x) {
        if (is.data.frame(x@data)) {
          data.frame(rows = nrow(x@data), cols = ncol(x@data))
            } else {
              data.frame(rows = NA, cols = NA)
            }
          })
      ddim <- do.call(rbind.data.frame, ddim)
      dshow <- data.frame(name = names(object@data), class = dclass, ddim)
      rownames(dshow) <- 1:nrow(dshow)
      pkg <- lapply(1:length(object@data), function(x) attr(object@data[[x]]@data, 'class'))
      pkg <- sapply(pkg, function(x) {
          at <- attr(x, 'package')
          if (is.null(at)) at <- NA
          at
      })
      dshow$package <- pkg
    
    print(dshow)
    }

    invisible(NULL)
})

#' @aliases c,amModelLib-method
#' @rdname methods-amModelLib
setMethod('c', 'amModelLib', function(x, ..., recursive = FALSE) {
    y <- list(...)
    # Add a routine to keep names unique with the decimal system
    ymods <- unlist(lapply(y, function(z) z@models), recursive = FALSE)
    ymnames <- unlist(lapply(y, function(z) names(z@models)))
    xmnames <- names(x@models)
    modvals <- c(x@models, ymods)
    names(modvals) <- make.names(c(xmnames, ymnames), unique = TRUE)
    hasmodel <- sapply(modvals, function(z) as.logical(length(z)))
    x@models <- modvals[hasmodel]
    
    ydata <- lapply(y, function(z) z@data)
    ydnames <- unlist(lapply(y, function(z) names(z@data)))
    xdnames <- names(x@data)
    datavals <- c(x@data, unlist(ydata, recursive = FALSE))
    names(datavals) <- make.names(c(xdnames, ydnames), unique = TRUE)
    hasdata <- sapply(datavals, function(z) as.logical(length(z)))
    x@data <- datavals[hasdata]
    x
})

#' @aliases [,amModelLib,ANY,ANY,ANY-method [,amModelLib-method
#' @rdname methods-amModelLib
setMethod('[', 'amModelLib', function(x, i, j, ..., drop = TRUE) {
    mods <- x@models
    if (!missing(i)) {
        if (missing(j)) j <- NULL
        get <- c(i, j, ...)
        getmods <- mods[get]
    } else {
        getmods <- mods[]
    }
    moddata <- unique(unlist(c(
        sapply(getmods, function(x) x@metadata[['data']]),
        sapply(getmods, function(x) x@metadata[['Data']]),
        sapply(getmods, function(x) x@metadata[['DATA']])
#        sapply(getmods, function(x) grep('data', names(x@metadata), ignore.case=TRUE))
    )))
    getdata <- x@data[moddata]
    x@models <- getmods
    if (!length(moddata)) getdata <- unname(getdata)
    x@data <- getdata
    x
})

#' @aliases [[,amModelLib,ANY,ANY-method [[,amModelLib-method
#' @rdname methods-amModelLib
setMethod('[[', 'amModelLib', function(x, i) {
    x[i]
})


#' @aliases $,amModelLib-method
#' @rdname methods-amModelLib
setMethod('$', 'amModelLib', function(x, name) {
    x[name]
})

#' @aliases [[<-,amModelLib,ANY,ANY-method [[<-,amModelLib-method
#' @rdname methods-amModelLib
setMethod('[[<-', 'amModelLib', function(x, i, j, ..., value) {
    get <- c(i, j, ...)
    if (length(get) > 1) stop('Attempt to select multiple elements.')
    mods <- x@models
    moddata <- x@data
    if (methods::is(value, 'amModel')) {
        mods[get] <- list(value)
        x@models <- mods
        x
    } else if (methods::is(value, 'amData')) {
        moddata[get] <- list(value)
        x@data <- moddata
        x
    } else {
        stop('No method implemented for objects of class ', class(value))
    }
})








