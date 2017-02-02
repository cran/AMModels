#' modelMgr
#' @title Graphical UI For The AMModels Package
#' @description The model manager is a graphical user interface supplied to accomplish nearly all functions within the AMModels package. Models and data can be tagged with metadata and organized within model libraries. Model libraries can be imported, searched, subset, edited, and exported through the GUI. The modelMgr GUI allows users to import models and data from either a .RData or .rda file or from the user's \code{.GlobalEnv}.
#' @param \dots Additional arguments to \code{shiny::runApp}.
#' @keywords misc
#' @export 
#' @author Jon Katz
#' @examples
#' \dontrun{ 
#' # The shiny app
#' modelMgr()
#' 
#' # Accepts args to shiny::runApp
#' modelMgr(quiet = TRUE)
#' }


modelMgr <- function(...) {
  appDir <- system.file("modelMgr", package = "AMModels")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `AMModels`.", call. = FALSE)
  }

  shiny::runApp(appDir, ...)
}
