#' @name simCovariate
#' @aliases  simCovariate
#' @title Simulate A Dataframe Of Uncorrelated Covariate(s) 
#' @description Quickly create a dataframe of uncorrelated random variables which can be used as covariates. Values are drawn from the normal, uniform, beta, binomial, poisson or bernoulli distributions.
#' @details \code{simCovariate} will create a vector(s) of random variables from a specified R probability distribution. The distribution can be specified by entering the name or the name of the R function; partial matching is performed. For example, specifying a distribution as runif, 'runif', uniform, or u can be be used to generate random samples from a uniform distribution, in which case R's \code{\link[stats]{runif}} function is called.  Additional arguments to the \code{\link[stats]{runif}} function are separated by commas. The function can be parameterized so that multiple covariates can be simulated from either the same distribution or from different distributions. \cr 
#' @param cov.list  A named list of covariates to be simulated and their required arguments.
#' @param ... additional arguments to be passed to \code{distr}. Argument in which the simulating distribution its corresponding arguments are specified.Minimally
#' contains \code{n}, for number of samples to draw, \code{shape1, shape2} for \code{\link{rbeta}},
#' and \code{size, prob} for \code{\link[stats]{rbinom}}. May also contain optional arguments such as 
#' \code{min, max} for \code{\link[stats]{runif}}, \code{mean, sd} for \code{\link[stats]{rnorm}}, and \code{ncp} for \code{\link[stats]{rbeta}}. Accepts single values, or vectors that will be applied to multiple columns. Vectors should be used with care as lengths are not checked.     
#' @param n  The number of samples to generate from each covariate.
#' @param add.yr Logical, if \code{TRUE} a field named \code{yr} is added with indices from \code{1:n}.
#' @return A data frame of random numbers from the specified distribution, 
#' with number of columns equal to the the number of cov.names (ncol=length(cov.names)).
#' @seealso \code{\link{amData}}
#' @family popmod
#' @keywords datagen, distribution
#' @export 
#' @examples
#' # We can specify the distribution using a function, function name, 
#' # or distribution name. Partial matching is performed. The examples 
#' # below generate data for a single covariate; random seeds are not 
#' # provided.
#' 
#' # All four examples provide same results and generate 10 random numbers 
#' # from a uniform distribution.  In some examples the results are rounded; 
#' # in other examples add.yr is set to TRUE to add a covariate called yr (year); 
#' # in other examples a random seed is provided to ensure reproducibility.
#' 
#' simCovariate(u1 =list(dist= runif), n=10, add.yr=FALSE)
#' simCovariate(u2=list(dist = 'runif', round=2), n = 10, add.yr=TRUE)
#' simCovariate(u3=list(dist ='uniform', seed=302), n=10, add.yr=TRUE)
#' simCovariate(u4 = list(dist ='u', seed=302, round=3, min=0, max=10), n=10, add.yr=TRUE)
#' 
#' # If multiple covariates are to be simulated, create a list of covariates 
#' # and then pass this covariate list as the argument, cov.list.  Here, create 
#' # a dataframe with seven covariates from five distributions, and 
#' # add a covariate called yr.  
#' cov.list <- list(
#'     unif1 = list(dist = 'runif', min=0, max=10, seed=334, round=0),
#'     unif2 = list(dist = 'runif', min=0, max=10, seed=668, round=0),
#'     norm1=list(dist = 'normal', mean = 10,sd = 2, seed=10, round=1),
#'     norm2=list(dist = 'normal',  mean = 50, sd = 10, seed=15, round=2),
#'     beta1=list(dist = rbeta, shape1=2, shape2=1, seed=1002),
#'     binom1=list(dist = 'bin', size=20, prob=0.5, seed=561),
#'     bern1=list(dist='bernoulli', size = 1, prob = 0.5, seed = 6)
#' )
#' 
#' simCovariate(cov.list, n = 10, add.yr = TRUE)

simCovariate <- function(
  cov.list=NULL,
  ...,
  n,
  add.yr = TRUE
) {
    
  if (is.null(cov.list)) {
    cov.list <- list(...)
    if (length(cov.list) == 1) {
      if (is.null(names(cov.list))) {
        cov.list <- cov.list[[1]]
      }
    }
  } 
    
        
    # set up list to hold output
    datalist <- list()
    
    distribution <-  c('uniform','normal','beta','binomial', 'poisson', 'bernoulli')
    
    seeds.used <- vector()
    
    for (i in 1:length(cov.list)) {
        
        cov <- cov.list[[i]]
        cov.name <- names(cov.list[i])
            
        # set the generating function
        distr <- cov[['dist']]
        if (!is.function(distr)) {
            if (distr == "bernoulli") {dist <- 'binomial'}
            distr <- tolower(distr)
            if (substr(distr,1,1) == 'r') distr <- substr(distr,2,nchar(distr))
            distr <- match.arg(distr,distribution)
            distr <- switch(distr, uniform=stats::runif, normal = stats::rnorm, beta = stats::rbeta, binomial = stats::rbinom, poisson = stats::rpois, bernoulli = stats::rbinom, sample = sample, multinomial = stats::rmultinom)
            }  
            
            # keep only necessary arguments
            arg.names <- names(formals(fun = distr))
            keepers <- names(cov) %in% arg.names
            rand.args <- cov[keepers == TRUE]
            rand.args$n <- n
            
            # set the seed
            if(is.null(cov[['seed']])) {seed <- sample(.Random.seed,1) 
            warning(paste("You did not provide a random seed for the simulation for covariate named ", cov.name , ".  Data have been simulated using", seed, "as the random seed.", sep = ""))} else {seed <- cov[['seed']]}
            
            seed <- as.integer(seed)
            set.seed(seed)
            seeds.used <- append(seeds.used, values = seed)
            
            # call the function
            dataset <- do.call(what = distr, args = rand.args)
            
            #  round if requested
            if (!is.null(cov[['round']])) dataset <- round(dataset, cov[['round']])
            
            # add to output list
            datalist[[cov.name]] <- dataset
    
      } # end of covariate type i
    
    # convert datalist to dataframe
    dataset <- as.data.frame(datalist)
    
    # add yr if requested
    if (add.yr == TRUE) {dataset$yr <- 1:nrow(dataset)}
    
    # add attributes
    attr(dataset, which = 'seeds.used') <- seeds.used
    attr(dataset, which = 'cov.list') <- cov.list
    attr(dataset, which = 'n') <- n
    
    return(dataset)

} # end of function
    


