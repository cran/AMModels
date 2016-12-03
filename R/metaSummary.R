# summarize metadata list and truncate text to window width

metaSummary <- function(x) {
    max.nm.w <- max(nchar(names(x))) + 5
    vals <- unlist(x)
    maxw <- getOption('width')
    vals <- sapply(vals, function(z) {
        if((nchar(z) + max.nm.w) > maxw) paste0(substr(z, 1, maxw - max.nm.w - 3), '...')
        else z        
    })
    y <- data.frame(name=names(x), value=vals)
    rownames(y) <- 1:nrow(y)
    y
}
