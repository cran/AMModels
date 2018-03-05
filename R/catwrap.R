# For printing pretty metadata to the console. 
# Lightly adapted from base::strwrap() to remove some flexibility and 
# automatically cat() rather than just returning y.
catwrap <- function (x, width = 0.9 * getOption("width")) {
    if (!is.character(x)) 
        x <- as.character(x)
    UB <- TRUE
    if (all(Encoding(x) == "UTF-8")) 
        UB <- FALSE
    else {
        enc <- Encoding(x) %in% c("latin1", "UTF-8")
        if (length(enc)) 
            x[enc] <- enc2native(x[enc])
    }
    z <- lapply(strsplit(x, "\n[ \t\n]*\n", perl = TRUE, useBytes = UB), 
        strsplit, "[ \t\n]", perl = TRUE, useBytes = UB)
    for (i in seq_along(z)) {
        for (j in seq_along(z[[i]])) {
            words <- z[[i]][[j]]
            nc <- nchar(words, type = "w")
            if (anyNA(nc)) {
                nc0 <- nchar(words, type = "b")
                nc[is.na(nc)] <- nc0[is.na(nc)]
            }
            if (any(nc == 0L)) {
                zLenInd <- which(nc == 0L)
                zLenInd <- zLenInd[!(zLenInd %in% (grep("[.?!][)\"']{0,1}$", 
                  words, perl = TRUE, useBytes = TRUE) + 1L))]
                if (length(zLenInd)) {
                  words <- words[-zLenInd]
                  nc <- nc[-zLenInd]
                }
            }
            currentIndex <- 0L
            lowerBlockIndex <- 1L
            upperBlockIndex <- integer()
            lens <- cumsum(nc + 1L)
            first <- TRUE
            maxLength <- width
            while (length(lens)) {
                k <- max(sum(lens <= maxLength), 1L)
                if (first) {
                  first <- FALSE
                  maxLength <- width - 3
                }
                currentIndex <- currentIndex + k
                if (nc[currentIndex] == 0L) 
                  upperBlockIndex <- c(upperBlockIndex, currentIndex - 
                    1L)
                else upperBlockIndex <- c(upperBlockIndex, currentIndex)
                if (length(lens) > k) {
                  if (nc[currentIndex + 1L] == 0L) {
                    currentIndex <- currentIndex + 1L
                    k <- k + 1L
                  }
                  lowerBlockIndex <- c(lowerBlockIndex, currentIndex + 
                    1L)
                }
                if (length(lens) > k) 
                  lens <- lens[-seq_len(k)] - lens[k]
                else lens <- NULL
            }
            nBlocks <- length(upperBlockIndex)
            y <- lapply(seq_len(nBlocks), function(k) 
              paste(words[lowerBlockIndex[k]:upperBlockIndex[k]], collapse = " "))
        }
    }
    y <- as.character(unlist(y))
    print(noquote(y[1]))
    if(length(y) > 1)
      for(i in y[2:length(y)]) cat('   ',i,'\n')
}



