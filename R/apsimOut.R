# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   9:06 PM Sunday, 6 May 2012
# * Copyright: AS IS
# *

#' Read apsim out file
#'
#' @param filename filename
#' @export
readApsimOut <- function(filename)
{
    temp <- readLines(filename, n = 100)
    if (length(temp) == 0)
    {
        return (NULL)
    }
    temp <- temp[grep("=", temp)]
    
    res <- utils::read.table(filename,
            sep = '', header = FALSE, as.is = TRUE, 
            skip = length(temp) + 2,
            col.names = scan(filename, '', 
                    sep = '', skip = length(temp), 
                    nlines = 1, quiet = TRUE))
    return (res)
}

#' Read head information from out file
#'
#' @param filename filename
#' @param headname headname
#' @param split split
#' @param ... ...
#' @export
readApsimHead <- function(filename, headname, split = ' *, *', ...)
{
    temp <- readLines(filename, n = 100)
    temp <- temp[grep(paste(headname, " *= *", sep = ''), temp)]
    
    if (length(temp) < 1)
    {
        return(NA)
    }
    
    arg <- list(...)
    if (!is.null(arg[['title.fun']]))
    {
        temp <- do.call(arg[['title.fun']], list(temp))
    }
    
    temp <- strsplit(temp, paste(headname, ' *= *', sep = ''))[[1]][2]
    # if (length(grep(' *, *', temp)) > 0)
    {
        temp <- strsplit(temp, split)[[1]]
        temp <- strsplit(temp, ' *= *')
        temp <- as.data.frame(t(as.data.frame(temp)))
        row.names(temp) <- seq(length = nrow(temp))
        names(temp) <- c('name', 'value')
        temp$name <- as.character(temp$name)
        temp$value <- as.character(temp$value)
    }
    return(temp)
}


#' Split a title
#'
#' @param title title
#' @param split split
#' @export
splitTitle <- function(title, split = ',')
{
    temp <- strsplit(title, paste0(' *', split, ' *'))[[1]]
    temp <- strsplit(temp, ' *= *')
    temp <- as.data.frame(t(as.data.frame(temp)))
    row.names(temp) <- seq(length = nrow(temp))
    names(temp) <- c('name', 'value')
    res <- as.character(temp$value)
    names(res) <- as.character(temp$name)
    res <- as.data.frame(t(as.data.frame(res)))
    for (i in seq(ncol(res)))
    {
        res[[i]] <- as.character(res[[i]])
    }
    return(res)
}
