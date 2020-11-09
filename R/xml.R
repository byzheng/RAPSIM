# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   8:50 PM Monday, 17 September 2012
# * Copyright: AS IS
# *

# XML related functions

#' Find a row for element name
#' @param name name
#' @param xml xml
#' @export
findElement <- function(name, xml)
{
    pos <- grep(paste("(^.*<", name,
                    ")(>| .*).*(>.*$)",
                    sep = ""), xml)
    return(pos)
}


#' Replace a row for element name
#' @param value value
#' @param xml xml
#' @param key key
#' @export
replaceElementVlaue <- function(value, xml, key = NULL)
{    
    if (!is.null(key))
    {
        pos <- grep(sprintf('<%s.*>(.*)</%s>', key, key), xml)
        if (length(pos) == 0)
        {
            stop(paste0('Cannot find "', key, '".'))
        }
    } else
    {
        pos <- seq(along = xml)
    }
    value <- rep(value, length = length(pos))
    value <- as.character(unlist(value))
    for (i in seq(along = pos))
    {
        c_value <- gsub("\\\\", "\\\\\\\\", value[i])
        xml[pos[i]] <- sub(paste("(^.*<.*>).*(</.*>.*$)", sep = ""),
                paste("\\1", c_value, "\\2", sep = ""), xml[pos[i]])
    }
    return(xml)
}



#' Remove all attribure of a vector
#' @param x x
#' @export
removeAttribure <- function(x)
{
    x <- as.list(x)
    res <- NULL
    for (i in seq(along = x))
    {
        res <- c(res, as.character(x[[i]]))
    }
    return(res)
}

#' Replace a row for element name
#' @param xml xml
#' @export
getElementVlaue <- function(xml)
{
    newvalue <- sub(paste("(^.*<.*>)(.*)(</.*>.*$)", sep = ""),
            "\\2", xml)
    return(newvalue)
}

#' Change attribute for a xml element
#' @param xml A xml element
#' @param attri The attribute name
#' @param new The new attribute value
#' @export
changeAttribute <- function(xml, attri, new)
{
    return(gsub(paste('(^.*', attri, '=")(.*)(" +.*$)', sep = ''), 
        paste('\\1', new, '\\3', sep = ''), xml))
}

#' Find the values with key from xml
#'
#' @param xml the xml document
#' @param key the name of xml element
#' @export
findXmlValue <- function(xml, key)
{
    library(stringr)
    value <- str_trim(gsub(sprintf('<%s.*>(.*)</%s>', key, key), 
        '\\1', xml[grep(sprintf('</%s>', key), xml)]))
    if (length(value) == 0)
    {
        value <- ''
    }
    value
}
