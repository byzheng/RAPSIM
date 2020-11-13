# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   8:50 PM Monday, 17 September 2012
# * Copyright: AS IS
# *

# sim related functions

#' Generate simulations according several factors
#' 
#' @param template File path to template sim files 
#' @param factors A data frame which contained all combinations of factors.
#' @param parent The xml node to insert new attributes
#' The column names of data frame are parameter names in the sim file.
#' @return A list which contain all simulations. Row names of factors 
#' are used for simulation names.
#' @export
generateSim <- function(template, factors, parent = 'plant')
{
    sim_names <- row.names(factors)
    factor_names <- names(factors)
    # read template files
    if (length(template) == 1)
    {
        template_sim <- readLines(template, warn = FALSE)
    } else
    {
        template_sim <- template
    }
    all_sim <- NULL
    for (i in seq(length= nrow(factors)))
    {
        new_sim <- template_sim
        
        # # replace simulation names
        # sim_title <- grep("(^.*simulation name.*=.*\")(.*)(\".*executable.*$)", 
                # new_sim)
        # new_sim[sim_title] <- sub("(^.*simulation name.*=.*\")(.*)(\".*executable.*$)", 
                # paste("\\1", sim_names[i], "\\3", sep = ""), 
                # new_sim[sim_title])
        # sim_title <- grep("<title>.*</title>", new_sim)
        # new_sim[sim_title] <- gsub("<title>.*</title>", 
                # paste("<title>", sim_names[i], "</title>", sep = ""), 
                # new_sim[sim_title])
        # replace each factor
        c_factor <- removeAttribure(factors[i,])
        replace_num <- 0
        for (j in seq(along = factor_names))
        {
            if (is.na(c_factor[j]))
            {
                warning(paste("NA found ", c_factor[j], " in ",
                                factor_names[j], ". Skip it.", sep = ""))
                next
            }
            #<date type="text" description="Enter sowing date (dd-mmm) : ">16-apr</date>
            factor_row <- findElement(factor_names[j], new_sim) 
            
            if (length(factor_row) == 0)
            {
                parent_row <- findElement(parent, new_sim) 
                if (length(parent_row) == 0)
                {
                    warning(paste("Can not find factor for ", c_factor[j], " in ",
                                parent, ". Skip it.", sep = ""))
                    next
                }
                new_ele <- paste('<', factor_names[j], '>', c_factor[j], 
                    '</', factor_names[j], '>', sep = '')
                new_sim <- append(new_sim, new_ele, after = parent_row)
                replace_num <- replace_num + 1
                next
            }
            
            # testing whether cultivar parameters
            base_cul_row <- grep("<base_cultivar>", new_sim[1:factor_row[1]])
            if (length(base_cul_row) == 0)
            {
                if (length(factor_row) > 1)
                {
                    warning(paste("There are several rows found for ", 
                                    c_factor[j], " in ",
                                    factor_names[j], 
                                    ". Only first row is used.", sep = ""))
                }
                
                new_sim[factor_row[1]] <- replaceElementVlaue(c_factor[j], 
                        new_sim[factor_row[1]])
                        
            } else
            { # For cultivar parameters
                # find cultivar 
                cultivar <- findElement("cultivar", new_sim) 
                cultivar <- sub("(^.*<cultivar.*>)(.*)(</cultivar>.*$)", 
                        "\\2", new_sim[cultivar])
                
                # Find starting and end row of cultivar
                cultivar_s_row <- grep(paste("^.*<", tolower(cultivar),
                                " .*cultivar.*=.*\"yes\".*>.*$", 
                                sep = ""), new_sim)
                cultivar_e_row <- grep(paste("^.*</", tolower(cultivar), ">.*$", 
                                sep = ""), new_sim)
                if (length(cultivar_s_row) == 0 | 
                        length(cultivar_e_row) == 0)
                {
                    warning(paste("Can not find parameters for Cultivar ", 
                                    cultivar, ". Skip it.", sep = ""))
                    next
                }
                cul_xml <- new_sim[cultivar_s_row:cultivar_e_row]
                cul_factor_row <- findElement(factor_names[j], cul_xml)
                
                if (length(cul_factor_row) > 0)
                { # Find attribute for this cultivar
                    new_sim[cultivar_s_row + cul_factor_row - 1 ] <- 
                            replaceElementVlaue(c_factor[j], cul_xml[cul_factor_row])
                } else
                { # Not find attribute for this cultivar, add a new row
                    new_row <- paste("          <", factor_names[j], ">", 
                            c_factor[j], "</", factor_names[j], 
                            ">", sep = "")
                    new_sim <- c(new_sim[1:(cultivar_e_row-1)], new_row, 
                            new_sim[(cultivar_e_row):length(new_sim)])
                }
            }
            replace_num <- replace_num + 1
        }
        if (replace_num == 0)
        {
            warning("There are nothing to generate new simulation. Skip it.")
        } else
        {
            all_sim[[sim_names[i]]] <- new_sim
        }
    }
    return(all_sim)
}


#' Remove a component from sim file
#' @param sim A character vector of sim 
#' @param pattern A character string containing a regular expression 
#' to match a component
#' @export
removeComponent <- function(sim, pattern)
{
    match_pos <- grep(pattern, sim)
    if (length(match_pos) == 0)
    {
        warning('No matched component.')
        return (sim)
    }
    remove_rows <- NULL
    for (i in seq(along = match_pos))
    {
        start_pos <- grep('^.*<component .*>.*$', sim[1:match_pos[i]])
        end_pos <- grep('^.*</component>.*$', sim[match_pos[i]:length(sim)])
        if (length(start_pos) == 0 | length(end_pos) == 0)
        {
            warning('Component cannot find. Skip it')
            next
        }
        start_pos <- max(start_pos)
        end_pos <- min(end_pos) + match_pos[i] - 1
        remove_rows <- c(remove_rows, start_pos:end_pos)
    }
    if (is.null(remove_rows))
    {
        warning('No matched component.')
        return (sim)
    }
    return(sim[-remove_rows])
}

#' Add a new component to sim file
#' @param sim A character vector of sim 
#' @param component A character vec of new component
#' @param parent A character string containing a regular expression 
#' to match parent component
#' @export
addComponent <- function(sim, component, parent = '<system name="paddock"')
{
    match_pos <- grep(parent, sim)
    if (length(match_pos) == 0)
    {
        stop('No matched parent.')
    }
    if (length(match_pos) > 1)
    {
        stop('Multiple matched parent.')
    }
    match_pos <- min(grep('<component', sim[match_pos:length(sim)])) + match_pos - 2
    sim <- append(sim, component, match_pos)
    return(sim)
}


#' Add irrigation at fixed date component
#' @param amount Irrigation amount
#' @param date date
#' @param name name
#' @export
irrigationAtFixedDate <- function(amount, date, name = 'Irrigate on fixed date')
{    
    template <- componentTemplate('IrrigationAtFixedDate')
    pos <- findElement('amount', template)
    template[pos] <- replaceElementVlaue(amount, template[pos])
    pos <- findElement('irrigDatesStr', template)
    template[pos] <- replaceElementVlaue(paste(date, collapse = ' '), template[pos])    
    template[1] <- changeAttribute(template[1], 'name', name)
    return(template)
}

#' Add fertilizaton at sowing component
#' @param amount Irrigation amount
#' @param type type
#' @param name name
#' @export
fertiliseAtSowing <- function(amount, type, name = 'Fertilise at sowing')
{    
    template <- componentTemplate('FertiliseAtSowing')
    pos <- findElement('fert_amount_sow', template)
    template[pos] <- replaceElementVlaue(amount, template[pos])
    pos <- findElement('fert_type_sow', template)
    template[pos] <- replaceElementVlaue(type, template[pos])    
    template[1] <- changeAttribute(template[1], 'name', name)
    return(template)
}

#' Add fertilizaton at fixed date component
#' @param amount Irrigation amount
#' @param type type
#' @param date date
#' @param name name
#' @export
fertiliseAtFixed <- function(amount, type, date, name = 'Fertilise on fixed date')
{    
    template <- componentTemplate('FertiliseAtFixedDate')
    pos <- findElement('fert_amount', template)
    template[pos] <- replaceElementVlaue(amount, template[pos])
    pos <- findElement('fert_type', template)
    template[pos] <- replaceElementVlaue(type, template[pos])    
    pos <- findElement('fert_date', template)
    template[pos] <- replaceElementVlaue(date, template[pos])
    template[1] <- changeAttribute(template[1], 'name', name)
    return(template)
}
