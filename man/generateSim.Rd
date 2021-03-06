% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.R
\name{generateSim}
\alias{generateSim}
\title{Generate simulations according several factors}
\usage{
generateSim(template, factors, parent = "plant")
}
\arguments{
\item{template}{File path to template sim files}

\item{factors}{A data frame which contained all combinations of factors.}

\item{parent}{The xml node to insert new attributes
The column names of data frame are parameter names in the sim file.}
}
\value{
A list which contain all simulations. Row names of factors 
are used for simulation names.
}
\description{
Generate simulations according several factors
}
