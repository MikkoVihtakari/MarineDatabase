% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confirm_species_list.R
\name{confirm_species_list}
\alias{confirm_species_list}
\title{Confirm species names from the World Register of Marine Species}
\usage{
confirm_species_list(
  x,
  ranks = c("kingdom", "phylum", "class", "order", "genus")
)
}
\arguments{
\item{x}{A \code{\link[=make_species_list]{speciesList}} object.}

\item{ranks}{Ranks to be included in the output (see \code{Value})}
}
\description{
Uses the \code{\link[=taxize]{taxize}} package to confirm a \link[=make_species_list]{species list} against the World Register of Marine Species database.
}
\author{
Mikko Vihtakari
}
