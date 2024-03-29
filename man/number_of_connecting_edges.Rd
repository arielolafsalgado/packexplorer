% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/number_of_connecting_edges.R
\name{number_of_connecting_edges}
\alias{number_of_connecting_edges}
\title{Get the number of conecting edges between two groups}
\usage{
number_of_connecting_edges(g, group1, group2, mode = "all")
}
\arguments{
\item{g}{The graph object}

\item{group1}{The departure set of nodes}

\item{group2}{The arrival set of nodes}

\item{mode}{The direction of the edges to be considered. If 'in', the edges considered will be incidents over group1, if 'out', will be incident over group2, if 'all', all edges are considered.}
}
\description{
It calculates the number of edges from group1 to group2
}
