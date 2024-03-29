% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/voting_matrix.R
\name{voting_matrix}
\alias{voting_matrix}
\title{Matrix construction, used to rank packages}
\usage{
voting_matrix(G, my_packs = row.names(utils::installed.packages()),
  kind.of = "to_packages")
}
\arguments{
\item{G}{The package graph, as an igraph object. Usually the Suggests graph.}

\item{my_packs}{The packages that you are interested to use as reference. Default is your installed packages}

\item{kind.of}{The direction of the relations. If you want packages recommending my_packs, it should be "to_packages", and if you want packages recommended by my_packs, it should be "from_packages". It can also be "all".}
}
\value{
A list with packages and scores for each one.
}
\description{
This function creates the voting matrix from your packages and the package graph.
}
\examples{
gs = my_network()[['Suggests']]
pack = my_packages()[1]
X = voting_matrix(gs,my_packs=pack)
}
