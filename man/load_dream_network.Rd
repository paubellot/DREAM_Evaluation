% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_dream_network.R
\name{load_dream_network}
\alias{load_dream_network}
\title{Load dream network
This function strips-off the "G" characters from the gene identifiers}
\usage{
load_dream_network(A)
}
\arguments{
\item{A}{data.frame matrix that contains the edgelist with gene identifiers}
}
\value{
Edgelist matrix matrix
}
\description{
Load dream network
This function strips-off the "G" characters from the gene identifiers
}
\examples{
net <- load_dream_network(data.frame(TF=c("G1","G3"),TG=c("G2","G1"),w=c(1,0.5)))
}
