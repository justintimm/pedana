% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_trait_xr.R
\name{sim_trait_xr}
\alias{sim_trait_xr}
\title{Simulate an X-chromosomal recessive trait}
\usage{
sim_trait_xr(x)
}
\arguments{
\item{x}{Object of class \code{ped}, usually created using the
\code{\link[=sim_pedigree]{sim_pedigree()}} function.}
}
\value{
Returns an object of class \code{ped}.
}
\description{
\code{\link[=sim_trait_xr]{sim_trait_xr()}} simulates an X-chromosomal recessive trait. This function
is faster than \code{\link[=sim_trait]{sim_trait()}}. For this purpose, however, the simulated
pedigree is modified during the simulation. If this is not desired, the
function \code{\link[=sim_trait]{sim_trait()}} should be used instead. Often the simulation is not
successful. Use with caution.
}
\examples{
set.seed(7)
obj <- sim_pedigree(generations = 4, tfr = 2)
obj <- sim_trait_xr(obj)
plot(obj)

}
\seealso{
\code{\link[=sim_trait_xd]{sim_trait_xd()}} for X-linked dominant traits and the more general
function \code{\link[=sim_trait]{sim_trait()}}.
}
