\encoding{latin1}
\name{krfun}
\alias{krfun}
\title{Multiscale second-order neigbourhood analysis of a multivariate spatial point pattern using Rao quandratic entropy}
\description{
  Computes distance-dependent estimates of Rao's quadratic entropy from a multivariate spatial point pattern 
  in a simple (rectangular or circular) or complex sampling window. Computes optionally local confidence limits of the functions
  under the null hypothesis of a random allocation of species labels (see Details).
}
\usage{
krfun(p, upto, by, nsim=0, dis = NULL, alpha = 0.01)
}
\arguments{
  \item{p }{a \code{"spp"} object defining a spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{nsim }{number of Monte Carlo simulations to estimate local confidence limits of the null hypothesis of a random allocation of species labels (see Details).
  By default \code{nsim = 0}, so that no confidence limits are computed.}
  \item{dis }{(optional) a \code{"dist"} object defining Euclidean distances between species. By default \eqn{dis = NULL} so that species are considered equidistant.}
  \item{alpha }{if \code{nsim>0}, significant level of the confidence limits. By default \eqn{\alpha = 0.01}.}
}
\details{
 Function \code{krfun} computes distance-dependent functions of Rao (1982) quadratic entropy.\eqn{Kr(r)} is an equivalent to Shimatani \eqn{\alpha(r)} while \eqn{gr(r)} in an equivalent
 of Shimatani \eqn{\beta(r)}, but both weigted with species distances defined by matrix \code{dis}. Matrix \code{dis} typically contains a taxonomic, phylogentic or functional distance between
  species. When \code{dis = NULL}, species are considered each other equidistant and \code{krfun} returns the same results than \code{\link{ksfun}}.
  
 The program introduces an edge effect correction term according to the method proposed by Ripley (1977)
  and extended to circular and complex sampling windows by Goreaud & Pélissier (1999).\cr\cr
 
  Theoretical values under the null hypothesis of random allocation of species labels is Rao diversity coefficient (see \code{\link{divc}} in package \code{ade4}).
  Local Monte Carlo confidence limits and p-values of departure from this hypothesis are estimated at each distance \eqn{r} by reallocating at random 
  the species labels among points of the pattern, keeping the point locations unchanged.
}
\value{
 A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
  \item{gr }{a data frame containing values of the function \eqn{gr(r)}.}
  \item{kr }{a data frame containing values of the function \eqn{Kr(r)}.\cr\cr}
    \item{}{Each component except \code{r} is a data frame with the following variables:\cr\cr}
\item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values, i.e. Rao \eqn{HD} for all the points.}
 \item{sup }{(optional) if \code{nsim>0} a vector of the upper local confidence limits of a random distribution of species labels at a significant level \eqn{\alpha}.}
 \item{inf }{(optional) if \code{nsim>0} a vector of the lower local confidence limits of a random distribution of species labels at a significant level \eqn{\alpha}.}
 \item{pval }{(optional) if \code{nsim>0} a vector of local p-values of departure from a random distribution of species labels.}
}
\references{
 Rao, C.R. 1982. Diversity and dissimilarity coefficient: a unified approach. \emph{Theoretical Population Biology}, 21:24-43.

 Pavoine, S., Ollier, S., & Pontier, D. 2005. Measuring diversity from dissimilarities with Rao's quadratic entropy: are any dissimilarities suitable? \emph{Theoretical Population Biology}, 67:231-239.
 
 Goreaud F. & Pélissier R. 1999. On explicit formulas of edge effect correction for Ripley's K-function. \emph{Journal of Vegetation Science}, 10:433-438.

 Ripley B.D. 1977. Modelling spatial patterns. \emph{Journal of the Royal Statistical Society B}, 39:172-192.
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
\seealso{
\code{\link{plot.fads}},
 \code{\link{spp}},
 \code{\link{ksfun}},
 \code{\link{divc}}.
}
\examples{
  data(Paracou15)
  P15<-Paracou15
  # spatial point pattern in a rectangle sampling window of size 125 x 125
  swmr <- spp(P15$trees, win = c(125, 125, 250, 250), marks = P15$species)
  krwmr <- krfun(swmr, dis = P15$FuncDist, 50, 5, 50)
  #running more simulations is slow
  #krwmr <- krfun(swmr, dis = P15$FuncDist, 50, 5, 500)
  plot(krwmr)

  # spatial point pattern in a circle with radius 50 centred on (125,125)
  swmc <- spp(P15$trees, win = c(125,125,50), marks = P15$species)
  krwmc <- krfun(swmc, dis = P15$FuncDist, 50, 5, 100)
  #running more simulations is slow
  #krwmc <- krfun(swmc, dis = P15$FuncDist, 50, 5, 500)
  plot(krwmc)
  
  # spatial point pattern in a complex sampling window
  swrt <- spp(P15$trees, win = c(125,125,250,250), tri = P15$tri, marks = P15$species)
  krwrt <- krfun(swrt, dis = P15$spdist, 50, 5)
  #running simulations is slow
  #krwrt <- krfun(swrt, dis = P15$spdist, 50, 5, 500)
  plot(krwrt)
}
\keyword{spatial}