\encoding{latin1}
\name{ksfun}
\alias{ksfun}
\title{Multiscale second-order neigbourhood analysis of a multivariate spatial point pattern}
\description{
  Computes estimates of Shimatani's \emph{alpha} and \emph{beta} functions of Simpson diversity from a multivariate spatial point pattern 
  in a simple (rectangular or circular) or complex sampling window. Computes optionally local confidence limits of the functions
  under the null hypothesis of a random allocation of species labels (see Details).
}
\usage{
ksfun(p, upto, by, nsim=0, alpha=0.01)
}
\arguments{
  \item{p }{a \code{"spp"} object defining a spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{nsim }{number of Monte Carlo simulations to estimate local confidence limits of the null hypothesis of a random allocation of species labels (see Details).
  By default \code{nsim=0}, so that no confidence limits are computed.}
  \item{alpha }{if \code{nsim>0}, significant level of the confidence limits. By default \eqn{\alpha=0.01}.}
}
\details{
 Function \code{ksfun} computes Shimatani's \eqn{\alpha(r)} and \eqn{\beta(r)} functions of Simpson diversity, called here \eqn{Ks(r)} and \eqn{gs(r)}, respectively.\cr\cr
 For a multivariate point pattern consisting of \eqn{i} species with intensity \eqn{\lambda}i, Shimatani (2001) showed that
  a distance-dependent measure of Simpson (1949) diversity can be estimated from Ripley (1977) \eqn{K}-function computed for each species separately and for all the points grouped toghether (see also Eckel et al. 2008). Function \code{ksfun} is thus a simple wrapper function of \code{\link{kfun}}.
  
  
  \eqn{Ks(r) = 1 - sum(\lambda i * \lambda i * Ki(r)) / (\lambda * \lambda * K(r))} which is an estimator of \eqn{\alpha(r)} in Shimatani (2001).\cr\cr
  \eqn{gs(r) = 1 - sum(\lambda i * \lambda i * gi(r)) / (\lambda * \lambda * g(r))} is the derivative of \eqn{Ks(r)} corresponding to \eqn{\beta(r)} in Shimatani (2001).\cr\cr
  
  The program introduces an edge effect correction term according to the method proposed by Ripley (1977)
  and extended to circular and complex sampling windows by Goreaud & Pélissier (1999).\cr\cr
 
  Theoretical values under the null hypothesis of random allocation of species labels is Simpson diversity index \eqn{D = 1 - sum(\lambda i * \lambda i) / (\lambda * \lambda)}.
  Local Monte Carlo confidence limits and p-values of departure from this hypothesis are estimated at each distance by reallocating at random the species labels among points of the pattern, keeping the point locations unchanged\eqn{r}.
}
\value{
 A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
  \item{gs }{a data frame containing values of the function \eqn{gs(r)}.}
  \item{ks }{a data frame containing values of the function \eqn{Ks(r)}.\cr\cr}
    \item{}{Each component except \code{r} is a data frame with the following variables:\cr\cr}
\item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values, i.e. Simpson \eqn{D} for all the points.}
 \item{sup }{(optional) if \code{nsim>0} a vector of the upper local confidence limits of a random distribution of species labels at a significant level \eqn{\alpha}.}
 \item{inf }{(optional) if \code{nsim>0} a vector of the lower local confidence limits of a Prandom distribution of species labels at a significant level \eqn{\alpha}.}
 \item{pval }{(optional) if \code{nsim>0} a vector of local p-values of departure from a random distribution of species labels.}
}
\references{
 Shimatani K. 2001. Multivariate point processes and spatial variation in species diversity. \emph{Forest Ecology and Managaement}, 142:215-229.

 Eckel, S., Fleisher, F., Grabarnik, P. and Schmidt V. 2008. An investigation of the spatial correlations for relative purchasing power in Baden-Württemberg. \emph{AstA - Advances in Statistical Analysis}, 92:135-152.
 
 Simpson, E.H. 1949. Measurement of diversity. \emph{Nature}, 688:163.
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
\seealso{
\code{\link{plot.fads}},
 \code{\link{spp}},
 \code{\link{kfun}},
 \code{\link{k12fun}},
 \code{\link{kijfun}},
 \code{\link{ki.fun}}.
}
\examples{
  data(BPoirier)
  BP<-BPoirier
  # spatial point pattern in a rectangle sampling window of size [0,110] x [0,90]
  swmr <- spp(BP$trees, win=BP$rect, marks=BP$species)
  kswmr <- ksfun(swmr, 25, 1, 500)
  plot(kswmr)

  # spatial point pattern in a circle with radius 50 centred on (55,45)
  swmc <- spp(BP$trees, win=c(55,45,45), marks=BP$species)
  kswmc <- kfun(swmc, 25, 1, 500)
  plot(kswmc)
  
  # spatial point pattern in a complex sampling window
  swrt <- spp(BP$trees, win=BP$rect, tri=BP$tri2, marks=BP$species)
  kswrt <- kfun(swrt, 25, 1, 500)
  plot(kswrt)
}
\keyword{spatial}