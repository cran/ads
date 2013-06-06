\encoding{latin1}
\name{kijfun}
\alias{kijfun}
\title{Multiscale second-order neigbourhood analysis of a multivariate spatial point pattern}
\description{
  Computes a set of \emph{K}- and \emph{K12}-functions for all possible pairs of marks \eqn{(i,j)} in a multivariate spatial 
  point pattern defined in a simple (rectangular or circular) 
  or complex sampling window (see Details).
}
\usage{
 kijfun(p, upto, by)
}
\arguments{
  \item{p    }{a \code{"spp"} object defining a multivariate spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by   }{interval length between successive sample circles radii (see Details).}
}
\details{
  Function \code{kijfun} is simply a wrapper to \code{\link{kfun}} and \code{\link{k12fun}}, which computes either \emph{K(r)} 
  for points of mark \eqn{i} when \eqn{i=j} or \emph{K12(r)} between the marks \eqn{i} and \eqn{j} otherwise.
}
\value{
A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced distances (\code{seq(by,upto,by)}).}
  \item{labij }{a vector containing the \eqn{(i,j)} paired levels of \code{p$marks}.}
  \item{gij }{a data frame containing values of the pair density functions \eqn{g(r)} and \eqn{g12(r)}.}
  \item{nij }{a data frame containing values of the local neighbour density functions \eqn{n(r)} and \eqn{n12(r)}.}
  \item{kij }{a data frame containing values of the \eqn{K(r)} and \eqn{K12(r)} functions.}
\item{lij }{a data frame containing values of the modified \eqn{L(r)} and \eqn{L12(r)} functions.\cr\cr}
 \item{ }{Each component except \code{r} is a data frame with the following variables:\cr}
 \item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected under the null hypotheses of spatial randomness (see \code{\link{kfun}}) and
  population independence (see \code{\link{kijfun}}).}
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
  \code{\link{ki.fun}}.
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  # multivariate spatial point pattern in a rectangle sampling window 
  swrm <- spp(BP$trees, win=BP$rect, marks=BP$species)
  kijswrm <- kijfun(swrm, 25, 1)
  plot(kijswrm)
  
 # multivariate spatial point pattern in a circle with radius 50 centred on (55,45)
  swcm <- spp(BP$trees, win=c(55,45,45), marks=BP$species)
  kijswcm <- kijfun(swcm, 25, 1)
  plot(kijswcm)
  
  # multivariate spatial point pattern in a complex sampling window
  swrtm <- spp(BP$trees, win=BP$rect, tri=BP$tri2, marks=BP$species)
  kijswrtm <- kijfun(swrtm, 25, 1)
  plot(kijswrtm)

}
\keyword{spatial}
