\name{ki.fun}
\alias{ki.fun}
\title{ Multiscale second-order neigbourhood analysis of a multivariate spatial point pattern}
\description{
Computes a set of \emph{K12}-functions between all possible marks \eqn{i} and the other marks in
 a multivariate spatial point pattern defined in a simple (rectangular or circular) 
  or complex sampling window (see Details).
}
\usage{
ki.fun(p, upto, by)
}
\arguments{
  \item{p    }{a \code{"spp"} object defining a multivariate spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by   }{interval length between successive sample circles radii (see Details).}
}
\details{
   Function \code{ki.fun} is simply a wrapper to \code{\link{k12fun}}, which computes \emph{K12(r)} between each mark \eqn{i} of the pattern
   and all other marks grouped together (the \eqn{j} points).
}
\value{
  A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced distances (\code{seq(by,upto,by)}).}
  \item{labi }{a vector containing the levels \eqn{i} of \code{p$marks}.}
  \item{gi.  }{a data frame containing values of the pair density function \eqn{g12(r)}.}
  \item{ni.  }{a data frame containing values of the local neighbour density function \eqn{n12(r)}.}
  \item{ki.  }{a data frame containing values of the \eqn{K12(r)} function.}
  \item{li.  }{a data frame containing values of the modified \eqn{L12(r)} function.\cr\cr}
 \item{ }{Each component except \code{r} is a data frame with the following variables:\cr\cr}
 \item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected under the null hypothesis of population independence (see \code{\link{k12fun}}).}
}
\author{\email{Raphael.Pelissier@mpl.ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
\seealso{
\code{\link{plot.fads}},
  \code{\link{spp}},
  \code{\link{kfun}},
  \code{\link{k12fun}},
  \code{\link{kijfun}}.
}
\examples{
  data(BPoirier)
  BP<-BPoirier
  # multivariate spatial point pattern in a rectangle sampling window 
  swrm<-spp(BP$trees,win=BP$rect,marks=BP$species)
  ki.swrm<-ki.fun(swrm,25,1)
  plot(ki.swrm)
  
 # multivariate spatial point pattern in a circle with radius 50 centred on (55,45)
 swcm<-spp(BP$trees,win=c(55,45,45),marks=BP$species)
  ki.swcm<-ki.fun(swcm,25,1)
  plot(ki.swcm)
  
  # multivariate spatial point pattern in a complex sampling window
  swrtm<-spp(BP$trees,win=BP$rect,tri=BP$tri2,marks=BP$species)
  ki.swrtm<-ki.fun(swrtm,25,1)
  plot(ki.swrtm)
}
\keyword{spatial}
