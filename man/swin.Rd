\encoding{latin1}
\name{swin}
\alias{swin}
\alias{print.swin}
\alias{summary.swin}
\alias{print.summary.swin}
\alias{plot.swin}
\alias{swin2owin}
\title{Creating a sampling window}
\description{
  Function \code{swin} creates an object of class \code{"swin"}, which represents 
  the sampling window (or study region) in which a spatial point pattern was
  observed. The \code{ads} library supports simple (rectangular or circular) and complex
  sampling windows.   
}
\synopsis{
 swin(window, triangles)
}
\usage{
 swin(window=c(xmin,ymin,xmax,ymax))
 swin(window=c(x0,y0,r0))
 swin(window, triangles)
}
\arguments{
  \item{window}{a vector defining the limits of a simple sampling window: \code{c(xmin,ymin,xmax,ymax)}
   for a rectangle ; \code{c(x0,y0,r0)} for a circle.}
  \item{triangles}{(optional) a list of triangles removed from a simple initial window to define a complex 
  sampling window (see Details).}
}
\details{
A sampling window may be of simple or complex type. A simple sampling window may be a rectangle or a circle.
	A complex sampling window is defined by removing triangular surfaces from a simple (rectangular or circular)
	initial sampling window.
  \itemize{
     \item
	\bold{rectangular window:}
    \code{window=c(ximn,ymin,xmax,ymax)} a vector of length 4 giving the coordinates \eqn{(ximn,ymin)} and \eqn{(xmax,ymax)}
	of the origin and the opposite corner of a rectangle.
	\item
	\bold{circular window:}
    \code{window=c(x0,y0,r0)} a vector of length 3 giving the coordinates \eqn{(x0,y0)}
	of the centre and the radius \eqn{r0} of a circle.
	\item
	\bold{complex window:}
    \code{triangles} is a list of 6 variables giving the vertices coordinates \eqn{(ax,ay,bx,by,cx,cy)}
	of the triangles to remove from a simple (rectangular or circular) initial window. The triangles may be removed 
	near the boundary of a rectangular window in order to design a polygonal sampling window, or within a rectangle
	or a circle, to delineating holes in the initial sampling window (see Examples). The triangles do not overlap each other, nor overlap boundary
	of the initial sampling window. Any polygon (possibly with holes) can be decomposed into contiguous triangles using \code{\link{triangulate}}.
  }
}
\value{
 An object of class \code{"swin"} describing the sampling window. It may be of four different types
 with different arguments:
 \item{\code{$type=c("simple","rectangle"),$xmin,$ymin,$xmax,$ymax}}
 \item{\code{$type=c("simple","circle"),$x0,$y0,$r0}}
 \item{\code{$type=c("complex","rectangle"),$xmin,$ymin,$xmax,$ymax,$triangles}}
 \item{\code{$type=c("complex","circle"),$x0,$y0,$r0,$triangles}}
}
\references{
 Goreaud, F. and Pélissier, R. 1999. On explicit formula of edge effect correction for Ripley's \emph{K}-function. \emph{Journal of Vegetation Science}, 10:433-438.
}
\note{
There are printing, summary and plotting methods for \code{"swin"} objects.\cr
Function \code{swin2owin(swin.object)} coerces an object of class \code{"swin"} into an \code{\link[spatstat]{owin.object}} suitable for use with package \code{spatstat}.
}
\seealso{
  \code{\link{area.swin}},
  \code{\link{inside.swin}},
  \code{\link{swin2owin}},
  \code{\link{spp}}
  }
\author{
 \email{Raphael.Pelissier@ird.fr}
}
\examples{
  #rectangle of size [0,110] x [0,90]
  wr<-swin(c(0,0,110,90))
  summary(wr)
  plot(wr)
  
  #circle with radius 50 centred on (55,45)
  wc<-swin(c(55,45,50))
  summary(wc)
  plot(wc)
  
 # polygon (diamond shape)
 t1<-c(0,0,55,0,0,45)
 t2<-c(55,0,110,0,110,45)
 t3<-c(0,45,0,90,55,90)
 t4<-c(55,90,110,90,110,45)
 wp<-swin(wr,rbind(t1,t2,t3,t4))
 summary(wp)
 plot(wp)
 
 #rectangle with a hole
 h1<-c(25,45,55,75,85,45)
 h2<-c(25,45,55,15,85,45)
 wrh<-swin(wr,rbind(h1,h2))
 summary(wrh)
 plot(wrh)

 #circle with a hole
 wch<-swin(wc,rbind(h1,h2))
 summary(wch)
 plot(wch)
}
\keyword{spatial}