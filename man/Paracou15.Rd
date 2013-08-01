\encoding{latin1}
\name{Paracou15}
\alias{Paracou15}
\docType{data}
\title{Tree spatial pattern in control plot 15, Paracou experimental station, French Guiana}
\description{
Spatial pattern of 4128 trees of 332 diffrent species in a 250 m X 250 m control plot in Paracou experimental station, French Guiana. 
}
\usage{data(Paracou15)}
\format{
A list with 5 components:\cr
\code{$rect     } is a vector of coordinates \eqn{(xmin,ymin,xmax,ymax)} of the origin and the opposite corner of a 250 by 250 m rectangular plot.\cr
\code{$trees    } is a list of tree coordinates \eqn{(x,y)}.\cr
\code{$species  } is a factor with 332 levels corresponding to species names of the trees.\cr
\code{$FuncDist } is an object of class \code{"dist"} giving between-species distances based on functional traits (see Paine et al. 2011).\cr
\code{$PhyloDist} is an object of class \code{"dist"} giving between-species phylogentic distances based on APG III system (APG 2009).\cr
}
\source{
Gourlet-Fleury, S., Ferry, B., Molino, J.-F., Petronelli, P. & Schmitt, L. 2004. \emph{Exeprimental plots: key features.} Pp. 3-60 In Gourlet-Fleury, S., Guehl, J.-M. & Laroussinie, O. (Eds.), Ecology and Managament of a Neotropical rainforest - Lessons drawn from Paracou, a long-term experimental research site in French Guiana. Elsevier SAS, France.
}

\references{
Paine, C. E. T., Baraloto, C., Chave, J. & Hérault, B. 2011. Functional traits of individual trees reveal ecological constraints on community assembly in tropical rain forests. \emph{Oikos}, 120: 720-727.\cr\cr
Angiosperm Phylogeny Group 2009. An update of the Angiosperm Phylogeny Group classification for the orders and families of flowering plants: APG III. \emph{Botanical Journal of the Linnean Society}, 161: 105-121.
}
\examples{
data(Paracou15)
P15.spp <- spp(Paracou15$trees, mark = Paracou15$species, window = Paracou15$rect)
plot(P15.spp, chars = rep("o", 332), cols = rainbow(332), legend = FALSE, maxsize = 0.5)
}
\keyword{dataset}
