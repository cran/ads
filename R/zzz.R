.First.lib <- function(lib,pkg)
{   # require(spatstat,quietly=TRUE)
	library.dynam("ads", pkg, lib)
	x <- read.dcf(file = system.file("DESCRIPTION", package = "ads"))
	cat("\n")
	write.dcf(x)
}
