print.fads<-function(x,...) {
	UseMethod("print.fads")
}

print.fads.kfun<-function(x,...) {
	cat("Univariate second-order neighbourhood functions:\n")
	str(x)
#	if(!is.null(x$call[["MC"]])) {
#		lev<-x$call[["lev"]]
#		p<-ifelse(!is.null(lev),signif(100*(1-lev),digit=6),99)
#		cat(paste("with",p,"% confidence intervals and local p-values of complete spatial randomness\n"))
#		}
#	cat("class: ",class(x),"\n")
#	cat("call: ")
#	print(x$call)
#	cat("\n")
#	sumry<-array("",c(1,4),list(1:1,c("vector","length","mode","content")))
#	sumry[1,] <- c("$r", length(x$r),mode(x$r), "distance (r)")
#	class(sumry) <- "table"
#	print(sumry)
#	cat("\n")	
#	sumry<-array("",c(4,4),list(1:4,c("data.frame","nrow","ncol","content")))
#	sumry[1,] <- c("$g", nrow(x$g), ncol(x$g), "pair density function g(r)")
#	sumry[2,] <- c("$n", nrow(x$n), ncol(x$n), "local neighbour density function n(r)")
#	sumry[3,] <- c("$k",nrow(x$k),ncol(x$k), "Ripley's function K(r)")
#	sumry[4,] <- c("$l",nrow(x$l),ncol(x$l), "modified Ripley's function L(r)")
#	class(sumry) <- "table"
#   print(sumry)	
}

print.fads.k12fun<-function(x,...) {
	#verifyclass(x,"k12func")
	cat("Bivariate second-order neighbourhood functions:\n")
	str(x)
	#ifelse(!is.null(x$call[["MC"]]),ci<-TRUE,ci<-FALSE)
	#if(ci) {
	#	lev<-x$call[["lev"]]
	#	p<-ifelse(!is.null(lev),signif(100*(1-lev),digit=6),99)
	#	cat(paste("with",p,"% confidence intervals")) 
	#	cat(" and local p-values of ")
	#	if(is.null(x$call[["H0"]])||(x$call[["H0"]]=="popindep"))
	#		cat("population independence hypothesis\n")
	#	else
	#		cat("random labelling hypothesis\n")
	#}
	#cat("class: ",class(x),"\n")
	#cat("call: ")
	#print(x$call)
	#cat("mark1: ",x$marks[1],"\n")
	#cat("mark2: ",x$marks[2],"\n")
	#cat("\n")
	#if(!ci) {
	#	sumry<-array("",c(5,4),list(1:5,c("vector","length","mode","content")))
	#	sumry[1,] <- c("$r", length(x$r),mode(x$r), "distance (r)")
	#	sumry[2,] <- c("$g12", length(x$g12),mode(x$g12), "pair density function g12(r)")
	#	sumry[3,] <- c("$n12", length(x$n12),mode(x$n12), "local neighbour density function n12(r)")
	#	sumry[4,] <- c("$k12", length(x$k12),mode(x$k12), "intertype function K12(r)")
	#	sumry[5,] <- c("$l12", length(x$l12),mode(x$l12), "modified intertype function L12(r)")
	#}
	#else {
	#	sumry<-array("",c(1,4),list(1:1,c("vector","length","mode","content")))
	#	sumry[1,] <- c("$r", length(x$r),mode(x$r), "distance (r)")
	#	class(sumry) <- "table"
	#	print(sumry)
	#	cat("\n")	
	#	sumry<-array("",c(4,4),list(1:4,c("data.frame","nrow","ncol","content")))
	#	sumry[1,] <- c("$g12", nrow(x$g12), ncol(x$g12), "pair density function g12(r)")
	#	sumry[2,] <- c("$n12", nrow(x$n12), ncol(x$n12), "local neighbour density function n12(r)")
	#	sumry[3,] <- c("$k12",nrow(x$k12),ncol(x$k12), "intertype function K12(r)")
	#	sumry[4,] <- c("$l12",nrow(x$l12),ncol(x$l12), "modified intertype function L12(r)")
	#}
	#class(sumry) <- "table"
    #print(sumry)	
}

print.fads.kijfun<-function(x,...) {
	#verifyclass(x,"kijfun")
	cat("Multivariate second-order neighbourhood functions :\n")
	cat("Interaction between each category i and each category j\n")
	str(x)
	#cat("class: ",class(x),"\n")
    #cat("call: ")
	#print(x$call)
	#cat("\n")
	#sumry<-array("",c(2,4),list(1:2,c("vector","length","mode","content")))
	#sumry[1,] <- c("$r", length(x$r),mode(x$r), "distance (r)")
	#sumry[2,] <- c("$labij", length(x$labij), mode(x$labij), "i-j labels for combinations of categories")
	#class(sumry) <- "table"
	#print(sumry)
	#cat("\n")	
	#sumry<-array("",c(4,4),list(1:4,c("matrix","nrow","ncol","content")))
	#sumry[1, ] <- c("$gij", nrow(x$gij), ncol(x$gij), "pair density functions gij(r)")
	#sumry[2, ] <- c("$nij", nrow(x$nij), ncol(x$nij), "local neighbour density functions nij(r)")
	#sumry[3, ] <- c("$kij", nrow(x$kij), ncol(x$kij), "Ripley's and intertype functions Kij(r)")
	#sumry[4, ] <- c("$lij", nrow(x$lij), ncol(x$lij), "modified Ripley's and intertype functions Lij(r)")
	#class(sumry) <- "table"
    #print(sumry)
}

print.fads.ki.fun<-function(x,...) {
	#verifyclass(x,"kisfun")
	cat("Multivariate second-order neighbourhood functions:\n")
	cat("Interaction between each category i and all the remaining categories.\n")
	str(x)
	#cat("class: ",class(x),"\n")
    #cat("call: ")
	#print(x$call)
	#cat("\n")    
    #sumry<-array("",c(2,4),list(1:2,c("vector","length","mode","content")))
	#sumry[1,] <- c("$r", length(x$r),mode(x$r), "distance (r)")
	#sumry[2, ] <- c("$labi", length(x$labi), mode(x$r), "i labels for categories")
	#class(sumry) <- "table"
	#print(sumry)
	#cat("\n")	
	#sumry<-array("",c(4,4),list(1:4,c("data.frame","nrow","ncol","content")))	
	#sumry[1, ] <- c("$nis", nrow(x$nis), ncol(x$nis), "local neighbour density functions nis(r)")
	#sumry[2, ] <- c("$gis", nrow(x$gis), ncol(x$gis), "pair density functions gis(r)")	
	#sumry[3, ] <- c("$kis", nrow(x$kis), ncol(x$kis), "intertype functions Kis(r)")
	#sumry[4, ] <- c("$lis", nrow(x$lis), ncol(x$lis), "modified intertype functions Lis(r)")
	#class(sumry) <- "table"
    #print(sumry)
}

print.fads.kmfun<-function(x,...) {
	#verifyclass(x,"kisfun")
	cat("Mark correlation functions:\n")
	str(x)
	#cat("class: ",class(x),"\n")
    #cat("call: ")
	#print(x$call)
	#cat("\n")    
    #sumry<-array("",c(2,4),list(1:2,c("vector","length","mode","content")))
	#sumry[1,] <- c("$r", length(x$r),mode(x$r), "distance (r)")
	#sumry[2, ] <- c("$labi", length(x$labi), mode(x$r), "i labels for categories")
	#class(sumry) <- "table"
	#print(sumry)
	#cat("\n")	
	#sumry<-array("",c(4,4),list(1:4,c("data.frame","nrow","ncol","content")))	
	#sumry[1, ] <- c("$nis", nrow(x$nis), ncol(x$nis), "local neighbour density functions nis(r)")
	#sumry[2, ] <- c("$gis", nrow(x$gis), ncol(x$gis), "pair density functions gis(r)")	
	#sumry[3, ] <- c("$kis", nrow(x$kis), ncol(x$kis), "intertype functions Kis(r)")
	#sumry[4, ] <- c("$lis", nrow(x$lis), ncol(x$lis), "modified intertype functions Lis(r)")
	#class(sumry) <- "table"
    #print(sumry)
}
