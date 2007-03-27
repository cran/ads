plot.fads<-function (x,opt,cols,lty,main,sub,legend,csize,...) {
	UseMethod("plot.fads")
}

plot.fads.kfun<-function (x,opt=c("all","L","K","n","g"),cols,lty,main,sub,legend=TRUE,csize=1,...) {
	ifelse(!is.null(x$call$nsim)&&(x$call$nsim>0),ci<-TRUE,ci<-FALSE)
	def.par <- par(no.readonly = TRUE)
	on.exit(par(def.par))
	if(options()$device=="windows")
		csize<-0.75*csize
	opt<-opt[1]
	if(opt=="all")
		mylayout<-layout(matrix(c(1,1,1,1,2,2,3,3,2,2,3,3,4,4,5,5,4,4,5,5),ncol=4,byrow=TRUE))
	else if(opt%in%c("L","K","n","g"))
		mylayout<-layout(matrix(c(1,1,1,1,rep(2,16)),ncol=4,byrow=TRUE))
	else
		stopifnot(opt%in%c("all","L","K","n","g"))
	if(missing(cols))
		cols=c(1,2,3)
	else if(length(cols)!=3)
		cols=c(cols,cols,cols)
	if(missing(lty))
			lty=c(1,3,2)
	else if(length(lty)!=3)
		lty=c(lty,lty,lty)
	if(missing(main))
		main<-deparse(x$call,width.cutoff=100)
	if(missing(sub))
		sub<-c("pair density function","second-order neighbour density function","Ripley's K-function","L-function : sqrt[K(r)/pi]-r")
	if(ci) {
		alpha<-x$call[["alpha"]]
		p<-ifelse(!is.null(alpha),signif(100*(1-alpha),digit=6),99)
		par(mar=c(0.1,0.1,0.1,0.1),cex=csize)
		plot(x$r,x$g$obs/2,type="n",axes=FALSE,xlab="",ylab="")
		if(legend)
			legend("center",c("obs","theo (CSR)",paste(p,"% CI of CSR")),cex=1.5,lty=lty[1:3],bty="n",horiz=TRUE,title=main,col=cols[1:3],...)
		else
			legend("center","",cex=1.5,bty="n",horiz=TRUE,title=main,...)
		par(mar=c(5,5,0.1,2),cex=ifelse(opt%in%c("all"),0.75*csize,csize))
		if(opt%in%c("all","g")) { # g-function
			lim<-range(x$g[,1:4])
			plot(x$r,x$g$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[1]),type="n",xlab="distance (r)",ylab="g(r)",cex.lab=1.25,...)
			lines(x$r,x$g$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$g$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$g$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$g$inf,lty=lty[3],col=cols[3],...)	
		}
		if(opt%in%c("all","n")) {# n-function
			lim<-range(x$n[,1:4])
			plot(x$r,x$n$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[2]),type="n",xlab="distance (r)",ylab="n(r)",cex.lab=1.25,...)
			lines(x$r,x$n$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$n$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$n$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$n$inf,lty=lty[3],col=cols[3],...)
		}
		if(opt%in%c("all","K")) { # K-function
			plot(x$r,x$k$obs,ylim=range(x$k[,1:4]),main=paste("\n\n",sub[3]),type="n",xlab="distance (r)",ylab="K(r)",cex.lab=1.25,...)
			lines(x$r,x$k$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$k$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$k$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$k$inf,lty=lty[3],col=cols[3],...)
		}
		if(opt%in%c("all","L")) { # L-function
			lim<-range(x$l[,1:4])
			plot(x$r,x$l$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[4]),type="n",xlab="distance (r)",ylab="L(r)",cex.lab=1.25,...)
			lines(x$r,x$l$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$l$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$l$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$l$inf,lty=lty[3],col=cols[3],...)
		}
	}
	else {
		par(mar=c(0.1,0.1,0.1,0.1),cex=csize)
		plot(x$r,x$g$obs/2,type="n",axes=FALSE,xlab="",ylab="")
		if(legend)
			legend("center",c("obs","theo (CSR)"),cex=1.5,lty=lty[1:2],bty="n",horiz=TRUE,title=main,col=cols[1:2],...)
		else
			legend("center","",cex=1.5,bty="n",horiz=TRUE,title=main,...)
		par(mar=c(5,5,0.1,2),cex=ifelse(opt%in%c("all"),0.75*csize,csize))
		if(opt%in%c("all","g")) { # g-function
			lim<-range(x$g)
			plot(x$r,x$g$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[1]),type="n",xlab="distance (r)",ylab="g(r)",cex.lab=1.25,...)
			lines(x$r,x$g$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$g$theo,lty=lty[2],col=cols[2],...)
		}
		if(opt%in%c("all","n")) { # n-function
			lim<-range(x$n)
			plot(x$r,x$n$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[2]),type="n",xlab="distance (r)",ylab="n(r)",cex.lab=1.25,...)
			lines(x$r,x$n$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$n$theo,lty=lty[2],col=cols[2],...)
		}
		if(opt%in%c("all","K")) { # k-function
			plot(x$r,x$k$obs,ylim=range(x$k),main=paste("\n\n",sub[3]),type="n",xlab="distance (r)",ylab="K(r)",cex.lab=1.25,...)
			lines(x$r,x$k$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$k$theo,lty=lty[2],col=cols[2],...)
		}
		if(opt%in%c("all","L")) { # L-function
			lim<-range(x$l)
			plot(x$r,x$l$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[4]),type="n",xlab="distance (r)",ylab="L(r)",cex.lab=1.25,...)
			lines(x$r,x$l$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$l$theo,lty=lty[2],col=cols[2],...)
		}
	}	
}

plot.fads.k12fun<-function(x,opt=c("all","L","K","n","g"),cols,lty,main,sub,legend=TRUE,csize=1,...) {
	#ifelse(!is.null(x$call[["nsim"]]),ci<-TRUE,ci<-FALSE)
	ifelse(!is.null(x$call$nsim)&&(x$call$nsim>0),ci<-TRUE,ci<-FALSE)
	ifelse((is.null(x$call[["H0"]])||(x$call[["H0"]]=="pi")),h0<-"PI",h0<-"RL")
	def.par <- par(no.readonly = TRUE)
	on.exit(par(def.par))
	if(options()$device=="windows")
		csize<-0.75*csize
	#par(cex=csize)
	opt<-opt[1]
	if(opt=="all")
		mylayout<-layout(matrix(c(1,1,1,1,2,2,3,3,2,2,3,3,4,4,5,5,4,4,5,5),ncol=4,byrow=TRUE))
	else if(opt%in%c("L","K","n","g"))
		mylayout<-layout(matrix(c(1,1,1,1,rep(2,16)),ncol=4,byrow=TRUE))
	else
		stopifnot(opt%in%c("all","L","K","n","g"))
	if(missing(cols))
		cols=c(1,2,3)
	else if(length(cols)!=3)
		cols=c(cols,cols,cols)
	if(missing(lty))
			lty=c(1,3,2)
	else if(length(lty)!=3)
		lty=c(lty,lty,lty)
	if(missing(main))
		main<-deparse(x$call,width.cutoff=100)		
	if(missing(sub))
		sub<-c("pair density function","second-order neighbour density function","intertype function","modified intertype function : sqrt[K12(r)/pi]-r")
	if(ci) {
		alpha<-x$call[["alpha"]]
		p<-ifelse(!is.null(alpha),signif(100*(1-alpha),digit=6),99)
		par(mar=c(0.1,0.1,0.1,0.1),cex=csize)
		#ifelse((is.null(x$call[["H0"]])||(x$call[["H0"]]=="pi")),h0<-"PI",h0<-"RL")
		plot(x$r,x$g12$obs/2,type="n",axes=FALSE,xlab="",ylab="")
		if(legend)
			legend("center",c("obs",paste("theo (",h0,")",sep=""),paste(p,"% CI of",h0)),cex=1.5,lty=lty[1:3],bty="n",horiz=TRUE,title=main,col=cols[1:3],...)
		else
			legend("center","",cex=1.5,bty="n",horiz=TRUE,title=main,...)
		par(mar=c(5,5,0.1,2),cex=ifelse(opt%in%c("all"),0.75*csize,csize))
		if(opt%in%c("all","g")) { # g12-function
			lim<-range(x$g12[,1:4])
			plot(x$r,x$g12$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[1]),type="n",xlab="distance (r)",ylab="g12(r)",cex.lab=1.25)
			lines(x$r,x$g12$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$g12$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$g12$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$g12$inf,lty=lty[3],col=cols[3],...)
		}
		if(opt%in%c("all","n")) { # n12-function
			lim<-range(x$n12[,1:4])
			plot(x$r,x$n12$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[2]),type="n",xlab="distance (r)",ylab="n12(r)",cex.lab=1.25)
			lines(x$r,x$n12$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$n12$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$n12$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$n12$inf,lty=lty[3],col=cols[3],...)
		}
		if(opt%in%c("all","K")) { # K-function
			plot(x$r,x$k12$obs,ylim=range(x$k12[,1:4]),main=paste("\n\n",sub[3]),type="n",xlab="distance (r)",ylab="K12(r)",cex.lab=1.25)
			lines(x$r,x$k12$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$k12$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$k12$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$k12$inf,lty=lty[3],col=cols[3],...)
		}
		if(opt%in%c("all","L")) { # L-function
			lim<-range(x$l12[,1:4])
			plot(x$r,x$l12$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[4]),type="n",xlab="distance (r)",ylab="L12(r)",cex.lab=1.25)
			lines(x$r,x$l12$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$l12$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$l12$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$l12$inf,lty=lty[3],col=cols[3],...)
		}
	}
	else {
		par(mar=c(0.1,0.1,0.1,0.1),cex=csize)
		plot(x$r,x$g12$obs/2,type="n",axes=FALSE,xlab="",ylab="")
		if(legend)
			legend("center",c("obs",paste("theo (",h0,")",sep="")),cex=1.5,lty=lty[1:2],bty="n",horiz=TRUE,title=main,col=cols[1:2],...)
		else
			legend("center","",cex=1.5,bty="n",horiz=TRUE,title=main,...)
		par(mar=c(5,5,0.1,2),cex=ifelse(opt%in%c("all"),0.75*csize,csize))
		if(opt%in%c("all","g")) { # g-function
			lim<-range(x$g12)
			plot(x$r,x$g12$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[1]),type="n",xlab="distance step (r)",ylab="g12(r)",cex.lab=1.25,...)
			lines(x$r,x$g12$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$g12$theo,lty=lty[2],col=cols[2],...)
		}
		if(opt%in%c("all","n")) { # n-function
			lim<-range(x$n12)
			plot(x$r,x$n12$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[2]),type="n",xlab="distance step (r)",ylab="n12(r)",cex.lab=1.25,...)
			lines(x$r,x$n12$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$n12$theo,lty=lty[2],col=cols[2],...)
		}
		if(opt%in%c("all","K")) { # k-function
			plot(x$r,x$k12$obs,ylim=range(x$k),main=paste("\n\n",sub[3]),type="n",xlab="distance step (r)",ylab="K12(r)",cex.lab=1.25,...)
			lines(x$r,x$k12$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$k12$theo,lty=lty[2],col=cols[2],...)
		}
		if(opt%in%c("all","L")) { # L-function
			lim<-range(x$l12)
			plot(x$r,x$l12$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[4]),type="n",xlab="distance step (r)",ylab="L12(r)",cex.lab=1.25,...)
			lines(x$r,x$l12$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$l12$theo,lty=lty[2],col=cols[2],...)
		}
	}	
}

plot.fads.kijfun<-function (x,opt=c("L","K","n","g"),cols,lty,main,sub,legend=TRUE,csize=1,...) {
	na<-length(x$labij)
	nf<-ceiling(sqrt(na))
	def.par <- par(no.readonly = TRUE)
	on.exit(par(def.par))
	if(options()$device=="windows")
		csize<-0.75*csize
	#par(cex=csize)
	mylayout<-layout(matrix(c(rep(1,nf),seq(2,((nf*nf)+1),1)),(nf+1),nf,byrow=TRUE))
	opt<-opt[1]
	if(opt=="g") {
		val<-x$gij
		theo<-matrix(rep(rep(1,na),each=length(x$r)),ncol=na)
		ylab=paste("gij(r)",sep="")
	}
	if(opt=="n") {
		val<-x$nij
		theo<-matrix(rep(rep(x$intensity,nf),each=length(x$r)),ncol=na)
		ylab=paste("nij(r)",sep="")
	}
	if(opt=="K") {
		val<-x$kij
		theo<-matrix(rep(pi*x$r^2,na),ncol=na)
		ylab=paste("Kij(r)",sep="")
	}
	if(opt=="L") {
		val<-x$lij
		theo<-matrix(rep(rep(0,na),each=length(x$r)),ncol=na)
		ylab=paste("Lij(r)",sep="")
	}
	if(missing(cols))
		cols=c(1,2)
	else if(length(cols)!=2)
		cols=c(cols,cols)
	if(missing(lty))
			lty=c(1,3)
	else if(length(lty)!=2)
		lty=c(lty,lty)
	if(missing(main))
		main<-deparse(x$call,width.cutoff=100)
	if(missing(sub))
		sub<-x$labij
	lim<-range(val)
	par(mar=c(0.1,0.1,0.1,0.1),cex=csize)
	plot.default(val[,1],val[,2]/2,type="n",axes=FALSE,xlab="",ylab="")
	if(legend)
		legend("center",c("obs","theo (CSR/PI)"),cex=1.5,lty=lty[1:2],bty="n",horiz=TRUE,title=main,col=cols[1:2],...)
	else
			legend("center","",cex=1.5,bty="n",horiz=TRUE,title=main,...)
	par(mar=c(5,5,0.1,2),cex=0.66*csize)
	for(i in 1:na) {
		plot(x$r,val[,i],ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[i]),type="n",xlab="distance (r)",ylab=ylab,cex.lab=1.25,...)
		lines(x$r,val[,i],lty=lty[1],col=cols[1],...)
		lines(x$r,theo[,i],lty=lty[2],col=cols[2],...)
	}	
}

plot.fads.ki.fun<-function (x,opt=c("L","K","n","g"),cols,lty,main,sub,legend=TRUE,csize=1,...) {
	na<-length(x$labi)
	nf<-ceiling(sqrt(na))
	def.par <- par(no.readonly = TRUE)
	on.exit(par(def.par))
	if(options()$device=="windows")
		csize<-0.75*csize
	#par(cex=csize)
	mylayout<-layout(matrix(c(rep(1,nf),seq(2,((nf*nf)+1),1)),(nf+1),nf,byrow=TRUE))
	opt<-opt[1]
	if(opt=="g") {
		val<-x$gi.
		theo<-matrix(rep(rep(1,na),each=length(x$r)),ncol=na)
		ylab=paste("gi.(r)",sep="")
	}
	if(opt=="n") {
		val<-x$ni.
		intensity<-sum(x$intensity)-x$intensity
		theo<-matrix(rep(intensity,each=length(x$r)),ncol=na)
		ylab=paste("ni.(r)",sep="")
	}
	if(opt=="K") {
		val<-x$ki.
		theo<-matrix(rep(pi*x$r^2,na),ncol=na)
		ylab=paste("Ki.(r)",sep="")
	}
	if(opt=="L") {
		val<-x$li.
		theo<-matrix(rep(rep(0,na),each=length(x$r)),ncol=na)
		ylab=paste("Li.(r)",sep="")
	}
	if(missing(cols))
		cols=c(1,2)
	else if(length(cols)!=2)
		cols=c(cols,cols)
	if(missing(lty))
			lty=c(1,3)
	else if(length(lty)!=2)
		lty=c(lty,lty)
	if(missing(main))
		main<-deparse(x$call,width.cutoff=100)
	if(missing(sub))
		sub<-paste(x$labi,"-all others",sep="")
	lim<-range(val)
	par(mar=c(0.1,0.1,0.1,0.1),cex=csize)
	plot.default(val[,1],val[,2]/2,type="n",axes=FALSE,xlab="",ylab="")
	if(legend)
		legend("center",c("obs","theo (PI)"),cex=1.5,lty=lty[1:2],bty="n",horiz=TRUE,title=main,col=cols[1:2],...)
	else
		legend("center","",cex=1.5,bty="n",horiz=TRUE,title=main,...)
	par(mar=c(5,5,0.1,2),cex=0.66*csize)
	for(i in 1:na) {
		plot(x$r,val[,i],ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[i]),type="n",xlab="distance (r)",ylab=ylab,cex.lab=1.25,...)
		lines(x$r,val[,i],lty=lty[1],col=cols[1],...)
		lines(x$r,theo[,i],lty=lty[2],col=cols[2],...)
	}	
}

plot.fads.kmfun<-function (x,opt=c("all","K","g"),cols,lty,main,sub,legend=TRUE,csize=1,...) {
	#ifelse(!is.null(x$call[["nsim"]]),ci<-TRUE,ci<-FALSE)
	ifelse(!is.null(x$call$nsim)&&(x$call$nsim>0),ci<-TRUE,ci<-FALSE)
	def.par <- par(no.readonly = TRUE)
	on.exit(par(def.par))
	if(options()$device=="windows")
		csize<-0.75*csize
	#par(cex=csize)
	opt<-opt[1]
	if(opt=="all")
		mylayout<-layout(matrix(c(1,1,1,1,rep(2,8),rep(3,8)),ncol=4,byrow=TRUE))
	else if(opt%in%c("K","g"))
		mylayout<-layout(matrix(c(1,1,1,1,rep(2,16)),ncol=4,byrow=TRUE))
	else
		stopifnot(opt%in%c("all","K","g"))
	if(missing(cols))
		cols=c(1,2,3)
	else if(length(cols)!=3)
		cols=c(cols,cols,cols)
	if(missing(lty))
			lty=c(1,3,2)
	else if(length(lty)!=3)
		lty=c(lty,lty,lty)
	if(missing(main))
		main<-deparse(x$call,width.cutoff=100)
	if(missing(sub))
		sub<-c("pair correlation function","mark correlation function")
	if(ci) {
		alpha<-x$call[["alpha"]]
		p<-ifelse(!is.null(alpha),signif(100*(1-alpha),digit=6),99)
		par(mar=c(0.1,0.1,0.1,0.1),cex=csize)
		plot(x$r,x$gm$obs/2,type="n",axes=FALSE,xlab="",ylab="")
		if(legend)
			legend("center",c("obs","theo (No Correlation)",paste(p,"% CI of NC")),cex=1.5,lty=lty[1:3],bty="n",horiz=TRUE,title=main,col=cols[1:3],...)
		else
			legend("center","",cex=1.5,bty="n",horiz=TRUE,title=main,...)
		par(mar=c(5,5,0.1,2),cex=ifelse(opt%in%c("all"),0.85*csize,csize))
		if(opt%in%c("all","g")) { # gm-function
			lim<-range(x$gm[,1:4])
			plot(x$r,x$gm$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[1]),type="n",xlab="distance (r)",ylab="gm(r)",cex.lab=1.25,...)
			lines(x$r,x$gm$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$gm$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$gm$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$gm$inf,lty=lty[3],col=cols[3],...)	
		}
		if(opt%in%c("all","K")) { # K-function
			plot(x$r,x$km$obs,ylim=range(x$km[,1:4]),main=paste("\n\n",sub[2]),type="n",xlab="distance (r)",ylab="Km(r)",cex.lab=1.25,...)
			lines(x$r,x$km$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$km$theo,lty=lty[2],col=cols[2],...)
			lines(x$r,x$km$sup,lty=lty[3],col=cols[3],...)
			lines(x$r,x$km$inf,lty=lty[3],col=cols[3],...)
		}
	}
	else {
		par(mar=c(0.1,0.1,0.1,0.1),cex=csize)
		plot(x$r,x$gm$obs/2,type="n",axes=FALSE,xlab="",ylab="")
		if(legend)
			legend("center",c("obs","theo (no correlation)"),cex=1.5,lty=lty[1:2],bty="n",horiz=TRUE,title=main,col=cols[1:2],...)
		else
			legend("center","",cex=1.5,bty="n",horiz=TRUE,title=main,...)
		par(mar=c(5,5,0.1,2),cex=ifelse(opt%in%c("all"),0.85*csize,csize))
		if(opt%in%c("all","g")) { # g-function
			lim<-range(x$gm)
			plot(x$r,x$gm$obs,ylim=c(lim[1],lim[2]+0.1*diff(lim)),main=paste("\n\n",sub[1]),type="n",xlab="distance (r)",ylab="gm(r)",cex.lab=1.25,...)
			lines(x$r,x$gm$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$gm$theo,lty=lty[2],col=cols[2],...)
		}
		if(opt%in%c("all","K")) { # k-function
			plot(x$r,x$km$obs,ylim=range(x$km),main=paste("\n\n",sub[2]),type="n",xlab="distance (r)",ylab="Km(r)",cex.lab=1.25,...)
			lines(x$r,x$km$obs,lty=lty[1],col=cols[1],...)
			lines(x$r,x$km$theo,lty=lty[2],col=cols[2],...)
		}
	}	
}

