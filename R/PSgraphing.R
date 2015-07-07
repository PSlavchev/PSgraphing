require(plotrix)
require(meta)

PSgraphing <- function(estimate, count, text, name){


	count$PercentExp=count$event.e/count$n.e
	count$PercentCtrl=count$event.c/count$n.c
	count$Total=count$n.c+count$n.e

	results=meta::metabin(data=count,event.e=event.e, n.e=n.e, event.c=event.c,n.c=n.c)
	exp(results$TE.random)


	par(mar=c(6,5,2,2), bg="transparent")
	plotrix::plotCI(estimate$decile, estimate$RR, ui=estimate$RR_upper, li=estimate$RR_lower, log=c("y"),
		   ylog=T,ylab='Risk Ratio', xlab="",axes=F, xlim=c(-3,11), ylim=c(0.05,15),)

	axis(side=1, at=c(1:10),labels=c(1:10))
	axis(side=2, at=c(0.1,0.2,0.5,1,2,5,10), labels=c(0.1,0.2,0.5,1,2,5,10))

	mtext(side=1, text='Propensity Score Decile', line=2.5)
	mtext(side=1,text=text, line=4)

	text(x=-2.8,y=10,labels="% events Exp:", adj=0, cex=0.9)
	text(x=-2.8,y=8,labels="% events Non-Exp:", adj=0, cex=0.9)

	text(x=1,y=c(10,8),labels=c(round(count$PercentExp*100,0)[1],round(count$PercentCtrl*100,0)[1]), cex=0.9)
	text(x=2,y=c(10,8),labels=c(round(count$PercentExp*100,0)[2],round(count$PercentCtrl*100,0)[2]), cex=0.9)
	text(x=3,y=c(10,8),labels=c(round(count$PercentExp*100,0)[3],round(count$PercentCtrl*100,0)[3]), cex=0.9)
	text(x=4,y=c(10,8),labels=c(round(count$PercentExp*100,0)[4],round(count$PercentCtrl*100,0)[4]), cex=0.9)
	text(x=5,y=c(10,8),labels=c(round(count$PercentExp*100,0)[5],round(count$PercentCtrl*100,0)[5]), cex=0.9)
	text(x=6,y=c(10,8),labels=c(round(count$PercentExp*100,0)[6],round(count$PercentCtrl*100,0)[6]), cex=0.9)
	text(x=7,y=c(10,8),labels=c(round(count$PercentExp*100,0)[7],round(count$PercentCtrl*100,0)[7]), cex=0.9)
	text(x=8,y=c(10,8),labels=c(round(count$PercentExp*100,0)[8],round(count$PercentCtrl*100,0)[8]), cex=0.9)
	text(x=9,y=c(10,8),labels=c(round(count$PercentExp*100,0)[9],round(count$PercentCtrl*100,0)[9]), cex=0.9)
	text(x=10,y=c(10,8),labels=c(round(count$PercentExp*100,0)[10],round(count$PercentCtrl*100,0)[10]), cex=0.9)


	# plots lines only with PS decile range
	# lines(x=1:10, y=exp(rep(results$TE.random,10)),lwd=2)
	# lines(x=1:10, y=exp(rep(results$upper.random,10)), lty="dashed")
	# lines(x=1:10, y=exp(rep(results$lower.random,10)), lty="dashed")

	# plots lines extends beyond PS decile range
	lines(x=-1:11, y=exp(rep(results$TE.fixed,13)),lwd=2)
	lines(x=-1:11, y=exp(rep(results$upper.fixed,13)), lty="dashed")
	lines(x=-1:11, y=exp(rep(results$lower.fixed,13)), lty="dashed")

	# plot text summary
	# text(x=-2,
	#      y=c(exp(results$upper.fixed),exp(results$TE.fixed),exp(results$lower.fixed)),
	#      labels=c(round(exp(results$upper.random),2),round(exp(results$TE.random),2),
	#               round(exp(results$lower.random),2)),
	#      cex=0.6)

	text(x=-2.8, y=c(0.25), labels=c("Heterogeneity:"),adj=0,cex=0.9)
	text(x=-2.8, y=c(0.20), labels=expression(paste(I^2," = 68.4%"),sep=""),adj=0,cex=0.9)
	text(x=-2.8, y=c(0.15), labels=expression(paste(tau^2," = 0.043"),sep=""),adj=0,cex=0.9)

	dev.copy(pdf,file=name, width=10, height=8)
	dev.off()
}

