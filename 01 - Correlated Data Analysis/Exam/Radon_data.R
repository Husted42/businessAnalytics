#Code for Figure 1 in the Exam assignment
getwd()
remove(list=ls())
df<-read.table("C:/Users/huste/Documents/Github/businessAnalytics/Correlated Data/Exam/Radon_MN.csv",header = T)
attach(df)

(J <- length(unique(county.name)))
(ybarbar = mean(y))

(sample.size <- as.vector(table(county)))
(sample.size.jittered <- sample.size*exp(runif (J, -.1, .1)))
(cty.mns = tapply(y,county,mean))
(cty.vars = tapply(y,county,var))
(cty.sds = mean(sqrt(cty.vars[!is.na(cty.vars)]))/sqrt(sample.size))
(cty.sds.sep = sqrt(tapply(y,county,var)/sample.size))

par(mfrow=c(1,1))
plot (sample.size.jittered, cty.mns, cex.lab=.9, cex.axis=1,
      xlab="sample size in county j",
      ylab="Mean log radon in county j",
      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
      ylim=c(0,3.2), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=.9, mgp=c(1.5,.5,0))
axis (2, seq(0,3), cex.axis=.9, mgp=c(1.5,.5,0))
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
         cty.mns[j] + c(-1,1)*cty.sds[j], lwd=.5)
  #         cty.mns[j] + c(-1,1)*mean(cty.sds[!is.na(cty.sds)]), lwd=.5)
}
abline(h=mean(cty.mns),lwd=.5 )
title("Observed mean response values per county",cex.main=.9, line=1)

cty.mns[cty.mns==max(cty.mns)]
cty.mns[cty.mns==min(cty.mns)]

points(sample.size.jittered[36],cty.mns[36],cex=2) #max
points(sample.size.jittered[37],cty.mns[37],cex=2) #min
detach(df)