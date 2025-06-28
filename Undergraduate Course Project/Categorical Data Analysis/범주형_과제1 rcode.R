#1
pi.hat=486/1500
pi.hat-qnorm(0.995)*sqrt(pi.hat*(1-pi.hat)/1500)
pi.hat+qnorm(0.995)*sqrt(pi.hat*(1-pi.hat)/1500)

#2
pi.null=0.5
z.test = (pi.hat-pi.null)/sqrt(pi.null*(1-pi.null)/1500)
z.test
2*pnorm(z.test)

#3
pi1.hat=570/555453
pi2.hat=433/8482
prop.test(c(570,433), c(555453,8482), conf.level = 0.95, correct=FALSE)

#4
pic.hat=(570+433)/(555453+8482)
z.stat = (pi1.hat-pi2.hat)/(sqrt(pic.hat*(1-pic.hat)*(1/555453+1/8482)))
z.stat
2*pnorm(z.stat)

#5
chisq.stat=z.stat^2
chisq.stat
#or
data = matrix(c(570,433,554883,8049),nrow=2,ncol=2)
chisq.test(data,correct=F)


#7
data2 = matrix(c(802,34,53,494),nrow=2,ncol=2)
chisq.stat2=chisq.test(data2,correct=F)
ls(chisq.stat2)
lrt.stat=2*(sum(chisq.stat2$observed*log(chisq.stat2$observed/chisq.stat2$expected)))
lrt.stat
1-pchisq(lrt.stat,df=1)

#8
sample.or=(802*494)/(34*53)
log.odds=log(sample.or)
SE=sqrt(1/802+1/494+1/34+1/53)
log.lb=log.odds-qnorm(0.975)*SE
log.ub=log.odds+qnorm(0.975)*SE
c(exp(log.lb),exp(log.ub))
