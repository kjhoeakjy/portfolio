#김연주
#2021250461
#범주형자료분석방법론 HW2

crabs=read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)
head(crabs)

#poisson regression model
#log link
model1=glm(sat~width, data=crabs, family=poisson(link=log))
summary(model1)
#LRT
632.79-567.88
qchisq(0.95,df=1)

#identity link
model2=glm(sat~width, data=crabs, family=poisson(link=identity), start=coef(model1))
summary(model2)
#LRT
632.79-557.71

#비교
ind=order(crabs$width)
plot(crabs$width,crabs$sat)
lines(x=crabs$width[ind],y=fitted(model1)[ind],col='red',lwd=3)
lines(x=crabs$width[ind],y=fitted(model2)[ind],col='blue',lwd=3)



#negative binomial regression
#log link
library(MASS)
model3=glm.nb(sat~width, data=crabs, link=log)
summary(model3)
#LRT
213.05-195.81
qchisq(0.95,df=171)

#identity link
model4=glm.nb(sat~width, data=crabs, link=identity, start=coef(model1))
summary(model4)
#LRT
216.51-195.52

#비교
plot(crabs$width,crabs$sat)
lines(x=crabs$width[ind],y=fitted(model3)[ind],col='red',lwd=3)
lines(x=crabs$width[ind],y=fitted(model4)[ind],col='blue',lwd=3)


#Poisson regression with quasi-likelihood
#log link
model5=glm(sat~width, data = crabs, family=quasi(link = "log", variance = "mu"))
summary(model5)

chisq=sum(resid(model1,type='pearson')^2)
chisq
phi=sqrt(chisq/171)
phi
phi*0.01997

#identity link
model6=glm(sat~width, data = crabs, family=quasi(link = "identity", variance= "mu"),start = coef(model1))
summary(model6)

chisq2=sum(resid(model2,type='pearson')^2)
chisq2
phi2=sqrt(chisq2/171)
phi2
phi2*0.02968

#비교
plot(crabs$width,crabs$sat)
lines(x=crabs$width[ind],y=fitted(model5)[ind],col='red',lwd=3)
lines(x=crabs$width[ind],y=fitted(model6)[ind],col='blue',lwd=3)
