mydata=read.csv("PSAdata.csv") 
attach(mydata)
head(mydata)
summary(mydata)
install.packages("nortest")
library(nortest)

#In order to use a multiple linear regression model for the data, we assume the error terms in the actual 
#population are identically normally distributed around the linear model of the population (all normally distributed with mean 0 and the same variance).
#To check these assumptions, we check if the error terms (residuals) in our model have a mean of zero and constant variance.
#In addition, independent variables (volume and invastion) and dependent variables (log(PSA)) must have a linear relationship.
#I first look at the plot of residuals vs fitted values:

VSq=volume^2 
LPSA=log(PSA)
model1=lm(LPSA~volume+invasion+VSq)
resids1=model1$residuals
fits1=model1$fitted
plot(resids1~fits1,col="red",pch=19,main="Residuals vs Fitted Values (log(PSA))")
abline(h=0)

#The plot of residuals versus fitted values (image shown in README file) are
#scattered randomly above and below the zero line, with no clear curvature or pattern away from this line. So, 
#assuming a linear relationship between volume and invasion and log(PSA) seems reasonable.
#The plot also supports our assumption of constant variance as well, as it  looks  as  if  the  data  isspread in moderately equally width bands above and below zero


#In addition, it seems reasonable to assume the residuals have a mean of zero (the number of residuals above zero look roughly equal to the number of those below and
#there's no clear indication of a pattern). To further check this assumption, I look at the histogram of residuals:

hist(resids1)

#The histogram of residuals looks to be centered at zero.

#The  histogram  of  the  residuals  look  somewhat  bell-shaped  (like  a  normal  distribution),  which  may support the assumption of normality for random errors.
#To better check the assumption of normality, we perform a Anderson-Darling Test.  In this hypothesis test, 
#our null hypothesis is that the data is consistent with normality, and our alternative hypothesis is that the data is not consistent with normality.

ad.test(resids1) #p-value=0.03, so at signicance level 0.05, we reject the null hypothesis in favor of the alternative
#and conclude the data is not consistent with normality. Practically, however, this p-value is rather borderline and close to 0.05, 
#so I look to the normal probability plot:

qqnorm(LPSA)
qqline(LPSA) #Graph of quantiles of residuals of the sample plotted against quantiles residuals we would expect to see in a sample of the same size that was perfectly normal

#The sample residuals are centered close to the line, especially between quantiles -1 and 1.  
#Additionally, it looks as if an equal amount of data lies below the line as above.  Thus, we can assume normality (but cautiously).

#An additional assumption for the use of this model is that errors are mutually independent from one another, and data was taken from a random process.
#We assume data was taken from random process. Mutual independence also seems reasonable to assume  as it would make sense that one individuals’PSA, and thus log(PSA), falling below or above the linear model
#would have no affect on another individuals’PSA,and thus log(PSA), falling below or above the linear model.


