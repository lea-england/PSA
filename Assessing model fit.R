mydata=read.csv("PSAdata.csv") 
attach(mydata)
head(mydata)
summary(mydata)

#We now test the effectiveness of each of the predictors in the model, (Volume, Volume^2, Invasion).

#I preform a t-test with null hypothesis that the  coefficient  of Volume in  the  population  regression  line  based  on Volume, Volume^2, and 
#Invasion is zero, and alternative hypothesis that this coefficient is not zero. 

VSq=volume^2 
LPSA=log(PSA)
model1=lm(LPSA~volume+invasion+VSq)
summary(model1)

#The p-value for this test is 6.15e-6, so at significance level 0.05, this sample provides strong evidence that the 
#coefficient of Volume is different from zero and thus that the Volume variable has some predictive power in this model 
#to help explain the variability in log(PSA), above and beyond Volume^2 and Invasion. In other words, the variable is helpful to include in the model.

#The code above performs the same test on the invasion and volume^2 predictors. The p-values are 0.0113 and 0.013, respectively. 
#Thus, we have significant evidence to conclude that the variables are helpful to include in the model, and have predictive power in explaining variability in PSA above and beyond the other variables.

#We now look at the confidence intervals of the coefficients of these parameters:

confint(model1)
  
#The  95%  confidence  interval  for  the  coefficient  of Volume is (0.08018,0.19352).   
#In  other  words,  we are  95%  confident  that  as  volume  increases  by  one  mg, PSA will  actually  increase  by  between e^0.080=1.083  and e^0.194=  1.214  mg/ml.   
#This  confidence  interval  was  calculated  using  a  method  that  produces intervals containing the true value of the coefficient of Volume (in the population regression line based on Volume, Volume^2, and Invasion) in 95% of samples of this size.
#Using this same logic, the 95% confidence interval for Invasion is (0.15056, 1.15112) and Volume^2 is (-0.00351,-0.00042). 
#Of note, since these intervals do not contain zero, this supports our above tests that the variables are helpful to include in the model.


