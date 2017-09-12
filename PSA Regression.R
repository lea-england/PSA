mydata=read.csv("/Users/leaengland/Desktop/untitled.csv") 
attach(mydata)
head(mydata)
summary(mydata)
install.packages("leaps")
install.packages("HH")
library(leaps)
library(HH)

#Developing  a second order linear  model  that  uses volume, weight, age, hyperplasia, invasion, capsular,and gleason (or an appropriate subset thereof) 
#to describe and predict the value of the response variable, PSAlevel (serum prostate-specific antigen level (mg/ml)).

#To begin our process of finding the best fitting model for the prostate data, we conduct a best subsets procedure  on  the  response PSA 
#with  all  seven  predicting  variables: volume, weight, age, hyperplasia, invasion, capsular, and gleason. I define the “best” model to minimize Cp while including the elements of interaction terms or second order terms.


#Defining second order varirables:
VSq=volume^2 
WSq=weight^2
ASq=age^2
HSq=hyperplasia^2
CSq=capsular^2
ISq=invasion^2 
GSq=gleason^2

#Using stepwise regression to narrow down predictors:
null=lm(PSA~1,data=mydata) #creating null value for stepwise regression to start with
full=lm(PSA~((volume+weight+age+hyperplasia+invasion+capsular+gleason)^2+VSq+WSq+ASq+HSq+CSq+ISq+GSq),data=mydata) #creating full model so stepwise regression knows the range of possible models to search through
step(null, scope = list(upper=full), data=mydata, direction="both") #Stepwise command. Model chosen by stepwise: PSA ~ volume + CSq + capsular + invasion + GSq + 
#VSq + volume:capsular + capsular:invasion. We cannot use this model since it contains the term Gleason^2 but not Gleason. The next best model (lowest AIC) is PSA ~ volume + CSq + capsular + invasion.


#Since the stepwise regression chose the volume, capsular, and invasion variables, I find the best subsets of these variables--including their interaction and second order terms--using an exahuastive search.
vci<-regsubsets(PSA~((volume+capsular+invasion)^2+VSq+ISq+CSq),nbest=20, data=mydata) #Command to choose the best model--requires the leaps package 
summaryHH(vci) #lists info (Cp value, r^2, adjusted r^2, etc) for all models found by regsubsets

#Model with lowest Cp roughly equal to their number of parameters ("best"):
#PSA~volume-capsular-invasion-VSq-volume:capsular, parameters= 5, Cp=6.08, adjusted R2= 0.533

#To see if I can find a model with a smaller Cp, I preform the same exhaustive best subset search on volume and capsular, volume and invasion, and capsular and invasion.
vc<-regsubsets(PSA~((volume+capsular)^2+VSq+CSq),nbest=20,data=mydata)
summaryHH(vc)
#"Best" viable model: Volume, capsular, VSq,volume:capsular. Parameters=5, Cp=4.01, adjusted R^2= 0.506

vi<-regsubsets(PSA~((volume+invasion)^2+VSq+ISq),nbest=20, data=mydata) 
summaryHH(vi)
#"Best" viable model: Volume, invasion, volume:invasion. Parameters=4, Cp=3.63, adjusted R^2= 0.440


ci<-regsubsets(PSA~((capsular+invasion)^2+CSq+ISq),nbest=20,data=mydata) 
summaryHH(ci)
#"Best" viable model: capsular, invasion, CSq, capsular:invasion. Parameters=4, Cp=4.000000, adjusted R^2= 0.404


#Model with lowest Cp and highest adjusted R^2: Volume, invasion, volume:invasion. 

#Out of the models selected, model 2--containing just id and leaps--has a Cp closest to its number of parameters

#Now, I attempt to find the best model, using volume and invasion (including interaction and second order terms) using different transformations of the response variable, PSA.

##########################################
#TRANSFORMATIONS
#Non-transformed PSA
#The best subsets procedure on the non-transformed PSA was previously done on line 45. We now plot residuals vs fitted values to aid in our residual analysis, .
par(mfrow=c(2,3))#Set up a plotting environment of two rows and three columns
model1=lm(PSA~volume+invasion+volume:invasion) 
resids1=model1$residuals
fits1=model1$fitted
plot(resids1~fits1,col="red",pch=19,main="Residuals vs Fitted Values (PSA)")
abline(h=0)


#Repeat process for 1/PSA transformation
PSAI=1/PSA
allmods=regsubsets(PSAI~volume+invasion+volume:invasion+VSq+ISq,nbest=30,data=mydata) 
summaryHH(allmods) #"Best" viable model:volume+invasion+VSq. Cp=2.486, adjusted R^2=0.1566
model1=lm(PSAI~volume+invasion+VSq) 
resids1=model1$residuals
fits1=model1$fitted
plot(resids1~fits1,col="red",pch=19,main="Residuals vs Fitted Values(1/PSA)")
abline(h=0)

#Repeat process for PSA^2 transformation
PSAQ=(PSA)^2
allmods=regsubsets(PSAQ~volume+invasion+volume:invasion+VSq+ISq,nbest=20,data=mydata) 
summaryHH(allmods) #"Best" viable model:volume+invasion+VSq+volume:invasion. Cp=4.00, adjusted R^2=0.286
model1=lm(PSAQ~volume+invasion+VSq+volume:invasion)
resids1=model1$residuals
fits1=model1$fitted
plot(resids1~fits1,col="red",pch=19,main="Residuals vs Fitted Values ((PSA)^2)")
abline(h=0)

#Repeat process for square root of PSA transformation
RPSA=sqrt(PSA)
allmods=regsubsets(RPSA~volume+invasion+volume:invasion+VSq+ISq,nbest=20,data=mydata) 
summaryHH(allmods)#"Best" viable model:volume+invasion. Cp=1.75, adjusted R^2=0.545
model1=lm(RPSA~volume+invasion)
resids1=model1$residuals
fits1=model1$fitted
plot(resids1~fits1,col="red",pch=19,main="Residuals vs Fitted Values (sqrt(PSA))")
abline(h=0)

LPSA=log(PSA)
allmods=regsubsets(LPSA~volume+invasion+volume:invasion+VSq+ISq,nbest=30,data=mydata) 
summary(allmods)
summaryHH(allmods)#"Best" viable model:volume+invasion+VSq. Cp=2.25, adjusted R^2=0.501
model1=lm(LPSA~volume+invasion+VSq)
resids1=model1$residuals
fits1=model1$fitted
plot(resids1~fits1,col="red",pch=19,main="Residuals vs Fitted Values (log(PSA))")
abline(h=0)

#Based on lowest Cp and highest adjusted R^2, we proceed with log(PSA) and sqrt(PSA). Examining the residual vs fitted value plots, log(PSA) looks to be the best model.
#The residuals are scattered more randomly above and below zero than the other transformations, which show clusters or patterns. Specifically, sqrt(PSA) shows megaphone-like pattern, which is a common violation indicates nonconstant variance.
#####################################################
par(mfrow=c(1,2))
#Stepwise regression to confirm our log(PSA) model 
#Complete Second Order Model
fullmodel=lm(LPSA~volume+invasion+VSq,data=mydata)
MSE=(summary(fullmodel)$sigma)^2
none=lm(LPSA~1)
step(none,scope=list(upper=fullmodel),scale=MSE)





