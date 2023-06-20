rm(list=ls())
library(xlsx)
library(openxlsx)
library (survival)
library(survminer)
library (lattice) 
library (Formula)
library(ggplot2)
library (Hmisc)
library (rms)
library (XML)
library(scales)
library(paletteer) 


setwd('workspace')

lxrdata<-read.xlsx("nomogram_test.xlsx",sheet =1)

dd=datadist(lxrdata)
options(datadist="dd")
ckf<-lrm(label~radscore+ph,data=lxrdata,x=T,y=T)
nom<-nomogram(ckf,fun=plogis,lp=F,fun.at = c(0.1,0.5,0.9),funlabel="FNClCC Grade")
plot(nom)
summary(ckf)
ckf$stats



cal<-calibrate(ckf,method = 'boot',B=1000)
plot(cal,ylim=c(0,1),xlim = c(0,1),legend=FALSE)

abline(0,1,col = "black",lty = 2,lwd = 2)
lines(cal[,c("predy","calibrated.orig")], type = "l",lwd = 2,col="red",pch =16)
lines(cal[,c("predy","calibrated.corrected")], type = "l",lwd = 2,col="blue",pch =16)
legend(0.55,0.35,
       c("Apparent","Ideal","Bias-corrected"),
       lty = c(2,1,1),
       lwd = c(2,1,1),
       col = c("black","red","blue"),
       bty = "n") # "o"为加边框

ckf
C<-rcorrcens(label~predict(ckf,newdata=lxrdata),data=lxrdata)
C[1,1]
C[1,1]-1.96*C[1,4]/2
C[1,1]+1.96*C[1,4]/2 



formula1<-as.formula(label~radscore+ph)
formula2<-as.formula(label~radscore)
 


mod1<- glm(formula1,data=lxrdata,family=binomial(link=logit))

install.packages('ResourceSelection')

library(ResourceSelection)

hl1 <-hoslem.test(mod1$y, fitted(mod1), g=10)
hl1


pred.logit = predict(ckf1,newdata = lxrdata)
y_pred <- 1/(1+exp(-pred.logit))
ckf2$y

data: ckf2$y,fitted(ckf2)

hl <- hoslem.test(ckf1$y,y_pred,g=10)
hl


library(rmda)

model_1<-decision_curve(formula1,data=lxrdata,family=binomial(link='logit'),thresholds = seq(0,1,by=0.005),confidence.intervals = 0.95,study.design = 'case-control')
plot_decision_curve(model_1,curve.name=c('moedl_1'),ylim=c(-0.2,1),xlim=c(0,1),cost.benefit.axis=FALSE,col = c('red'),confidence.intervals=FALSE,standardize=FALSE)

model_2<-decision_curve(formula2,data=lxrdata,family=binomial(link='logit'),thresholds = seq(0,1,by=0.005),confidence.intervals = 0.95,study.design = 'case-control')
plot_decision_curve(model_2,curve.name=c('model_2'),ylim=c(0,1),xlim=c(0,0.8),cost.benefit.axis=FALSE,col = c('red'),confidence.intervals=FALSE,standardize=FALSE)

model_all<- list(model_1,model_2)

plot_decision_curve(model_all,
                    curve.name=c('Nomogram','TMV-PTV Model'),ylim=c(-0.2,1),
                    xlim=c(0,1),cost.benefit.axis=FALSE,
                    col = c('red','blue'),confidence.intervals=FALSE,
                    standardize=FALSE,legend.position = "bottomleft",cex= 0.1)

head(model_all)


