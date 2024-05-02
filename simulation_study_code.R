#-------------------------------------------------------------------------------
# R code accompanying "Investigating the causal effects of multiple treatments
# using longitudinal data: a simulation study" 

# Author: Emily Granger
# Date last modified: 30/04/2024

# This file contains two sections: 1. Data Generation & 2. Data analysis

# Section 1 provides all code for generating data under the nine scenarios
# described in the manuscript. Some code will need to be commented in or out for 
# different scenarios. When this is the case, it is explained in the comments. 

# Section 2 provides code for analysing one simulated dataset using the five 
# analysis approaches described in the manuscript. We ran this 1000 times on 
# 1000 simulated datasets to obtain estimates of the performance measures.  

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Preliminaries 

#Load libraries
library(tidyr)
library(dplyr)
library(nnet)
library(data.table)
library(gfoRmula)

#Define functions 
expit <- function(x){exp(x)/(1+exp(x))}
cum_sum_0 = function(x)
{
  cs = cumsum(x)
  cs - cummax((x == 0) * cs)
}

#-------------------------------------------------------------------------------
# 1. Data generation
#-------------------------------------------------------------------------------

#sample size
n=10000

#number of visits
n.visit=5

#generate ID
id<-rep(1:n,each=(n.visit+1))

#time variable (here, time 0 represents time prior to any treatment initiation)
k<-rep(0:n.visit, times=n)

#Create dataset with empty vars
L<-rep(0,n*(n.visit+1))
A<-rep(0,n*(n.visit+1))
B<-rep(0,n*(n.visit+1))
Y<-rep(0,n*(n.visit+1))
dat<-data.frame(id,k,L,A,B,Y)

#Create data for time 0 (prior to treatment initiation, so A_0=0 and B_0=0)
dat$L[dat$k==0]=rnorm(n,0,1)
dat$A[dat$k==0]=0
dat$B[dat$k==0]=0

#Use following code for Y_0 in Scenarios 1,2-9
dat$Y[dat$k==0]=rnorm(n,0.05*dat$L[dat$k==0]+1*dat$A[dat$k==0]+
                        0.5*dat$B[dat$k==0],1)
#Use following code for Y_0 in Scenario 3
#dat$Y[dat$k==0]=rnorm(n,0.3*dat$L[dat$k==0]+1*dat$A[dat$k==0]+
#                        0.5*dat$B[dat$k==0],1)

#Create null lag vars for treatment A (this is only needed for scenario 8)
#dat$lag1_A<-0;dat$lag2_A<-0;dat$lag3_A<-0;dat$lag4_A<-0

#Different code used to simulate follow-up years in each scenario.
#-------------------Comment in relevant code--------------------#

#-------------------------------
#Follow-up years for scenario 1

for(i in 1:n.visit){
  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
  dat$A[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$A[dat$k==(i-1)]+
0.12*dat$Y[dat$k==(i-1)]))
  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$B[dat$k==(i-1)]+
0.12*dat$Y[dat$k==(i-1)]))
  dat$Y[dat$k==i]=rnorm(n, 0.05*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
1*dat$A[dat$k==i]+0.5*dat$B[dat$k==i],1)
}

#-------------------------------
#Follow-up years for scenario 2

#for(i in 1:n.visit){
#  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
#0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
#  dat$A[dat$k==i]=rbinom(n,1,expit(-2.3+0.3*dat$L[dat$k==i]+
#4*dat$A[dat$k==(i-1)]+0.12*dat$Y[dat$k==(i-1)]))
#  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+4*dat$B[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$Y[dat$k==i]=rnorm(n, 0.05*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
#1*dat$A[dat$k==i]+0.5*dat$B[dat$k==i],1)
#}

#-------------------------------
#Follow-up years for scenario 3

#for(i in 1:n.visit){
#  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
#0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
#  dat$A[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+
#1.8*dat$A[dat$k==(i-1)]+0.12*dat$Y[dat$k==(i-1)]))
#  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+
#1.8*dat$B[dat$k==(i-1)]+0.12*dat$Y[dat$k==(i-1)]))
#  dat$Y[dat$k==i]=rnorm(n, 0.3*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
#1*dat$A[dat$k==i]+0.5*dat$B[dat$k==i],1)
#}

#-------------------------------
#Follow-up years for scenario 4

#for(i in 1:n.visit){
#  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
#0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
#  dat$A[dat$k==i]=rbinom(n,1,expit(dat$L[dat$k==i]+1.8*dat$A[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$B[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$Y[dat$k==i]=rnorm(n, 0.05*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
#1*dat$A[dat$k==i]+0.5*dat$B[dat$k==i],1)
#}

#-------------------------------
#Follow-up years for scenario 5

#for(i in 1:n.visit){
#  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
#0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
#  dat$A[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+
#1.8*dat$A[dat$k==(i-1)]-0.2*dat$B[dat$k==(i-1)]+0.12*dat$Y[dat$k==(i-1)]))
#  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+
#1.8*dat$B[dat$k==(i-1)]-0.2*dat$A[dat$k==(i-1)]+0.12*dat$Y[dat$k==(i-1)]))
#  dat$Y[dat$k==i]=rnorm(n, 0.05*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
#1*dat$A[dat$k==i]+0.5*dat$B[dat$k==i],1)
#}

#-------------------------------
#Follow-up years for scenario 6

#for(i in 1:n.visit){
#  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
#0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
#  dat$A[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$A[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$B[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$Y[dat$k==i]=rnorm(n, 0.05*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
#0.5*dat$B[dat$k==i],1)
#}

#-------------------------------
#Follow-up years for scenario 7

#for(i in 1:n.visit){
#  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
#0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
#  dat$A[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$A[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$B[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$Y[dat$k==i]=rnorm(n, 0.05*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
#1*dat$A[dat$k==i]+0.5*dat$B[dat$k==i]+0.25*dat$A[dat$k==i]*dat$B[dat$k==i],1)
#}

#-------------------------------
#Follow-up years for scenario 8

#for(i in 1:n.visit){
#  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
#0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
#  dat$A[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$A[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$lag1_A[dat$k==i]=dat$A[dat$k==(i-1)]
#  dat$lag2_A[dat$k==i]=dat$lag1_A[dat$k==(i-1)]
#  dat$lag3_A[dat$k==i]=dat$lag2_A[dat$k==(i-1)]
#  dat$lag4_A[dat$k==i]=dat$lag3_A[dat$k==(i-1)]
#  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+1.8*dat$B[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$Y[dat$k==i]=rnorm(n, 0.05*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
#1*dat$A[dat$k==i]+0.5*dat$B[dat$k==i]+
#                          0.2*dat$lag1_A[dat$k==i]+0.1*dat$lag2_A[dat$k==i]+
#  0.05*dat$lag3_A[dat$k==i]+0.001*dat$lag4_A[dat$k==i],1)
#}

#-------------------------------
#Follow-up years for scenario 9

#for(i in 1:n.visit){
#  dat$L[dat$k==i]=rnorm(n,0.2*dat$L[dat$k==(i-1)]+0.2*dat$A[dat$k==(i-1)]+
#0.2*dat$B[dat$k==(i-1)]+0.01*dat$Y[dat$k==(i-1)],1)
#  dat$A[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+0.2*dat$A[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat<- dat %>% group_by(id) %>% mutate(cum_A=cum_sum_0(A))
#  dat$B[dat$k==i]=rbinom(n,1,expit(0.3*dat$L[dat$k==i]+0.2*dat$B[dat$k==(i-1)]+
#0.12*dat$Y[dat$k==(i-1)]))
#  dat$Y[dat$k==i]=rnorm(n, 0.05*dat$L[dat$k==i]+0.1*dat$Y[dat$k==(i-1)]+
#(1-0.2*(dat$cum_A[dat$k==i]-1))*dat$A[dat$k==i]+0.5*dat$B[dat$k==i],1)
#}

#Outcome at time 0 (this is treated as a baseline confounder)
dat$Y_0[dat$k==0]<-dat$Y[dat$k==0]
dat <- dat %>%  group_by(id) %>% fill(Y_0, .direction="down")

#Lag outcome (this is treated as a time-varying confounder)
dat<-dat%>%group_by(id)%>%mutate(lag1_Y=dplyr::lag(Y,n=1,default=NA))
dat$lag1_Y[dat$k==0]<-0

#Treatment variable
dat$trt<-ifelse(dat$A==0 & dat$B==0, 0, 
                ifelse(dat$A==1 & dat$B==0, 1,
                       ifelse(dat$B==1 & dat$A==0,2,3)))
#Lag treatments
dat<-dat%>%group_by(id)%>%mutate(lag1_trt=dplyr::lag(trt,n=1,default=NA))
dat$lag1_trt[dat$k==1|dat$k==0]<-0
dat<-dat%>%group_by(id)%>%mutate(lag2_trt=dplyr::lag(lag1_trt,n=1,default=NA))
dat$lag2_trt[dat$k==1|dat$k==0]<-0
dat<-dat%>%group_by(id)%>%mutate(lag3_trt=dplyr::lag(lag2_trt,n=1,default=NA))
dat$lag3_trt[dat$k==1|dat$k==0]<-0
dat<-dat%>%group_by(id)%>%mutate(lag4_trt=dplyr::lag(lag3_trt,n=1,default=NA))
dat$lag4_trt[dat$k==1|dat$k==0]<-0

#Baseline treatment variable
dat$baseline_trt<-NA
dat$baseline_trt[dat$k==1]<-dat$trt[dat$k==1]
dat <- dat %>%  group_by(id) %>% fill(baseline_trt, .direction="down")

#Set treatment variables as factors
dat$trt<-as.factor(dat$trt)
dat$lag1_trt<-as.factor(dat$lag1_trt)
dat$lag2_trt<-as.factor(dat$lag2_trt)
dat$lag3_trt<-as.factor(dat$lag3_trt)
dat$lag4_trt<-as.factor(dat$lag4_trt)

dat.st<-dat #Keep dat.st for the sequential trials code
dat<-dat[dat$k>0,]#for other analyses, only need data from k=1,2,3,4,5. 

#-------------------------------------------------------------------------------
# 2. Data analysis
#-------------------------------------------------------------------------------
#Same analysis code is used across all scenarios

#-------------------------------------------------------------------------------
#Inverse-probability-of-treatment weighted estimation of MSMs

#Estimate propensity scores
ps_model<-multinom(trt~lag1_trt+Y_0+lag1_Y+L, data=dat)
ps<-fitted(ps_model)

#Add estimated propensity scores to the dataset
dat$temp1<-ps[,1]; dat$temp2<-ps[,2]; dat$temp3<-ps[,3]; dat$temp4<-ps[,4]
dat$ps<-NULL
dat$ps[dat$trt==0]<-dat$temp1[dat$trt==0]
dat$ps[dat$trt==1]<-dat$temp2[dat$trt==1]
dat$ps[dat$trt==2]<-dat$temp3[dat$trt==2]
dat$ps[dat$trt==3]<-dat$temp4[dat$trt==3]

#Estimate numerator for stabilised weights
num_model<-multinom(trt~lag1_trt+Y_0, data=dat)
num<-fitted(num_model)

#Add numerator to the dataset
dat$temp1<-num[,1]; dat$temp2<-num[,2]; dat$temp3<-num[,3]; dat$temp4<-num[,4]
dat$num<-NULL
dat$num[dat$trt==0]<-dat$temp1[dat$trt==0]
dat$num[dat$trt==1]<-dat$temp2[dat$trt==1]
dat$num[dat$trt==2]<-dat$temp3[dat$trt==2]
dat$num[dat$trt==3]<-dat$temp4[dat$trt==3]

#Estimate IPTW weights
dat <- dat %>% group_by(id) %>% mutate(iptw = cumprod(num/ps) )

#Marginal structural model
msm<-lm(Y~trt+lag1_trt+lag2_trt+lag3_trt+lag4_trt+Y_0, weights=iptw, data=dat)

#--------------------------
#Estimate treatment effects

#Set up newdata (ND) sets
ND0<-dat ; ND0$trt<-"0"
ND1<-dat ; ND1$trt<-"1"
ND2<-dat ; ND2$trt<-"2"
ND3<-dat ; ND3$trt<-"3"

#TE10 (A v no treatment): estimated treatment effects at times 1-5

#Time 1
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce101<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 2
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce102<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 3
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce103<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 4
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce104<-mean(predict(msm,newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 5
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce105<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#TE20 (B v no treatment): estimated treatment effects at times 1-5

#Time 1
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce201<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 2
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce202<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 3
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce203<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 4
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce204<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 5
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce205<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#TE30 (A & B v no treatment): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce301<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce302<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce303<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce304<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
iptw.ce305<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#TE21 (B v A): estimated treatment effects at times 1-5

#Time 1
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
iptw.ce211<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 2
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
iptw.ce212<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 3
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
iptw.ce213<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 4
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0"
iptw.ce214<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 5
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1"
iptw.ce215<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#TE31 (A & B v A): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
iptw.ce311<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
iptw.ce312<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
iptw.ce313<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0"
iptw.ce314<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1"
iptw.ce315<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#TE32 (A & B v B): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
iptw.ce321<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
iptw.ce322<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
iptw.ce323<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0"
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0"
iptw.ce324<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3"
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2"
iptw.ce325<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#-------------------------------------------------------------------------------
#Censoring and weighting approach

#Create censoring variable
dat$censor<-NA
dat$censor[dat$k==2]<-ifelse(dat$trt[dat$k==2]!=dat$baseline_trt[dat$k==2],1,
                             dat$censor[dat$k==2])
dat <- dat %>%  group_by(id) %>% fill(censor, .direction="down") 
dat$censor[dat$k==3]<-ifelse(dat$trt[dat$k==3]!=dat$baseline_trt[dat$k==3],1,
                             dat$censor[dat$k==3])
dat <- dat %>%  group_by(id) %>% fill(censor, .direction="down") 
dat$censor[dat$k==4]<-ifelse(dat$trt[dat$k==4]!=dat$baseline_trt[dat$k==4],1,
                             dat$censor[dat$k==4])
dat <- dat %>%  group_by(id) %>% fill(censor, .direction="down") 
dat$censor[dat$k==5]<-ifelse(dat$trt[dat$k==5]!=dat$baseline_trt[dat$k==5],1,
                             dat$censor[dat$k==5])
dat$censor[is.na(dat$censor)]<-0

#censor people when they switch treatments
dat_censored<-dat[dat$censor==0,] 

#Marginal structural model (using iptw weights previously defined)
msm<-lm(Y~trt+lag1_trt+lag2_trt+lag3_trt+lag4_trt+Y_0, weights=iptw, 
        data=dat_censored)

#--------------------------
#Estimate treatment effects

#Set up newdata sets
ND0<-dat_censored ; ND0$trt<-"0"
ND1<-dat_censored ; ND1$trt<-"1"
ND2<-dat_censored ; ND2$trt<-"2"
ND3<-dat_censored ; ND3$trt<-"3"

#TE10 (A v no treatment): estimated treatment effects at times 1-5

#Time 1
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce101<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 2
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce102<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 3
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce103<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 4
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce104<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 5
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce105<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0)) 

#TE20 (B v no treatment): estimated treatment effects at times 1-5

#Time 1
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce201<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 2
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce202<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 3
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce203<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 4
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce204<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 5
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce205<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#TE30 (A & B v no treatment): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce301<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce302<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce303<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce304<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3" 
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0" 
cens.ce305<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#TE21 (B v A): estimated treatment effects at times 1-5

#Time 1
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
cens.ce211<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 2
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
cens.ce212<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 3
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
cens.ce213<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 4
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0" 
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0" 
cens.ce214<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 5
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2" 
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1" 
cens.ce215<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#TE31 (A & B v A): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0" 
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
cens.ce311<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0" 
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
cens.ce312<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0" 
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0" 
cens.ce313<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0" 
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0" 
cens.ce314<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3" 
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1" 
cens.ce315<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#TE32 (A & B v B): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
cens.ce321<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
cens.ce322<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0" 
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0" 
cens.ce323<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0" 
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0" 
cens.ce324<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3" 
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2" 
cens.ce325<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#-------------------------------------------------------------------------------
#Sequential trials approach

#Extract data for each "trial"
for(t in 1:5){
  
  dat_temp<-dat.st[dat.st$k>=(t-1),]
  dat_temp$trial<-t
  
  #Redefine visit year variable
  dat_temp<-dat_temp%>%mutate(ones=1)%>%group_by(id)%>%mutate(k=cumsum(ones))
  dat_temp$k<-dat_temp$k-1
  
  #Select eligible individuals (those with no history of treatment with A or B)
  dat_temp$elig<-NA
  dat_temp[dat_temp$k==0 & dat_temp$trt==0 & dat_temp$lag1_trt==0 & 
             dat_temp$lag2_trt==0 & dat_temp$lag3_trt==0 &
             dat_temp$lag4_trt==0,]$elig<-1
  dat_temp <- dat_temp %>%  group_by(id) %>% fill(elig, .direction="down") 
  dat_temp<-dat_temp[!is.na(dat_temp$elig),]
  
  #Outcome at baseline 
  dat_temp$Y_0<-NA
  dat_temp[dat_temp$k==0,]$Y_0<-dat_temp[dat_temp$k==0,]$Y
  dat_temp <- dat_temp %>%  group_by(id) %>% fill(Y_0, .direction="down") 
  
  #Keep follow-up years
  dat_temp<-dat_temp[dat_temp$k>0,]
  
  #Baseline treatment
  dat_temp$baseline_trt<-NA
  dat_temp[dat_temp$k==1,]$baseline_trt<-dat_temp[dat_temp$k==1,]$trt
  dat_temp <- dat_temp%>%group_by(id)%>%fill(baseline_trt, .direction="down") 
  
  #Save trial
  eval(parse(text=paste0("dat.",t,"=dat_temp")))
  
}

#Combine trials
dat_seqt=rbind(dat.1,dat.2,dat.3,dat.4,dat.5)
dat_seqt<-dat_seqt[order(dat_seqt$trial, dat_seqt$id, dat_seqt$k),]

#Estimate propensity scores
ps_model<-multinom(trt~lag1_trt+Y_0+lag1_Y+L+trial, data=dat_seqt)
ps<-fitted(ps_model)

#Add estimated propensity scores to the dataset
dat_seqt$temp1<-ps[,1]
dat_seqt$temp2<-ps[,2]
dat_seqt$temp3<-ps[,3]
dat_seqt$temp4<-ps[,4]
dat_seqt$ps<-NULL
dat_seqt$ps[dat_seqt$trt==0]<-dat_seqt$temp1[dat_seqt$trt==0]
dat_seqt$ps[dat_seqt$trt==1]<-dat_seqt$temp2[dat_seqt$trt==1]
dat_seqt$ps[dat_seqt$trt==2]<-dat_seqt$temp3[dat_seqt$trt==2]
dat_seqt$ps[dat_seqt$trt==3]<-dat_seqt$temp4[dat_seqt$trt==3]

#Estimate numerator for stabilised weights
num_model<-multinom(trt~lag1_trt+Y_0+trial, data=dat_seqt)
num<-fitted(num_model)

#Add numerator to the dataset
dat_seqt$temp1<-num[,1]
dat_seqt$temp2<-num[,2]
dat_seqt$temp3<-num[,3]
dat_seqt$temp4<-num[,4]
dat_seqt$num<-NULL
dat_seqt$num[dat_seqt$trt==0]<-dat_seqt$temp1[dat_seqt$trt==0]
dat_seqt$num[dat_seqt$trt==1]<-dat_seqt$temp2[dat_seqt$trt==1]
dat_seqt$num[dat_seqt$trt==2]<-dat_seqt$temp3[dat_seqt$trt==2]
dat_seqt$num[dat_seqt$trt==3]<-dat_seqt$temp4[dat_seqt$trt==3]

#Estimate IPTW weights
dat_seqt <- dat_seqt %>% group_by(trial, id) %>% mutate(iptw = cumprod(num/ps))

#Create censoring variable
dat_seqt$censor<-NA
dat_seqt$censor[dat_seqt$k==2]<-ifelse(dat_seqt$trt[dat_seqt$k==2]!=
        dat_seqt$baseline_trt[dat_seqt$k==2],1,dat_seqt$censor[dat_seqt$k==2])
dat_seqt<-dat_seqt%>%group_by(trial, id) %>% fill(censor, .direction="down") 

dat_seqt$censor[dat_seqt$k==3]<-ifelse(dat_seqt$trt[dat_seqt$k==3]!=
        dat_seqt$baseline_trt[dat_seqt$k==3],1,dat_seqt$censor[dat_seqt$k==3])
dat_seqt<-dat_seqt%>%group_by(trial, id) %>% fill(censor, .direction="down") 

dat_seqt$censor[dat_seqt$k==4]<-ifelse(dat_seqt$trt[dat_seqt$k==4]!=
        dat_seqt$baseline_trt[dat_seqt$k==4],1,dat_seqt$censor[dat_seqt$k==4])
dat_seqt<-dat_seqt%>%group_by(trial, id) %>% fill(censor, .direction="down") 

dat_seqt$censor[dat_seqt$k==5]<-ifelse(dat_seqt$trt[dat_seqt$k==5]!=
        dat_seqt$baseline_trt[dat_seqt$k==5],1,dat_seqt$censor[dat_seqt$k==5])
dat_seqt$censor[is.na(dat_seqt$censor)]<-0

#censor people when they switch treatments
dat_seqt_censored<-dat_seqt[dat_seqt$censor==0,] 

#Marginal structural model
msm<-lm(Y~trt+lag1_trt+lag2_trt+lag3_trt+lag4_trt+Y_0+trial, weights=iptw,
        data=dat_seqt_censored)

#--------------------------
#Estimate treatment effects

#Set up ND sets
ND0<-dat_seqt_censored ; ND0$trt<-"0"
ND1<-dat_seqt_censored ; ND1$trt<-"1"
ND2<-dat_seqt_censored ; ND2$trt<-"2"
ND3<-dat_seqt_censored ; ND3$trt<-"3"

#TE10 (A v no treatment): estimated treatment effects at times 1-5

#Time 1
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce101<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 2
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce102<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 3
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce103<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 4
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce104<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#Time 5
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce105<-mean(predict(msm, newdata=ND1))-mean(predict(msm, newdata=ND0))

#TE20 (B v no treatment): estimated treatment effects at times 1-5

#Time 1
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce201<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 2
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce202<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 3
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce203<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 4
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce204<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#Time 5
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce205<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND0))

#TE30 (A & B v no treatment): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce301<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce302<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce303<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce304<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3"
ND0$lag1_trt="0" ; ND0$lag2_trt="0" ; ND0$lag3_trt="0" ; ND0$lag4_trt="0"
seqt.ce305<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND0))

#TE21 (B v A): estimated treatment effects at times 1-5

#Time 1
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
seqt.ce211<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 2
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
seqt.ce212<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 3
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
seqt.ce213<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 4
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0"
seqt.ce214<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#Time 5
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1"
seqt.ce215<-mean(predict(msm, newdata=ND2))-mean(predict(msm, newdata=ND1))

#TE31 (A & B v A): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND1$lag1_trt="0" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
seqt.ce311<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="0" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
seqt.ce312<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="0" ; ND1$lag4_trt="0"
seqt.ce313<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="0"
seqt.ce314<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3"
ND1$lag1_trt="1" ; ND1$lag2_trt="1" ; ND1$lag3_trt="1" ; ND1$lag4_trt="1"
seqt.ce315<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND1))

#TE32 (A & B v B): estimated treatment effects at times 1-5

#Time 1
ND3$lag1_trt="0" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND2$lag1_trt="0" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
seqt.ce321<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 2
ND3$lag1_trt="3" ; ND3$lag2_trt="0" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND2$lag1_trt="2" ; ND2$lag2_trt="0" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
seqt.ce322<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 3
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="0" ; ND3$lag4_trt="0"
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="0" ; ND2$lag4_trt="0"
seqt.ce323<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 4
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="0"
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="0"
seqt.ce324<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))

#Time 5
ND3$lag1_trt="3" ; ND3$lag2_trt="3" ; ND3$lag3_trt="3" ; ND3$lag4_trt="3"
ND2$lag1_trt="2" ; ND2$lag2_trt="2" ; ND2$lag3_trt="2" ; ND2$lag4_trt="2"
seqt.ce325<-mean(predict(msm, newdata=ND3))-mean(predict(msm, newdata=ND2))


#-------------------------------------------------------------------------------
#G-estimation
# This code is adapted from the "gestMultiple" command code (gesttools package)
# Original code available here [last accessed: 30/04/2024]: 
# "https://github.com/danieltompsett/gesttools/blob/master/gestMultiple.R 

#------------------------------
# Set up required data frames

#Set time vars
T<-max(dat$k)
cutoff<-T

#Set up counterfactual outcomes for exposure at the preceding time period
dat$H<-dat$Y
dat.c<-dat
dat.c$cntstep<-1

#Create a copy of the data set with complete cases for initial estimate of psi
dat.com<-dat[complete.cases(dat),]

#Set up an augmented dataset 'dat.c', adding additional rows to hold the 2-step, 
#up to c-step counterfactuals. with c=cutoff. 
#That is the counterfactuals for time s-c under outcome s H_{s(s-c)}. 
#'cntstep' indicates the step length of the counterfactual the row holds.
for (i in 2:cutoff){
  dat.2<-dat[dat$k %in% seq(1,T-(i-1),by=1),]
  dat.2$cntstep<-i
  dat.c<-rbind(dat.c,dat.2)
}
dat.c<-dat.c[order(dat.c$id,dat.c$k),]

#------------------------------
# Perform g-estimation for SNMM 

# Obtain estimate of psi_{s-1}
out<-lm(H~trt+lag1_trt+lag2_trt+lag3_trt+lag4_trt+Y_0+lag1_Y+L+ps, data=dat.com)
psi<-c(out$coefficients[2], out$coefficients[3], out$coefficients[4])
psilist<-as.list(NULL)
psilist[[1]]<-psi

dat.c$ind1<-0
dat.c$ind1[dat.c$trt==1]<-1
dat.c$ind2<-0
dat.c$ind2[dat.c$trt==2]<-1
dat.c$ind3<-0
dat.c$ind3[dat.c$trt==3]<-1

# Obtain counterfactuals as before where each j step counterfactual is estimated 
#using the estimate of psi_{s-j+1}
i<-2
while(i<=cutoff && i<=T){
  j<-2
  while(j<=i) {
    for(k in 1:(T-(j-1))){
      dat.c[dat.c$cntstep==j & dat.c$k==k,]$H<-dat.c[dat.c$cntstep==(j-1) & 
                                                       dat.c$k==(k+1),]$H-(
        dat.c[dat.c$cntstep==(j-1) & dat.c$k==(k+1),]$ind1*psilist[[j-1]][1]+
          dat.c[dat.c$cntstep==(j-1) & dat.c$k==(k+1),]$ind2*psilist[[j-1]][2]+
          dat.c[dat.c$cntstep==(j-1) & dat.c$k==(k+1),]$ind3*psilist[[j-1]][3])
    }
    j<-j+1
  }
  # Obtain relevant data and calculate psi_{s-2}
  dat.t<-dat.c[dat.c$cntstep %in% i,]
  dat.tcom<-dat.t[complete.cases(dat.t),]
  if (i==T){
    out<-lm(H~trt+Y_0+lag1_Y+L+ps, data=dat.tcom)
    psi<-c(out$coefficients[2], out$coefficients[3], out$coefficients[4])
    psilist[[i]]<-psi
  }else if(i==(T-1)){
    out<-lm(H~trt+Y_0+lag1_trt+lag1_Y+L+ps, data=dat.tcom)
    psi<-c(out$coefficients[2], out$coefficients[3], out$coefficients[4])
    psilist[[i]]<-psi
  }else if(i==(T-2)){
    out<-lm(H~trt+Y_0+lag1_trt+lag2_trt+lag1_Y+L+ps, data=dat.tcom)
    psi<-c(out$coefficients[2], out$coefficients[3], out$coefficients[4])
    psilist[[i]]<-psi
  }else if(i==(T-3)){
    out<-lm(H~trt+Y_0+lag1_trt+lag2_trt+lag3_trt+lag1_Y+L+ps, data=dat.tcom)
    psi<-c(out$coefficients[2], out$coefficients[3], out$coefficients[4])
    psilist[[i]]<-psi
  }
  i<-i+1
}

#--------------------------
#Estimate treatment effects

#TE10 (A v no treatment): estimated treatment effects at times 1-5
gest.ce101<-psilist[[1]][1]
gest.ce102<-psilist[[1]][1]+psilist[[2]][1]
gest.ce103<-psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1]
gest.ce104<-psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1]+psilist[[4]][1]
gest.ce105<-psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1]+psilist[[4]][1]+
  psilist[[5]][1]

#TE20 (B v no treatment): estimated treatment effects at times 1-5
gest.ce201<-psilist[[1]][2]
gest.ce202<-psilist[[1]][2]+psilist[[2]][2]
gest.ce203<-psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2]
gest.ce204<-psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2]+psilist[[4]][2]
gest.ce205<-psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2]+psilist[[4]][2]+
  psilist[[5]][2]

#TE30 (A & B v no treatment): estimated treatment effects at times 1-5
gest.ce301<-psilist[[1]][3]
gest.ce302<-psilist[[1]][3]+psilist[[2]][3]
gest.ce303<-psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3]
gest.ce304<-psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3]+psilist[[4]][3]
gest.ce305<-psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3]+psilist[[4]][3]+
  psilist[[5]][3]

#TE21 (B v A): estimated treatment effects at times 1-5
gest.ce211<-psilist[[1]][2]-psilist[[1]][1]
gest.ce212<-(psilist[[1]][2]+psilist[[2]][2])-(psilist[[1]][1]+psilist[[2]][1])
gest.ce213<-(psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2])-
              (psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1])
gest.ce214<-(psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2]+psilist[[4]][2])-
              (psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1]+psilist[[4]][1])
gest.ce215<-(psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2]+psilist[[4]][2]+
               psilist[[5]][2])-
            (psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1]+psilist[[4]][1]+
               psilist[[5]][1])

#TE31 (A & B v A): estimated treatment effects at times 1-5
gest.ce311<-psilist[[1]][3]-psilist[[1]][1]
gest.ce312<-(psilist[[1]][3]+psilist[[2]][3])-(psilist[[1]][1]+psilist[[2]][1])
gest.ce313<-(psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3])-
  (psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1])
gest.ce314<-(psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3]+psilist[[4]][3])-
  (psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1]+psilist[[4]][1])
gest.ce315<-(psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3]+psilist[[4]][3]+
               psilist[[5]][3])-
            (psilist[[1]][1]+psilist[[2]][1]+psilist[[3]][1]+psilist[[4]][1]+
               psilist[[5]][1])

#TE32 (A & B v B): estimated treatment effects at times 1-5
gest.ce321<-psilist[[1]][3]-psilist[[1]][2]
gest.ce322<-(psilist[[1]][3]+psilist[[2]][3])-(psilist[[1]][2]+psilist[[2]][2])
gest.ce323<-(psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3])-
  (psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2])
gest.ce324<-(psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3]+psilist[[4]][3])-
  (psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2]+psilist[[4]][2])
gest.ce325<-(psilist[[1]][3]+psilist[[2]][3]+psilist[[3]][3]+psilist[[4]][3]+
               psilist[[5]][3])-
            (psilist[[1]][2]+psilist[[2]][2]+psilist[[3]][2]+psilist[[4]][2]+
               psilist[[5]][2])

#-------------------------------------------------------------------------------
#G-formula

#Redefine time variable (requirement to start from 0 for gfoRmula package)
dat$k<-dat$k-1

#Define new data sets for different years
dat5<-dat
dat4<-dat[dat$k<4,]
dat3<-dat[dat$k<3,]
dat2<-dat[dat$k<2,]
dat1<-dat[dat$k<1,]

#Convert data into a data.table (needed for gfoRmula)
setDT(dat1); setDT(dat2); setDT(dat3); setDT(dat4); setDT(dat5)

#Define interventions
intvars <- list("trt", "trt", "trt", "trt")
int_descript <- c('Treat0', 'Treat1', 'Treat2', 'Treat3')
int_times5 <- list(list(0:4), list=(0:4), list=(0:4), list=(0:4))
int_times4 <- list(list(0:3), list=(0:3), list=(0:3), list=(0:3))
int_times3 <- list(list(0:2), list=(0:2), list=(0:2), list=(0:2))
int_times2 <- list(list(0:1), list=(0:1), list=(0:1), list=(0:1))
interventions5 <- list(list(c(static, rep("0",5))),list(c(static, rep("1",5))),
                       list(c(static, rep("2",5))),list(c(static, rep("3",5))))
interventions4 <- list(list(c(static, rep("0",4))),list(c(static, rep("1",4))),
                       list(c(static, rep("2",4))),list(c(static, rep("3",4))))
interventions3 <- list(list(c(static, rep("0",3))),list(c(static, rep("1",3))),
                       list(c(static, rep("2",3))),list(c(static, rep("3",3))))
interventions2 <- list(list(c(static, rep("0",2))),list(c(static, rep("1",2))),
                       list(c(static, rep("2",2))),list(c(static, rep("3",2))))

#G-formula for 5 year treatment effects
gform<- gformula_continuous_eof(obs_data = dat5,
                                id = "id",
                                time_name = "k",
                                outcome_name = "Y",
                                basecovs=c("Y_0"),
                                covnames = c("trt","L", "lag1_Y"),
                                covtypes = c("categorical", "normal", "normal"),
                                histories = c(lagged), 
                                histvars = list(c("trt", "L", "lag1_Y")),
                                covparams = list(covmodels=c(
                                  trt~lag1_trt+L+lag1_Y,
                                  L~lag1_L+lag1_trt+lag1_Y,
                                  lag1_Y~lag1_trt+lag1_trt+lag2_trt+lag3_trt+
                                    lag4_trt+lag1_L+lag1_lag1_Y
                                )), 
                                ymodel = Y~trt+lag1_trt+lag2_trt+lag3_trt+
                                  lag4_trt+lag1_Y+L,
                                intvars = intvars,
                                interventions = interventions5,
                                int_times = int_times5,
                                int_descript = int_descript,
                                nsimul = 10000, seed = 07122010)

#Treatment effects:
gform.ce105<-as.numeric(gform$result[3,4]-gform$result[2,4])
gform.ce205<-as.numeric(gform$result[4,4]-gform$result[2,4])
gform.ce305<-as.numeric(gform$result[5,4]-gform$result[2,4])
gform.ce215<-as.numeric(gform$result[4,4]-gform$result[3,4])
gform.ce315<-as.numeric(gform$result[5,4]-gform$result[3,4])
gform.ce325<-as.numeric(gform$result[5,4]-gform$result[4,4])

#G-formula for 4 year treatment effects
gform<- gformula_continuous_eof(obs_data = dat4,
                                id = "id",
                                time_name = "k",
                                outcome_name = "Y",
                                basecovs=c("Y_0"),
                                covnames = c("trt","L", "lag1_Y"),
                                covtypes = c("categorical", "normal", "normal"),
                                histories = c(lagged), 
                                histvars = list(c("trt", "L", "lag1_Y")),
                                covparams = list(covmodels=c(
                                  trt~lag1_trt+L+lag1_Y,
                                  L~lag1_L+lag1_trt+lag1_Y,
                                  lag1_Y~lag1_trt+lag2_trt+lag3_trt+lag1_L+
                                    lag1_lag1_Y
                                )), 
                                ymodel = Y~trt+lag1_trt+lag2_trt+lag3_trt+
                                  lag1_Y+L,
                                intvars = intvars,
                                interventions = interventions4,
                                int_times = int_times4,
                                int_descript = int_descript,
                                nsimul = 10000, seed = 07122010)

#Treatment effects:
gform.ce104<-as.numeric(gform$result[3,4]-gform$result[2,4])
gform.ce204<-as.numeric(gform$result[4,4]-gform$result[2,4])
gform.ce304<-as.numeric(gform$result[5,4]-gform$result[2,4])
gform.ce214<-as.numeric(gform$result[4,4]-gform$result[3,4])
gform.ce314<-as.numeric(gform$result[5,4]-gform$result[3,4])
gform.ce324<-as.numeric(gform$result[5,4]-gform$result[4,4])

#G-formula for 3 year treatment effects
gform<- gformula_continuous_eof(obs_data = dat3,
                                id = "id",
                                time_name = "k",
                                outcome_name = "Y",
                                basecovs=c("Y_0"),
                                covnames = c("trt","L", "lag1_Y"),
                                covtypes = c("categorical", "normal", "normal"),
                                histories = c(lagged), 
                                histvars = list(c("trt", "L", "lag1_Y")),
                                covparams = list(covmodels=c(
                                  trt~lag1_trt+L+lag1_Y,
                                  L~lag1_L+lag1_trt+lag1_Y,
                                  lag1_Y~lag1_trt+lag2_trt+lag1_L+lag1_lag1_Y
                                )), 
                                ymodel = Y~trt+lag1_trt+lag2_trt+lag1_Y+L,
                                intvars = intvars,
                                interventions = interventions3,
                                int_times = int_times3,
                                int_descript = int_descript,
                                nsimul = 10000, seed = 07122010)

#Treatment effects:
gform.ce103<-as.numeric(gform$result[3,4]-gform$result[2,4])
gform.ce203<-as.numeric(gform$result[4,4]-gform$result[2,4])
gform.ce303<-as.numeric(gform$result[5,4]-gform$result[2,4])
gform.ce213<-as.numeric(gform$result[4,4]-gform$result[3,4])
gform.ce313<-as.numeric(gform$result[5,4]-gform$result[3,4])
gform.ce323<-as.numeric(gform$result[5,4]-gform$result[4,4])

#G-formula for 2 year treatment effects
gform<- gformula_continuous_eof(obs_data = dat2,
                                id = "id",
                                time_name = "k",
                                outcome_name = "Y",
                                basecovs=c("Y_0"),
                                covnames = c("trt","L", "lag1_Y"),
                                covtypes = c("categorical", "normal", "normal"),
                                histories = c(lagged), 
                                histvars = list(c("trt", "L", "lag1_Y")),
                                covparams = list(covmodels=c(
                                  trt~lag1_trt+L+lag1_Y,
                                  L~lag1_L+lag1_trt+lag1_Y,
                                  lag1_Y~lag1_trt+lag1_L+lag1_lag1_Y
                                )), 
                                ymodel = Y~trt+lag1_trt+lag1_Y+L,
                                intvars = intvars,
                                interventions = interventions2,
                                int_times = int_times2,
                                int_descript = int_descript,
                                nsimul = 10000, seed = 07122010)

#Treatment effects:
gform.ce102<-as.numeric(gform$result[3,4]-gform$result[2,4])
gform.ce202<-as.numeric(gform$result[4,4]-gform$result[2,4])
gform.ce302<-as.numeric(gform$result[5,4]-gform$result[2,4])
gform.ce212<-as.numeric(gform$result[4,4]-gform$result[3,4])
gform.ce312<-as.numeric(gform$result[5,4]-gform$result[3,4])
gform.ce322<-as.numeric(gform$result[5,4]-gform$result[4,4])

#Treatment effects:
gform.ce102<-as.numeric(gform$result[3,4]-gform$result[2,4])
gform.ce202<-as.numeric(gform$result[4,4]-gform$result[2,4])
gform.ce302<-as.numeric(gform$result[5,4]-gform$result[2,4])
gform.ce212<-as.numeric(gform$result[4,4]-gform$result[3,4])
gform.ce312<-as.numeric(gform$result[5,4]-gform$result[3,4])
gform.ce322<-as.numeric(gform$result[5,4]-gform$result[4,4])

#G-formula for 1 year treatment effects

#Set up datasets
trt0<-dat1; trt0$trt<-"0"
trt1<-dat1; trt1$trt<-"1"
trt2<-dat1; trt2$trt<-"2"
trt3<-dat1; trt3$trt<-"3"

ymodel<-lm(Y~as.factor(trt)+L+Y_0, dat=dat1)

#Obtain expected potential outcomes
E_Ya0<-mean(predict(ymodel, newdata=trt0))
E_Ya1<-mean(predict(ymodel, newdata=trt1))
E_Ya2<-mean(predict(ymodel, newdata=trt2))
E_Ya3<-mean(predict(ymodel, newdata=trt3))

#Treatment effects:
gform.ce101<-E_Ya1-E_Ya0
gform.ce201<-E_Ya2-E_Ya0
gform.ce301<-E_Ya3-E_Ya0
gform.ce211<-E_Ya2-E_Ya1
gform.ce311<-E_Ya3-E_Ya1
gform.ce321<-E_Ya3-E_Ya2
