#4. Main analyses

#Load packages
library(tidyverse)
library(patchwork)
library(Hmisc)
library(survival)
library(lubridate)
library(Publish)
library(gtsummary)
library(ggsurvfit)


#create table for results
table<-matrix(NA,nrow=10, ncol=4) #fit rows and table numbers to content.
#run analyses and get estimate (x), lowerCI, upperCI or percentiles
x<-number(x, accuracy=0.01) #will specify a new object X from the last run code with 2dp
lower<-number(lowCI, accuracy=0.01) #will specify lower CI with 2dp
upper<-number(upperCI, accuracy=0.01) #will specify upper CI with 2dp
5p<-number(x, accuracy=0.01)
95p<-number(x, accuracy=0.01)

table[1,1]<-"Column1heading"
table[1,2]<-"column1content1"
table[1,3]<-"column1content2"
table[2,1]glue("{x} ({lower};{upper})") #this will glue estimates defined earlier into my matrix-table. The above hould be done for all content/estimates before next code
table<-data.frame(table)
kable(table,"oprionshere")

#Cox regression
# Examples:
# cox <- coxph(Surv(time,event= death) ~ cenc0,
#              data=azat,
#              ties='breslow')
# summary(cox) #gives estimate as log(HR)=coef, and HR=exp(coef)
# publish(cox) #to get estimate as HR with 95%CI
# tbl_regression(cox, exponentiate=TRUE) #to get estimate as HR with 95%CI
#
# fit.azat <- survfit(Surv(time,event= death) ~ cenc0,
#                     data=azat,
#                     conf.type = 'log-log')
#
# #log-log plot
# plot(fit.azat,
#      col=1:2,
#      fun='cloglog',
#      main='Ex 8.2, log-log plot')
# And to check the second assumption of linearity between log hazards (=HR) and each covariate (residual plot)
#
# azat$res<-predict(cox)
# g1<-azat %>%
#     ggplot(aes(res,age)) +
#     geom_point()+
#     geom_smooth(method='coxph', formula= y~x)
#
# g2<-azat %>%
#     ggplot(aes(res,logb0)) +
#     geom_point()+
#     geom_smooth(method='coxph', formula= y~x)
#
# g3<-azat %>%
#     ggplot(aes(res,alb0)) +
#     geom_point()+
#     geom_smooth(method='coxph', formula= y~x)
#
# g1+g2+g3

#Set survival time
data<-data %>%
    mutate(startdate=date of diagnosis,
           enddate=date of diagnosis OR date of death before end of follow-up OR lost to follow-up OR end-of-follow-up if after start and before end of follow-up ,
           time=difftime(enddate, startdate)/365.25) #time in years
# p41270,Diagnoses - ICD10,440017,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270
# p41280,Date of first in-patient diagnosis - ICD10,440014,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280
# #Fra web: The corresponding ICD-10 diagnosis codes can be found in data-field Field 41270 and the two fields can be linked using the array structure.

# p41272,Operative procedures - OPCS4,440159,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41272
# p41282,Date of first operative procedure - OPCS4,440153,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41282
        #Hvordan fungerer koblingen mellem dato og diagnose?

# p191,Date lost to follow-up,1298,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=191
# p40000,Date of death,37897,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=40000

#Gallbladder
coxstone<-coxph(Surv(time, event=gallstone)~exposure+covar1+covar2+covar3...,
                data=dataname,
                ties='breslow')
#Hvordan laver man stset med mine data?
#Hvad med eksponeringen, det er vel både fx. 30g legumes of 30g mindre kød? Hvordan gør man det? Er det noget ala legumes+30 og meats-30?
legume - meats
        poultry
        fish

Model 1

        Model check
Model 2

