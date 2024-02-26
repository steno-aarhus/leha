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


# Load sorted-data --------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))

# Set survival time -------------------------------------------------------

# Set cut-off date for follow-up ------------------------------------------
# Estimated last follow-up date for ICD10 codes (stagnation of diagnoses)
# proportionen for hvornår der ikke kommer flere bælgfrugtsindtag
# Den første måling (folk nogensinde har) og antallet der spiser bælgfrugter?
#   % der spiser bælg - vælge start ud fra stagnering
# JA % - 25 - 30 - 40 - 40 -
#   Nej
#
# NASH, K75.8
# NAFLD, K76.0,



age at baseline
death
loss to follow-up
incident disease

event=matches(icd10_k80_date)


# Recoding covariables
# Alcohol as spline with 4 knots
df <- 4
sorted_data <- sorted_data %>%
    mutate(alcohol_spline = predict(bs(alcohol_intake, df = df, degree = 3, knots = NULL)))
# Entry into follow-up time should be middle of birthmonth


Age will be used as the underlying time scale in the analyses. Follow-up
time will begin with participants' last completed Oxford WebQ. As
participants in UKB are still followed-up today, participants will be
right censored at the date of the most recent registry update of full
follow-up for the outcomes. Otherwise, censoring will occur at the event
of death, loss to follow-up from the study, or date of diagnosis of
MASLD or MASH, whichever comes first. The substitution analyses will be
conducted with different adjustment levels.

#Cox regression
# Examples:
# cox <- coxph(Surv(time,event= death) ~ cenc0, <-- alle variable i modellen (kost, bælg, confoundere) kød/fisk/poultry er ikke med som variabel i analysen. Her skal man skalere sit indtag så det passer med modellen, fx pr 30g eller 30kcal
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
#Set survival time
data<-data %>%
    mutate(startdate=date of diagnosis,
           enddate=date of diagnosis OR date of death before end of follow-up OR lost to follow-up OR end-of-follow-up if after start and before end of follow-up ,
           time=difftime(enddate, startdate)/365.25) #time in years
# p41270,Diagnoses - ICD10,440017,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270
# p41280,Date of first in-patient diagnosis - ICD10,440014,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280
# #Fra web: The corresponding ICD-10 diagnosis codes can be found in data-field Field 41270 and the two fields can be linked using the array structure.
        #Skal være diagnoserne K75.8 (NASH) og K76.0 (NAFLD)
        #Hvordan fungerer koblingen mellem dato og diagnose?
# p191,Date lost to follow-up,1298,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=191
# p40000,Date of death,37897,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=40000

#NAFLD/NASH
coxnafld<-coxph(Surv(time, event=nafld)~exposure+covar1+covar2+covar3...,
                data=dataname,
                ties='breslow')
#Hvordan laver man stset med mine data?
#Hvad med eksponeringen, det er vel både fx. 30g legumes of 30g mindre kød? Hvordan gør man det? Er det noget ala legumes+30 og meats-30? Kan måske lade sig gøre.
Fortolkning:
    En måde: fjerner noget, men man kan ikke fjerne begge ting. Alt andet skal holdes lige, fx totalindtag eller totalenergi. Alt er inde i modellen, og så kan man trække kød og bælg fra hinanden og derved udtale sig om at spise mere af noget i stedet for det andet.

Den anden måde: alle fødevarer er med, bortset fra den man gerne vil "spise" mindre af. I den model har man også totalenergi eller totalvægt, som er alt lagt sammen. Det kan tolkes som, at det samlede indtag af mad/kcal skal være det samme for de personer man sammenligner, men den ene har et højere indtag af
Energiindtag skal være ens bortset fra indtaget af en fødevare, som ikke er med i modellen. Den ene fødevare bidrgaer stadig til totalindtag. Regressionskoef må komme fra den fødevare der ikke er inde i selve modellen, fordi det totale og alt andet, bortset fra den udtagne variabel, er med i modellen. Koefficienterne kan trækkes fra hinanden.


legume - meats
        poultry
        fish

Model 1

        Model check
Model 2

**Model 1** will be minimally adjusted for strata of age at recruitment
(\<45, 45-49, 50-54, 55-59, 60-64, ≤65 years) and geographical region of
recruitment (ten UK regions), sex, and intake of all other dietary
components apart from the substitute components (red and processed
meats; poultry; fish). When substituting g legumes/day, the unit for all
dietary components will be g/day and the analyses will be adjusted for
total amount of consumed foods in g/day. When substituting calories of
legumes, the unit for all dietary components will be calories/day and
the analyses will be adjusted for total amount of consumed calories/day.

**Model 2** will be further adjusted for alcohol consumption, ethnic
group (white, mixed, Asian, black, other, unknown), socioeconomic status
(Townsend deprivation score, educational level), living with a wife or
partner (yes, no), physical activity (low \[0-9.9 METs/week\], moderate
                                      \[10-49.9 METs/week\], and high \[≥50 METs/week\], unknown), smoking
status (never, former, current 1-15 cigarettes per day, current ≥15
        cigarettes per day, current but number of cigarettes per day unknown,
        and smoking status unknown), and self-reported diagnosis of diabetes,
hypertension, or high cholesterol (yes, no, unknown).

**Model 3** will further adjust for anthropometry (BMI ≥ 30 kg/m2), as
obesity may either confound or mediate the association between replacing
red and processed meats, poultry, or fish with legumes and risk of MASLD



# Create table with results

table2 <- data %>%
  select([outcome], [results]) %>%
  tbl_summary(by = nafld,
              statistic = list(all_continuous() ~  "{coef} ({ci_lower}, {ci_upper})", # for incidence rate
                                all_continuous() ~  "{coef} ({ci_lower}, {ci_upper})", # for HR
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 2. Risk of incident non-alcoholic fatty liver disease when substituting 30 g/day legumes for meat in UK Biobank cohort (N=126812)") %>%
  as_flex_table()

flextable::save_as_html(table2, path = here("doc", "table2.html"))
