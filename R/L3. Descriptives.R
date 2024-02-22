#3. Descriptive

# Load packages -----------------------------------------------------------


install.packages(glue)
library(glue)
library(scales)


# Load data ---------------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))



# Table 1 -----------------------------------------------------------------
# Create a matrix to feed results into
table <- matrix(NA,nrow=20, ncol=3) #fit rows and table numbers to content.
#run analyses and get estimate (x), lowerCI, upperCI or percentiles
x<-number(x, accuracy=0.01) #will specify a new object X from the last run code with 2dp
lower<-number(10p, accuracy=0.01) #will specify 10 percentile with 2dp
upper<-number(90p, accuracy=0.01) #will specify 90 percentile with 2dp
percent <- number(x/ nrows() *100, accuracy = 0.01) # will specify percentage with 2dp
5p<-number(x, accuracy=0.01)
95p<-number(x, accuracy=0.01)

table[1,1]<-"Characteristics"
table[1,2]<-"All participants"
table[1,3]<-"Individuals with NAFLD or NASH"
table[2,1] <-"Sex, female"
table[2,2]glue("{x}, {percent}")
table[2,3]glue("{x}, {percent}")
table[3,1] <- "Age at recruitment, years"
table[3,2]glue("{x} ({lower};{upper})")
table[3,3]glue("{x} ({lower};{upper})")
table[4,1] <-
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")
table[4,1]glue("{x} ({lower};{upper})")

glue("{x} ({lower};{upper})") #this will glue estimates defined earlier into my matrix-table. Should be done for all content before next code
table<-data.frame(table)
kable(table,"optionshere")





