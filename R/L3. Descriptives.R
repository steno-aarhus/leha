#3. Descriptive
install.packages(glue)
library(glue)
library(scales)


#Table 1
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
table[2,1]glue("{x} ({lower};{upper})") #this will glue estimates defined earlier into my matrix-table. Should be done for all content before next code
table<-data.frame(table)
kable(table,"oprionshere")


https://www.danieldsjoberg.com/gtsummary/ --> kan være et godt alternativ også.
