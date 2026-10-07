setwd("C:/Users/Alessia/Desktop/proiect_ssd")
date<- read.csv(file='setdedate.csv', header=TRUE, sep=',', dec='.', row.names = 1)
View(date) 

#completitudinea datelor
colSums(is.na(date))

#acuratete
#verificarea plajelor de valoripentru variabilele numerice
summary(date[, c("Unit.price", "Quantity", "Tax.5.", "Sales", "cogs", "gross.income", "Rating")])
#verificarea preciziei calculelor(taxa de 5%)
taxa_calculata <- date$cogs * 0.05
head(data.frame(Taxa_Originala = date$Tax.5., Taxa_Calculata = taxa_calculata, Diferenta = date$Tax.5. - taxa_calculata))
#verificarea constantei  marjei brute(gross margin percentage)
unique(date$gross.margin.percentage)
#verificarea limitelor logice pentru rating
range(date$Rating)
#verifivarea validitatii calendaristice
date$Date <- as.Date(date$Date, format="%m/%d/%Y")
summary(date$Date)
#verificarea integritatii categoriilor
table(date$Product.line)
table(date$City)
table(date$Payment)


#disponibilitatea la timp
#verificarea interalului temporal
range(date$Date)
#verificarea dstributiei tranzatiilor pe luni
date$Month <- format(date$Date, "%m")
table(date$Month)
#verificarea continuitatii
table(weekdays(date$Date))
#analiza disponibilitatii pe ore
range(date$Time)
#vizualizarea densitatii tranzactiilor in timp
hist(date$Date, breaks="weeks", freq=TRUE, 
     main="Disponibilitatea datelor pe săptămâni", 
     xlab="Perioada (Ianuarie - Martie 2019)", 
     col="purple4")


#validitate
#verificarea structurii tipurilor de date
str(date)
#verificarea incadrarii in categorii
invalid_gender <- date$Gender[!(date$Gender %in% c("Male", "Female"))]
invalid_customer <- date$Customer.type[!(date$Customer.type %in% c("Member", "Normal"))]

length(invalid_gender)
length(invalid_customer) 
#verificarea validitatii metodelor de plata
unique(date$Payment)
#verificarea formatlui orei
#install.packages("lubridate")
library(lubridate)
sum(is.na(parse_date_time(date$Time, orders = "HM")))
#verificarea corelatiei intre oras si filiala
table(date$City, date$Branch)

#consistenta si integritatea datelor
#verificarea uniformitatii preturilor pe produs
aggregate(Unit.price ~ Product.line, data = date, function(x) length(unique(x)))
#verificarea relatiei matematice intre cogs tax si sales
consistenta_calcul <- date$Sales - (date$cogs + date$Tax.5.)
sum(consistenta_calcul > 0.01)
#identificarea randurilor duplicate
sum(duplicated(date))
#verificarea unicitatii identificatorului de tranzactie
sum(duplicated(date$Invoice.ID))
#verificarea corelatiei intre unit price, quantity si cogs
verificare_cogs <- (date$Unit.price * date$Quantity) - date$cogs
max(abs(verificare_cogs))


#analiza structurii datelor
str(date)
summary(date)
#identificarea outlierilor
boxplot(date$Sales, 
        main="Identificarea Outlierilor: Vânzări", 
        col="pink", 
        horizontal=TRUE, 
        notch=TRUE)
boxplot(date$Rating, 
        main="Distribuția Rating-ului", 
        col="magenta4", 
        horizontal=TRUE)
#frecventele
prop.table(table(date$Product.line)) * 100
table(date$Gender, date$Customer.type)
#analiza de densitate
hist(date$Sales, 
     breaks=30, 
     col="purple3", 
     main="Histograma Vânzărilor", 
     xlab="Valoare Vânzare", 
     ylab="Frecvență")
