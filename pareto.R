master3 <- filter(master2, Country %in% c("China", "Russia", "Afghanistan","Russia","Norway","India","United States","Brazil"))  
  master3 <- filter(master3, date == "2021-08-01")
View(master3)
TDPM_Country=aggregate(data=master3,
                              total_deaths_per_million~Country,
                              FUN=sum)
View(master2)
TDPM_Country

TDPM_Country=TDPM_Country[order(-TDPM_Country$total_deaths_per_million),]
row.names(TDPM_Country)=NULL
head(TDPM_Country,10)


TDPM_Country$Percent=TDPM_Country$total_deaths_per_million/sum(TDPM_Country$total_deaths_per_million)
TDPM_Country$PercentCum=cumsum(TDPM_Country$Percent)
TDPM_Country$TDPM.Cum=cumsum(TDPM_Country$total_deaths_per_million)

TDPM_Country

library(ggQC) 

base=ggplot(data=TDPM_Country,
            aes(x=Country,y=total_deaths_per_million)) + # just the counts
  theme_classic()
paretoDraft=base + stat_pareto() 
paretoDraft