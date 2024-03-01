master <- read.csv("owid-covid-data.csv")
master <- rename(master, Country = location)

master2 <- master |>
  mutate(GDP_Status = case_when(
    gdp_per_capita >= 35000 ~ "High",
    gdp_per_capita >= 20000 ~ "Average",
    gdp_per_capita >= 10000 ~ "Low",
    gdp_per_capita < 10000 ~ "Lowest")
  )

master2 <- select(master2,Country,date,total_cases_per_million,total_deaths_per_million,gdp_per_capita, GDP_Status)
master2 <- rename(master2,GDP_per_capita = gdp_per_capita)
master2 <- filter(master2, date == "2021-08-01")
master2 <- filter(master2, !Country %in% c("World", "Africa", "Europe", "High income", "Low income",
                                           "Lower middle income", "Micronesia (country)",
                                           "North America", "Oceania", "Reunion",
                                           "Saint Martin (French part)", "South America",
                                           "Upper middle income", "United States Virgin Islands",
                                           "Sint Maarten (Dutch part)", "Equatorial Guinea"))



str(master2,width = 70,strict.width='cut')
absoluteT=table(master2$GDP_Status,
                exclude = 'nothing')

names(absoluteT)[5]='Unknown'
propT=prop.table(absoluteT)*100

(tableFreq=as.data.frame(absoluteT))

names(tableFreq)=c("GDP_status","Count")
tableFreq$Percent=as.vector(propT)
tableFreq

# original before reorderbase= ggplot(data = tableFreq, 
             #aes(x = GDP_status,
                 #y = Percent))

LABELS=paste0(round(tableFreq$Percent,2), '%')
base= ggplot(data = tableFreq, 
             aes(x = reorder(GDP_status,Percent),y = Percent)) 
base= base + theme_classic()

plot1 = base + geom_bar(fill ="blue",
                        stat = 'identity') 

titleText='What are the GDP status of Countries affected by Covid-19?'
sub_titleText='From Per Capita GDP'
sourceText='Source: ourworldindata.org'
x.AxisText="GDP Status"
y.AxisText="Percent"

plot2 = plot1 + labs(title=titleText,
                     subtitle = sub_titleText,
                     x = NULL, 
                     y = NULL,
                     caption = sourceText) 
plot2

base= ggplot(data = tableFreq, 
             aes(x = GDP_status,
                 y = Percent)) 

plot1 = base + geom_bar(fill ="blue",
                        stat = 'identity') 

plot2 = plot1 + labs(title=titleText,
                     x = NULL, 
                     y = NULL,
                     caption = sourceText)

plot3 = plot2 + geom_hline(yintercept = 15, 
                           linetype="dashed", 
                           linewidth=1.5, 
                           alpha=0.5) 
plot3

library(scales)

plot4 = plot3 + scale_y_continuous(breaks=c(0,10,25,40),
                                   limits = c(0, 40), 
                                   labels=unit_format(suffix = '%')) 
plot4

plot5 = plot4 + theme(plot.caption = element_text(hjust = 0), 
                      plot.title = element_text(hjust = 0.5))
plot5

LABELS=paste0(round(tableFreq$Percent,2), '%')

plot6 = plot5 + geom_text(vjust=0, 
                          size = 5,
                          aes(y = Percent ,
                              label = LABELS))
plot6


