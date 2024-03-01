library(tidyverse) library(ggplot2)
CovidData <- read_csv("/Users/chrischris/Desktop/690V/covid.csv")
View(CovidData)
# Create scatter plot
ggplot(CovidData, aes(x = gdp_per_capita, y = total_deaths_per_million)) +
  geom_point() + 
  labs(x = "GDP", y = "Total Deaths per Million", title = "Total Deaths per Million vs. GDP") +
  theme_minimal()  
#

Table_2 <- Table2[c("location", "gdp_per_capita", "GDP Status")]
> View(Table2)
> Table_1 <- CovidData[c("location", "gdp_per_capita", "GDP Status")]
> new_table <- data.frame(Table_1)
> view(Table_1)
> Table_2 <- Table_1 %>%
  +   distinct(location, .keep_all = FALSE)




