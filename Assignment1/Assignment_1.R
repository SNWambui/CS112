#Question 1:load the dataset into a dataframe 
un_data <- read.table(file = "https://tinyurl.com/UNpckpdata", header=TRUE, sep = ',', fill= TRUE, quote="")

#Print out the first 5 rows
head(un_data)

#Shows the structure of the dataframe
str(un_data)

#Question 2: Check if there are any null/empty values and print the total in each column
colSums(is.na(un_data))

#remove the empty/null values
un_data <- na.omit(un_data)

#confirm values have been removed
colSums(is.na(un_data))

#Question 3: change the column with dates to class 'date'
un_data$Last_Reporting_Date <- as.Date(un_data$Last_Reporting_Date, format = "%d/%m/%Y")
head(un_data)

#confirm that the class of the column has changed to 'Date' and show the structure of the dataframe
class(un_data$Last_Reporting_Date)
str(un_data)

#Question 4: Percentage of women serving in formed police units
unique(un_data$Personnel_Type)

#load the dplyr package to use the filter function
library(dplyr)

#filter the data by the Personnel type and the last reporting data so as to calculate the percentage
subset <-un_data %>%
  filter(Personnel_Type == "Formed Police Units", Last_Reporting_Date >= "2020-07-1")

#confirm dataset is filtered
head(subset)

#calculate the total number of female personnel in formed police units
female_fpolice <- sum(subset$Female_Personnel)
female_fpolice

#calculate the total number of total personnel in formed police units
male_fpolice <- sum(subset$Male_Personnel)
male_fpolice

#calculate the total number of personnel in formed police units
total_fpolice <- female_fpolice + male_fpolice
total_fpolice

#calculate the percentage of female personnel formed police units in 2020 
percentage_2020 <- (female_fpolice/total_fpolice * 100)
percentage_2020
#based on the value, the goal has not yet been achieved

#Question 5: Barplot showing the percentages of women in 2017, 2018, 2019 and 2020
#install and load the ggplot2 package to create a bar plot
install.packages('ggplot2')
library(ggplot2)

#filter 2019 data on formed police units
subset1 <-un_data %>%
  filter(Personnel_Type == "Formed Police Units", Last_Reporting_Date <= "2019-12-31" & Last_Reporting_Date > "2018-12-31")

#calculate the percentage of female personnel in formed police units in 2019
pecent_2019 <- (sum(subset1$Female_Personnel)/(sum(subset1$Male_Personnel) + sum(subset1$Female_Personnel))) * 100
pecent_2019

#filter 2018 data on formed police units
subset2 <-un_data %>%
  filter(Personnel_Type == "Formed Police Units", Last_Reporting_Date <= "2018-12-31" & Last_Reporting_Date > "2017-12-31")

#calculate the percentage of female personnel in formed police units in 2018
pecent_2018 <- (sum(subset2$Female_Personnel)/(sum(subset2$Male_Personnel) + sum(subset2$Female_Personnel))) * 100
pecent_2018

#filter 2017 data on formed police units
subset3 <-un_data %>%
  filter(Personnel_Type == "Formed Police Units", Last_Reporting_Date <= "2017-12-31" & Last_Reporting_Date > "2016-12-31")

#calculate the percentage of female personnel in formed police units in 2018
pecent_2017 <- (sum(subset3$Female_Personnel)/(sum(subset3$Male_Personnel) + sum(subset3$Female_Personnel))) * 100
pecent_2017

#create dataframe with years and percentages
df_bar <- data.frame(percentages = c(pecent_2017, pecent_2018, pecent_2019, percentage_2020), 
                     years = c('2017', '2018', '2019', '2020'))
ggplot(data = df_bar, aes(x=years, y=percentages)) +
         geom_bar(stat="identity", fill="steelblue")+
         geom_text(aes(label=percentages), vjust=-0.3, size=3.5)+
         theme_minimal()

#Question 6: ISO Function

#the function below takes the ISO code of a country and returns with a list of unique missions
#and number of missions personnel have been sent by the country
head(un_data$ISOCode3)
unique_missions_country <- function(iso){
  iso_df <- un_data %>%
    filter(ISOCode3 == iso)
  #trim whitespace in the missions column
  mission <- trimws(iso_df$Mission_Acronym, 'r')
  #get a list with unique missions
  unique_miss_list <- list(unique(mission))
  total_unique <- length(unique(mission))
  print(unique_miss_list)
  print(total_unique)
}

#confirm that function works by testing on what we already know
unique_missions_country('ARG')

#create a list with the countries we are interested in
iso_list <- list('USA', 'KOR', 'IND', 'DEU', 'ARG', 'GBR')
iso_list
#loop through the list above and call the unique missions functions
for(count in iso_list){
 unique_missions_country(count)
}

#Question 7: MINUSMA dataframe
minusma_df <- un_data%>%
  filter(Mission_Acronym == "MINUSMA")

#sum up the female and male personnel into one column
personell_total <- minusma_df$Female_Personnel + minusma_df$Male_Personnel

#Create dataframe with the personnel total by the Last_Reporting_Date
minusma_person <- aggregate(personell_total, by=list(DATE=minusma_df$Last_Reporting_Date), FUN=sum)

#Rename the columns in the new dataframe
minusma_person %>%
  rename(
    Total_Number_Of_Personnel = x
  )

#calculate the avergae of personnel over time
mean_minusma <- mean(minusma_person$x)
mean_minusma

#calculate the median personnel value
median_minusma <- median(minusma_person$x)
median_minusma

#calculate the 25th and 75th percentile for personnel
perc <- c(0.25, 0.75)
quantile_minusma <- quantile(minusma_person$x, perc)
quantile_minusma

#calculate the lowest personnel value and print out when it was
lowest_minusma <- min(minusma_person$x)
lowest_minusma_df <- minusma_person[minusma_person$x == lowest_minusma, c('DATE', 'x')]
lowest_minusma_df

#calculate the lowest personnel value and print out when it was
highest_minusma <- max(minusma_person$x)
highest_minusma_df <- minusma_person[minusma_person$x == highest_minusma, c('DATE', 'x')]
highest_minusma_df
