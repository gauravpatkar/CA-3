
# Importing Environment Pollution dataset

environment_problem_df <- read.csv("D:/data science/New folder/gases.csv", header = TRUE)

# Using dplyr Package

library(dplyr)

# Data cleaning is performed and all NA's are removed from are dataset 

environment_problem_df <- na.omit(environment_problem_df)

# Checking structure of dataframe 

str(environment_problem_df)

#  Selecting three major columns i.e Gases, year when calculated and
#Total emiision in the same year

environment_problem_df <- environment_problem_df %>%
  select(Gases, year, Total.emissions)

# Grouping by the year and caluculating total emmision by all the four gases 

environment_problem_df <- data.frame(Groupn= rep(c(environment_problem_df$year)),
                 Total.emision = c(environment_problem_df$Total.emissions))

# Importing rainfall dataset

rainfall_df <- read.csv("rainfall_new.csv")

# Extracting specific columns as per the need 
# In our case it is yearand the reading recorded by  The Belmullet observatory

rainfall_df <- subset(rainfall_df, select = c(year ,Belmullet ,Valentia.Observatory))

# Further cleaning the dataset by removing NA's

rainfall_df <- na.omit(rainfall_df)

# In our dataset there are multiple entries of the same year 
# Hence, Grouping them together and taking mean of all the values will certainly 
# make our hypothesis more accurate

rainfall_df$year <- sub("^(\\d{4}).*$", "\\1", rainfall_df$year)

rainfall_df <- rainfall_df %>% group_by(year) %>% summarize(Belmullet = mean(Belmullet))

# merging two data sets

rainfall_and_environment_df <- merge(environment_problem_df, rainfall_df, by.x = "Groupn", by.y = "year")

rainfall_and_environment_df <- aggregate(rainfall_and_environment_df[, 2:3], list(rainfall_and_environment_df$Groupn), mean)


# Changing columns name to more appropriate name

names(rainfall_and_environment_df)[1]<-paste("Year")
names(rainfall_and_environment_df)[2]<-paste("Total_Emission")
names(rainfall_and_environment_df)[3]<-paste("Rainfall_Readings")

# Groupimg the dataframe by Total emmission and 
# rainfall reading for a logical comparison between two

rainfall_and_environment_df <- data.frame(
  Pollution = rep(c("Total_Emission", "Rainfall_Readings"), each = 16),
  Reading = c(rainfall_and_environment_df$Total_Emission, rainfall_and_environment_df$Rainfall_Readings),
  Year = c(rainfall_and_environment_df$Year))

install.packages("ggpubr")
library(ggpubr)

str(rainfall_and_environment_df)


ggboxplot(rainfall_and_environment_df, x = "Pollution", y = "Reading", color = "Pollution", palette = c("#00AFBB", "#E7B800"),
       ylab = "Reading", xlab = "Pollution")



# Shapiro-Wilk normality test for Men's weights
with(final_data, shapiro.test(Reading[Pollution == "Total Emission By $ Gases"]))# p = 0.1
# Shapiro-Wilk normality test for Women's weights
with(final_data, shapiro.test(Reading[Pollution == "Bellumet Observatory Rainfall Readings"])) # p = 0.6



res.ftest <- var.test(Reading ~ Pollution, data = final_data)
res.ftest

# Compute t-test
res <- t.test(Final$Total_Emission_By_Gases, Final$Bellumet_Observatory_Rainfall_Readings, var.equal = TRUE)
res

# Compute t-test
res <- t.test(Reading ~ Pollution, data = final_data, var.equal = TRUE)
res

# printing the p-value
res$p.value

# printing the mean
res$estimate

# printing the confidence interval
res$conf.int
