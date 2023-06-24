#1 
library(dplyr) 
#2 
dataset <- read.csv("inclusive_society.csv") 
#3 
summary(dataset) 
#4 
n <- nrow(dataset) 
#5 
for(i in 1:n){ 
  dataset$Inclusiveness[i] <- 0 
} 
#6 
#filtering dataset 
good_frame <- filter(dataset, factor == "good") 
#7 
#subsetting data 
good_sentiments <- good_frame[, c("Gender", "Income", "Age")] 
#8 
#calculating the number of positive sentiments by gender 
positive_sentiment_by_gender <- as.data.frame(table(good_sentiments$Gender)) 
#9 
#calculating the number of positive sentiments by income 
positive_sentiment_by_income <- as.data.frame(table(good_sentiments$Income)) 
#10 
#calculating the number of positive sentiments by age 
positive_sentiment_by_age <- as.data.frame(table(good_sentiments$Age)) 
#11 
#creating a function to calculate the inclusiveness score 
calculate_inclusiveness <- function(x, y, z) { 
  (x + y + z)/3 
} 
#12
#loop through the data 
for(i in 1:n) { 
  gender <- dataset$Gender[i] 
  income <- dataset$Income[i] 
  age <- dataset$Age[i] 
  dataset$Inclusiveness[i] <- calculate_inclusiveness( 
    positive_sentiment_by_gender[gender], 
    positive_sentiment_by_income[income], 
    positive_sentiment_by_age[age] 
  ) 
} 
#13 
#saving the data 
write.csv(dataset, "inclusive_society_updated.csv") 
#14 
#plotting the inclusiveness score 
plot(dataset$Inclusiveness, xlab = "Inclusiveness Score", ylab = "Number of Entries", 
     col = "red", lwd = 2, cex = 0.8, xlim = c(0,1), ylim = c(0, n)) 
#15 
#checking how many entries are more inclusive than 0.5 
inclusive_entries <- nrow(filter(dataset, Inclusiveness > 0.5)) 
#16 
percent_inclusive_entries <- (inclusive_entries/n)*100 
#17 
#printing the result 
cat("The number of entries which are more inclusive than 0.5 is", inclusive_entries) 
cat("The percentage of entries which are more inclusive than 0.5 is", percent_inclusive_entries) 
#18 
#creating summary statistics 
summary_stats <- as.data.frame(t(tapply(dataset$Inclusiveness, dataset$Gender, summary))) 
#19 
#plotting the mean inclusiveness by gender 
barplot(summary_stats$Mean, names.arg = summary_stats$Gender, xlab = "Gender", 
        ylab = "Inclusiveness Score", col = c("red", "blue")) 
#20 
#reading the file with updated values 
updated_dataset <- read.csv("inclusive_society_updated.csv") 
#21 
#writing the updated file to csv 
write.csv(updated_dataset, "inclusive_society_updated.csv") 
#22 
#creating a function to calculate the average inclusiveness of all entries 
calculate_total_inclusiveness <- function(data) { 
  mean(data$Inclusiveness) 
} 
#23 
#calculating the average inclusiveness of all entries 
total_inclusiveness <- calculate_total_inclusiveness(updated_dataset) 
#24 
#printing the result 
cat("The average inclusiveness of all entries is", total_inclusiveness) 
#25 
#creating a new column in the dataset 
updated_dataset$Classification <- as.character(rep("NA", nrow(updated_dataset))) 
#26 
#adding values to the Classification column 
updated_dataset$Classification[updated_dataset$Inclusiveness > total_inclusiveness] <- "High" 
updated_dataset$Classification[updated_dataset$Inclusiveness < total_inclusiveness] <- "Low" 
#27 
#creating a function to calculate the number of high and low inclusive values 
calculate_inclusiveness_counts <- function(data) { 
  High <- nrow(data[data$Classification == "High"]) 
  Low <- nrow(data[data$Classification == "Low"]) 
  return(list(High, Low)) 
} 
#28 
#calculating the number of high and low inclusive values 
inclusiveness_counts <- calculate_inclusiveness_counts(updated_dataset) 
#29 
#printing the result 
cat("The number of entries which are highly inclusive is", inclusiveness_counts$High) 
cat("The number of entries which are not highly inclusive is", inclusiveness_counts$Low) 
#30 
#creating a chart to visualise the inclusiveness of the entries 
pie(c(inclusiveness_counts$High, inclusiveness_counts$Low), 
    labels = c("High", "Low"), col = c("red", "blue"), 
    main = "Inclusiveness of Entries") 
#31 
#creating a function to calculate the proportion of inclusive entries 
calculate_inclusiveness_percentage <- function(data) { 
  tot <- nrow(data) 
  High <- nrow(data[data$Classification == "High"]) 
  return((High/tot)*100) 
} 
#32 
#calculating the proportion of inclusive entries 
inclusiveness_percentage <- calculate_inclusiveness_percentage(updated_dataset) 
#33 
#printing the result 
cat("The percentage of entries which are highly inclusive is", inclusiveness_percentage) 
#34 
#calculating the proportion of non-inclusive entries 
non_inclusiveness_percentage <- 100 - inclusiveness_percentage 
#35 
#printing the result 
cat("The percentage of entries which are not highly inclusive is", non_inclusiveness_percentage) 
#36 
#creating a chart to visualise the percentage of inclusive and non-inclusive entries 
pie(c(inclusiveness_percentage, non_inclusiveness_percentage), 
    labels = c("Highly Inclusive", "Not Highly Inclusive"), 
    col = c("red", "blue"), main = "Proportion of Entries") 
#37 
#creating a function to print the summary 
print_report <- function(counts, percentage) { 
  cat("The number of entries which are highly inclusive is", counts$High, "\n") 
  cat("The number of entries which are not highly inclusive is", counts$Low, "\n") 
  cat("The percentage of entries which are highly inclusive is", percentage, "\n") 
  cat("The percentage of entries which are not highly inclusive is", 
      100-percentage, "\n") 
} 
#38 
#printing the summary 
print_report(inclusiveness_counts, inclusiveness_percentage) 
#39 
#creating a table to visualise the summary 
table(updated_dataset$Classification, updated_dataset$Gender) 
#40 
#creating a function to calculate the average income of the highly inclusive entries 
calculate_income_of_inclusives <- function(data) { 
  mean(data$Income[data$Classification == "High"]) 
} 
#41 
#calculating the average income of the highly inclusive entries 
income_of_inclusives <- calculate_income_of_inclusives(updated_dataset) 
#42 
#printing the result 
cat("The average income of highly inclusive entries is", income_of_inclusives) 
#43 
#calculating the average income of the non-highly inclusive entries 
income_of_non_inclusives <- mean(updated_dataset$Income[updated_dataset$Classification == "Low"]) 
#44 
#printing the result 
cat("The average income of non-highly inclusive entries is", income_of_non_inclusives) 
#45 
#creating a function to calculate the average age of the highly inclusive entries 
calculate_age_of_inclusives <- function(data) { 
  mean(data$Age[data$Classification == "High"]) 
} 
#46 
#calculating the average age of the highly inclusive entries 
age_of_inclusives <- calculate_age_of_inclusives(updated_dataset) 
#47 
#printing the result 
cat("The average age of highly inclusive entries is", age_of_inclusives) 
#48 
#calculating the average age of the non-highly inclusive entries 
age_of_non_inclusives <- mean(updated_dataset$Age[updated_dataset$Classification == "Low"]) 
#49 
#printing the result 
cat("The average age of non-highly inclusive entries is", age_of_non_inclusives) 
#50 
#creating a data frame of summary statistics 
summary_stats <- as.data.frame(cbind( 
  Means = c(income_of_inclusives, income_of_non_inclusives, age_of_inclusives, age_of_non_inclusives), 
  Groups = c("Highly Inclusive Income", "Non-Highly Inclusive Income", 
             "Highly Inclusive Age", "Non-Highly Inclusive Age") 
)) 
#51 
#plotting the summary statistics 
barplot(summary_stats$Means, names.arg = summary_stats$Groups, 
        xlab = "Groups", ylab = "Average Values", col = c("red", "blue")) 
#52 
#creating a function to calculate the proportion of people from each gender who are highly inclusive 
calculate_gender_proportions <- function(data) { 
  gender <- c("Female", "Male") 
  Female <- nrow(data[data$Gender == "Female" & data$Classification == "High"])/ 
            nrow(data[data$Gender == "Female"]) * 100 
  Male <- nrow(data[data$Gender == "Male" & data$Classification == "High"])/ 
          nrow(data[data$Gender == "Male"]) * 100
  return(list(Female, Male)) 
} 
#53 
#calculating the proportion of people from each gender who are highly inclusive 
gender_proportions <- calculate_gender_proportions(updated_dataset) 
#54 
#creating a data frame of gender proportions 
gender_proportion_df <- as.data.frame(cbind( 
  Proportion = c(gender_proportions$Female, gender_proportions$Male), 
  Gender = c("Female", "Male") 
)) 
#55 
#plotting the gender proportions 
barplot(gender_proportion_df$Proportion, names.arg = gender_proportion_df$Gender, 
        xlab = "Gender", ylab = "Percentage", col = c("red", "blue")) 
#56 
#creating a function to calculate the inclusiveness of each income range 
calculate_income_inclusiveness <- function(data) { 
  income <- c("Low", "Medium", "High") 
  Low <- mean(data$Inclusiveness[data$Income == "Low"]) 
  Medium <- mean(data$Inclusiveness[data$Income == "Medium"]) 
  High <- mean(data$Inclusiveness[data$Income == "High"]) 
  return(list(Low, Medium, High)) 
} 
#57 
#calculating the inclusiveness of each income range 
income_inclusiveness <- calculate_income_inclusiveness(updated_dataset) 
#58 
#creating a data frame of the inclusiveness of each income range 
income_inclusiveness_df <- as.data.frame(cbind( 
  Inclusiveness = c(income_inclusiveness$Low, income_inclusiveness$Medium, income_inclusiveness$High), 
  Income = c("Low", "Medium", "High")
)) 
#59 
#plotting the inclusiveness of each income range 
barplot(income_inclusiveness_df$Inclusiveness, names.arg = income_inclusiveness_df$Income, 
        xlab = "Income Range", ylab = "Inclusiveness Score", 
        col = c("red", "blue")) 
#60 
#creating a function to calculate the inclusiveness of each age range 
calculate_age_inclusiveness <- function(data) { 
  age <- c("Young", "Middle", "Old") 
  Young <- mean(data$Inclusiveness[data$Age == "Young"]) 
  Middle <- mean(data$Inclusiveness[data$Age == "Middle"]) 
  Old <- mean(data$Inclusiveness[data$Age == "Old"]) 
  return(list(Young, Middle, Old)) 
} 
#61 
#calculating the inclusiveness of each age range 
age_inclusiveness <- calculate_age_inclusiveness(updated_dataset) 
#62 
#creating a data frame of the inclusiveness of each age range 
age_inclusiveness_df <- as.data.frame(cbind( 
  Inclusiveness = c(age_inclusiveness$Young, age_inclusiveness$Middle, age_inclusiveness$Old), 
  Age = c("Young", "Middle", "Old") 
)) 
#63 
#plotting the inclusiveness of each age range 
barplot(age_inclusiveness_df$Inclusiveness,