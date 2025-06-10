library(ggplot2)
library(dplyr)
library(corrplot)
library(reshape2)
library(RColorBrewer)
install.packages("missForest")
library(missForest)

install.packages(c("ggplot2", "reshape2"))



patients<- read.csv("/Users/arnenyecknyeck/Desktop/Statistical-Inference-Package-Class/patients.csv")
patients


##a Examining data structure

head(patients)
summary(patients)
colnames(patients)

## check data types
str(patients)

## check missing values 
colSums(is.na(data))

# 
Positive <- subset(patients, Diagnosis == 1)
head(Positive, 5)



## diagnosis

ggplot(patients, aes(x = factor(Diagnosis))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Count of Gestational Diabetes Diagnoses",
    x = "Diagnosis (0 = No, 1 = Yes)",
    y = "Count"
  ) +
  theme_minimal()



# Checking for skewness.
head(patients$Glucose)

## We can say that glucose is skewed left by looking at the graph.

ggplot(patients, aes(x = BloodPressure, fill = ..count..)) +
  geom_histogram(binwidth = 10, color = "black") +
  scale_fill_viridis_c() +
  labs(title = "Histogram of Blood Pressure",
       x = "Blood Pressure",
       y = "Count") +
  theme_minimal()
## We can say that Bp is skewed left by looking at the graph.....

ggplot(patients, aes(x = SkinThickness, fill = ..count..)) +
  geom_histogram(binwidth = 10, color = "black") +
  scale_fill_viridis_c() +
  labs(title = "Skin Thickness Histogram",
       x = "Skin Thickness",
       y = "Count") +
  theme_minimal()
## We can say that Bp is skewed left by looking at the graph.....concentration between...


ggplot(patients, aes(x = Insulin, fill = ..count..)) +
  geom_histogram(binwidth = 10, color = "black") +
  scale_fill_viridis_c() +
  labs(title = "Insulin Histogram",
       x = "Insulin",
       y = "Count") +
  theme_minimal()
## We can say that Bp is skewed left by looking at the graph.

ggplot(patients, aes(x = BMI, fill = ..count..)) +
  geom_histogram(binwidth = 2, color = "black") +
  scale_fill_viridis_c() +
  labs(title = "Histogram of BMI",
       x = "BMI",
       y = "Count") +
  theme_minimal()

## concentration tween 20-40


## transpore the data to observe structure
numeric_summary_wide <- patients %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    count = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    q25 = ~quantile(., 0.25, na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    q75 = ~quantile(., 0.75, na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  )))

print(numeric_summary_wide)

## plot graphs again
ggplot(patients, aes(x = Glucose)) + geom_histogram(binwidth = 10) + theme_minimal() 
## We can say that glucose is skewed left by looking at the graph.

ggplot(patients, aes(x = BloodPressure)) + geom_histogram(binwidth = 10) + theme_minimal()
## We can say that Bp is skewed left by looking at the graph.....

ggplot(patients, aes(x = SkinThickness)) + geom_histogram(binwidth = 10) + theme_minimal()
## We can say that Bp is skewed left by looking at the graph.....concentration between...


ggplot(patients, aes(x =Insulin )) + geom_histogram(binwidth = 10) + theme_minimal()
## We can say that Bp is skewed left by looking at the graph.

ggplot(patients, aes(x =BMI )) + geom_histogram(binwidth = 10) + theme_minimal()


library(ggplot2)

ggplot(Positive, aes(x = BloodPressure, y = Glucose)) +
  geom_point(color = "blue") +
  labs(title = "BloodPressure & Glucose",
       x = "BloodPressure",
       y = "Glucose") +
  theme_minimal()

ggplot(patients, aes(x = Age, y = Glucose, color = as.factor(Diagnosis))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     name = "Diagnosis",
                     labels = c("Negative", "Positive")) +
  labs(title = "Glucose vs Age Colored by Diagnosis",
       x = "Age",
       y = "Glucose") +
  theme_minimal()


# BloodPressure vs Age
ggplot(patients, aes(x = Age, y = BloodPressure, color = as.factor(Diagnosis))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     name = "Diagnosis",
                     labels = c("Negative", "Positive")) +
  labs(title = "BloodPressure vs Age Colored by Diagnosis",
       x = "Age",
       y = "BloodPressure") +
  theme_minimal()

# SkinThickness vs Age
ggplot(patients, aes(x = Age, y = SkinThickness, color = as.factor(Diagnosis))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     name = "Diagnosis",
                     labels = c("Negative", "Positive")) +
  labs(title = "SkinThickness vs Age Colored by Diagnosis",
       x = "Age",
       y = "SkinThickness") +
  theme_minimal()

# Insulin vs Age
ggplot(patients, aes(x = Age, y = Insulin, color = as.factor(Diagnosis))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     name = "Diagnosis",
                     labels = c("Negative", "Positive")) +
  labs(title = "Insulin vs Age Colored by Diagnosis",
       x = "Age",
       y = "Insulin") +
  theme_minimal()

# BMI vs Age
ggplot(patients, aes(x = Age, y = BMI, color = as.factor(Diagnosis))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     name = "Diagnosis",
                     labels = c("Negative", "Positive")) +
  labs(title = "BMI vs Age Colored by Diagnosis",
       x = "Age",
       y = "BMI") +
  theme_minimal()



# Assuming your data frame is called 'patients'
cor_matrix <- cor(patients[, sapply(patients, is.numeric)], use = "pairwise.complete.obs")
print(cor_matrix)


# Starting from patients data frame:
patients_corr <- patients[, sapply(patients, is.numeric)]

# Now calculate correlation matrix
cor_mat <- cor(patients_corr, use = "pairwise.complete.obs")

corrplot(cor_mat, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45)




### let's add some values

# Now use brewer.pal
cols <- colorRampPalette(brewer.pal(9, "YlGnBu"))(100)

# Then plot
corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                   tl.cex = 0.8,
                   addCoef.col = "black",
                   col = cols)

## CLEANING Random Forest.

# Check for factor 
str(patients) 
## convert
patients$Diagnosis <- as.factor(patients$Diagnosis)

set.seed(123)  
## Impute missing values.
imputed_result <- missForest(patients) 
# Out-of-bag imputation error
print(imputed_result$OOBerror)  


##########################################################
# Katie Section Begins

## Data Cleaning

#data <- patients (Continuing from Arne file)
datap <- read_csv("C:/Users/katie/Documents/CIS-690/patients.csv")

library(readr)
summary(datap)
str(datap)

# Replace 0s with NA in selected columns (where 0 indicates missing)
# Leaving pregnancies zeros and Diagnosis zeros as these are meaningful
datap$Insulin[datap$Insulin == 0] <- NA
datap$BMI[datap$BMI == 0] <- NA
datap$SkinThickness[datap$SkinThickness == 0] <- NA
datap$BloodPressure[datap$BloodPressure == 0] <- NA
datap$Pedigree[datap$Pedigree == 0] <- NA
datap$Glucose[datap$Glucose == 0] <- NA
datap$Age[datap$Age == 0] <- NA

#Count NAs
sum(is.na(datap))
#652 NAs total
sum(apply(datap, 1, function(row) any(is.na(row))))
#376 rows have a missing value
sum(datap == 0, na.rm = TRUE)
#611 zeros in total 

#Double checking that conversion was successful for each column

sum(datap$Age == 0, na.rm = TRUE)
#All Age zeros converted to NA
sum(datap$SkinThickness == 0, na.rm = TRUE)
#All SkinTckness zeros converted to NA
sum(datap$Insulin == 0, na.rm = TRUE)
#All Insulin zeros have been converted to NA
sum(datap$BMI == 0, na.rm = TRUE)
#All BMI zeros have been converted to NA
sum(datap$Pedigree == 0, na.rm = TRUE)
#All pedigree zeros have been converted to NA
sum(datap$Glucose == 0, na.rm = TRUE)
#All glucose zeros have been converted to NA
sum(datap$BloodPressure == 0, na.rm = TRUE)
#All blood pressure zeros have been converted to NA
sum(datap$Diagnosis == 0, na.rm = TRUE)
sum(datap$Diagnosis == 1, na.rm = TRUE)
#Diagnosis zeros have been left as zeros (Binary- positive or negative diagnosis)
#268 postive diagnosis in the dataset
#500 negative diagnosis in the dataset

## Correlation Matrix- check for multicollinearity
# Check if Arne has done this. 

#correlation matrix
install.packages("corrplot")
library(corrplot)

#Ignore NAs and visualize correlation between variables
cor_matrix2 <- cor(datap[, 1:9], use = "complete.obs")
print(cor_matrix2)

corrplot(cor_matrix2, method = "circle")
#Highly correlated features: 
#Age & number of pregnancies- 0.68
#Glucose & Insulin- 0.58
#BMI & skin thickness- 0.66
#Diagnosis & glucose- 0.515


## Investigate missingness
install.packages("naniar")
library(naniar)
# Show rows with any missing data
datap[!complete.cases(datap), ]
# Visualize missingness by variable
gg_miss_var(datap)
# Visualize missing data patterns
vis_miss(datap)

#NA & Zeros Totals
colSums(is.na(datap))
colSums(datap == 0, na.rm = TRUE)
#3 columns with <35 missing rows
#111 rows where pregnancies == 0
#500 rows where Diagnosis == 0

# Find rows where this count is 3 or more
(rows_4_or_more <- sum(zero_or_na_per_row >= 3))
#44 rows missing data from 4 or more columns

#Vizualize missingness together
gg_miss_upset(datap)
#SkinThickness and Insulin missing together 192 times. 
#Hypothesis: more invasive procedures. Only done for certain clients or appointment types. 

#Check if both missing is correlated with age or Pregnancies

# Add a column to flag rows where both are missing
data.missing <- datap %>%
  mutate(both_missing = is.na(SkinThickness) & is.na(Insulin))

ggplot(data.missing, aes(x = Age, fill = both_missing)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  scale_fill_manual(values = c("grey70", "red"), labels = c("Present", "Both Missing")) +
  labs(title = "Age Distribution by Missingness of SkinThickness & Insulin",
       x = "Age", y = "Count", fill = "Missing Pattern") +
  theme_minimal()
  
  ggplot(data.missing, aes(x = Pregnancies, fill = both_missing)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  scale_fill_manual(values = c("grey70", "red"), labels = c("Present", "Both Missing")) +
  labs(title = "Pregnancies Distribution by Missingness of SkinThickness & Insulin",
       x = "Pregnancies", y = "Count", fill = "Missing Pattern") +
  theme_minimal()
  
#No obvious pattern
#Doesn't give us information on why both of these are missing. 

## Are zeros in 'Pregnancy' errors? Or does this column ask about previous pregnancies, before current?

# Subset rows where Pregnancies == 0
preg0_df <- subset(datap, Pregnancies == 0)

# Count how many of those have Diagnosis == 1 (GDM)
gdm_with_preg0 <- sum(preg0_df$Diagnosis == 1, na.rm = TRUE)

# Total number of rows with Pregnancies == 0
total_preg0 <- nrow(preg0_df)

# Print results
cat("Total rows with Pregnancies == 0:", total_preg0, "\n")
cat("Rows with Pregnancies == 0 AND Diagnosis == 1 (GDM):", gdm_with_preg0, "\n")

#38 where pregnancy ==0 and diagnosis ==1. 
#We will assume 'Pregnancies' == 0 means prior pregnancies. 



## Cleaning

#We see that there are 33 rows where blood pressure, skin thickness, and insulin are missing together. We choose to remove these rows at leass than 4% of the data. 
# Remove rows with 3 or more NAs
df_clean <- datap[rowSums(is.na(datap)) < 3, ]

#Check for duplicates
any(duplicated(df_clean))

#Recheck NA's
colSums(is.na(df_clean))
# Identify columns with fewer than 6 NAs
cols_to_clean <- names(which(colSums(is.na(df_clean)) < 6))
cols_to_clean
# Remove rows with NAs in just those columns
df_clean <- df_clean %>%
  filter(if_all(all_of(cols_to_clean), ~ !is.na(.)))
#Recheck
colSums(is.na(df_clean))
#Only columns with NAs remaining are SkinThickness and Insulin

# Total number of rows
(total_rows <- nrow(df_clean))
#729 rows remaining after basic cleaning
# Percentage of missing SkinThickness
skin_missing_pct <- sum(is.na(df_clean$SkinThickness)) / total_rows * 100

# Percentage of missing Insulin
insulin_missing_pct <- sum(is.na(df_clean$Insulin)) / total_rows * 100

# Display the results
cat("Percentage of missing SkinThickness values:", round(skin_missing_pct, 2), "%\n")
cat("Percentage of missing Insulin values:", round(insulin_missing_pct, 2), "%\n")

#27% of Skin thickness values are missing.
#46% of Insulin values are missing. 

#Checking for outliers
vars_to_plot <- c("Insulin", "BMI", "SkinThickness", "BloodPressure", "Pedigree", "Glucose", "Age")

par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))

for (var_name in vars_to_plot) {
  boxplot(df_clean[[var_name]], 
          main = paste(var_name, "Boxplot"),
          horizontal = TRUE, 
          col = "lightblue",
          na.action = na.omit)
}

# Count and examine problematic values

# BloodPressure issues (0 or < 40)
bp_issues <- df_clean[ df_clean$BloodPressure < 40, ]
cat("BloodPressure < 40:", sum(df_clean$BloodPressure < 40, na.rm = TRUE), "\n")
cat("Total BloodPressure issues:", nrow(bp_issues), "\n\n")
#Remove 4 instances of blood pressure errors

# SkinThickness issues (0 values)
skin_issues <- df_clean[df_clean$SkinThickness < 5 | df_clean$SkinThickness > 60, ]
cat("SkinThickness < 5:", sum(df_clean$SkinThickness < 5, na.rm = TRUE), "\n")
cat("SkinThickness > 60:", sum(df_clean$SkinThickness > 60, na.rm = TRUE), "\n")
#Remove 2 Instances of skin thickness errors

# Glucose issues (< 50)
glucose_issues <- df_clean[df_clean$Glucose < 50, ]
cat("Glucose < 50:", nrow(glucose_issues), "\n\n")
#Remove 6 instances of glucose errors

#Age
print(which(df_clean_2$Age > 50))
print(which(df_clean_2$Age < 11))
#Inspect rows
print(as.data.frame(df_clean_2[c(8,  11,  12,  13,  21,  25,  27,  36,  40,  49,  61,  84, 106, 114, 119, 130, 137, 173,
                   190, 192, 193, 198, 207, 208, 221, 244, 247, 256, 260, 266, 267, 276, 281, 300, 322, 336,
                   337, 338, 349, 362, 375, 426, 428, 429, 444, 448, 455, 457, 462, 465, 475, 478, 482, 483,
                   485, 500, 505, 509, 515, 520, 523, 544, 546, 550, 561, 613, 615, 621, 629, 631, 655, 668,
                   670, 685, 708, 710, 714), ]))
                   
# Specify the row indices to remove
rows_to_remove <- c(8, 11, 12, 13, 21, 25, 27, 36, 40, 49, 61, 84, 106, 114, 119, 130, 137, 173,
                    190, 192, 193, 198, 207, 208, 221, 244, 247, 256, 260, 266, 267, 276, 281, 300,
                    322, 336, 337, 338, 349, 362, 375, 426, 428, 429, 444, 448, 455, 457, 462, 465,
                    475, 478, 482, 483, 485, 500, 505, 509, 515, 520, 523, 544, 546, 550, 561, 613,
                    615, 621, 629, 631, 655, 668, 670, 685, 708, 710, 714)

# Create new cleaned dataset by removing those rows
df_clean_2 <- df_clean[-rows_to_remove, ]

print(df_clean[c(419, 545, 16,  58, 118, 562, 564), ])
#row 419 looks normal comparing to bmi- keep

# Remove rows 545, 16, 58, 118, 562, 564
rows_to_remove <- c(545, 16, 58, 118, 562, 564)

# Create new dataframe without these rows
df_clean_2 <- df_clean_2[-rows_to_remove, ]

#Check
summary(df_clean_2)
str(df_clean_2)



## Imputation

#Create two different sets of data: One with Insulin and SkinThickness imputed with RF, and the other imputed with MICE

install.packages("mice")
library(mice)

