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


