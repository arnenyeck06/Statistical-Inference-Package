patients<- read.csv("/Users/arnenyecknyeck/Desktop/Statistical-Inference-Package-Class/patients.csv")
patients

head(patients)

summary(patients)
colnames(patients)
## check data types
str(patients)
## abnormatlities are glucos, blood pressure, skin thickness, insulin, BMI cant be zero

# Convert Diagnosis to categorical factor
patients$Diagnosis <- factor(patients$Diagnosis, levels = c(0, 1), labels = c("Negative", "Positive"))

## mean
meanp(patients$Pregnancies)

## median
median(patients$Pregnancies)

## sd
sd(patients$Pregnancies)

quantile(patients$Pregnancies, probs = c(0.25, 0.5, 0.75))

# Columns you want to analyze
## Pregnancies
## Pregnancies
mean(patients$Pregnancies, na.rm = TRUE)
median(patients$Pregnancies, na.rm = TRUE)
sd(patients$Pregnancies, na.rm = TRUE)
quantile(patients$Pregnancies, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

## Glucose
mean(patients$Glucose, na.rm = TRUE)
median(patients$Glucose, na.rm = TRUE)
sd(patients$Glucose, na.rm = TRUE)
quantile(patients$Glucose, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

## BloodPressure
mean(patients$BloodPressure, na.rm = TRUE)
median(patients$BloodPressure, na.rm = TRUE)
sd(patients$BloodPressure, na.rm = TRUE)
quantile(patients$BloodPressure, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

## SkinThickness
mean(patients$SkinThickness, na.rm = TRUE)
median(patients$SkinThickness, na.rm = TRUE)
sd(patients$SkinThickness, na.rm = TRUE)
quantile(patients$SkinThickness, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

## Insulin
mean(patients$Insulin, na.rm = TRUE)
median(patients$Insulin, na.rm = TRUE)
sd(patients$Insulin, na.rm = TRUE)
quantile(patients$Insulin, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)


### to discuss ranges and distribution,we look at graph first?
hist(patients$Glucose, main = "Glucose", xlab = "Glucose", col = "skyblue")



hist(patients$Pregnancies,
     main = "Distribution of Pregnancies",
     xlab = "Pregnancies",
     col = "skyblue",
     border = "white")

hist(patients$BloodPressure,
     main = "Distribution of Blood Pressure",
     xlab = "Blood Pressure",
     col = "skyblue",
     border = "white")


hist(patients$SkinThickness,
     main = "Distribution of Skin Thickness",
     xlab = "Skin Thickness",
     col = "skyblue",
     border = "white")

hist(patients$Insulin,
     main = "Distribution of Insulin",
     xlab = "Insulin",
     col = "skyblue",
     border = "white")

hist(patients$BMI,
     main = "Distribution of BMI",
     xlab = "BMI",
     col = "skyblue",
     border = "white")

hist(patients$Pedigree,
     main = "Distribution of Diabetes Pedigree Function",
     xlab = "Pedigree",
     col = "skyblue",
     border = "white")

hist(patients$Age,
     main = "Distribution of Age",
     xlab = "Age",
     col = "skyblue",
     border = "white")


## boxplot
boxplot(patients$BMI, main = "BMI Boxplot", y = "BMI")
boxplot(BMI ~ Diagnosis, data = patients, main = "BMI by Diagnosis", col = c("red", "green"))





## BMI
mean(patients$BMI, na.rm = TRUE)
median(patients$BMI, na.rm = TRUE)
sd(patients$BMI, na.rm = TRUE)
quantile(patients$BMI, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

## Pedigree
mean(patients$Pedigree, na.rm = TRUE)
median(patients$Pedigree, na.rm = TRUE)
sd(patients$Pedigree, na.rm = TRUE)
quantile(patients$Pedigree, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

## Age
mean(patients$Age, na.rm = TRUE)
median(patients$Age, na.rm = TRUE)
sd(patients$Age, na.rm = TRUE)
quantile(patients$Age, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
