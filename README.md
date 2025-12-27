### Gestational Diabetes Risk Prediction

## REPOSITORY STRUCTURE
```bash

├── Data_Exploration_and_Preprocessing.Rmd
├── Logistic_Regression.Rmd
├── Random_Forest.Rmd
├── SVM.Rmd
├── XGBoost.Rmd
├── patients.csv
├── completed_data_mice.csv
├── completed_data_rf.csv
├── completed_data_rf_log_transformed_feature_subset_for_SVM.csv
├── patienstAnalysis copy.R
└── README.md
```
---
## Project Summary

This project designs an end-to-end data pipeline to prepare, transform, and model clinical healthcare data for predicting the risk of gestational diabetes mellitus (GDM). The focus is on data reliability, preprocessing at scale, and model-ready datasets, rather than only model accuracy.
The system demonstrates how raw healthcare data can be transformed into clean, validated, and feature-engineered datasets suitable for downstream machine learning and analytics use cases.

---
## Business / Clinical Objective

- Predict the likelihood of gestational diabetes during pregnancy

- Support early intervention and preventive care

- Improve decision-making for healthcare providers
---
## Dataset Description

The dataset (patients.csv) contains clinical and demographic information for women aged 21 years and older.

## FEATURES:

**Pregnancies** – Number of times pregnant

**Glucose** – Plasma glucose concentration (2-hour OGTT)

**BloodPressure** – Diastolic blood pressure (mm Hg)

**SkinThickness** – Triceps skin fold thickness (mm)

**Insulin** – 2-hour serum insulin (μU/ml)

**BMI** – Body mass index (kg/m²)

**Pedigree** – Diabetes pedigree function (family history)

**Age** – Age in years

## Target Variable

**Diagnosis** – Gestational diabetes diagnosis

1 = Yes

0 = No

---
## Project Workflow

The analysis follows a structured data science lifecycle:

Literature Review

Review of existing gestational diabetes prediction approaches

Data Exploration & Preprocessing

Exploratory Data Analysis (EDA)

Missing value handling (MICE imputation)

Feature transformations and normalization

Feature Selection & Engineering

Log transformations

Subset selection for model compatibility

Model Development

Logistic Regression

Random Forest

Support Vector Machine (SVM)

XGBoost

Model Evaluation & Validation

Accuracy, sensitivity, specificity

Model comparison and interpretation

Analysis & Limitations

Clinical relevance

Statistical assumptions

Data and model limitations

Reporting & Interpretation

Model insights

Recommendations for real-world use

---

| Model                  | File                      |
| ---------------------- | ------------------------- |
| Logistic Regression    | `Logistic_Regression.Rmd` |
| Random Forest          | `Random_Forest.Rmd`       |
| Support Vector Machine | `SVM.Rmd`                 |
| XGBoost                | `XGBoost.Rmd`             |
---
