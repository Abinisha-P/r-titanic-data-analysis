# Titanic Passenger Data Analysis in R

Virtual R Data Analyst Internship project by **Abinisha P**.

This project follows one dataset, the Titanic passenger data, through the full data analysis cycle in R: data cleaning, visualization, statistical testing and predictive modeling. The goal is to understand who survived the disaster and why, and to build a model that predicts survival.

## Dataset

- Kaggle Titanic training file (also available in the R package `titanic` as `titanic_train`)
- 891 passengers, 12 original variables
- Target variable: `Survived` (0 = did not survive, 1 = survived)

## Project Structure

| File / Folder | Description |
|---|---|
| `week1_titanic_cleaning.R` | Data cleaning, outlier handling, scaling, encoding and correlations (Week 1) |
| `week2_titanic_visualization.R` | 10 ggplot2 charts with interpretation (Week 2) |
| `week3_titanic_modeling.R` | Hypothesis tests, logistic regression models, cross-validation and diagnostics (Week 3) |
| `plots_week1/` | Charts from Week 1 (8 charts) |
| `plots_week2/` | Charts from Week 2 (10 charts) |
| `plots_week3/` | Charts from Week 3 (7 charts) |
| `titanic_clean.csv` / `titanic_clean.rds` | Cleaned dataset created in Week 1 and reused in later weeks |
| `titanic_model_week3.rds` | Saved models from Week 3 |
| `report/Week4_Final_Report_Titanic_Analysis.docx` | Final report combining all four weeks |

## Workflow

1. **Week 1: Data cleaning.** Handled missing values (Cabin 77.1%, Age 19.9%, Embarked 0.22%), capped Fare outliers at the IQR upper fence (65.63), created `Title`, `FamilySize` and `HasCabin`, scaled numeric variables and encoded categorical ones.
2. **Week 2: Visualization.** Created bar charts, histograms, a boxplot, a scatter plot, line charts and a heatmap showing survival by class, sex, age, family size and title.
3. **Week 3: Statistical testing and modeling.** Tested six hypotheses (chi-square, t-test, Wilcoxon, ANOVA with Tukey, Kruskal-Wallis, Spearman), then built and compared two logistic regression models using an 80/20 train/test split and 10-fold cross-validation.
4. **Week 4: Final report.** Combined all work into one report with discussion, challenges, lessons learned and future directions.

## Key Findings

- 38.4% of passengers survived: 74.2% of women versus 18.9% of men.
- Survival by class: 1st class 63.0%, 2nd class 47.3%, 3rd class 24.2%.
- Sex (Cramer's V = 0.54) and passenger class (V = 0.34) are strongly linked to survival (p < 2.2e-16).
- Age is not significant in a simple t-test (p = 0.082), but is significant in the regression model once sex and class are controlled for.
- Best model: logistic regression with a class-by-sex interaction, with 10-fold CV accuracy of 82.1%, CV AUC of 0.861 and test accuracy of 80.3%.
- Main weakness: recall of about 65% to 68% at the default 0.5 threshold, so about one in three real survivors is missed.

## Requirements

R packages: `dplyr`, `ggplot2`, `corrplot`, `titanic`, `pROC`, `car`

Install them with:

```r
install.packages(c("dplyr", "ggplot2", "corrplot", "titanic", "pROC", "car"))
```

## How to Run

1. Clone or download this repository.
2. Open R or RStudio and set the working directory to the project folder:
   ```r
   setwd("path/to/r-titanic-data-analysis")
   ```
3. Run the scripts in order, because each one uses the output of the one before:
   ```r
   source("week1_titanic_cleaning.R")       # creates titanic_clean.rds
   source("week2_titanic_visualization.R")  # creates plots_week2/
   source("week3_titanic_modeling.R")       # creates plots_week3/ and the saved models
   ```

## Limitations and Future Work

- The results show associations, not causes.
- About 20% of ages were estimated using group medians.
- The test set is small (178 passengers), so small differences between models are within normal variation.
- Next steps: compare random forests or gradient boosting, improve age imputation, use regularized regression, and build an interactive Shiny dashboard.

