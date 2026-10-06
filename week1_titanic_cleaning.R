# =====================================================
# Week 1: Data Cleaning and Preliminary Analysis (Titanic)
# =====================================================

# ---- 0. Setup ----
# install.packages(c("titanic", "dplyr", "ggplot2", "corrplot"))
library(titanic)
library(dplyr)
library(ggplot2)
library(corrplot)

dir.create("plots", showWarnings = FALSE)

# ---- 1. Load dataset ----
df <- titanic_train          # 891 passengers, 12 variables
cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")
head(df)

# ---- 2. Structure and summary ----
str(df)
summary(df)

# ---- 3. Missing values ----
# Empty strings count as missing too
df[df == ""] <- NA
missing_report <- data.frame(
  variable = names(df),
  n_missing = colSums(is.na(df)),
  pct_missing = round(colSums(is.na(df)) / nrow(df) * 100, 2)
)
print(missing_report[order(-missing_report$pct_missing), ])

# Visualize missingness
ggplot(missing_report, aes(x = reorder(variable, pct_missing), y = pct_missing)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Missing Values by Variable", x = "", y = "% Missing") +
  theme_minimal()
ggsave("plots/01_missing_values.png", width = 7, height = 5)

# ---- 4. Handle missing values ----
# Cabin: ~77% missing -> keep a flag, drop the column
df$HasCabin <- ifelse(is.na(df$Cabin), 0, 1)
df$Cabin <- NULL

# Age: impute with median by Pclass and Sex (more accurate than global median)
df <- df %>%
  group_by(Pclass, Sex) %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  ungroup()

# Embarked: impute with the mode
mode_embarked <- names(sort(table(df$Embarked), decreasing = TRUE))[1]
df$Embarked[is.na(df$Embarked)] <- mode_embarked

cat("Missing values remaining:", sum(is.na(df)), "\n")

# ---- 5. Feature engineering / drop irrelevant columns ----
df$Title <- gsub("(.*, )|(\\..*)", "", df$Name)
rare <- c("Dr", "Rev", "Col", "Major", "Capt", "Don", "Jonkheer",
          "the Countess", "Sir", "Lady", "Dona")
df$Title[df$Title %in% rare] <- "Rare"
df$Title[df$Title %in% c("Mlle", "Ms")] <- "Miss"
df$Title[df$Title == "Mme"] <- "Mrs"
df$FamilySize <- df$SibSp + df$Parch + 1
df <- df %>% select(-PassengerId, -Name, -Ticket)

# ---- 6. Outlier detection (IQR method) ----
detect_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr)
}
cat("Age outliers:", sum(detect_outliers(df$Age)), "\n")
cat("Fare outliers:", sum(detect_outliers(df$Fare)), "\n")

# Boxplots before treatment
png("plots/02_boxplots_before.png", width = 800, height = 500)
par(mfrow = c(1, 2))
boxplot(df$Age, main = "Age (before)", col = "lightblue")
boxplot(df$Fare, main = "Fare (before)", col = "lightgreen")
dev.off()

# Treat Fare outliers by capping (winsorizing) at 1.5*IQR upper fence
q <- quantile(df$Fare, c(0.25, 0.75))
upper <- q[2] + 1.5 * (q[2] - q[1])
df$Fare_capped <- pmin(df$Fare, upper)

png("plots/03_boxplots_after.png", width = 800, height = 500)
par(mfrow = c(1, 2))
boxplot(df$Fare, main = "Fare (original)", col = "lightgreen")
boxplot(df$Fare_capped, main = "Fare (capped)", col = "orange")
dev.off()

# ---- 7. Normalization ----
min_max <- function(x) (x - min(x)) / (max(x) - min(x))
df$Age_scaled <- min_max(df$Age)
df$Fare_scaled <- min_max(df$Fare_capped)
df$Age_z <- as.numeric(scale(df$Age))     # z-score alternative
summary(df[, c("Age_scaled", "Fare_scaled", "Age_z")])

# ---- 8. Encoding categorical variables ----
df$Survived <- factor(df$Survived, levels = c(0, 1), labels = c("No", "Yes"))
df$Pclass <- factor(df$Pclass, levels = c(1, 2, 3), ordered = TRUE)
df$Sex <- factor(df$Sex)
df$Embarked <- factor(df$Embarked)
df$Title <- factor(df$Title)

# Label encoding
df$Sex_num <- ifelse(df$Sex == "male", 1, 0)
# One-hot encoding
onehot <- model.matrix(~ Embarked + Title - 1, data = df)
df <- cbind(df, onehot)
str(df)

# ---- 9. Exploratory analysis ----
# Descriptive statistics
desc <- df %>% summarise(
  mean_age = mean(Age), median_age = median(Age), sd_age = sd(Age),
  mean_fare = mean(Fare), median_fare = median(Fare), sd_fare = sd(Fare)
)
print(desc)

# Survival rates
prop.table(table(df$Survived))
round(prop.table(table(df$Sex, df$Survived), 1) * 100, 1)
round(prop.table(table(df$Pclass, df$Survived), 1) * 100, 1)

# Plots
ggplot(df, aes(Survived, fill = Survived)) + geom_bar() +
  labs(title = "Survival Count") + theme_minimal()
ggsave("plots/04_survival_count.png", width = 6, height = 4)

ggplot(df, aes(Sex, fill = Survived)) + geom_bar(position = "fill") +
  labs(title = "Survival Rate by Sex", y = "Proportion") + theme_minimal()
ggsave("plots/05_survival_by_sex.png", width = 6, height = 4)

ggplot(df, aes(Pclass, fill = Survived)) + geom_bar(position = "fill") +
  labs(title = "Survival Rate by Passenger Class", y = "Proportion") +
  theme_minimal()
ggsave("plots/06_survival_by_class.png", width = 6, height = 4)

ggplot(df, aes(Age, fill = Survived)) + geom_histogram(bins = 30, alpha = 0.7) +
  labs(title = "Age Distribution by Survival") + theme_minimal()
ggsave("plots/07_age_hist.png", width = 7, height = 4)

# Correlation matrix (numeric variables)
num_vars <- df %>%
  mutate(Survived_num = as.numeric(Survived) - 1, Pclass_num = as.numeric(Pclass)) %>%
  select(Survived_num, Pclass_num, Age, SibSp, Parch, Fare_capped,
         FamilySize, HasCabin, Sex_num)
cor_mat <- cor(num_vars)
print(round(cor_mat, 2))

png("plots/08_correlation.png", width = 700, height = 700)
corrplot(cor_mat, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", number.cex = 0.7)
dev.off()

# ---- 10. Save cleaned data (reused in Weeks 2-4) ----
saveRDS(df, "titanic_clean.rds")
write.csv(df, "titanic_clean.csv", row.names = FALSE)


