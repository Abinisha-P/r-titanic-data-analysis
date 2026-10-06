# =====================================================
# Week 3: Statistical Analysis and Predictive Modeling (Titanic)
# Uses the cleaned data saved in Week 1 (titanic_clean.rds)
# Goal: predict whether a passenger survived (classification)
# =====================================================

# ---- 0. Setup ----
# install.packages(c("pROC", "car"))      # run once if not installed
library(dplyr)
library(ggplot2)
library(pROC)
library(car)

setwd("C:/Users/My Laptop/Documents/yuva intern")   # change if your folder is different
df <- readRDS("titanic_clean.rds")
dir.create("plots_week3", showWarnings = FALSE)

df$Survived_num <- ifelse(df$Survived == "Yes", 1, 0)
df$PclassF <- factor(df$Pclass, ordered = FALSE)    # normal (non-ordered) factor for models/tests
cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")

# =====================================================
# PART A: EXPLORATORY STATISTICAL ANALYSIS
# =====================================================
# Hypotheses (alpha = 0.05):
# H1: Survival is associated with Sex            (chi-square test)
# H2: Survival is associated with Passenger Class (chi-square test)
# H3: Mean Age differs between survivors and non-survivors (t-test / Wilcoxon)
# H4: Fare differs between survivors and non-survivors      (Wilcoxon / t-test)
# H5: Age differs across passenger classes                  (ANOVA / Kruskal-Wallis)
# H6: Fare is correlated with Passenger Class               (Spearman correlation)

# ---- A1. Distribution and normality ----
shapiro.test(df$Age)
shapiro.test(df$Fare_capped)

qq_age <- ggplot(df, aes(sample = Age)) + stat_qq(color = "#2E86C1") + stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot: Age", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 12)
print(qq_age)
ggsave("plots_week3/01_qq_age.png", qq_age, width = 6, height = 5, dpi = 300)

qq_fare <- ggplot(df, aes(sample = Fare_capped)) + stat_qq(color = "#27AE60") + stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot: Fare (capped)", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 12)
print(qq_fare)
ggsave("plots_week3/02_qq_fare.png", qq_fare, width = 6, height = 5, dpi = 300)

# ---- A2. H1: Sex vs Survival (chi-square) ----
tab_sex <- table(df$Sex, df$Survived)
print(tab_sex)
chi_sex <- chisq.test(tab_sex)
print(chi_sex)
cat("Cramer's V (Sex):", round(sqrt(unname(chi_sex$statistic) / sum(tab_sex)), 3), "\n")

# ---- A3. H2: Class vs Survival (chi-square) ----
tab_class <- table(df$PclassF, df$Survived)
print(tab_class)
chi_class <- chisq.test(tab_class)
print(chi_class)
cat("Cramer's V (Class):", round(sqrt(unname(chi_class$statistic) / sum(tab_class)), 3), "\n")

# ---- A4. H3: Age vs Survival ----
t.test(Age ~ Survived, data = df)                     # Welch t-test
wilcox.test(Age ~ Survived, data = df, exact = FALSE) # non-parametric check

# ---- A5. H4: Fare vs Survival ----
wilcox.test(Fare_capped ~ Survived, data = df, exact = FALSE)
df %>% group_by(Survived) %>% summarise(mean_fare = mean(Fare_capped), median_fare = median(Fare_capped))

# ---- A6. H5: Age across classes (ANOVA + Kruskal-Wallis) ----
anova_age <- aov(Age ~ PclassF, data = df)
summary(anova_age)
TukeyHSD(anova_age)
kruskal.test(Age ~ PclassF, data = df)

# ---- A7. H6: Correlation tests ----
cor.test(df$Fare_capped, as.numeric(df$PclassF), method = "spearman", exact = FALSE)
cor.test(df$Age, df$Fare_capped, method = "spearman", exact = FALSE)

# =====================================================
# PART B: MODEL BUILDING (Logistic Regression)
# =====================================================
# ---- B1. Train / test split (80% / 20%) ----
set.seed(123)
train_idx <- sample(seq_len(nrow(df)), size = round(0.8 * nrow(df)))
train <- df[train_idx, ]
test  <- df[-train_idx, ]
cat("Training rows:", nrow(train), " Test rows:", nrow(test), "\n")
prop.table(table(train$Survived)); prop.table(table(test$Survived))

# ---- B2. Helper functions ----
metrics <- function(actual, prob, threshold = 0.5) {
  pred <- ifelse(prob > threshold, 1, 0)
  cm <- table(Predicted = factor(pred, levels = c(0, 1)), Actual = factor(actual, levels = c(0, 1)))
  TP <- cm["1", "1"]; TN <- cm["0", "0"]; FP <- cm["1", "0"]; FN <- cm["0", "1"]
  prec <- TP / (TP + FP); rec <- TP / (TP + FN)
  list(cm = cm, accuracy = (TP + TN) / sum(cm), precision = prec, recall = rec,
       f1 = 2 * prec * rec / (prec + rec),
       auc = as.numeric(auc(roc(actual, prob, quiet = TRUE))))
}

cv_glm <- function(formula, data, k = 10, seed = 123) {
  set.seed(seed)
  folds <- sample(rep(1:k, length.out = nrow(data)))
  res <- data.frame(fold = 1:k, accuracy = NA, auc = NA)
  for (i in 1:k) {
    tr <- data[folds != i, ]; te <- data[folds == i, ]
    m <- glm(formula, data = tr, family = binomial)
    prob <- predict(m, newdata = te, type = "response")
    res$accuracy[i] <- mean(ifelse(prob > 0.5, 1, 0) == te$Survived_num)
    res$auc[i] <- as.numeric(auc(roc(te$Survived_num, prob, quiet = TRUE)))
  }
  res
}

# ---- B3. Model 1: baseline logistic regression ----
f1 <- Survived_num ~ PclassF + Sex + Age + FamilySize + Fare_capped + Embarked + HasCabin
m1 <- glm(f1, data = train, family = binomial)
summary(m1)

# Odds ratios with 95% confidence intervals
or1 <- exp(cbind(OR = coef(m1), confint.default(m1)))
print(round(or1, 3))

# ---- B4. 10-fold cross-validation (Model 1) ----
cv1 <- cv_glm(f1, train)
print(round(cv1, 3))
cat("Model 1 CV mean accuracy:", round(mean(cv1$accuracy), 3),
    " mean AUC:", round(mean(cv1$auc), 3), "\n")

# ---- B5. Test-set performance (Model 1) ----
prob1 <- predict(m1, newdata = test, type = "response")
res1 <- metrics(test$Survived_num, prob1)
print(res1$cm)
print(round(unlist(res1[c("accuracy", "precision", "recall", "f1", "auc")]), 3))

# =====================================================
# PART C: DIAGNOSTICS
# =====================================================
# ---- C1. Multicollinearity (VIF) ----
vif(m1)

# ---- C2. Deviance residuals vs fitted values ----
diag_df <- data.frame(fitted = fitted(m1), resid = residuals(m1, type = "deviance"))
p_res <- ggplot(diag_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.4, color = "#2E86C1") +
  geom_smooth(method = "loess", formula = y ~ x, color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Deviance Residuals vs Fitted Probabilities (Model 1)",
       x = "Fitted probability", y = "Deviance residual") +
  theme_minimal(base_size = 12)
print(p_res)
ggsave("plots_week3/03_residuals.png", p_res, width = 7, height = 5, dpi = 300)

# ---- C3. Influential observations (Cook's distance) ----
cd <- cooks.distance(m1)
cat("Observations with Cook's D > 4/n:", sum(cd > 4 / nrow(train)), "\n")
cd_df <- data.frame(index = seq_along(cd), cooks = cd)
p_cd <- ggplot(cd_df, aes(x = index, y = cooks)) +
  geom_col(fill = "#8E44AD") +
  geom_hline(yintercept = 4 / nrow(train), color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance (Model 1)", x = "Observation", y = "Cook's distance") +
  theme_minimal(base_size = 12)
print(p_cd)
ggsave("plots_week3/04_cooks_distance.png", p_cd, width = 7, height = 5, dpi = 300)

# ---- C4. Confusion matrix plot (test set) ----
cm_df <- as.data.frame(res1$cm)
p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 7) +
  scale_fill_gradient(low = "white", high = "#2E86C1") +
  scale_x_discrete(labels = c("0" = "Died", "1" = "Survived")) +
  scale_y_discrete(labels = c("0" = "Died", "1" = "Survived")) +
  labs(title = "Confusion Matrix (Model 1, Test Set)") +
  theme_minimal(base_size = 12)
print(p_cm)
ggsave("plots_week3/05_confusion_matrix.png", p_cm, width = 6, height = 5, dpi = 300)

# ---- C5. Odds ratio plot ----
or_df <- as.data.frame(or1)[-1, ]
names(or_df) <- c("OR", "lower", "upper")
or_df$term <- rownames(or_df)
p_or <- ggplot(or_df, aes(x = reorder(term, OR), y = OR)) +
  geom_point(size = 3, color = "#1B4F72") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "#1B4F72") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() + scale_y_log10() +
  labs(title = "Odds Ratios with 95% CI (Model 1)", x = "", y = "Odds ratio (log scale)") +
  theme_minimal(base_size = 12)
print(p_or)
ggsave("plots_week3/06_odds_ratios.png", p_or, width = 8, height = 5, dpi = 300)

# =====================================================
# PART D: OPTIMIZATION (Model 2) AND COMPARISON
# =====================================================
# Week 2 charts suggested (1) the effect of class differs for women and men
# and (2) survival is non-linear in family size. Model 2 adds both ideas.
f2 <- Survived_num ~ PclassF * Sex + Age + FamilySize + I(FamilySize^2) + Fare_capped + Embarked + HasCabin
m2 <- glm(f2, data = train, family = binomial)
summary(m2)

# Does Model 2 fit significantly better? (likelihood ratio test)
anova(m1, m2, test = "Chisq")
cat("AIC Model 1:", round(AIC(m1), 1), "  AIC Model 2:", round(AIC(m2), 1), "\n")

cv2 <- cv_glm(f2, train)
cat("Model 2 CV mean accuracy:", round(mean(cv2$accuracy), 3),
    " mean AUC:", round(mean(cv2$auc), 3), "\n")

prob2 <- predict(m2, newdata = test, type = "response")
res2 <- metrics(test$Survived_num, prob2)
print(res2$cm)

comparison <- data.frame(
  Model = c("Model 1 (baseline)", "Model 2 (improved)"),
  CV_Accuracy = round(c(mean(cv1$accuracy), mean(cv2$accuracy)), 3),
  CV_AUC = round(c(mean(cv1$auc), mean(cv2$auc)), 3),
  Test_Accuracy = round(c(res1$accuracy, res2$accuracy), 3),
  Test_Precision = round(c(res1$precision, res2$precision), 3),
  Test_Recall = round(c(res1$recall, res2$recall), 3),
  Test_F1 = round(c(res1$f1, res2$f1), 3),
  Test_AUC = round(c(res1$auc, res2$auc), 3)
)
print(comparison)

# ---- ROC curves ----
roc1 <- roc(test$Survived_num, prob1, quiet = TRUE)
roc2 <- roc(test$Survived_num, prob2, quiet = TRUE)
p_roc <- ggroc(list("Model 1 (baseline)" = roc1, "Model 2 (improved)" = roc2), linewidth = 1.1) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("#E74C3C", "#2E86C1"), name = "") +
  labs(title = "ROC Curves on the Test Set", x = "Specificity", y = "Sensitivity") +
  theme_minimal(base_size = 12)
print(p_roc)
ggsave("plots_week3/07_roc_curves.png", p_roc, width = 7, height = 5, dpi = 300)

# ---- Threshold analysis (Model 2) ----
thr <- seq(0.3, 0.7, by = 0.1)
thr_tab <- t(sapply(thr, function(t) {
  r <- metrics(test$Survived_num, prob2, t)
  round(c(threshold = t, accuracy = r$accuracy, precision = r$precision, recall = r$recall, f1 = r$f1), 3)
}))
print(thr_tab)

# ---- Save final model ----
saveRDS(m2, "titanic_model_week3.rds")
cat("\nDone! Charts saved in 'plots_week3' folder.\n")