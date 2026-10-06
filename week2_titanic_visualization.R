# =====================================================
# Week 2: Data Visualization and Insight Communication (Titanic)
# Uses the cleaned data saved in Week 1 (titanic_clean.rds)
# =====================================================

# ---- 0. Setup ----
library(dplyr)
library(ggplot2)

setwd("C:/Users/My Laptop/Documents/yuva intern")   # change if your folder is different
df <- readRDS("titanic_clean.rds")
dir.create("plots_week2", showWarnings = FALSE)

df$Survived_num <- ifelse(df$Survived == "Yes", 1, 0)
pal <- c("No" = "#E74C3C", "Yes" = "#2E86C1")        # red = died, blue = survived

cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")
str(df[, c("Survived", "Pclass", "Sex", "Age", "Fare_capped", "FamilySize", "Title", "Embarked")])

# ---- Chart 1: BAR - passengers by class and survival ----
p1 <- ggplot(df, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = pal) +
  labs(title = "Passengers by Class and Survival",
       x = "Passenger Class", y = "Number of Passengers") +
  theme_minimal(base_size = 12)
print(p1)
ggsave("plots_week2/chart1_class_bar.png", p1, width = 7, height = 5, dpi = 300)
print(table(df$Pclass, df$Survived))

# ---- Chart 2: BAR - survival rate by sex within each class ----
rate_sc <- df %>% group_by(Pclass, Sex) %>%
  summarise(rate = mean(Survived_num) * 100, n = n(), .groups = "drop")
p2 <- ggplot(rate_sc, aes(x = Pclass, y = rate, fill = Sex)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(rate, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("female" = "#C0392B", "male" = "#2874A6")) +
  ylim(0, 105) +
  labs(title = "Survival Rate by Sex and Class",
       x = "Passenger Class", y = "Survival Rate (%)") +
  theme_minimal(base_size = 12)
print(p2)
ggsave("plots_week2/chart2_sex_class_rate.png", p2, width = 7, height = 5, dpi = 300)
print(rate_sc)

# ---- Chart 3: HISTOGRAM - age distribution by survival ----
p3 <- ggplot(df, aes(x = Age, fill = Survived)) +
  geom_histogram(bins = 25, color = "white") +
  facet_wrap(~ Survived, labeller = labeller(Survived = c(No = "Did not survive", Yes = "Survived"))) +
  scale_fill_manual(values = pal) +
  labs(title = "Age Distribution of Passengers by Survival",
       x = "Age (years)", y = "Number of Passengers") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
print(p3)
ggsave("plots_week2/chart3_age_histogram.png", p3, width = 8, height = 5, dpi = 300)

# ---- Chart 4: HISTOGRAM - fare distribution (capped) ----
p4 <- ggplot(df, aes(x = Fare_capped)) +
  geom_histogram(bins = 30, fill = "#27AE60", color = "white") +
  labs(title = "Distribution of Ticket Fares (after outlier capping)",
       x = "Fare (capped)", y = "Number of Passengers") +
  theme_minimal(base_size = 12)
print(p4)
ggsave("plots_week2/chart4_fare_histogram.png", p4, width = 7, height = 5, dpi = 300)

# ---- Chart 5: BOXPLOT - age by class ----
p5 <- ggplot(df, aes(x = Pclass, y = Age, fill = Pclass)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Age Distribution by Passenger Class",
       x = "Passenger Class", y = "Age (years)") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
print(p5)
ggsave("plots_week2/chart5_age_boxplot.png", p5, width = 7, height = 5, dpi = 300)
print(df %>% group_by(Pclass) %>% summarise(median_age = median(Age), mean_age = round(mean(Age), 1)))

# ---- Chart 6: SCATTER - age vs fare, split by class ----
p6 <- ggplot(df, aes(x = Age, y = Fare_capped, color = Survived)) +
  geom_point(alpha = 0.6, size = 1.8) +
  facet_wrap(~ Pclass, labeller = labeller(Pclass = c("1" = "1st Class", "2" = "2nd Class", "3" = "3rd Class"))) +
  scale_color_manual(values = pal) +
  labs(title = "Age vs Fare by Class and Survival",
       x = "Age (years)", y = "Fare (capped)") +
  theme_minimal(base_size = 12)
print(p6)
ggsave("plots_week2/chart6_age_fare_scatter.png", p6, width = 9, height = 5, dpi = 300)

# ---- Chart 7: LINE - survival rate by age group ----
df$AgeGroup <- cut(df$Age, breaks = c(0, 12, 18, 30, 45, 60, Inf),
                   labels = c("0-12", "13-18", "19-30", "31-45", "46-60", "60+"))
age_rate <- df %>% group_by(AgeGroup) %>%
  summarise(rate = mean(Survived_num) * 100, n = n(), .groups = "drop")
p7 <- ggplot(age_rate, aes(x = AgeGroup, y = rate, group = 1)) +
  geom_line(color = "#2E86C1", linewidth = 1.2) +
  geom_point(size = 3.5, color = "#1B4F72") +
  geom_text(aes(label = paste0(round(rate, 1), "%")), vjust = -1, size = 3.5) +
  ylim(0, 80) +
  labs(title = "Survival Rate by Age Group",
       x = "Age Group", y = "Survival Rate (%)") +
  theme_minimal(base_size = 12)
print(p7)
ggsave("plots_week2/chart7_age_group_line.png", p7, width = 7, height = 5, dpi = 300)
print(age_rate)

# ---- Chart 8: LINE - survival rate by family size ----
fam_rate <- df %>% group_by(FamilySize) %>%
  summarise(rate = mean(Survived_num) * 100, n = n(), .groups = "drop")
p8 <- ggplot(fam_rate, aes(x = FamilySize, y = rate)) +
  geom_line(color = "#8E44AD", linewidth = 1.2) +
  geom_point(aes(size = n), color = "#5B2C6F") +
  scale_x_continuous(breaks = 1:11) +
  labs(title = "Survival Rate by Family Size",
       x = "Family Size (passenger + relatives aboard)", y = "Survival Rate (%)",
       size = "Passengers") +
  theme_minimal(base_size = 12)
print(p8)
ggsave("plots_week2/chart8_family_size_line.png", p8, width = 7, height = 5, dpi = 300)
print(fam_rate)

# ---- Chart 9: BAR - survival rate by title ----
title_rate <- df %>% group_by(Title) %>%
  summarise(rate = mean(Survived_num) * 100, n = n(), .groups = "drop")
p9 <- ggplot(title_rate, aes(x = reorder(Title, rate), y = rate, fill = Title)) +
  geom_col() +
  geom_text(aes(label = paste0(round(rate, 1), "%")), hjust = -0.1, size = 3.5) +
  coord_flip() + ylim(0, 100) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Survival Rate by Passenger Title", x = "Title", y = "Survival Rate (%)") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
print(p9)
ggsave("plots_week2/chart9_title_bar.png", p9, width = 7, height = 5, dpi = 300)
print(title_rate)

# ---- Chart 10: HEATMAP - survival rate by age group and class ----
heat <- df %>% group_by(AgeGroup, Pclass) %>%
  summarise(rate = mean(Survived_num) * 100, n = n(), .groups = "drop")
p10 <- ggplot(heat, aes(x = Pclass, y = AgeGroup, fill = rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(rate), "%")), size = 4) +
  scale_fill_gradient(low = "#FADBD8", high = "#1F618D", name = "Survival %") +
  labs(title = "Survival Rate by Age Group and Class",
       x = "Passenger Class", y = "Age Group") +
  theme_minimal(base_size = 12)
print(p10)
ggsave("plots_week2/chart10_heatmap.png", p10, width = 7, height = 5, dpi = 300)

cat("\nDone! 10 charts saved in the 'plots_week2' folder.\n")
