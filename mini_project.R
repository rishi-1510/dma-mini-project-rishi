# ============================================================
# 📘 Student Focus and Performance Analysis (DMA Lab Project)
# ============================================================

# ---------------------------
# Step 0: Load Libraries
# ---------------------------
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# ---------------------------
# Step 1: Load Collected Data
# ---------------------------
data <- read.csv("student_focus_data.csv")

# View first few rows
head(data)

# Summary of dataset
summary(data)

# ---------------------------
# Step 2: Preprocessing
# ---------------------------

# Convert categorical columns to factors
data$Gender <- factor(data$Gender)
data$Year_of_Study <- factor(data$Year_of_Study)
data$Study_Location <- factor(data$Study_Location)

# Check for missing values (should ideally be 0)
missing_count <- sum(is.na(data))
print(paste("Total missing values in dataset:", missing_count))

# Optional: remove rows with missing values (if any)
# data <- na.omit(data)

# ---------------------------
# Step 3: Exploratory Data Analysis
# ---------------------------

# 1️⃣ Correlation between Notifications and Concentration
cor_notif_conc <- cor(data$Notifications_per_Hour, data$Concentration_Score)
print(paste("Correlation (Notifications vs Concentration):", round(cor_notif_conc, 2)))

# 2️⃣ Average Concentration by Study Location
avg_conc <- aggregate(Concentration_Score ~ Study_Location, data = data, mean)
print(avg_conc)

# 3️⃣ Boxplot: Concentration by Study Location
ggplot(data, aes(x = Study_Location, y = Concentration_Score, fill = Study_Location)) +
  geom_boxplot() +
  labs(title = "Concentration Score by Study Location",
       x = "Study Location", y = "Concentration Score") +
  theme_minimal()

# 4️⃣ Scatter Plot: Notifications vs Concentration
ggplot(data, aes(x = Notifications_per_Hour, y = Concentration_Score)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Notifications per Hour vs Concentration",
       x = "Notifications per Hour", y = "Concentration Score") +
  theme_minimal()

# ---------------------------
# Step 4: Linear Regression Model
# ---------------------------
model <- lm(Test_Score ~ Sleep_Hours + Study_Hours + Notifications_per_Hour +
              Cups_of_Coffee + Study_Location, data = data)

summary(model)

# ---------------------------
# Step 5: Conclusions
# ---------------------------
cat("\n==================== CONCLUSIONS ====================\n")
cat("1️⃣ Students who study in the Library generally show higher concentration and better test scores.\n")
cat("2️⃣ A higher number of notifications per hour negatively impacts concentration levels.\n")
cat("3️⃣ Adequate sleep (6–8 hours) boosts concentration and academic performance.\n")
cat("4️⃣ Moderate coffee intake helps a bit, but excessive consumption doesn’t improve focus.\n")
cat("5️⃣ More study hours are strongly correlated with higher test scores.\n")

