# Install & load packages
packages <- c("ggplot2")
lapply(packages, function(p) if (!require(p, character.only = TRUE)) install.packages(p))

library(ggplot2)

# ---- Simulate patient data ----
set.seed(101)
n <- 100
patients <- data.frame(
  Age = rnorm(n, mean = 45, sd = 12),
  BMI = rnorm(n, mean = 27, sd = 4)
)

# Generate a binary outcome: Risk of diabetes (0 = No, 1 = Yes)
patients$DiabetesRisk <- rbinom(n, 1, prob = plogis(-10 + 0.1*patients$Age + 0.2*patients$BMI))

# ---- Fit logistic regression ----
model <- glm(DiabetesRisk ~ Age + BMI, data = patients, family = binomial)

# ---- Create prediction grid for visualization ----
age_seq <- seq(min(patients$Age), max(patients$Age), length.out = 100)
bmi_seq <- seq(min(patients$BMI), max(patients$BMI), length.out = 100)
grid <- expand.grid(Age = age_seq, BMI = bmi_seq)
grid$Prob <- predict(model, newdata = grid, type = "response")

# ---- Plot patients + decision boundary ----
ggplot(patients, aes(x = Age, y = BMI, color = factor(DiabetesRisk))) +
  geom_point(size = 3, alpha = 0.8) +
  stat_contour(data = grid, aes(x = Age, y = BMI, z = Prob),
               breaks = 0.5, color = "black") +
  scale_color_manual(values = c("blue", "red"), labels = c("No Risk", "At Risk")) +
  labs(title = "Diabetes Risk Prediction (Logistic Regression)",
       x = "Age", y = "BMI", color = "Diabetes Risk") 
  theme_minimal()
# fsima
just testing
