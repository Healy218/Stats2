# ====================================================
# Single Factor ANOVA Demo in R (Five Groups)
# ====================================================

# Step 1: Create the Data
# -----------------------
# Plant growth for each fertilizer
growth_A <- c(20, 22, 19, 23, 21)  # Fertilizer A
growth_B <- c(25, 27, 26, 28, 30)  # Fertilizer B
growth_C <- c(18, 17, 16, 19, 20)  # Fertilizer C
growth_D <- c(21, 20, 22, 19, 21)  # Fertilizer D
growth_E <- c(26, 25, 27, 26, 28)  # Fertilizer E

### Step 1a: Check equal variance ###
sd_A <- sd(growth_A)
sd_B <- sd(growth_B)
sd_C <- sd(growth_C)
sd_D <- sd(growth_D)
sd_E <- sd(growth_E)

cat("Standard deviation of Fertilizer A:", sd_A, "\n")
cat("Standard deviation of Fertilizer B:", sd_B, "\n")
cat("Standard deviation of Fertilizer C:", sd_C, "\n")
cat("Standard deviation of Fertilizer D:", sd_D, "\n")
cat("Standard deviation of Fertilizer E:", sd_E, "\n")

# Combine all data
growth <- c(growth_A, growth_B, growth_C, growth_D, growth_E)
group <- factor(rep(c("A", "B", "C", "D", "E"), each = 5))
plant_data <- data.frame(growth, group)

# Step 2: Visualize the Data
boxplot(growth ~ group, data = plant_data,
        main = "Plant Growth by Fertilizer",
        xlab = "Fertilizer Type",
        ylab = "Growth (cm)",
        col = c("lightblue", "lightgreen", "pink", "orange", "purple"))

# Step 3: Perform Single Factor ANOVA
anova_result <- aov(growth ~ group, data = plant_data)
print("ANOVA Result:")
print(summary(anova_result))

# Step 3a: Check normality of residuals
residuals_anova <- residuals(anova_result)
qqnorm(residuals_anova)
qqline(residuals_anova, col = "red")

# Step 4: Post-Hoc Test (Tukey HSD)
tukey_result <- TukeyHSD(anova_result)
print("Tukey HSD Post-Hoc Test:")
print(tukey_result)


# Step 5: Interpretation
# ----------------------
# - ANOVA tests whether at least one group mean differs from the others
# - Tukey HSD shows which specific groups differ
# - Groups A & D and B & E should cluster together since their means are similar


