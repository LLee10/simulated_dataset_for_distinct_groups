
```{r setup}
#load necessary libraries
library(dplyr)
library(ggplot2)
```

#STEP 1: Create simulated dataset
```{r}
# Set random seed for reproducibility
set.seed(31)

# Simulated data parameters
num_samples_per_group <- 500
```

```{r}
# Generate random data for group A
mean_age_group_A <- 25
std_dev_age_group_A <- 5
min_salary_group_A <- 30000
max_salary_group_A <- 100000

ages_group_A <- rnorm(num_samples_per_group, mean = mean_age_group_A, sd = std_dev_age_group_A)
salaries_group_A <- sample(min_salary_group_A:max_salary_group_A, num_samples_per_group, replace = TRUE)
sex_group_A <- sample(c("Male", "Female"), num_samples_per_group, replace = TRUE)
group_A <- rep("A", num_samples_per_group)
```

```{r}
# Generate random data for group B
mean_age_group_B <- 35
std_dev_age_group_B <- 3
min_salary_group_B <- 45000
max_salary_group_B <- 120000

ages_group_B <- rnorm(num_samples_per_group, mean = mean_age_group_B, sd = std_dev_age_group_B)
salaries_group_B <- sample(min_salary_group_B:max_salary_group_B, num_samples_per_group, replace = TRUE)
sex_group_B <- sample(c("Male", "Female"), num_samples_per_group, replace = TRUE)
group_B <- rep("B", num_samples_per_group)

```


```{r}
# Combine data for both groups using data.frame
simulated_data <- data.frame(
  Age = c(ages_group_A, ages_group_B),
  Salary = c(salaries_group_A, salaries_group_B),
  Sex = c(sex_group_A, sex_group_B),
  Group = c(group_A, group_B)
)
```

```{r}
# Add a hidden effect for females in Group A
simulated_data$Effect <- ifelse(simulated_data$Group == "A" & simulated_data$Sex == "Female", rnorm(2*num_samples_per_group, mean = 10, sd = 5), 0)
```

```{r}
# Display the first few rows of the simulated dataset
head(simulated_data)
```
# STEP 2:Visualizations to showcase the data and highlight differences
```{r}
# Box plot of Age by Group and Sex
ggplot(simulated_data, aes(x = Group, y = Age, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by Group and Sex", x = "Group", y = "Age")
```


```{r}
# Box plot of Salary by Group and Sex
ggplot(simulated_data, aes(x = Group, y = Salary, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Box Plot of Salary by Group and Sex", x = "Group", y = "Salary")
```

```{r}
# Violin plot of Effect by Group and Sex
ggplot(simulated_data, aes(x = Group, y = Effect, fill = Sex)) +
  geom_violin() +
  labs(title = "Violin Plot of Effect by Group and Sex", x = "Group", y = "Effect")
```

# STEP 3: Carry out statistical tests to ascertain differences between group A and B
```{r}
# Data preparation
group_A_data <- filter(simulated_data, Group == "A")
group_B_data <- filter(simulated_data, Group == "B")
```

```{r}
# 1. t-test for numerical variables (Age and Salary)
# Assume data follows a normal distribution
t_test_age <- t.test(group_A_data$Age, group_B_data$Age)
t_test_salary <- t.test(group_A_data$Salary, group_B_data$Salary)

# Print t-test results
print("T-Test for Age:")
print(t_test_age)
print("T-Test for Salary:")
print(t_test_salary)
```

```{r}
# 2. Chi-square test for categorical variable (Sex and Group membership)
chi_square_test_sex <- chisq.test(table(simulated_data$Sex, simulated_data$Group))

# Print chi-square test results
print("Chi-Square Test for Sex and Group:")
print(chi_square_test_sex)
```

```{r}
# 3. ANOVA test for comparing Effect across Groups and Sex
anova_effect <- aov(Effect ~ Group * Sex, data = simulated_data)

# Print ANOVA results
print("ANOVA Test for Age and Group:")
print(summary(anova_effect))
```

