# Strategic HR Analytics: Logistic Regression and EDA in Employee Attrition Analysis

by Syarif Yusuf Effendi

# Introduction

In an era where data is emerging as one of the most valuable assets for organizations, data analysis has played a very important role in assisting intelligent decision-making. Especially when it comes to HR, professionals now have access to tools and techniques that allow them to extract valuable insights from employee data. One important aspect of HR analysis is understanding the factors that influence employee turnover, or what is often known as employee attrition. This is why, in this article, we will explore and outline the steps to perform employee turnover analysis using the R programming language.

# Data Collection

In this article, we will use open data sources on Kaggle, which you can access via the following link: [click here.](https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset)

# Data Cleaning and Preparation

The first step before cleaning the data is to first import the dataset file that was previously downloaded and use the `readr` library.

```{r library readr}
library(readr)
attrition.data <- read.csv("D:/Dataset/Employee Attrition/WA_Fn-UseC_-HR-Employee-Attrition.csv")
```

Then, after the data is successfully imported, use the `dplyr` library to start cleaning it so the data is ready for use.

```{r library dplyr}
library(dplyr)
```

Before doing data cleaning, let's do some checking of the dataset to see the columns, structure, data type, etc.

```{r function head}
head(attrition.data)
```

The `head()` function in R programming is used to display the first few rows of a data object, such as a dataframe, matrix, or vector. The main benefit of the `head()` function is that it provides an initial view of the data.

```{r function str}
str(attrition.data)
```

The `str()` function in R programming is used to present structural information from a data object.

Next, we will see descriptive analysis for columns with integer data types, so we need to write the syntax as follows:

```{r descriptive analysis}
# The name of the column you want to exclude
exclude_cols <- c("Attrition", "BusinessTravel", "Department", "EducationField",
                  "Gender", "JobRole", "MaritalStatus", "Over18", "OverTime")

# Executes summary(), excluding the desired column
summary(attrition.data[, !(names(attrition.data) %in% exclude_cols)])
```

`attrition.data[, !(names(attrition.data) %in% exclude_cols)]` is how we select columns that are not in `exclude_cols` from the attrition.data data object. This uses column selection using indexing and the `%in%` operator to check if the column name is in `exclude_cols`.

Next, we check for variables with character data types.

```{r variable char type}
table(attrition.data$Attrition)
table(attrition.data$BusinessTravel)
table(attrition.data$Department)
table(attrition.data$EducationField)
table(attrition.data$Gender)
table(attrition.data$JobRole)
table(attrition.data$MaritalStatus)
table(attrition.data$Over18)
table(attrition.data$OverTime)
```

The `table()` function in R is used to create contingency tables, which calculate the frequency of observations among combinations of values of one or more variables.

Then do a final check to see if there is an NA value in the dataset.

```{r NA value}
# Calculates the total value of NA in all columns
total_na <- sum(colSums(is.na(attrition.data)))

# Print total_na
total_na
```

The result is that there are no NA values in all columns in the dataset.

Next, after doing some checks, we need to make adjustments to our dataset. We need to remove the columns that can't provide useful insights like `EmployeeCount`, `Over18`, and `StandardHours.` Also add a new column, namely `EducationLevel`.

```{r data cleaning}
# Data cleaning
df.attrition <- attrition.data %>%
  select(-EmployeeCount, -Over18, -StandardHours) %>% # Delete column
  mutate(EducationLevel = c("Below College", "College", "Bachelor", "Master", "Doctor")[Education]) # Added new column

# Review new table
head(df.attrition)
```

The `%>%` operator, known as the pipeline operator, is used in packages such as `dplyr` and `tidyverse` in R to facilitate the processing of data with a more concise and readable syntax.

# Data Exploration

In an increasingly data-driven world, data exploration is a critical initial step before conducting in-depth research. This is the process of exploring your dataset to learn about its characteristics, patterns, and potential. Data exploration assists in the identification of early trends, the identification of possible problems, and the formulation of deeper questions for research.

Before starting data exploration, let's prepare the `ggplot2` and `reshape2` library first, because at this stage we will do some visualizations such as histograms, boxplots, etc.

```{r library ggplot2 and reshape2}
library(ggplot2)
library(reshape2)
```

## How was the age distribution of employees based on attrition?

```{r age distribution histogram}
ggplot(df.attrition, aes(x = Age, y = ..density.., fill = Attrition)) +
  geom_histogram(binwidth = 5, color = "black", position = "stack") +
  geom_density(alpha = 0.5) +
  labs(title = "Employee Age Distribution by Attrition", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Attrition)
```

Based on the histogram above, both employees with attrition values of "Yes" and "No" tend to slant to the right. This indicates that the mean, median, and mode values in the age column do not have the same value. From the histogram, we also get information that employees with Attrition "No" are predominantly aged 30--40 years, while employees with Attrition "Yes" are dominated by those in their 30s or around 27--33 years, with a density that almost reaches 0.06.

## How was the monthly income distribution based on attrition?

```{r monthly income distribution box plot}
ggplot(attrition.data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Monthly Income Distribution by Attrition", y = "Monthly Income (USD)")+
  theme(plot.title = element_text(hjust = 0.5))
```

Based on the box plot above, the median monthly income for employees with "No" attrition is higher than "Yes" attrition, which is around 6800 USD, while "Yes" is around 4700 USD. This may indicate that higher income may be a factor influencing an employee's decision to stay with the company.

It can also be seen that there are outliers, indicating the existence of extreme values in the monthly income data. This could be because some employees have incomes that are much higher or lower than the majority of employees. There is also an asymmetry between the two, as indicated by the unequal size of the upper and lower squares, indicating that there is an abnormal distribution.

## How was the correlation between numerical variables?

```{r numerical variables correlation heatmap}
# Create numeric variables values
numeric_columns <- c("Age", "DailyRate", "DistanceFromHome", "Education", "EnvironmentSatisfaction", 
                     "HourlyRate", "JobInvolvement", "JobLevel", "JobSatisfaction", "MonthlyIncome", 
                     "MonthlyRate", "NumCompaniesWorked", "PerformanceRating", "RelationshipSatisfaction", 
                     "PercentSalaryHike", "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", 
                     "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole",
                     "YearsSinceLastPromotion", "YearsWithCurrManager")

# Calculate the correlation matrix
correlation_matrix <- cor(attrition.data[, numeric_columns])

# Create a correlation heatmap
ggplot(melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Numeric Variables Correlation Heatmap", x = "Variable", y = "Variable") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))
```

There is a strong positive correlation between the variables MonthlyIncome and `JobLevel`, `MonthlyIncome` and `TotalWorkingYear`, `YearsWithCurrManager` and `YearsAtCompany`, `JobLevel` and `TotalWorkingYear`, and `Age` and `TotalWorkingYears.` Which is marked with a deep red color, or an estimated correlation value of 0.75 to 1.0.

## Let's try to prove it again statistically using the Spearman correlation test

```{r spearman correlation test}
cor(df.attrition[, numeric_columns], method = "spearman")
```

`JobLevel` and `MonthlyIncome` have a strong positive correlation of around 0.920, indicating that the higher the JobLevel, the higher the `MonthlyIncome` tends to be. Additionally, `YearsAtCompany` and `YearsWithCurrManager` also have a strong positive correlation of around 0.843, indicating that the longer someone has worked at a company (`YearsAtCompany`), they tend to have more time with the current manager (`YearsWithCurrManager`).

So we will exclude `JobLevel` and `YearsAtCompany` from the model.

# Employee Attrition Analysis

In this section, we will explore the in-depth steps of analyzing the factors that contribute to employee turnover and how these steps can provide valuable insights for decision-making in companies. I will use logistic regression in creating the model.

Then we need to call the `stats` library first, and after that, we start creating the first model.

Before that we also need to change the Attrition variable with the values ​​1 and 0, 1 as "Yes" and 0 as "No".

```{r library stats}
library(stats)
```

```{r change variable values}
df.attrition$Attrition <- ifelse(df.attrition$Attrition == "Yes", 1, 0)
```

## Logistic Regression

```{r logit model 1}
# Create first model as logit1
logit1 <- glm(Attrition ~ Age + DailyRate + DistanceFromHome + Education + EnvironmentSatisfaction +
                HourlyRate + JobInvolvement + JobSatisfaction + MonthlyIncome +
                MonthlyRate + NumCompaniesWorked + PerformanceRating + RelationshipSatisfaction +
                PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit1 using the summary function to see the estimation results
summary(logit1)
```

There are still a number of independent variables that are not significant, so we need to repeat it again by removing 1 variable, namely `Education`, and then generating the model, which becomes `logit2`.

```{r logit model 2}
# Create second model as logit2
logit2 <- glm(Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction +
                HourlyRate + JobInvolvement + JobSatisfaction + MonthlyIncome +
                MonthlyRate + NumCompaniesWorked + PerformanceRating + RelationshipSatisfaction +
                PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit2 using the summary function to see the estimation results
summary(logit2)
```

There are still a number of independent variables that are not significant, so we need to repeat it again by removing 1 variable, namely `HourlyRate`, and then generating the model, which becomes `logit3`. We need to repeat this again and again until all variables are significant.

```{r logit model 3}
# Create third model as logit3
logit3 <- glm(Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction +
                JobInvolvement + JobSatisfaction + MonthlyIncome +
                MonthlyRate + NumCompaniesWorked + PerformanceRating + RelationshipSatisfaction +
                PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit3 using the summary function to see the estimation results
summary(logit3)
```

```{r logit model 4}
# Create fourth model as logit4
logit4 <- glm(Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction +
                JobInvolvement + JobSatisfaction + MonthlyIncome +
                NumCompaniesWorked + PerformanceRating + RelationshipSatisfaction +
                PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit4 using the summary function to see the estimation results
summary(logit4)

```

```{r logit model 5}
# Create fifth model as logit5
logit5 <- glm(Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction +
                JobInvolvement + JobSatisfaction + MonthlyIncome +
                NumCompaniesWorked + RelationshipSatisfaction +
                PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit5 using the summary function to see the estimation results
summary(logit5)
```

```{r logit model 6}
# Create sixth model as logit6
logit6 <- glm(Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction +
                JobInvolvement + JobSatisfaction + MonthlyIncome +
                NumCompaniesWorked + RelationshipSatisfaction +
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit6 using the summary function to see the estimation results
summary(logit6)
```

```{r logit model 7}
# Create seventh model as logit7
logit7 <- glm(Attrition ~ Age + DailyRate + DistanceFromHome + EnvironmentSatisfaction +
                JobInvolvement + JobSatisfaction + MonthlyIncome +
                NumCompaniesWorked + RelationshipSatisfaction +
                StockOptionLevel + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit7 using the summary function to see the estimation results
summary(logit7)
```

```{r logit model 8}
# Create eighth model as logit8
logit8 <- glm(Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction +
                JobInvolvement + JobSatisfaction + MonthlyIncome +
                NumCompaniesWorked + RelationshipSatisfaction +
                StockOptionLevel + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit8 using the summary function to see the estimation results
summary(logit8)
```

The `RelationshipSatisfaction` variable is still not significant at 0.05; let's try again to create a logit model as `logit9`.

```{r logit model 9}
# Create ninth model as logit9
logit9 <- glm(Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction +
                JobInvolvement + JobSatisfaction + MonthlyIncome +
                NumCompaniesWorked +
                StockOptionLevel + TrainingTimesLastYear +
                WorkLifeBalance + YearsInCurrentRole +
                YearsSinceLastPromotion + YearsWithCurrManager, family = "binomial",
              data = df.attrition) 
# Call logit9 using the summary function to see the estimation results
summary(logit9)
```

Finally, all independent variables are significant. Now we need to choose the best logit model using the `aic` function.

## AIC Criteria

AIC is a metric used to compare and select the most suitable model in statistics. The lower the AIC value, the better the model is at explaining the data. Below is how we want to compare the AIC values of each model.

```{r aic criteria}
# Create a vector that contains previously created models as logit_model
logit_model <- c("logit1", "logit2", "logit3", "logit4", "logit5", "logit6", "logit7",
                 "logit8", "logit9")

# Create a vector containing the AIC values of each model
AIC <- c(logit1$aic, logit2$aic, logit3$aic, logit4$aic, logit5$aic, logit6$aic, logit7$aic,
         logit8$aic, logit9$aic)

# Create a data frame containing the logit_model vector and AIC as criteria
criteria <- data.frame(logit_model, AIC)

# Calling the data frame `criteria`
criteria
```

From these results, we can conclude that `logit8` has the lowest AIC value, namely around 1108.273, so it can be considered the most suitable model among all the models compared. Okay, next we need to check the goodness of fit using `logit8`.

## Goodness of Fit

Goodness of fit in logistic regression refers to the extent to which the model that has been built fits the observed data. It is important to measure how well our logistic regression model explains the variation in the actual data. There are several methods that can be used to carry out this test, but in this article, I will use deviance.

```{r deviance}
# Create a null model with only the intercept
null_model <- glm(Attrition ~ 1, data = df.attrition, family = "binomial")

# Performing a likelihood ratio test between the null model and the complex model
lr_test <- anova(null_model, logit8, test = "Chisq")

# Load the results of the likelihood ratio test
lr_test
```

The results of the "Analysis of Deviance Table" show that Model 2 (with a number of additional predictor variables) is significantly better at explaining variation in the Attrition data compared to Model 1 (base model). The addition of predictor variables increases the model's ability to predict the possibility of Attrition. Therefore, Model 2 is recommended for Attrition analysis and prediction.

After that, load the `logit8` and interpret it.

```{r logit model 8}
summary(logit8)
```

The star at each coefficient value indicates the significance of the independent variable for the dependent variable. The more stars, the higher the level of significance. The following is an interpretation of the influence of each variable: [clik here.](www.medium.com/@syarifyusuf)

Okay, after this, we will test the predicted results of classification using the confusion matrix.

## Confusion Matrix

Confusion matrix is a table used in statistical analysis, particularly in the context of classification or prediction. It is a useful tool for evaluating the performance of predictive models, such as logistic regression models or machine learning models, in distinguishing between various categories or classes. Here is how to create a confusion matrix table.

```{r confusion matrix}
table(TRUE == df.attrition$Attrition, pred = round(fitted(logit8)))
```

Interpretation: [click here.](www.medium.com/@syarifyusuf)

After conducting data analysis, the next step is to generate visualizations of several variables that we have reviewed. These visualizations will aid in a better understanding of patterns and trends present in the data. Visualizing data serves as a crucial tool in conveying findings and insights from data analysis to stakeholders and team members. Therefore, data visualization becomes an essential step in effectively presenting the results of the analysis.

# Data Visualization

Visualization has strong revealing power. Instead of just looking at a table of numbers, visualization allows us to see patterns, trends, and relationships among the data. This helps data users see the big picture quickly, gain immediate insights, and explore relationships that may not be visible in numbers. In the context of data analysis, visualization helps turn numbers into easy-to-understand images, thereby facilitating decision-making. In this stage, I will explain the importance of visualization in presenting analytical findings and how to create informative graphs.

Before starting, let's add the `AttritionStatus` variable, because previously we changed the value of the `Attrition` variable

```{r create new variable as attritionstatus}
df.attrition <- df.attrition %>%
  mutate(AttritionStatus = ifelse(Attrition == 1, "Yes", "No"))
```

now we are ready to visualize the data.

## What is the percentage of employees between those who are attrited and those who are not?

```{r pie chart attrition}
# Count attrition value
attrition.count <- df.attrition %>%
  count(AttritionStatus)

# Create attrition distribution pie chart
ggplot(attrition.count, aes(x = "", y = n, fill = AttritionStatus)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = scales::percent(n / sum(n))), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Attrition Distiribution (Percentage)", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))
```

Insight: [click here.](www.medium.com/@syarifyusuf)

## How is attrition affected by gender?

```{r column chart attrition by gender}
ggplot(df.attrition, aes(x = Gender, fill = AttritionStatus)) +
  geom_bar(position = "stack") +
  labs(title = "Attrition Status by Gender") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5) +
  theme(plot.title = element_text(hjust = 0.5))
```

Insight: [click here.](www.medium.com/@syarifyusuf)

## What job role has the highest turnover rate?

```{r bar chart turnover rate by job role}
# Calculates the number of "Yes" Attrition and percentages within each Job Role
jobrole.data <- df.attrition %>%
  group_by(JobRole) %>%
  summarise(AttritionYesCount = sum(AttritionStatus == "Yes"),
            TotalCount = n(),
            AttritionPercentage = (AttritionYesCount / TotalCount) * 100)

# Create a bar chart with Attrition percentage "Yes"
ggplot(jobrole.data %>%
         arrange(desc(AttritionPercentage)),
         aes(y = reorder(JobRole, AttritionPercentage), 
                           x = AttritionPercentage, fill = JobRole)) +
  geom_bar(stat = "identity") +
  labs(title = "Turnover Rate Job Role", subtitle = "From Highest to Lowest",
       y = "Job Role", x = "Turnover Rate") +
  geom_text(aes(label = scales::percent(AttritionPercentage / 100, accuracy = 0.01), 
                x = AttritionPercentage), 
            hjust = -0.2) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_discrete() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
```

Insight: [click here.](www.medium.com/@syarifyusuf)

## How distance from home affect attrition?

```{r distance from home distribution}
ggplot(df.attrition, aes(x = DistanceFromHome, fill = AttritionStatus)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distance From Home Distribution",
       x = "Distance From Home", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 30))
```

Insight: [click here.](www.medium.com/@syarifyusuf)

## What marital status has the highest turnover rate?

```{r bar chart turnover rate by marital status}
# Calculates the number of "Yes" Attrition and percentages within each Marital Status
marital.data <- df.attrition %>%
  group_by(MaritalStatus) %>%
  summarise(AttritionYesCount = sum(AttritionStatus == "Yes"),
            TotalCount = n(),
            AttritionPercentage = (AttritionYesCount / TotalCount) * 100) %>%
  arrange(desc(AttritionPercentage))

# Create a bar chart with Attrition percentage "Yes"
ggplot(marital.data, aes(y = MaritalStatus, 
                           x = AttritionPercentage, fill = MaritalStatus)) +
  geom_bar(stat = "identity") +
  labs(title = "Turnover Rate by Marital Status", subtitle = "From Highest to Lowest",
       y = "Marital Status", x = "Turnover Rate") +
  geom_text(aes(label = scales::percent(AttritionPercentage / 100, accuracy = 0.01), 
                x = AttritionPercentage), 
            hjust = -0.2) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_discrete() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
```

Insight: [click here.](www.medium.com/@syarifyusuf)

## How is business travel affect attrition?

```{r column chart attrition by business travel}
ggplot(df.attrition, aes(x = BusinessTravel, fill = AttritionStatus)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5) +
  labs(title = "Attrition Status by Business Travel") +
  theme(plot.title = element_text(hjust = 0.5))
```

Insight: [click here.](www.medium.com/@syarifyusuf)

## What education level has the highest turnover rate?

```{r bar chart turnover rate by education}
# Calculates the number of "Yes" Attrition and percentages within each Marital Status
education.data <- df.attrition %>%
  group_by(EducationLevel) %>%
  summarise(AttritionYesCount = sum(AttritionStatus == "Yes"),
            TotalCount = n(),
            AttritionPercentage = (AttritionYesCount / TotalCount) * 100)

# Create a bar chart with Attrition percentage "Yes"
ggplot(education.data, aes(y = reorder(EducationLevel, AttritionPercentage), 
                           x = AttritionPercentage, fill = EducationLevel)) +
  geom_bar(stat = "identity") +
  labs(title = "Turnover Rate by Education Level", subtitle = "From Highest to Lowest",
       y = "Education Level", x = "Turnover Rate") +
  geom_text(aes(label = scales::percent(AttritionPercentage / 100, accuracy = 0.01), 
                x = AttritionPercentage), 
            hjust = -0.2) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_discrete() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
```

Insight: [click here.](www.medium.com/@syarifyusuf)

## Does overtime affect turnover rate?

```{r bar chart turnover rate by overtime}
# Calculates the number of "Yes" Attrition and percentages within each Overtime
overtime.data <- df.attrition %>%
  group_by(OverTime) %>%
  summarise(AttritionYesCount = sum(AttritionStatus == "Yes"),
            TotalCount = n(),
            AttritionPercentage = (AttritionYesCount / TotalCount) * 100)

# Create a bar chart with Attrition percentage "Yes"
ggplot(overtime.data, aes(y = reorder(OverTime, -AttritionPercentage), 
                           x = AttritionPercentage, fill = OverTime)) +
  geom_bar(stat = "identity") +
  labs(title = "Turnover Rate by Over Time",
       y = "Over Time", x = "Turnover Rate") +
  geom_text(aes(label = scales::percent(AttritionPercentage / 100, accuracy = 0.01), 
                x = AttritionPercentage), 
            vjust = -0.5) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_discrete() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
```

Insight: [click here.](www.medium.com/@syarifyusuf)

## Does having a lot of experience in several companies have an effect on attrition?

```{r bar chart num of companies work}
# Calculates the number of "Yes" Attrition and percentages within each number of companies worked
companiesworked.data <- df.attrition %>%
  group_by(NumCompaniesWorked) %>%
  summarise(AttritionYesCount = sum(AttritionStatus == "Yes"),
            TotalCount = n(),
            AttritionPercentage = (AttritionYesCount / TotalCount) * 100)

# Create a bar chart with Attrition percentage "Yes"
ggplot(companiesworked.data, aes(y = reorder(NumCompaniesWorked, AttritionPercentage), 
                           x = AttritionPercentage, fill = as.character(NumCompaniesWorked))) +
  geom_bar(stat = "identity") +
  labs(title = "Turnover Rate by Number of Companies Worked", fill = "Number of Companies Worked",
       y = "Number of Companies Worked", x = "Turnover Rate") +
  geom_text(aes(label = scales::percent(AttritionPercentage / 100, accuracy = 0.01), 
                x = AttritionPercentage), 
            hjust = -0.2) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_discrete() +
  theme(plot.title = element_text(hjust = 0.5))
```

# Conclusion

[click here.](www.medium.com/@syarifyusuf)
