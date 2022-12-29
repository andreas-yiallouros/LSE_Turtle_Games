## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

## Turtle Games, a game manufacturer and retailer, manufacture and sell their
## own products, along with sourcing and selling products manufactured by other
## companies. Their product range includes books, board games, video games and
## toys. They have a global customer base.
## One of their business objectives is to improve sales performance by 
## utilising customer trends. 

## We completed data analysis in Python to help Turtle Games understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##   campaigns (Week 3)

## We will use R to help Turtle Games understand:
## - what their sales per product data may be telling them (Week 4)
## - the reliability of their sales data (e.g. normal distribution, Skewness,
##   Kurtosis) (Week 5)
## - if there is a relationship(s) between North America, Europe, and global
##   sales (Week 6).

################################################################################

# Week 4 Exploratory Data Analysis

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Import the data set.
read.table(file = "turtle_sales.csv", sep = ',', header = TRUE,
           stringsAsFactors = FALSE)
df_sales_original <- read.csv("turtle_sales.csv", header = TRUE)

# Print the data frame.
print(df_sales_original)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns.
df_sales_clean <- select(df_sales_original, -Ranking, -Year, -Genre, - Publisher)

# Convert 'product' to character.
df_sales_clean <- mutate(df_sales_clean,
                         Product = as.character(Product))

# View the data frame.
head(df_sales_clean)

# Check for duplicate products.
n_occur <- data.frame(table(df_sales_clean$Product))
n_occur[n_occur$Freq > 1,]

# Create data frame of duplicate products.
df_duplicate_prod <- n_occur[n_occur$Freq > 1,]

# View the descriptive statistics for df_duplicate_prod.
summary(df_duplicate_prod)

# Understand why we have duplicate products.
# Duplicate products are normal because the same product can be on
# multiple platforms.

# If we wanted to check for duplicate records we would have to 
# concatenate the 'Product' and 'Platform' variables and then check
# for duplicates in the concatenated variable.
# Concatenating 'Product' and 'Platform' will be useful as a unique
# identifier.

# Concatenate 'Product' and 'Platform'
df_sales_clean2 <- df_sales_clean%>%
  unite(Prod_Plat, Product, Platform, remove = FALSE)

# Check for duplicate 'Prod_Plat'.
n_occur2 <- data.frame(table(df_sales_clean2$Prod_Plat))
n_occur2[n_occur$Freq > 0,]

# Create data frame of duplicate 'Prod_Plat'.
df_duplicate_prod_plat <- n_occur2[n_occur$Freq > 0,]

# View the descriptive statistics for df_duplicate_prod.
summary(df_duplicate_prod_plat)

# From the summary in the console we see that there are no 'Prod_Plat'
# duplicates.

# It might also be useful to have a variable for sales outside of US and EU
df_sales_clean_final <- mutate(df_sales_clean2, Other_Sales = (Global_Sales - NA_Sales - EU_Sales))

# View df_sale_clean_final as a tibble.
as_tibble(df_sales_clean_final)

# Move the Other_Sales column so it comes ager EU_Sales.
df_sales_clean_final <- df_sales_clean_final %>%
  relocate(Other_Sales, .after = EU_Sales)

# View df_sale_clean_final as a tibble.
as_tibble(df_sales_clean_final)

# View the descriptive statistics for df_sales_clean.
summary(df_sales_clean_final)

# The minimum for sales in other countries is 0.000 which means there are no
# obvious data accuracy issues.

# If there were missing values R would give a warning when running the summary
# statistics. We also saw there are no missing values from downloading the
# data into Excel.
# There are no missing values.

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# North America and European Union
scat_na_eu <- 
ggplot(df_sales_clean_final) +
  geom_point(aes(x = NA_Sales, y = EU_Sales),
             alpha = 0.2) +
  labs(
    title = paste(
      "Some products sales are high in both North America and in the the European Union"),
    x = "Sales in North America in £'m",
    y = "Sales in the EU in £'m"
  )

# View scatterplot.
scat_na_eu

# North America and countries outside NA and the EU
scat_na_other <-
ggplot(df_sales_clean_final) +
  geom_point(aes(x = NA_Sales, y = Other_Sales),
             alpha = 0.2) +
  labs(
    title = paste(
      "Some products sales are high in both North America and in countries other than NA and the EU"),
    x = "Sales in North America in £'m",
    y = "Sales in countries other than NA and the EU in £'m"
  )

# View scatterplot.
scat_na_other

# North America and global
scat_na_gl <- 
  ggplot(df_sales_clean_final) +
  geom_point(aes(x = NA_Sales, y = Global_Sales),
             alpha = 0.2) +
  labs(
    title = paste(
      "Some products sales are high in both North America and globally"),
    x = "Sales in North America in £'m",
    y = "Sales globally in £'m"
  )

# View scatterplot.
scat_na_gl

# EU and global
scat_eu_gl <- 
  ggplot(df_sales_clean_final) +
  geom_point(aes(x = EU_Sales, y = Global_Sales),
             alpha = 0.2) +
  labs(
    title = paste(
      "Some products sales are high in both the EU and globally"),
    x = "Sales in the EU in £'m",
    y = "Sales globally in £'m"
  )

# View scatterplot.
scat_eu_gl

# Other countries and global
scat_other_gl <- 
  ggplot(df_sales_clean_final) +
  geom_point(aes(x = Other_Sales, y = Global_Sales),
             alpha = 0.2) +
  labs(
    title = paste(
      "Some products sales are high in both other countries and globally"),
    x = "Sales in other countries in £'m",
    y = "Sales globally in £'m"
  )

# View scatterplot.
scat_other_gl

## 2b) Histograms
# Create histograms.

# North America count of products by sales value.
hist_na <- 
ggplot(df_sales_clean_final) +
  geom_histogram(aes(x = NA_Sales), binwidth = 1, boundary = 0) +
  labs(
    title = paste(
      "North America: number of products selling between £0 and £1m, £1m and £2m, etc."),
    x = "Sales in North America in £'m"
  )

# View histogram.
hist_na

# European Union count of products by sales value.
hist_eu <- 
ggplot(df_sales_clean_final) +
  geom_histogram(aes(x = EU_Sales), binwidth = 1, boundary = 0) +
  labs(
    title = paste(
      "European Union: number of products selling between £0 and £1m, £1m and £2m, etc."),
    x = "Sales in the European Union in £'m"
  )

# View histogram.
hist_eu

# Other countries count of products by sales value.
hist_other <- 
ggplot(df_sales_clean_final) +
  geom_histogram(aes(x = Other_Sales), binwidth = 1, boundary = 0) +
  labs(
    title = paste(
      "Other countries: number of products selling exactly £0, between £0 and £1m, £1m and £2m, etc."),
    x = "Sales in countries other than North America and the EU in £'m"
  )

# View histogram.
hist_other

# Global count of products by sales value.
hist_global <- 
ggplot(df_sales_clean_final) +
  geom_histogram(aes(x = Global_Sales), binwidth = 1, boundary = 0) +
  labs(
    title = paste(
      "Global: number of products selling between £0 and £1m, £1m and £2m, etc."),
    x = "Global sales in £'m"
  )

# View histogram.
hist_global

## 2c) Boxplots
# Create boxplots.

# North America sales boxplot.
# Each data point represents the sales value of a product.
box_na <- 
ggplot(df_sales_clean_final) +
  geom_boxplot(aes(y = NA_Sales),
               alpha = .3) +
  labs(
    title = paste(
      "Sales by product in North America: outliers to be understood - not ignored"),
    y = "Sales in North America in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

# View boxplot.
box_na

# European Union sales boxplot.
# Each data point represents the sales value of a product.
box_eu <- 
ggplot(df_sales_clean_final) +
  geom_boxplot(aes(y = EU_Sales),
               alpha = .3) +
  labs(
    title = paste(
      "Sales by product in the European Union: outliers to be understood - not ignored"),
    y = "Sales in the EU in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

# View boxplot.
box_eu

# Other countries sales boxplot.
# Each data point represents the sales value of a product.
box_other <- 
ggplot(df_sales_clean_final) +
  geom_boxplot(aes(y = Other_Sales),
               alpha = .3) +
  labs(
    title = paste(
      "Sales by product in other countries: outliers to be understood - not ignored"),
    y = "Sales in other countries in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

# View boxplot.
box_other

# Global sales boxplot.
# Each data point represents the sales value of a product.
box_global <- 
ggplot(df_sales_clean_final) +
  geom_boxplot(aes(y = Global_Sales),
               alpha = .3) +
  labs(
    title = paste(
      "Sales by product globally: outliers to be understood - not ignored"),
    y = "Global sales in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
        )

# View boxplot.
box_global

###############################################################################

# 3. Observations and insights

## No missing values and no obvious inaccuracies.
## Some of the products (games) are available in multiple gaming platforms.
## In terms of global sales, the middle 50% of products sell between £1.1m
## and £6.4m. There are approximately 20 outliers, with a sales value
## above £15m. The bottom 25% of products sell £1m or less.

## The sales performance for individual products seems relatively
## consistent across locations.





###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.


# Check output: Determine the min, max, and mean values.


# View the descriptive statistics.


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.


# View the data frame.


# Explore the data frame.



## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.


# Create histograms.


# Create boxplots.


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.



## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.



## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.



## 3d) Determine correlation
# Determine correlation.


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.



## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.



###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




