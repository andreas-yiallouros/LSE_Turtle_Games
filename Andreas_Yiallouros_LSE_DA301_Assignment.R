## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

#### PLEASE READ ####
#### Comments hashtags convention followed in this script: ####

# A single hashtag is for commenting out code.
## A double hashtag is for comments on the code that follows.
### A triple hashtag is for insights based on the code output.

###############################################################################

## Turtle Games, a game manufacturer and retailer, manufacture and sell their
## own products, along with sourcing and selling products manufactured by other
## companies. 
## Their product range includes books, board games, video games and toys.
## They have a global customer base.
## One of their business objectives is to improve sales performance by 
## utilising customer trends.

### Question to discuss: What is 'improving sales performance' in the mind of
### Turtle Games? For example, is it simply increasing sales as measured by
### the amount of money sales generate? Or is it improving sales performance to
### maximise profits?

## We completed data analysis in Python to help Turtle Games understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can customer online reviews be used in marketing campaigns (Week 3)

## We will use R to help Turtle Games understand:
## - what their sales per product data may be telling them (Week 4)
## - the reliability of their sales data (e.g. normal distribution, Skewness,
##   Kurtosis) (Week 5)
## - if there is a relationship(s) between North America, Europe, and global
##   sales (Week 6).

################################################################################

## Week 4 Exploratory Data Analysis

## Instructions

## 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.

## 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note our observations and diagrams that could be used to provide
##      insights to Turtle Games.

## 3. Write notes on our insights and observations.

###############################################################################

## 1. Load and explore the data

## Install Tidyverse.
## Commented out because it's only need to be done once.
# install.packages("tidyverse")

## Import Tidyverse.
library(tidyverse)

## Install skimr.
## Commented out as only need to be done once.
# install.packages("skimr")

## Import skimr
library(skimr)

## Install DataExplorer
## Commented out as only need to be done once.
# install.packages("DataExplorer")

## Import DataExplorer
library(DataExplorer)

## Install the moments package and load the library.
## Commented out as only need to be done once.
# install.packages('moments') 
library(moments)

## Set the working directory
## Commented out because it's of no value to people I share my script with.
# setwd("/Users/andreasyiallouros/Desktop/python/LSE_Course_3/LSE_DA301_assignment_files")

## Read the data set.
read.table(file = "turtle_sales.csv", sep = ',', header = TRUE,
           stringsAsFactors = FALSE)

## Create data frame object.
df_sales_original <- read.csv("turtle_sales.csv", header = TRUE)

## Print the data frame.
print(df_sales_original)

## Create a new data frame from a subset of the sales data frame.
## Remove unnecessary columns.
df_sales_clean <-
  select(df_sales_original, -Ranking, -Year, -Genre, - Publisher)

## Convert 'product' to character because it's not a numerical variable.
df_sales_clean <- mutate(df_sales_clean,
                         Product = as.character(Product))

## View the data frame head.
head(df_sales_clean)

## Check for duplicate products.
n_occur <- data.frame(table(df_sales_clean$Product))
n_occur[n_occur$Freq > 1,]

## Create data frame of duplicate products.
df_duplicate_prod <- n_occur[n_occur$Freq > 1,]

## View the descriptive statistics for df_duplicate_prod.
summary(df_duplicate_prod)

### Understand why we have duplicate products.
### Duplicate products are normal because the same product can be on
### multiple platforms.

### If we wanted to check for duplicate records we would have to 
### concatenate the 'Product' and 'Platform' variables and then check
### for duplicates in the concatenated variable.

### Concatenating 'Product' and 'Platform' will be useful as a unique
### identifier.

### Concatenate 'Product' and 'Platform'
df_sales_clean2 <- df_sales_clean%>%
  unite(Prod_Plat, Product, Platform, remove = FALSE)

## Check for duplicate 'Prod_Plat'.
n_occur2 <- data.frame(table(df_sales_clean2$Prod_Plat))
n_occur2[n_occur$Freq > 0,]

## Create data frame of duplicate 'Prod_Plat'.
df_duplicate_prod_plat <- n_occur2[n_occur$Freq > 0,]

## View the descriptive statistics for df_duplicate_prod.
summary(df_duplicate_prod_plat)

### From the summary in the console we see that there are no 'Prod_Plat'
### duplicates.

### It might also be useful to have a variable for sales outside of US and EU
df_sales_clean_final <-
  mutate(df_sales_clean2, Other_Sales = (Global_Sales - NA_Sales - EU_Sales))

## View df_sale_clean_final as a tibble.
as_tibble(df_sales_clean_final)

## Move the Other_Sales column so it comes after EU_Sales.
df_sales_clean_final <- df_sales_clean_final %>%
  relocate(Other_Sales, .after = EU_Sales)

## View df_sale_clean_final as a tibble.
as_tibble(df_sales_clean_final)

## View the descriptive statistics for df_sales_clean.
summary(df_sales_clean_final)

### The minimum for sales in other countries is 0.000 which means there are no
### obvious data accuracy issues.

### If there were missing values R would give a warning when running the
### summary statistics.
### We also saw there are no missing values from downloading the data into
### Excel.

### There are no missing values.

## Then learned about sum(is.na()) and practiced it here.
sum(is.na(df_sales_clean_final))

### There are no missing values.

################################################################################

## 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
## Create scatterplots.

## North America and European Union
scat_na_eu <- 
  ggplot(df_sales_clean_final, aes(x = NA_Sales, y = EU_Sales)) +
    geom_point(alpha = 0.3, color = "#00BA38") +
    scale_x_continuous(breaks = seq(2, 40, by = 2)) +
    scale_y_continuous(breaks = seq(2, 30, by = 2)) +
    labs(
      title = paste(
        "Most (but not all) sales are as high in North America...
as they are in the European Union"),
      subtitle = paste("Total number of products, and hence data points = 352"),
      x = "Sales in North America in £'m",
      y = "Sales in the EU in £'m"
  )

## View scatterplot.
scat_na_eu

## North America and countries outside NA and the EU
scat_na_other <-
  ggplot(df_sales_clean_final, aes(x = NA_Sales, y = Other_Sales)) +
    geom_point(alpha = 0.3, color = "#00BA38") +
    scale_x_continuous(breaks = seq(2, 40, by = 2)) +
    scale_y_continuous(breaks = seq(1, 11, by = 1)) +
    labs(
      title = paste(
        "Most (but not all) sales are as high in North America...
as they are in countries other than NA and the EU"),
      subtitle = paste("Total number of products, and hence data points = 352"),
      x = "Sales in North America in £'m",
      y = "Sales in countries other than NA and the EU in £'m"
  )

## View scatterplot.
scat_na_other

## North America and global
scat_na_gl <- 
  ggplot(df_sales_clean_final, aes(x = NA_Sales, y = Global_Sales)) +
    geom_point(alpha = 0.3, color = "#00BA38") +
    scale_x_continuous(breaks = seq(2, 40, by = 2)) +
    scale_y_continuous(breaks = seq(5, 70, by = 5)) +
    labs(
      title = paste(
        "Most (but not all) sales are as high in North America...
as they are globally"),
      subtitle = paste("Total number of products, and hence data points = 352"),
      x = "Sales in North America in £'m",
      y = "Sales globally in £'m"
  )

## View scatterplot.
scat_na_gl

## EU and global
scat_eu_gl <- 
  ggplot(df_sales_clean_final, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  scale_x_continuous(breaks = seq(2, 40, by = 2)) +
  scale_y_continuous(breaks = seq(5, 70, by = 5)) +
  labs(
    title = paste(
      "Most (but not all) sales are as high in the EU as they are globally"),
    subtitle = paste("Total number of products, and hence data points = 352"),
    x = "Sales in the EU in £'m",
    y = "Sales globally in £'m"
  )

## View scatterplot.
scat_eu_gl

## Other countries and global
scat_other_gl <- 
  ggplot(df_sales_clean_final, aes(x = Other_Sales, y = Global_Sales)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_y_continuous(breaks = seq(5, 70, by = 5)) +
  labs(
    title = paste(
      "Most (but not all) sales are as high in other countries...
as they are globally"),
    subtitle = paste("Total number of products, and hence data points = 352"),
    x = "Sales in other countries in £'m",
    y = "Sales globally in £'m"
  )

## View scatterplot.
scat_other_gl

## 2b) Histograms
## Create histograms.

## North America count of products by sales value.
hist_na <- 
ggplot(df_sales_clean_final, aes(x = NA_Sales)) +
  geom_histogram(binwidth = 2, boundary = 0, alpha = 0.6, fill = "#00BA38") +
  scale_x_continuous(breaks = seq(2, 40, by = 2)) +
  scale_y_continuous(breaks = seq(10, 200, by = 10)) +
  labs(
    title = paste(
      "Number of products selling between £0 and £2m, £2m and £4m, etc."),
    subtitle = paste("North America"),
    x = "Sales in North America in £'m",
    y = "Number of products (total 352)"
  )

## View histogram.
hist_na

### Positively skewed distribution.

## European Union count of products by sales value.
hist_eu <- 
ggplot(df_sales_clean_final, aes(x = EU_Sales)) +
  geom_histogram(binwidth = 1, boundary = 0, alpha = 0.6, fill = "#00BA38") +
  scale_x_continuous(breaks = seq(1, 25, by = 1)) +
  scale_y_continuous(breaks = seq(10, 160, by = 10)) +
  labs(
    title = paste(
      "Number of products selling between £0 and £1m, £1m and £2m, etc."),
    subtitle = paste("European Union"),
    x = "Sales in the European Union in £'m",
    y = "Number of products (total 352)"
  )

## View histogram.
hist_eu

### Positively skewed distribution.

## Other countries count of products by sales value.
hist_other <- 
ggplot(df_sales_clean_final, aes(x = Other_Sales)) +
  geom_histogram(binwidth = 1, boundary = 0, alpha = 0.6, fill = "#00BA38") +
  scale_x_continuous(breaks = seq(-1, 12, by = 1)) +
  scale_y_continuous(breaks = seq(10, 220, by = 10)) +
  labs(
    title = paste(
      "Number of products selling exactly £0, between £0 and £1m, etc."),
    subtitle = paste("Other countries"),
    x = "Sales in countries other than North America and the EU in £'m",
    y = "Number of products (total 352)"
  )

## View histogram.
hist_other

### Positively skewed distribution.


## Global count of products by sales value.
hist_global <- 
ggplot(df_sales_clean_final, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 5, boundary = 0, alpha = 0.6, fill = "#00BA38") +
  scale_x_continuous(breaks = seq(5, 70, by = 5)) +
  scale_y_continuous(breaks = seq(10, 220, by = 10)) +
  labs(
    title = paste(
      "Number of products selling between £0 and £5m, £5m and £10m, etc."),
    subtitle = paste("Global"),
    x = "Global sales in £'m",
    y = "Number of products (total 352)"
  )

## View histogram.
hist_global

### Positively skewed distribution.


## 2c) Boxplots
## Create boxplots.

## North America sales boxplot.
## Each data point represents the sales value of a product.
box_na <- 
ggplot(df_sales_clean_final) +
  geom_boxplot(aes(y = NA_Sales),
               alpha = .3, fill = "#00BA38") +
  scale_y_continuous(breaks = seq(2, 40, by = 2)) +
  labs(
    title = paste(
      "Sales by product in North America"),
    subtitle = paste(
      "Outliers to be understood - not ignored"),
    y = "Sales in North America in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

## View boxplot.
box_na

## European Union sales boxplot.
## Each data point represents the sales value of a product.
box_eu <- 
ggplot(df_sales_clean_final) +
  geom_boxplot(aes(y = EU_Sales),
               alpha = .3, fill = "#00BA38") +
  scale_y_continuous(breaks = seq(1, 30, by = 1)) +
  labs(
    title = paste(
      "Sales by product in the European Union"),
    subtitle = paste(
      "Outliers to be understood - not ignored"),
    y = "Sales in the EU in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

## View boxplot.
box_eu

## Other countries sales boxplot.
## Each data point represents the sales value of a product.
box_other <- 
ggplot(df_sales_clean_final) +
  geom_boxplot(aes(y = Other_Sales),
               alpha = .3, fill = "#00BA38") +
  scale_y_continuous(breaks = seq(0.5, 11, by = 0.5)) +
  labs(
    title = paste(
      "Sales by product in other countries"),
    subtitle = paste(
      "Outliers to be understood - not ignored"),
    y = "Sales in other countries in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

## View boxplot.
box_other

## Global sales boxplot.
## Each data point represents the sales value of a product.
box_global <- 
ggplot(df_sales_clean_final) +
  geom_boxplot(aes(y = Global_Sales),
               alpha = .3, fill = "#00BA38") +
  scale_y_continuous(breaks = seq(2, 80, by = 2)) +
  labs(
    title = paste(
      "Sales by product globally"),
    subtitle = paste(
      "Outliers to be understood - not ignored"),
    y = "Global sales in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
        )

## View boxplot.
box_global

###############################################################################

### 3. Observations and insights

### No missing values and no obvious inaccuracies.
### Some of the products (games) are available in multiple gaming platforms.
### In terms of global sales, the middle 50% of products sell between £1.1m
### and £6.4m. There are approximately 20 outliers, with a sales value
### above £15m. The bottom 25% of products sell £1m or less.

### The sales performance for individual products seems relatively
### consistent across locations.





###############################################################################
###############################################################################


## Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
## 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.

## 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.

## 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.

## 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.

## 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.

## 6. Note our insights and observations.

################################################################################

## 1. Load and explore the data

## View data frame created in Week 4.
as_tibble(df_sales_clean_final)

## Determine the min, max, and mean sales values for North America.
summarise(df_sales_clean_final, min(NA_Sales))
summarise(df_sales_clean_final, max(NA_Sales))
summarise(df_sales_clean_final, mean(NA_Sales))

## Determine the min, max, and mean sales values for the EU.
summarise(df_sales_clean_final, min(EU_Sales))
summarise(df_sales_clean_final, max(EU_Sales))
summarise(df_sales_clean_final, mean(EU_Sales))

## Determine the min, max, and mean sales values globally.
summarise(df_sales_clean_final, min(Global_Sales))
summarise(df_sales_clean_final, max(Global_Sales))
summarise(df_sales_clean_final, mean(Global_Sales))

## View the descriptive statistics with the skim() function.
skim(df_sales_clean_final)

## View the descriptive statistics with the summary() function.
summary(df_sales_clean_final)

###############################################################################

## 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
## Group data based on Product and determine the sum per Product.
df_sales_per_product <- group_by(df_sales_clean_final, Product) %>%
  summarise(total_per_product_na = sum(NA_Sales),
            total_per_product_eu = sum(EU_Sales),
            total_per_product_other = sum(Other_Sales),
            total_per_product_global = sum(Global_Sales))

## View the data frame.
as_tibble(df_sales_per_product)

## Explore the data frame with the summary() function.
summary(df_sales_per_product)

## Explore the data frame with the skim() function.
skim(df_sales_per_product)


## 2b) Determine which plot is the best to compare game sales.

## Create scatterplots.
## North America and European Union
scat_na_eu_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_na,
                                   y = total_per_product_eu)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  scale_x_continuous(breaks = seq(2, 40, by = 2)) +
  scale_y_continuous(breaks = seq(2, 30, by = 2)) +
  labs(
    title = paste(
      "Most (but not all) sales are as high in North America...
as they are in the European Union"),
    x = "Sales in North America grouped by product in £'m",
    y = "Sales in the EU grouped by product in £'m"
  )

## View scatterplot.
scat_na_eu_grouped

## North America and countries outside NA and the EU
scat_na_other_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_na,
                                   y = total_per_product_other)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  scale_x_continuous(breaks = seq(2, 40, by = 2)) +
  scale_y_continuous(breaks = seq(1, 11, by = 1)) +
  labs(
    title = paste(
      "Most (but not all) sales are as high in North America...
as they are in countries other than NA and the EU"),
    subtitle = paste("Total number of product groups, and hence data points = 175"),
    x = "Sales in North America grouped by product in £'m",
    y = "Sales in other countries grouped by product in £'m"
  )

## View scatterplot.
scat_na_other_grouped

## North America and global
scat_na_global_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_na,
                                   y = total_per_product_global)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  scale_x_continuous(breaks = seq(2, 40, by = 2)) +
  scale_y_continuous(breaks = seq(5, 70, by = 5)) +
  labs(
    title = paste(
      "Most (but not all) sales are as high in North America...
as they are globally"),
    subtitle = paste("Total number of product groups, and hence data points = 175"),
    x = "Sales in North America grouped by product in £'m",
    y = "Sales globally grouped by product in £'m"
  )

## View scatterplot.
scat_na_global_grouped

## EU and global
scat_eu_global_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_eu,
                                   y = total_per_product_global)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  scale_x_continuous(breaks = seq(2, 40, by = 2)) +
  scale_y_continuous(breaks = seq(5, 70, by = 5)) +
  labs(
    title = paste(
      "Most (but not all) sales are as high in the EU as they are globally"),
    subtitle = paste("Total number of product groups, and hence data points = 175"),
    x = "Sales in the EU grouped by product in £'m",
    y = "Sales globally grouped by product in £'m"
  )

## View scatterplot.
scat_eu_global_grouped

## Other countries and global
scat_other_global_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_other,
                                   y = total_per_product_global)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_y_continuous(breaks = seq(5, 70, by = 5)) +
  labs(
    title = paste(
      "Most (but not all) sales are as high in other countries...
as they are globally"),
    subtitle = paste("Total number of product groups, and hence data points = 175"),
    x = "Sales in other countries grouped by product in £'m",
    y = "Sales globally grouped by product in £'m"
  )

## View scatterplot.
scat_other_global_grouped


## Create histograms.
## North America count of product groups by sales value.
hist_na_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_na)) +
  geom_histogram(binwidth = 2, boundary = 0, alpha = 0.6, fill = "#00BA38") +
  scale_x_continuous(breaks = seq(2, 40, by = 2)) +
  scale_y_continuous(breaks = seq(5, 100, by = 5)) +
  labs(
    title = paste(
      "Number of product groups selling between £0 and £2m, £2m and £4m, etc."),
    subtitle = paste(
      "North America"),
    x = "Sales in North America in £'m",
    y = "Number of product groups (total 175)"
  )

## View histogram.
hist_na_grouped

## European Union count of product groups by sales value.
hist_eu_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_eu)) +
  geom_histogram(binwidth = 1, boundary = 0, alpha = 0.6, fill = "#00BA38") +
  scale_x_continuous(breaks = seq(1, 25, by = 1)) +
  scale_y_continuous(breaks = seq(5, 60, by = 5)) +
    labs(
    title = paste(
      "Number of product groups selling between £0 and £1m, £1m and £2m, etc."),
    subtitle = paste(
      "European Union"),
    x = "Sales in the European Union in £'m",
    y = "Number of product groups (total 175)"
  )

## View histogram.
hist_eu_grouped

## Other countries count of product groups by sales value.
hist_other_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_other)) +
  geom_histogram(binwidth = 1, boundary = 0, alpha = 0.6, fill = "#00BA38") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(breaks = seq(5, 60, by = 5)) +
  labs(
    title = paste(
      "Number of product groups selling between £0 and £1m, £1m and £2m, etc."),
    subtitle = paste(
      "Other countries"),
    x = "Sales in countries other than North America and the EU in £'m",
    y = "Number of product groups (total 175)"
  )

## View histogram.
hist_other_grouped

## Global count of product groups by sales value.
hist_global_grouped <- 
  ggplot(df_sales_per_product, aes(x = total_per_product_global)) +
  geom_histogram(binwidth = 5, boundary = 0, alpha = 0.6, fill = "#00BA38") +
  scale_x_continuous(breaks = seq(5, 70, by = 5)) +
  scale_y_continuous(breaks = seq(5, 100, by = 5)) +
  labs(
    title = paste(
      "Number of product groups selling between £0 and £5m, £5m and £10m, etc."),
    subtitle = paste(
      "Global"),
        x = "Global sales in £'m",
    y = "Number of product groups (total 175)"
  )

## View histogram.
hist_global_grouped


## Create boxplots.

## North America sales boxplot.
## Each data point represents the sales value of a product group.
box_na_grouped <- 
  ggplot(df_sales_per_product) +
  geom_boxplot(aes(y = total_per_product_na),
               alpha = .3, fill = "#00BA38") +
  scale_y_continuous(breaks = seq(2, 40, by = 2)) +
  labs(
    title = paste(
      "Sales by product group in North America"),
    subtitle = paste(
      "Outliers to be understood - not ignored"),
    y = "Sales in North America in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

## View boxplot.
box_na_grouped

## European Union sales boxplot.
## Each data point represents the sales value of a product.
box_eu_grouped <- 
  ggplot(df_sales_per_product) +
  geom_boxplot(aes(y = total_per_product_eu),
               alpha = .3, fill = "#00BA38") +
  scale_y_continuous(breaks = seq(1, 30, by = 1)) +
  labs(
    title = paste(
      "Sales by product group in the EU"),
    subtitle = paste(
      "Outliers to be understood - not ignored"),
    y = "Sales in the EU in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

## View boxplot.
box_eu_grouped

## Other countries sales boxplot.
## Each data point represents the sales value of a product.
box_other_grouped <- 
  ggplot(df_sales_per_product) +
  geom_boxplot(aes(y = total_per_product_other),
               alpha = .3, fill = "#00BA38") +
  scale_y_continuous(breaks = seq(0.5, 11, by = 0.5)) +
  labs(
    title = paste(
      "Sales by product group in other countries"),
    subtitle = paste(
      "Outliers to be understood - not ignored"),
    y = "Sales in other countries in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

## View boxplot.
box_other_grouped

## Global sales boxplot.
## Each data point represents the sales value of a product.
box_global_grouped <- 
  ggplot(df_sales_per_product) +
  geom_boxplot(aes(y = total_per_product_global),
               alpha = .3, fill = "#00BA38") +
  scale_y_continuous(breaks = seq(2, 80, by = 2)) +
  labs(
    title = paste(
      "Sales by product group globallt"),
    subtitle = paste(
      "Outliers to be understood - not ignored"),
    y = "Global sales in £'m") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )

## View boxplot.
box_global_grouped

###############################################################################


## 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots

## Create Q-Q Plots.

## North America.
qqnorm(df_sales_clean_final$NA_Sales)
qqline(df_sales_clean_final$NA_Sales)

## EU.
qqnorm(df_sales_clean_final$EU_Sales)
qqline(df_sales_clean_final$EU_Sales)

## Other countries.
qqnorm(df_sales_clean_final$Other_Sales)
qqline(df_sales_clean_final$Other_Sales)

## Global sales.
qqnorm(df_sales_clean_final$Global_Sales)
qqline(df_sales_clean_final$Global_Sales)

## 3b) Perform Shapiro-Wilk test.

## North America.
shapiro.test(df_sales_clean_final$NA_Sales)

## EU.
shapiro.test(df_sales_clean_final$EU_Sales)

## Other countries.
shapiro.test(df_sales_clean_final$Other_Sales)

## Global sales.
shapiro.test(df_sales_clean_final$Global_Sales)


## 3c) Determine Skewness and Kurtosis.

## Skewness and Kurtosis.

## North America.
skewness(df_sales_clean_final$NA_Sales)
kurtosis(df_sales_clean_final$NA_Sales)

## EU.
skewness(df_sales_clean_final$EU_Sales)
kurtosis(df_sales_clean_final$EU_Sales)

## Other countries.
skewness(df_sales_clean_final$Other_Sales)
kurtosis(df_sales_clean_final$Other_Sales)

## Global sales.
skewness(df_sales_clean_final$Global_Sales)
kurtosis(df_sales_clean_final$Global_Sales)


## 3d) Determine correlation

## Determine correlation.
cor(df_sales_clean_final$NA_Sales, df_sales_clean_final$EU_Sales)

cor(df_sales_clean_final$NA_Sales, df_sales_clean_final$Other_Sales)

cor(df_sales_clean_final$NA_Sales, df_sales_clean_final$Global_Sales)

cor(df_sales_clean_final$EU_Sales, df_sales_clean_final$Global_Sales)

cor(df_sales_clean_final$EU_Sales, df_sales_clean_final$Other_Sales)

cor(df_sales_clean_final$Global_Sales, df_sales_clean_final$Other_Sales)

## Select the numeric variables into a new data frame.
df_sales_clean_final_num_only <- select(df_sales_clean_final, -Prod_Plat,
                                        -Product, -Platform)

## View the new data frame.
head(df_sales_clean_final_num_only)

## View the correlation matrix.
cor(df_sales_clean_final_num_only)

###############################################################################

## 4. Plot the data

## Create plots to gain insights from the data.
## Choose the type of plot you think best suits the data set and what you want 
## to investigate.

## Explain your answer in your report.


###############################################################################

### 5. Observations and insights

### The data patterns are consistent across NA, EU and Global.

### Sales in NA are highly correlated with sales in the EU.
### Sales in NA and in the EU are respectively even more highly correlated
### with sales globally, which is as expected given sales globally include
### the sales in NA and in the EU.

### The Shapiro-Wilk test was statistically significant for all data sets
### indicating that normal distribution cannot be assumed.

### Skewness is consistent across all data sets with values above 4 indicating
### positively skewed data.

### Kurtosis is consistent across all data sets with values above 30.

### The Q-Q plots are also consistent across the data sets.
### They all confirm a heavy tail on the high end of sales values
### (some products sell at many standard deviations above the mean) and
### a light tail at the low end of sales values.

### Other country sales are slightly less predictable which could be simply
### due to relatively small numbers.


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand the relationship
## between North America, Europe, and global sales.

## Therefore, we will investigate possible relationships in the sales data
## using a simple linear regression model, and a multiple linear regression
## model. 

## Based on the two models, and our previous analysis (Weeks 1-5), we will 
## provide reasoned recommendations to Turtle Games based on:
##   - our confidence in the models based on goodness of fit and accuracy of
##     predictions
##   - our suggestions and recommendations
##   - how the models could be improved

## Instructions
## 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 

## 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.

## 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.

## 4. Predict global sales based on provided values. Compare your prediction to
##    the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

## 5. Include your insights and observations.

###############################################################################

## 1. Load and explore the data

## View data frame created in Week 5.
as_tibble(df_sales_clean_final)

## Determine a summary of the data frame.
summary(df_sales_clean_final)

###############################################################################

## 2. Create a simple linear regression model

## 2a) Determine the correlation between columns
cor(df_sales_clean_final_num_only)

## Create a linear regression model on the original data.

## EU Sales as the dependent variable and NA Sales as the independent variable.
model1 <- lm(EU_Sales ~ NA_Sales,
             data = df_sales_clean_final)

## View the model coefficients.
model1

## View the model summary.
summary(model1)

## Global Sales as the dependent variable and NA Sales as the independent
## variable.
model2 <- lm(Global_Sales ~ NA_Sales,
             data = df_sales_clean_final)

## View the model coefficients.
model2

## View the model summary.
summary(model2)

## Global Sales as the dependent variable and EU Sales as the independent
## variable.
model3 <- lm(Global_Sales ~ EU_Sales,
             data = df_sales_clean_final)

## View the model coefficients.
model3

## View the model summary.
summary(model3)

## Global Sales as the dependent variable and Other Sales as the independent
## variable.
model4 <- lm(Global_Sales ~ Other_Sales,
             data = df_sales_clean_final)

## View the model coefficients.
model4

## View the model summary.
summary(model4)

###############

## 2b) Create a plot (simple linear regression)
## Basic visualisation.

## EU Sales as the dependent variable and NA Sales as the independent variable.
plot(df_sales_clean_final$NA_Sales, df_sales_clean_final$EU_Sales)
abline(coefficients(model1))

## Global Sales as the dependent variable and NA Sales as the independent
## variable.
plot(df_sales_clean_final$NA_Sales, df_sales_clean_final$Global_Sales)
abline(coefficients(model2))

## Global Sales as the dependent variable and EU Sales as the independent
## variable.
plot(df_sales_clean_final$EU_Sales, df_sales_clean_final$Global_Sales)
abline(coefficients(model3))

## Global Sales as the dependent variable and Other Sales as the independent
## variable.
plot(df_sales_clean_final$Other_Sales, df_sales_clean_final$Global_Sales)
abline(coefficients(model4))

###############################################################################

## 3. Create a multiple linear regression model
## Select only numeric columns from the original data frame.
as_tibble(df_sales_clean_final_num_only)

## Multiple linear regression model.
model5 <- lm(Global_Sales ~ NA_Sales + EU_Sales,
             data = df_sales_clean_final_num_only)

## View the model coefficients.
model5

## View the model summary.
summary(model5)

###############################################################################

## 4. Predictions based on given values

## Create new data frame with for predictions.
sales_pred <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                         EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

## View new data frame.
sales_pred

## Predict.
predict(model5,
        newdata = sales_pred)


## Compare with observed values for a number of records.
as_tibble(df_sales_clean_final_num_only)


###############################################################################

### 5. Observations and insights

### As expected given our earlier calculations of correlation, sales in one
### location can be used to predict sales in another.


###############################################################################
###############################################################################




