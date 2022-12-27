# LSE_Turtle_Games

## Purpose
We will analyse data to provide insights that will help Turtle Games with its business objective to "improve overall sales performance by utilising cutomer trends".

## Approach
Turtle Games provided us with two data sets, and metadata for the two data sets combined in a text file.

The data set file names are:
'turtle_reviews.csv'
'turtle_sales.csv'

We will analyse the data over the following six weeks. Each week we will focus on one of the initial set of six questions that Turtle Games has come up with to break down their overall business objective of improving sales.

The six questions as provided by Turtle Games are:

1. How do customers accumulate loyalty points?
2. How can groups within the customer base be used to target specific market segments?
3. How can social data (e.g., customer reviews) be used to inform marketing campaigns?
4. What is the impact of each product on sales?
5. How reliable is the data? (e.g., normal distribution, skewness, or kurtosis)
6. What is the relationship(s) between North American, European, and global sales?

We will collaborate as a team of data analysts using a GitHub repository.

In addition to our code, our repository includes the data and metadata files.

## Technology
We will use Python for weeks 1 - 3 and R for weeks 4 - 6.

## Insights from week 1
No missing values in ‘turtle_reviews.csv’.

Customer age does not seem to predict loyalty points based on a linear model. However, there is some indication that customers in the age range between 30 and 40 seem to possibly have a higher number of loyalty points.

Using a linear model remuneration and spending scores seem to predict respectively approximately 38% and 45% of the variance in loyalty points. However, this is subject to potential limitations that should be explored further. See below.

To explore further:
- Outliers, especially for loyalty points. Outliers might indicate data accuracy issues or could reveal useful insights.
- Which countries are represented in ‘turtle_reviews.csv’. This is important for example in our interpretation of the remuneration data because of potential differences in the cost of living across different countries.
- Potential benefit from data transformation to address the heteroskedasticity issue for the relationship between loyalty points and remuneration and between loyalty points and spending scores.
- A non-linear model that may represent the relationship between age and loyalty points.
- Whether the fit of the models can be improved through the tools in Python’s Scikit-Learn for model validation.

Questions for our stakeholders at Turtle Games:
1.	What are the controls in place to ensure the accuracy, completeness, and validity of the data in 'turtle_reviews.csv'?
2.	How were the 2,000 customers selected?
3.	What other columns are available and relevant to our analysis, but not included in ‘turtle_reviews.csv’?
4.	The metadata describes 'spending_score' as 'a score (between 1 and 100) assigned to each customer based on the customer's spending nature and behaviour'.
How are 'spending nature' and 'spending behaviour' measured?
How is 'spending_score' mapped to 'spending nature' and spending behaviour' measures?
Are spending scores comparable across customers?
5.	The metadata describes 'loyalty_points' as 'a score based on the point value of the purchase'.
How are 'loyalty_points' mapped to 'purchase value' (for example, is it one loyalty point per £1 of purchase)?