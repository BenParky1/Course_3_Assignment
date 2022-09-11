## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.
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
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
## how customers accumulate loyalty points
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
sales <- read.csv(file.choose(), header = TRUE)

# Print the data frame.
View(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
sales2

# View the descriptive statistics.
summary(sales2)
as.tibble(sales2)
View(sales2)
################################################################################

# 2. Review plots to determine insights into the data set.
## the impact that each product has on sales
## how reliable the data is (e.g. normal distribution, skewness, or kurtosis)
## what the relationship(s) is/are (if any) between North American, European, and global sales.
## 2a) Scatterplots
# Create scatterplots.
qplot(EU_Sales, NA_Sales, data = sales2, geom='point')
qplot(Product, Global_Sales, data = sales2, geom='point')
## 2b) Histograms
# Create histograms.
qplot(Platform, data = sales2 ,geom = 'bar')

## 2c) Boxplots
# Create boxplots.
qplot(Platform, Global_Sales, data=sales2, geom='boxplot', colour=I('blue'))

###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# filter by a random existing Product ID to see if there are multiple rows for products
filter(sales2, Product == 535)
# Group data based on Product and determine the sum per Product.
sum_product <- aggregate(Global_Sales~Product, sales2, sum)
# View the data frame.
sum_product
# Explore the data frame.
summary(sum_product)

# Create a df aggregating the sum of the global sales per platform and product
plat_prod <- aggregate(Global_Sales~Platform+Product, sales2, sum)
plat_prod

## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product, Global_Sales, data = sum_product, geom='point')

# Create histograms.
qplot(Product, data =sum_product, geom='bar')


###############################################################################

# 4. Observations and insights

## Your observations and insights here ......
# There are outliers for a lot of global sales per Platform, as well as one outlier 
# in Global Sales per Product. There is one product that seems to have sold particularly well
# in NA and EU, this could also be an outlier

# Write csv file to folder
write.csv(sales2, "Sales_Week_4.csv")




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
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(sales2)

# Check for NA's
sum(is.na(sales2))

# Group by the Product
sales3 <- sales2 %>% group_by(Product) %>%
  summarise(NA_Sales=sum(NA_Sales),
            EU_Sales=sum(EU_Sales),
            Global_Sales=sum(Global_Sales),
            .groups='drop')

View(sales3)

# Create a df genre which is summing the sales per genre
genre <- sales %>% group_by(Genre) %>%
  summarise(NA_Sales=sum(NA_Sales),
            EU_Sales=sum(EU_Sales),
            Global_Sales=sum(Global_Sales),
            .groups='drop')

View(genre)

# Check output: Determine the min, max, and mean values.
# NA_sales
min(sales3$NA_Sales)
max(sales3$NA_Sales)
mean(sales3$NA_Sales)
median(sales3$NA_Sales)
# Mean and median not too far apart so data could be normally distributed

# EU_Sales
min(sales3$EU_Sales)
max(sales3$EU_Sales)
mean(sales3$EU_Sales)
median(sales3$EU_Sales)
# Global Sales
min(sales3$Global_Sales)
max(sales3$Global_Sales)
mean(sales3$Global_Sales)
median(sales3$Global_Sales)
# Mean and median  further apart so data may not be normally distributed

# View the descriptive statistics.
summary(sales3)

# Write csv file to folder
write.csv(sales3, "Sales_Week_5.csv")

###############################################################################

# 2. Determine the normality of the data set (NA_Sales).
hist(sales3$NA_Sales)
boxplot(sales3$NA_Sales)
# Histogram is skewed to the right, boxplot shows outliers at the higher sales values

# Determine the normality of the data set (EU_Sales)
hist(sales3$EU_Sales)
boxplot(sales3$EU_Sales)
# Histogram is skewed to the right, boxplot shows outliers at the higher sales values

# Determine the normality of the data set (Global_Sales)
hist(sales3$Global_Sales)
boxplot(sales3$Global_Sales)
# Histogram is skewed to the right, boxplot shows outliers at the higher sales values

## 2a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales3$NA_Sales)
qqline(sales3$NA_Sales)

qqnorm(sales3$EU_Sales)
qqline(sales3$EU_Sales)

qqnorm(sales3$Global_Sales)
qqline(sales3$Global_Sales)
# Slight correlation in the 3 plots at points. The 3 graphs looks similar

## 2b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales3$NA_Sales)
shapiro.test(sales3$EU_Sales)
shapiro.test(sales3$Global_Sales)

## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales3$NA_Sales)
kurtosis(sales3$NA_Sales)

skewness(sales3$EU_Sales)
kurtosis(sales3$EU_Sales)

skewness(sales3$Global_Sales)
kurtosis(sales3$Global_Sales)
# Skewness and kurtosis results show datasets skewed to the  with lots of outliers

## 2d) Determine correlation
# Determine correlation. NA_Sales vs EU_sales as Global sales is the total of both
round(cor(sales3$NA_Sales, sales3$EU_Sales), digits = 2)
# Positive value shows positive correlations, value is greater to 1 than 0 so this shows a close correlation
# between both sales regions
###############################################################################
View(sales)
# 3. Plot the data
# Create plots to gain insights into data.
# Bar plot of Global Sales per platform
ggplot(sales, aes(Platform)) +geom_bar(fill='dark green',
                                       color = 'black') +
  coord_flip() +
  labs(title = "Global Sales per Platform",
       x = "Platform",
       y = "Sales (£M)") + 
  theme_classic()

# Scatterplot of correlation between EU and NA sales
Scat <- ggplot(sales3, aes(x=EU_Sales, y=NA_Sales, color=Product)) +
  geom_point(size=2,
             alpha=0.75) +
  geom_smooth(method = 'lm') +
  labs(title = "Relationship between North American and European Sales for each Product",
       x= "Sales in Europe (£M)",
       y= "Sales in North America (£M)")

ggplotly(Scat)

# Check of the 3 'outliers for insights
filter(sales, Product == 326)
filter(sales, Product == 254)
filter(sales, Product == 123)

# Stacked Bar of Genre popularity for each Publisher
pop <- ggplot(sales, aes(x=Publisher, fill=Genre)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Genre popularity for each Publisher",
       y = "Number of Worldwide Sales") +
  theme_minimal()

ggplotly(pop)

# Create a barchart of Sales by Genre
ggplot(genre, aes(x=Genre, y=Global_Sales, fill=EU_Sales) +
  geom_bar(stat = 'identity',
           fill='blue') +
  labs(title = "Global Sales(£M) per Genre",
       y = "Sales(£M)")
 

###############################################################################

# 4. Observations and insights
# Your observations and insights here...
# Genre, Publisher and platform have a big impact on the sales. Some publishers
# are very popular for certain genres. PC, X360 and PS£ are the most popular
# platforms. There is correlation between sales in Europe and North America
# suggesting that certain products are popular/unpopular in both territories

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

sales6 <- read.csv(file.choose(), header = TRUE)

# Determine a summary of the data frame.
summary(sales6)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1 <- lm(NA_Sales~EU_Sales, data=sales6)
summary(model1)

# Plot the residuals
plot(model1$residuals)
plot(sales6$NA_Sales, sales6$EU_Sales)
abline(coefficients(model1))
## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales6$NA_Sales, sales6$EU_Sales)
abline(coefficients(model1))
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
str(sales)

# Multiple linear regression model.
model2 <- lm(Global_Sales~NA_Sales + EU_Sales + Year + Product + Ranking,
             data=sales)
summary(model2)

model3 <- lm(Global_Sales~NA_Sales + EU_Sales,
             data=sales)
summary(model3)
###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
# Select random rows from the sales dataframe
salestest<- sales[c(1,4,8,75,148),]
head(salestest)
# Use the predict function to predict values. Accuracy can be checked by looking 
# at confidence intervals
predictTest <- predict(model2, newdata=salestest,
                       interval = 'confidence')
# Print results
predictTest
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# Model 2 used as it had the highest Adjusted and multiple mean squared values.
# The comparison of the test model ran on the random data sample shows that the
# model is accurate, the fit, upr and lwr values are very close and bracket the 
# observed values.

###############################################################################
###############################################################################




