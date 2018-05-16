# Data source: https://www.kaggle.com/brijbhushannanda1979/bigmart-sales-data/data

# Objective: To predict the sales of retail chain.

library(readr) # For importing files
library(dplyr) # For data manipulation
library(stringr) # For string manipulation
library(ggplot2) # For visualization
library(descr) # For CrossTab
library(sqldf) # For SQL
library(moments) # For skewness and kurtosis




# Reading data to a data frame
Data_og <- read_csv("F:/IEM/Studies/Extra/R/DataProjects/Retail/RetailData.csv")


# Structure of the data
str(Data_og)

# Creating a copy of the Original data
Data_copy <- Data_og

# Converting character variables to factors
character_vars <- lapply(Data_copy, class) == "character"
Data_copy[, character_vars] <- lapply(Data_copy[, character_vars], as.factor)


# Exploring the data
summary(Data_copy)

# Observation:-
# Missing values found in Item_weight and Outlet_Size.
# Different notation for the same categories in Item_Fat_content. It needs to be cleaned. 
# Lots of categories in Item_Type and Item_Identifier. We will have to bin them for modeling purpose.
# Item_visibility has minimum value of 0 which seems to be a data entry error. It needs to be corrected.


# Cleaning Item_Fat_Content as there are variations in the spellings of Low Fat and Regular.
Data_copy$Item_Fat_Content <- revalue(Data_copy$Item_Fat_Content, c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))


# Checking Fat_Content and Item_Type
table(Data_copy$Item_Fat_Content,Data_copy$Item_Type)

# Observation:-
# In Item_Type variable, Health & Hygiene and Household categories have been shown having fat content.
# Fat content should be none in the above categories.


# Adding a none category to Item_Fat_Content.
# In Item_Type, Health and Hygiene and Household doesn't have fat. Thus imputing None instead of Low Fat.
levels(Data_copy$Item_Fat_Content) <- c(levels(Data_copy$Item_Fat_Content), "None")

# Replacing Low Fat with None for Health & Hygiene and Household categories
Data_copy[ which(Data_copy$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content <- "None"
Data_copy[ which(Data_copy$Item_Type == "Household") ,]$Item_Fat_Content <- "None"


# Cross Tab for Item_Fat_Content and Item_Type
table(Data_copy$Item_Fat_Content,Data_copy$Item_Type)



# Binning the categories for Item_Type and Item_Identifier.
# Using the first 2 character of the Item_Identifier to create a common category.
unique(str_sub(Data_copy$Item_Identifier,1,2))

# Observation:-
# 3 unique categories created - NC for Non-consumables, FD for food items and DR for drinks.
# These values can be used instead of Item_Identifier and Item_Type.


Data_copy$Category <- str_sub(Data_copy$Item_Identifier,1,2)
Data_copy$Category <- as.factor(Data_copy$Category)


# Itentifying the age of the outlet by Subtracting 2013 from the Outlet_Establishment_Year.
# Using 2013 as the as the study was conducted in the year 2013. 
Data_copy$Outlet_Age <- 2013 - Data_copy$Outlet_Establishment_Year

# Viewing all the columns in the dataframe.
colnames(Data_copy)

# Droping column Outlet_Establishment_Year as it won't be needed.
Data_copy <- Data_copy[-8]

colnames(Data_copy)


# Univariate Plots for Continuous Variables
ggplot(Data_copy,aes(Item_Weight)) + geom_histogram()
ggplot(Data_copy,aes(Item_Visibility)) + geom_histogram()
ggplot(Data_copy,aes(Item_MRP)) + geom_histogram()
ggplot(Data_copy,aes(Outlet_Age)) + geom_histogram()
ggplot(Data_copy,aes(Item_Outlet_Sales)) + geom_histogram()


# Identifying variables with missing values.
sapply(Data_copy, function(x) sum(is.na(x)))

#Observation:-
# Missing values present in Item_Weight and Outlet_Size. 

# Note:-
# The best way to determine the value of Item_Weight and Outlet_Size a is by contacting the BigMart official of each store.
# As this is not possible for this project, I will try to determine Item_Weight and Outlet_Size based on the data given to us.


# Determining the missing values for Outlet_Size

# Checking which Outlet_Identifer has missing Outlet_Size
sqldf("Select Outlet_Identifier, Outlet_Size From Data_copy where Outlet_Size is Null Group by Outlet_Identifier, Outlet_Size")

# Observation:-
# OUT010, 017 and 045 have missing Outlet_size


# Taking the help of Outlet_Identifier, Outlet_Location_Type, Outlet_Type and Item_Outlet_Sales to determine Outlet_Size.
sqldf("Select Outlet_Identifier, Outlet_Type, Outlet_Age, Outlet_Location_Type , sum(Item_Outlet_Sales) as Total_Sales,Outlet_Size
      From Data_copy
      Group by Outlet_Identifier, Outlet_Type, Outlet_Age, Outlet_Location_Type, Outlet_Size
      Order by Outlet_Age")

# Observation:- 
# OUT017, 035, 045 have similer properties with approximately same amount of sales.
# Thus assuming OUT017 and 045 are small size outlet.
# OUT010 and 019 both are grocery store with little difference in the sales.
# Thus assuming OUT010 is a small outlet.

# Imputing the missing values for Outlet_Size
O_I_miss <- c('OUT010','OUT017','OUT045')
for(i in O_I_miss){
Data_copy$Outlet_Size[which(Data_copy$Outlet_Identifier== i)] <- 'Small'}


# Working with the missing values for Item_Weight

sqldf("Select Distinct Count(Item_Identifier) From Data_copy")
# Observation:- There are 8523 distinct items in the dataset.

sqldf("Select  Count(Distinct Item_Identifier) From Data_copy where Item_Weight is Null")
# Observation:- 1142 out of 8523 distinct Items have missing weights which we need to impute.


# List of Items with missing values
sqldf("Select Distinct Item_Identifier, Item_Weight From Data_copy where Item_Weight is Null")

# List of Items and their respective weight
sqldf("Select Distinct Item_Identifier, Item_Weight From Data_copy where Item_Weight is not Null")

# Observation:- There are few items whose weight is missing in some observation but it's weight is mentioned in other observation.
# Note- For imputing the missing values for the above items, we can refer to its weight from another observation where it is present.


# Saving the Item_Identifier and its weight in a dataframe
Item_ref <- sqldf("Select Distinct Item_Identifier, Item_Weight from Data_copy where Item_Weight is not null")

# Refering the Item_Weight from Item_ref dataframe to impute the missing values in Data_copy dataset.Similar to VlookUp.
Data_copy$Item_Weight[is.na(Data_copy$Item_Weight)==TRUE] <- Item_ref$Item_Weight[match(Data_copy$Item_Identifier,Item_ref$Item_Identifier)]

# checking if there are still any missing values in Item_Weight.
summary(Data_copy$Item_Weight)

# Observation:- There are still 4 Items with missing weights.


# Checking Items with missing weights.
Item_weight_miss <- sqldf("Select Item_Identifier, Item_Weight From Data_copy Where Item_Weight is null")

unique(Item_weight_miss)

# Observation:- Items FDE52, FDK57, FDN52 and FDQ60 are missing.
# Note:- Their weight needs to be imputed refering other items of the same category in the dataset.

# Exploring the data.
(sqldf("Select Item_Identifier,Item_Type, Avg(Item_Weight) From Data_copy 
       where Item_Identifier Like 'FDE%' or
             Item_Identifier Like 'FDK%' or  
             Item_Identifier Like 'FDN%' or
             Item_Identifier Like 'FDQ%' 
       Group by Item_Identifier, Item_Type 
       Order by Item_Identifier"))

# Observations:- 
# Description of missing Item_Type
# FDE52 is a Diary product
# FDK57 is a Snack Food
# FDN52 is a Frozen Food
# FDQ60 is a Baking Good

# Convert Item_Weight back to numeric type
Data_copy$Item_Weight <- as.numeric(unlist(Data_copy$Item_Weight))

# Imputing the missing weight of FDE42 using the mean value of other Dairy products having 'FDE' in their Item_Identifier
Data_copy$Item_Weight[which(Data_copy$Item_Identifier == 'FDE52')] <- (sqldf("Select Avg(Item_Weight) From Data_copy where Item_Identifier Like 'FDE%' And Item_Type = 'Dairy'"))

# Converting Item_Weight back to numeric type
Data_copy$Item_Weight <- as.numeric(unlist(Data_copy$Item_Weight))


# Observing items of related to FDK and Snack Foods
(sqldf("Select Item_Identifier,Item_Type,Avg(Item_Weight) From Data_copy 
       Where Item_Identifier Like 'FDK%' and Item_Type = 'Snack Foods'
       Group by Item_Identifier, Item_Type 
       Order by Item_Identifier"))

# Note:-
# It is difficult to determine the weight of FDK57 based on looking at the weight of other items from similar category.
# One of the option is to impute median value based on observing weight of items in simiar category.


# Imputing the missing weight of FDK57 using the mean value of other Dairy products having 'FDK' in their Item_Identifier
Data_copy$Item_Weight[which(Data_copy$Item_Identifier == 'FDK57')] <- (sqldf("Select Median(Item_Weight) From Data_copy where Item_Identifier Like 'FDK%' And Item_Type = 'Snack Foods'"))

# Convert Item_Weight back to numeric type
Data_copy$Item_Weight <- as.numeric(unlist(Data_copy$Item_Weight))


# Observing items related to FDN and Frozen Foods
(sqldf("Select Item_Identifier,Item_Type,Avg(Item_Weight) From Data_copy 
       Where Item_Identifier Like 'FDN%' and Item_Type = 'Frozen Foods'
       Group by Item_Identifier, Item_Type 
       Order by Item_Identifier"))

# Note:-
# It is difficult to determine the weight of FDN52 based on looking at the weight of other items from similar category.
# One of the option is to impute median value based on observing weight of items in simiar category.

# Imputing the missing weight of FDK52 using the mean value of other Dairy products having 'FDN' in their Item_Identifier
Data_copy$Item_Weight[which(Data_copy$Item_Identifier == 'FDN52')] <- (sqldf("Select Avg(Item_Weight) From Data_copy where Item_Identifier Like 'FDN%' And Item_Type = 'Frozen Foods'"))

# Convert Item_Weight back to numeric type
Data_copy$Item_Weight <- as.numeric(unlist(Data_copy$Item_Weight))

# Observing items related to FDQ60 and Baking Goods.
(sqldf("Select Item_Identifier,Item_Type,Avg(Item_Weight) From Data_copy 
       Where Item_Identifier Like 'FDQ%' and Item_Type = 'Baking Goods'
       Group by Item_Identifier, Item_Type 
       Order by Item_Identifier"))

# Note:-
# It is difficult to determine the weight of FDQ60 based on looking at the weight of other items from similar category.
# One of the option is to impute median value after observing the data
  
  
# Imputing the missing weight of FDQ60 using the median value of other Baking Goods having 'FDQ' in their Item_Identifier
Data_copy$Item_Weight[which(Data_copy$Item_Identifier == 'FDQ60')] <- (sqldf("Select Avg(Item_Weight) From Data_copy where Item_Identifier Like 'FDQ%' And Item_Type = 'Baking Goods'"))

# Convert Item_Weight back to numeric type
Data_copy$Item_Weight <- as.numeric(unlist(Data_copy$Item_Weight))


# Observing the summary of the data
summary(Data_copy)


# Refering Item_visibility we can see that some items have 0 visibility.
# It seems to be mostly data entry error.

# Observations with 0 visibility
sqldf("Select * From Data_copy where Item_Visibility = 0")


# Replacing Item_Visibility with null value
Data_copy$Item_Visibility[Data_copy$Item_Visibility == 0] <- NA

# Summary of Item_Visibility variable.
summary(Data_copy$Item_Visibility)


# Replacing the missing values of Item_Visibility with mean value.
Data_copy$Item_Visibility[is.na(Data_copy$Item_Visibility)] <- sqldf("Select Avg(Item_Visibility) From Data_copy")


Data_copy$Item_Visibility <- as.numeric(Data_copy$Item_Visibility)

summary(Data_copy)

##################################################################################################################

# Splitting the data into Training and Testing dataset.
smp_size <- floor(0.75 * nrow(Data_copy))

## setting the seed to make the partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Data_copy)), size = smp_size)

Retail_train <- Data_copy[train_ind, ]
Retail_test <- Data_copy[-train_ind, ]


# Examining the number of rows in Retai_train and Retail_test
nrow(Retail_train)
nrow(Retail_test) 

summary(Retail_test)
###########################################################################################################

# Data Preparation for Linear Regression Model

# Visualizing the data
ggplot(Retail_train,aes(Item_Weight)) + geom_histogram()
ggplot(Retail_train,aes(Item_Visibility)) + geom_histogram()
ggplot(Retail_train,aes(Item_MRP)) + geom_histogram()
ggplot(Retail_train,aes(Outlet_Age)) + geom_histogram()
ggplot(Retail_train,aes(Item_Outlet_Sales)) + geom_histogram()


# Removing unnecessary varables from test dataset
Retail_test_new <- Retail_test[-c(1,5,7,11)]


# Linear regression - Backward Selection
FitAll_1 <- lm((Item_Outlet_Sales) ~ ((Item_Fat_Content+Item_Visibility+Outlet_Size+Outlet_Location_Type+Outlet_Type+Category+Item_Weight+Item_MRP+Outlet_Age)),   
             data = Retail_train)

summary(FitAll_1)


# Linear Regression - Stepwise 

Model_Step <- step(FitAll_1,direction='both')

# Making prediction
Target_Step_test <- predict(Model_Step, newdata = Retail_test_new)

# RMSE value for test dataset
sqrt(mean((Target_Step_test - Retail_test$Item_Outlet_Sales)^2))
# RMSE value is 1089.845

# Making prediction on the training dataset
Target_Step_train <- predict(Model_Step, newdata = Retail_train[-11])

# RMSE value for training dataset
sqrt(mean((Target_Step_train - Retail_train$Item_Outlet_Sales)^2))
# RMSE value is 1141.13


###############################################################################

# Linear regression - Backward Selection with interaction terms.
FitAll_2 <- lm((Item_Outlet_Sales) ~ ((Item_Fat_Content+Item_Visibility+Outlet_Size+Outlet_Location_Type+Outlet_Type+Category+Item_Weight+Item_MRP+Outlet_Age)^2),   
             data = Retail_train)

summary(FitAll_2)

Model_Both2 <- step(FitAll_2,direction='both')


# Making prediction on the test dataset
Target_Both2_test <- predict(Model_Both2, newdata = Retail_test_new)

# RMSE value for test dataset
sqrt(mean((Target_Both2_test - Retail_test$Item_Outlet_Sales)^2))
# RMSE value is 1035.181


# Making prediction on the training dataset
Target_Both2_train <- predict(Model_Both2, newdata = Retail_train[-11])

# RMSE value for training dataset
sqrt(mean((Target_Both2_train - Retail_train$Item_Outlet_Sales)^2))
# RMSE value is 1081.87

# Conclusion :
# As Linear Regression with the interaction term has the least RMSE, we select the model to predict the sales.
