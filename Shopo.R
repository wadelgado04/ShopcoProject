

# This script is based on the Shopco data- Shopco is a mall operator
# Shopco caselet and data (consumer.csv,  purchase.csv) 

################################################################################
#                                    Data
################################################################################

library(dplyr)
library(ggplot2)

# Read the data in: consumer and purchase tables

# read the data grape_juice.csv
consumer <- read.csv(file = "consumer.csv")
purchase <- read.csv(file = "purchase.csv")

################################################################################

# Explore consumer table using 5 functions
# age is in years

dim(consumer)

str(consumer)

colnames(consumer)

head(consumer)

tail(consumer)

# Explore purchase table using 5 functions
# Sales are in $ dollars

dim(purchase)

str(purchase)

colnames(purchase)

head(purchase)

tail(purchase)

################################################################################
#Correct the data types in consumer table #
table(consumer$gender)
table(consumer$loyalty_status)

# correct gender as a factor
consumer$gender<- as.factor(consumer$gender)

# correct loyalty_status as a factor
consumer$loyalty_status<- as.factor(consumer$loyalty_status)

# correct the data types in purchase table #
table(purchase$realtime_message)

# correct realtime_message as a factor
purchase$realtime_message<- as.factor(purchase$realtime_message)

################################################################################
#                         Descriptive Analytics
################################################################################

#Look at one table at a time: consumer table #

# continuous variables #
# Show what is the average age of consumer by gender
consumer %>% group_by(gender) %>% summarise(mean=mean(age), sd=sd(age), min=min(age), max=max(age)) 
# Average age of female consumer is 31 yrs and 33 yrs for male.

# categorical variables #
# How many consumers are female and male
count(consumer, gender)

# There are 6,903 female and 2,129 male.

# How many consumers are there in each loyalty_status
count(consumer, loyalty_status)
# There are 3,269 in loyalty status one, and 5,763 in loyalty status two.

# Conclusions about consumers of Shopco 
The consumers are majority female under the loyalty status that fall within their early 30's'
################################################################################

#Look at the second table : purchase  #

# continuous variables #

# How much, on an average, customers spend on second trip onwards
summary(purchase)
# The average spent on second trip purchases is 34.65$

# categorical variables #
# How many customers received realtime_message
table(purchase$realtime_message)
# 4,516 customers received real time messages.

# What can you say about the real time mobile experiment in the Shopco mall:

Half the customers in the experiment received real time messeage with an average price of $34.65 for their second trip
################################################################################
# Next, let's see what impacts sales

# Q6) before that, we will combine consumer and purchase data using inner join
# Using inner_join() on consumer_id create a new data frame called experiment

experiment <- consumer %>% inner_join(purchase, by="consumer_id")
dim(experiment)

# how many number of rows are there in the experiment table:
There are 9,032 rows

################################################################################
#                                  Predictive analytics
################################################################################
#                                    Regression
################################################################################

# Run regression to see what impacts sales from second store onwards

# Find the impact of age on sales from second store onwards

# visualize using scatter plot
ggplot(experiment, mapping = aes(x=age, y=from_second_store_sales)) + geom_point() + geom_smooth(method = "lm",se = FALSE, colour = "pink1") + ggtitle("Impact of age on second store sales") + xlab("Age") +ylab("Second Store Sales")

# run regression using lm()
reg1<-lm(from_second_store_sales ~ age, data = experiment)
  summary(reg1)

# dummy variable regression

# Find the impact of realtime mobile message on sales from second store onwards

# visualize using bar chart
ggplot(experiment,aes(x=realtime_message, y=from_second_store_sales, fill = realtime_message)) + geom_bar(stat="identity") + ggtitle("Impact of real time mobile messages on second store sales") + xlab("Realtime messages") +ylab("Second store sales")

# run regression using lm()

reg2<- lm(from_second_store_sales ~ realtime_message, data = experiment)
  summary(reg2)

# managerial implication:
Through my findings I can say that real time messages has a greater response with second store sales
################################################################################
#                                    Clustering
################################################################################

#Run clustering to find clusters of different customers

# use kmeans() to run cluster analysis on age and sales on second store onwards

# we know from experience that there are three clusters
cluster_1 <- kmeans(experiment[, c("age","from_second_store_sales")],3, nstart = 20)

cluster_1
# visualize using ggplot()

cluster_1$cluster <- as.factor(cluster_1$cluster)

# visualize clusters
ggplot(experiment, aes(age,from_second_store_sales, color = cluster_1$cluster)) + 
  geom_point() + 
  ggtitle("Clusters on Age and Second Store Sales") + 
  labs(colour = "Clusters")

# managerial implication:
Our second cluster infers greater second store sales as age with younger age
################################################################################
