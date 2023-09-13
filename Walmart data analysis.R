getwd()

library(readxl)

library(ggplot2)

walmart_data <- read.csv('C:/Users/user/Downloads/R course source files/11 - Walmart Data Exercise/data/walmart_data.csv')

walmart_features <- read.csv('C:/Users/user/Downloads/R course source files/11 - Walmart Data Exercise/data/walmart_features.csv')

n_rows <- nrow(walmart_data)

str(walmart_data)

summary(walmart_data)

walmart_data$Date = as.Date(walmart_data$Date)

head(walmart_data, 5)

sd(walmart_data$Weekly_Sales)

rows_per_store <- table(walmart_data$Store)

rows_per_store <- as.data.frame(rows_per_store)

colnames(rows_per_store) = c('store_number', 'count')

# Which store has the highest number of rows
rows_per_store[order(-rows_per_store$count),][1,]

# Sum of sales by store

sum_by_store <- aggregate(
          walmart_data$Weekly_Sales,
          by = list(walmart_data$Store),
          FUN = sum
          )

colnames(sum_by_store) = c('store_number', 'total_sales')

# A bar plot of the stores and total sales

barplot(sum_by_store[order(-sum_by_store$total_sales),"total_sales"], 
        col = 'darkgreen',
        main = 'Sales by store'
        )

# computing the mean of every column in the walmart_features table

walmart_features$Date = as.Date(walmart_features$Date)

wf_mean <- sapply(walmart_features, mean, na.rm = TRUE)

# Creating a new column in the walmart_features table

walmart_features$standardized_cpi <- (walmart_features$CPI - mean(walmart_features$CPI, na.rm = TRUE))/sd(walmart_features$CPI, na.rm = TRUE)

walmart_features['standardized_cpi']

# To produce a time series for sales for store 1 across all departments

store_1 = walmart_data[walmart_data$Store == 1,]

store_1_sales <- aggregate(
                            store_1$Weekly_Sales,
                            by = list(store_1$Date),
                            FUN = sum
                      )

colnames(store_1_sales) = c('Date', 'Sales')

plot(x = store_1_sales$Date, 
     y = store_1_sales$Sales,
     xlab='Weekly Sales',
     ylab='Date'
     )
lines(x = store_1_sales$Date, 
      y = store_1_sales$Sales)

# Plotting total sales per week for store 20 using ggplot

store_20 = walmart_data[walmart_data$Store == 20,]

store_20_sales <- aggregate(
          store_20$Weekly_Sales,
          by = list(store_20$Date),
          FUN = sum
        )
colnames(store_20_sales) <- c('date', 'total_sales')

str(store_20_sales)

(
ggplot(data = store_20_sales,
       mapping = aes(x = date,
                     y = total_sales)
       ) + geom_line(color = 'orange') + geom_point(color = 'darkorange')
        + xlab('Date') + ylab('Total Sales')
)

# plot the time series for the top 5 selling departments in store 2

table(walmart_data$Dept)

store_2 <- walmart_data[walmart_data$Store == 2,]

sales_by_dept <- aggregate(
                          store_2$Weekly_Sales,
                          by = list(store_2$Dept),
                          FUN = sum
                        )
colnames(sales_by_dept) <- c('dept', 'total_sales')

top_depts <- sales_by_dept[order(-sales_by_dept$total_sales), 'dept'][1:5]

top5_store2_dept_sales <- store_2[store_2$Dept %in% top_depts,]

table(top5_store2_dept_sales$Dept)

ggplot(
        data = top5_store2_dept_sales,
        aes(x = Date, y = Weekly_Sales, group = Dept, color = as.factor(Dept))
      ) + geom_line() + labs(title = 'Top 5 Department Sales', color = 'Departments')

