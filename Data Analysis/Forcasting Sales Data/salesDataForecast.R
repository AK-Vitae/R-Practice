############ LIBRARIES #####

library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)
library(gbm)
library(data.table)
library(tictoc)

#### Load  Data ######
tic("Data Loading ")
sales_data = fread("../input/sales_train.csv")
item_data = fread("../input/items.csv")
test_data = fread("../input/test.csv")
toc()

glimpse(sales_data)
glimpse(item_data)
glimpse(test_data)
#### Data Preparation  ##### 

# get the item category details in the sales data
sales_data = merge(sales_data, item_data[,c("item_id", "item_category_id")], by = "item_id", all.x = T)
sales_data$date = as.Date(sales_data$date, "%d.%m.%Y")

sales_data$year = year(sales_data$date)
sales_data$year =  as.factor(sales_data$year)

sales_data$month = month(sales_data$date)
sales_data$month = as.factor(sales_data$month)

sales_data$day = day(sales_data$date)
sales_data$day = as.factor(sales_data$day)

sales_data$weekdays =  weekdays(sales_data$date)
sales_data$weekdays = as.factor(sales_data$weekdays)

# sales_data$shop_id = as.factor(sales_data$shop_id)
# sales_data$item_id =  as.factor(sales_data$item_id)
sales_data$item_category_id =  as.factor(sales_data$item_category_id)

### EDA ####
# sales shop wise
sales_shopwise = sales_data %>%
  select(shop_id, item_cnt_day) %>%
  group_by(shop_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

ggplot(data =  sales_shopwise, 
       mapping = aes(x = reorder(shop_id, item_cnt_day), 
                     y = item_cnt_day, 
                     fill = factor(shop_id))) +
  geom_histogram(stat = "identity", color = "yellow") +
  # coord_flip() +
  xlab("Shop ID") + ylab("Sales Count")+
  # geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = item_cnt_day)) +
  ggtitle(label = "Shop wise sales") +
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
    # Change plot and panel background
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    # legend.position = c(0.6, 0.07),
    # legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # align title to top center, top ledt is by default.
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    # axis ticks to bold black
    axis.text=element_text(colour = "yellow",face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) 



# sales item category wise
sales_categorywise = sales_data %>%
  select(item_category_id, item_cnt_day) %>%
  group_by(item_category_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

ggplot(data =  sales_categorywise, 
       mapping = aes(x = reorder(item_category_id,item_cnt_day), 
                     y = item_cnt_day,
                     fill = factor(item_category_id))) +
  geom_histogram(stat = "identity", color = "yellow") +
  # coord_flip() +
  xlab("Item Category") + ylab("Sales Count") +
  ggtitle("Sale Item Category wise")+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
    # Change plot and panel background
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    # legend.position = c(0.6, 0.07),
    # legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # align title to top center, top ledt is by default.
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    # axis ticks to bold black
    axis.text=element_text(colour = "yellow",face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) 

# most items in shop

items_in_shop = sales_data %>%
  select(shop_id, item_id) %>%
  group_by(shop_id) %>%
  summarise(item_id = n_distinct(item_id))

ggplot(data = items_in_shop,
       mapping = aes(x = reorder(shop_id,item_id),
                     y = item_id,
                     fill = factor(shop_id)))+
  geom_histogram(stat = "identity", color = "yellow") +
  xlab(" Shop ID")+ ylab(" Items in shop")+
  ggtitle("Most Items in Shops") +
  coord_flip()+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
    # Change plot and panel background
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    # legend.position = c(0.6, 0.07),
    # legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # align title to top center, top ledt is by default.
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    # axis ticks to bold black
    axis.text=element_text(colour = "yellow",face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) 


# which category of item is available most 
items_in_category = sales_data %>%
  select(item_category_id, item_id) %>%
  group_by(item_category_id) %>%
  summarise(item_id =  n_distinct(item_id))

ggplot(data = items_in_category,
       mapping = aes(x = reorder(item_category_id,item_id),
                     y = item_id,
                     fill = factor(item_category_id)))+
  geom_histogram(stat = "identity", color = "yellow") +
  xlab(" Category ID")+ ylab(" Items in Category")+
  ggtitle("Most Items per Category") +
  coord_flip()+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
    # Change plot and panel background
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    # legend.position = c(0.6, 0.07),
    # legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # align title to top center, top ledt is by default.
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    # axis ticks to bold black
    axis.text=element_text(colour = "yellow",face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) 
