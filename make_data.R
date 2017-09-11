library(dplyr)
library(Hmisc)
library(dummies)


# kaggle-instacart project
rm(list=ls());gc()


# file reading -----
if(getwd()!="/home/rstudio/kaggle_instacart") {setwd("./kaggle_instacart/")}
source("./rscript/UDF.R")
load("./data/raw_data.Rdata")

# set_aisles      <- fread("./data/aisles.csv")
# set_departments <- fread("./data/departments.csv")
# set_products    <- fread("./data/products.csv")
# op_prior        <- fread("./data/order_products__prior.csv")
# op_train        <- fread("./data/order_products__train.csv")
# orders          <- fread("./data/orders.csv")
# 
# save(set_aisles, set_departments, set_products,
#      op_prior, op_train, orders, 
#      file="./data/raw_data.Rdata")



# factor -----
set_aisles$aisle            <- as.factor(set_aisles$aisle)
set_departments$department  <- as.factor(set_departments$department)
set_products$product_name   <- as.factor(set_products$product_name)
orders$eval_set             <- as.factor(orders$eval_set) 

op_train <- op_train %>%
  inner_join(orders %>% select(order_id, user_id), by="order_id")

# merge sets -----
set_products <- 
  set_products %>% 
  inner_join(set_aisles) %>% 
  inner_join(set_departments)



rm(set_aisles, set_departments)
gc()

# order_hour_of_day, order_dow discretization & one-hot-encoding
orders <- orders %>% 
  mutate(order_hour_of_day = ifelse(order_hour_of_day==0, 24, order_hour_of_day)) %>% 
  mutate(order_hour = ifelse(order_hour_of_day<=6, "1Q", 
                             ifelse(order_hour_of_day<=12, "2Q", 
                                    ifelse(order_hour_of_day<=18, "3Q", "4Q")))) %>% 
  select(-order_hour_of_day)

orders <- dummy.data.frame(orders, "order_hour")
orders <- dummy.data.frame(orders, "order_dow")
colnames(orders)[5:11] <- c("order_dow_sun", "order_dow_mon", "order_dow_tue", 
                            "order_dow_wed", "order_dow_thu", "order_dow_fri", 
                            "order_dow_sat")

# order x product + user information -----
op_u_prior <- 
  op_prior %>% 
  inner_join(orders, by="order_id")

rm(op_prior)
gc()

# product information : prefix 'p'
products_prior <- op_u_prior %>% 
  arrange(user_id, order_number, product_id)  %>% 
  group_by(user_id, product_id) %>% 
  mutate(p_order_sequence = row_number()) %>% 
  ungroup() %>% 
  group_by(product_id) %>%
  summarise(p_orders = n(),
            p_reorders = sum(reordered),
            p_first_orders = sum(p_order_sequence==1), # 모든제품 최소 1번 주문됨.
            p_second_orders = sum(p_order_sequence==2) # 다 F면 0
  ) %>% 
  mutate(p_first_retention = p_second_orders / p_first_orders, # 1->2 잔존율
         p_reorder_times = 1 + (p_reorders / p_first_orders) # 재주문 배수
  )


aisles_prior <- products_prior %>%
  select(product_id, p_reorders, p_first_orders, p_second_orders) %>% 
  inner_join(set_products %>% select(product_id, aisle_id), by="product_id") %>% 
  group_by(aisle_id) %>% 
  summarise(a_reorders = median(p_reorders),
            a_first_orders = median(p_first_orders),
            a_second_orders = median(p_second_orders)
  ) %>% 
  mutate(a_first_retention = a_second_orders / a_first_orders,
         a_reorder_times = 1 + (a_reorders / a_first_orders)
  )

products_prior <- products_prior %>% 
  inner_join(set_products %>% select(product_id, aisle_id), by="product_id") %>% 
  inner_join(aisles_prior, by="aisle_id") %>% 
  select(-p_reorders, -p_first_orders, -p_second_orders,
         -a_reorders, -a_first_orders, -a_second_orders, -aisle_id)

rm(aisles_prior)
gc()

# user information : prefix 'u'
users_prior <- op_u_prior %>% 
  group_by(user_id) %>% 
  summarise(u_order_interval = mean(days_since_prior_order, na.rm=T),
            u_orders = max(order_number),
            u_products_total = n(),
            u_products_distinct = n_distinct(product_id),
            u_hour_Q1 = sum(order_hour1Q),
            u_hour_Q2 = sum(order_hour2Q),
            u_hour_Q3 = sum(order_hour3Q),
            u_hour_Q4 = sum(order_hour4Q),
            u_day_sun = sum(order_dow_sun),
            u_day_mon = sum(order_dow_mon),
            u_day_tue = sum(order_dow_tue),
            u_day_wed = sum(order_dow_wed),
            u_day_thu = sum(order_dow_thu),
            u_day_fri = sum(order_dow_fri),
            u_day_sat = sum(order_dow_sat),
            u_reorder_ratio = sum(reordered==1)/sum(order_number>1)
  ) %>% 
  mutate(u_basket_size = u_products_total / u_orders) %>% 
  select(-u_products_total)

users_prior <- users_prior %>% 
  inner_join(orders %>% 
               filter(eval_set != "prior") %>% 
               select(user_id, order_id, eval_set, days_since_last_prior = days_since_prior_order), 
             by="user_id") %>% 
  droplevels()

# user x product : prefix 'up'
# final data set
data <- op_u_prior %>% 
  group_by(user_id, product_id) %>% 
  summarise(up_orders = n(),
            up_first_order = min(order_number),
            up_last_order = max(order_number),
            up_cart_position = mean(add_to_cart_order)
  ) %>% 
  data.frame()

data_temp <- op_u_prior %>% 
  group_by(user_id, order_number) %>% 
  summarise(days_since_prior_order=mean(days_since_prior_order)) %>% 
  ungroup() %>% 
  mutate(days_since_prior_order = ifelse(is.na(days_since_prior_order), 0, days_since_prior_order)) %>%
  group_by(user_id) %>% 
  mutate(days_to_next_order = lead(days_since_prior_order),
         days_cumsum = cumsum(days_since_prior_order)) %>% 
  ungroup() %>% 
  data.frame()


data <- data %>%
  left_join(data_temp, by = c("user_id"="user_id", "up_first_order" = "order_number")) %>%
  rename(dayMin = days_cumsum) %>%
  left_join(data_temp, by = c("user_id"="user_id", "up_last_order" = "order_number")) %>%
  rename(dayMax = days_cumsum) %>%
  mutate(days_total = dayMax-dayMin,
         up_order_interval = ifelse(up_orders==1, 999999, days_total/(up_orders-1))) %>%
  select(user_id, product_id, up_orders, up_cart_position, 
         up_first_order, up_last_order, up_order_interval)

rm(data_temp)
gc()

# add user, product information to data
data <- data %>% 
  inner_join(users_prior, by="user_id") %>% 
  inner_join(products_prior, by="product_id")

# additional 'up-' features
data$up_order_rate <- data$up_orders / data$u_orders 
data$up_orders_after_last_order <- data$u_orders - data$up_last_order
data$up_retention <- data$up_orders / (data$u_orders - data$up_first_order + 1)


# add target variable ----
data <- data %>% 
  left_join(op_train %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

rm(products_prior, set_products, users_prior, op_u_prior, orders, op_train)
gc()



# save(data, file="./data/final_data.Rdata")




