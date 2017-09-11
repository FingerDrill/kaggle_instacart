library(data.table)
library(dplyr)
library(tidyr)


# Load Data ---------------------------------------------------------------
rm(list=ls())
if(getwd()!="/home/rstudio/kaggle_instacart") {setwd("./kaggle_instacart/")}
source("./rscript/UDF.R")
load(file="./data/reference_data.Rdata")

# path <- "./data/"

# aisles <- fread(file.path(path, "aisles.csv"))
# departments <- fread(file.path(path, "departments.csv"))
# orderp <- fread(file.path(path, "order_products__prior.csv"))
# ordert <- fread(file.path(path, "order_products__train.csv"))
# orders <- fread(file.path(path, "orders.csv"))
# products <- fread(file.path(path, "products.csv"))
# 
# save(aisles, departments, orderp, ordert, orders, products, 
#      file="./data/reference_data.Rdata")

# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)

ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

# prior인 주문 상품정보에 order정보를 붙인다.
orders_products <- orders %>% inner_join(orderp, by = "order_id")

# rm(orderp)
# gc()


# Products ----------------------------------------------------------------

# 상품별로 묶여서 끝난다.
prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  # 유저의 해당 상품에 대한 구맥 몇 번째인지
  mutate(product_time = row_number()) %>%
  ungroup() %>% 
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),   # (유저랑 상관없이) 상품의 주문 횟수
    prod_reorders = sum(reordered),   # (유저랑 상관없이) 재주문 횟수
    prod_first_orders = sum(product_time == 1), # (유저랑 상관없이) 첫주문 횟수
    prod_second_orders = sum(product_time == 2)  # (유저랑 상관없이) 두번째 주문 횟수
  )

# 처음 주문한 상품이 두 번째 주문되는 비율
prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
# 재주문 배수
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
# 재주문 비율
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders


prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)

# rm(products)
# gc()

# Users -------------------------------------------------------------------

# prior 정보만을 가지고 유저에 대한 데이터 생성
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),   # 총 주문횟수
    user_period = sum(days_since_prior_order, na.rm = T),  # 첫주문~마지막주문 기간
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T) # 평균 주문간격
    # days_since_prior_order가 첫주문일 때 NA로 표시되어 na.rm=T를 넣어준다.
  )

# prior의 주문상품정보에서 유저특성 데이터 추출.
# 구체적인 주문 상품 항목에 대한 데이터를 가짐.
us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),  # 유저가 주문한 총 상품 항목수
    # 주문한 상품 중, 재주문인 상품의 비율
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),   
    # 주문한 상품 종류. distinct
    user_distinct_products = n_distinct(product_id)
  )

# 유저의 단순 오더정보 + 주문상품 항목 정보
users <- users %>% inner_join(us)
# 한번에 주문하는 평균 상품 수 
users$user_average_basket <- users$user_total_products / users$user_orders


# train + test로 한 이유는 prior의 주문건은 한 유저당 여러 건이 있을 수 있어서.
# 또한 예측 대상인 주문(train, test)과 관련된 변수값들을 붙이기 위함.
us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

# 붙임.
users <- users %>% inner_join(us)

# rm(us)
# gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(), # 이 유저가 이 상품을 주문한 횟수
    up_first_order = min(order_number), # 이 상품의 첫 주문이 유저의 몇 번째 주문이었는지
    up_last_order = max(order_number),  # 이 상품의 마지막 주문이 유저의 몇 번째 주문이었는지
    up_average_cart_position = mean(add_to_cart_order)) # 이상품의 카트에 담긴 평균 순위

# rm(orders_products, orders)
# gc()

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

# 유저의 이 상품 주문수/ 총 주문수
data$up_order_rate <- data$up_orders / data$user_orders 
# 이 상품의 마지막 주문 이후로 몇 번이나 주문했는지
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
# 이 상품의 첫 주문 이후로 이 상품의 주문 비율.
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)


# train set에만 reorder column값이 존재하고
# 나머지인 test는 NA
# 이 데이터 자체가 다 prior에서의 유저X상품 데이터이고
# reorder 컬럼은 모두 test/train의 결과값을 나타냄.
data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

# rm(ordert, prd, users)
# gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL
# train과 달리 order_id와 product_id를 제거하지 않았는데
# 최종 결과물에서 order_id와 product_id를 제출해야 하기 때문.
# 뒤에서 model apply용 xgb.Dmatrix를 만들 때는 두 개를 제외한다.


# rm(data)
# gc()


# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

subtrain <- train %>% sample_frac(0.1)
X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered)), label = subtrain$reordered)
model <- xgboost(data = X, params = params, nrounds = 80)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

# rm(X, importance, subtrain)
# gc()


# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X)

# T/F 벡터에 1을 곱하면 0/1로 변한다.
test$reordered <- (test$reordered > 0.21) * 1

# test에는 order_id와 product_id가 남아있음.
submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

# submission에서는 reordered==1 로 필터를 걸었기 때문에
# 모든 reordered==0이 나오는 order들은 포함되지 않았다.
# 이러한 order 까지 포함시키고, 제출 형식으로 요구하는 None까지 표기.
missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submit.csv", row.names = F)
