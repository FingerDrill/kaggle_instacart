library(dplyr)
library(xgboost)
library(data.table)


# file reading -----
rm(list=ls());gc()
if(getwd()!="/home/rstudio/kaggle_instacart") {setwd("./kaggle_instacart/")}
source("./rscript/UDF.R")
load("./data/final_data.Rdata")


# partition -----
train <- data %>% filter(eval_set=="train")
train <- train %>% select(-product_id, -order_id, -eval_set)
train <- train %>% mutate(reordered = ifelse(is.na(reordered), 0, reordered))

test <- data %>% filter(eval_set=="test")
test <- test %>% select(-user_id, -eval_set, -reordered)


# new model with default parameters
dtrain <- xgb.DMatrix(data = as.matrix(train %>% select(-user_id, -reordered)),
                      label = train$reordered)
attr(dtrain, 'user_id') <- train$user_id


# finding nround -----
params_d <- list(booster = "gbtree",
                 objective = "reg:logistic",
                 eta = 0.3,
                 gamma = 0,
                 max_depth = 6,
                 min_child_weight = 1,
                 subsample = 1,
                 colsample_bytree=1)


xgbcv <- xgb.cv(params = params_d, data=dtrain,
                nrounds = 100, 
                nfold = 5,
                showsd=T, 
                feval = xgb_eval_f1,
                stratified = T,
                print_every_n = 5,
                early_stopping_rounds = 10,
                maximize = T,
                )


# model -----
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

set.seed(421)
X <- xgb.DMatrix(as.matrix(train %>% select(-reordered)), label = train$reordered)
model <- xgboost(data = X, params = params, nrounds = 80)


importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

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
# write.csv(submission, file = "submit.csv", row.names = F)



