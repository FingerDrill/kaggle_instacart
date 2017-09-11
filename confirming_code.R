# 유저수.
# train : 131209
# test : 75000
# all : 206209
# 유저수 (test+train = prior) 확인

# 유저리스트
train_users <- orders %>% filter(eval_set=="train") %>% distinct(user_id) %>% data.frame()
test_users <- orders %>% filter(eval_set=="test") %>% distinct(user_id) %>% data.frame()
all_users <- orders %>% filter(eval_set=="prior") %>% distinct(user_id) %>% data.frame()


# train : 131209
# not NA : 122607
data_reorder_users <- data %>% filter(!is.na(reordered)) %>% 
  select(user_id) %>% distinct(user_id) %>% data.frame()

# test : 75000
# NA : 205888
data_na_users <- data %>% filter(is.na(reordered)) %>% 
  select(user_id) %>% distinct(user_id) %>% data.frame()

# NA는 train 유저인데 이번 주문(ordert)에서 
# 해당 product를 안샀기 때문에 ordert에 정보가 없는 것들
# 즉 많은 train 유저가 포함되어 있다.

# 여기에 test유저의 것들은 당연히 다 NA
# NA인 유저들에 test유저들이 모두 포함되있는 것을 볼 수 있다.
sum(data_na_users[[1]] %in% test_users[[1]])  # 75000

# 리오더 정보가 있는 유저들은 모두 train에 속함
nrow(data_reorder_users)  # 122607
sum(data_reorder_users[[1]] %in% train_users[[1]])  # 122607

# NA인 train 유저들
nrow(train_users)  # 131209
sum(data_na_users[[1]] %in% train_users[[1]])  # 130888

# NA의 전체 유저수
nrow(data %>% filter(is.na(reordered)) %>% distinct(user_id)) # 205888
# NA인 train유저들 + NA인 test 유저들 : 130888+75000 = 205888




head(iris %>% group_by(Sepal.Width) %>% summarise(m=mean(Sepal.Length)), n=150)
