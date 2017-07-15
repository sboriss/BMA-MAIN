
###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################


cat("\014")  # clear console
rm(list=ls(all=TRUE))

library(data.table)
library(dplyr)
library(tidyr)


# Load Data from T520 -----------------------------------------------------

# setwd('c:/BBB/PROPACAD/CAPSTONE/BASKET/')
# 
# #path <- "./input"
# path <- "./DATA"
# 
# aisles      <- fread(file.path(path, "aisles.csv"))
# departments <- fread(file.path(path, "departments.csv"))
# orderp      <- fread(file.path(path, "order_products__prior.csv"))
# ordert      <- fread(file.path(path, "order_products__train.csv"))
# orders      <- fread(file.path(path, "orders.csv"))
# products    <- fread(file.path(path, "products.csv"))

# Load Data from MTEC-0373 ------------------------------------------------

path_main = "C:/Users/sboriss/PROPACAD/"
path_data = paste0(path_main, 'DATA/')
path_subm = paste0(path_main, 'SUBM/')

orders      <- fread( paste0(path_data, 'orders.csv') ) 
products    <- fread( paste0(path_data, 'products.csv'             ) )
ordert      <- fread( paste0(path_data, 'order_products__train.csv') )
orderp      <- fread( paste0(path_data, 'order_products__prior.csv') )
aisles      <- fread( paste0(path_data, 'aisles.csv'               ) )
departments <- fread( paste0(path_data, 'departments.csv'          ) )

ordert_tmp1 = head( ordert, n = 20 )

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

ordert_tmp2 = head( ordert, n = 20 )

#only orders from prior left
orderp_products     = orders %>% inner_join(orderp, by = "order_id")
orderp_products_tmp = head( orders_products, n = 100 )

rm(orderp)

#identify users that always ordered the same basket
user_id_reordering = orderp_products %>% 
                     group_by( user_id, order_id ) %>%
                     summarise(
                       total_products_ordered = n(),
                       mean_product_id        = mean(product_id)
                     ) %>% 
                     ungroup() %>%
                     group_by( user_id ) %>%
                     summarise(
                       mean_tpo = mean(total_products_ordered),
                       min_tpo  = min( total_products_ordered),
                       max_tpo  = max( total_products_ordered),
                       min_pro  = min( mean_product_id),
                       max_pro  = max( mean_product_id)
                     ) %>%
                     filter( max_tpo == min_tpo & min_pro == max_pro ) %>%
                     arrange( desc(mean_tpo) )

dim( user_id_reordering )

tmp3 = orders_products %>% filter( user_id == 66951)

gc()

#select order_products in train
ordert_products     = orders %>% inner_join(ordert, by = c("order_id", "user_id") )
ordert_products_tmp = head( ordert_products, n = 100 ) 

tmp4 = ordert_products %>% filter( user_id %in% user_id_reordering$user_id )

length( unique( tmp4$user_id ) )

mean( tmp4$reordered )


# Products ----------------------------------------------------------------
prd <- orderp_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2)
  )

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)

rm(products)
gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

us <- orderp_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
  )

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders

us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

rm(us)
gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))

rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

rm(ordert, prd, users)
gc()

colnames( data )

data_tmp = head( data, n = 50 )

#reduce set of features
# data = data %>% select( user_id, product_id, eval_set, up_order_rate, up_orders_since_last_order, 
#                         up_order_rate_since_first_order, up_orders,
#                         prod_reorder_probability, user_reorder_ratio, prod_reorder_times)

#scale features




# Train / Test datasets ---------------------------------------------------
train_all <- as.data.frame(data[data$eval_set == "train",])

#split train into train (in) and validation (ud) sets: random selection of user_id (one order_id per user)
user_id_train_all  = unique( train_all$user_id ) #select user_id in train

user_id_train_in   = c( user_id_train_all %>% as.data.frame %>% sample_frac(0.2) )$.
length( user_id_train_in )

#train train set
train_in = train_all %>% filter( user_id %in% user_id_train_in )

#validation train set
train_ud = train_all %>% filter( !(user_id %in% user_id_train_in ) )

rm( train_all )

#remove unnecessary variables in train part
train_in$eval_set   <- NULL
train_in$user_id    <- NULL
train_in$product_id <- NULL
train_in$order_id   <- NULL
train_in$reordered[is.na(train_in$reordered)] <- 0

#remove unnecessary variables in validation part
train_ud$eval_set  = NULL
train_ud$user_id   = NULL
train_ud$reordered[is.na(train_ud$reordered)] <- 0

#remove unnecessary variables in test part
test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set  <- NULL
test$user_id   <- NULL
test$reordered <- NULL

rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)


# #original setup
# params <- list(
#   "objective"           = "reg:logistic",
#   "eval_metric"         = "logloss",
#   "eta"                 = 0.1,
#   "max_depth"           = 6,
#   "min_child_weight"    = 10,
#   "gamma"               = 0.70,
#   "subsample"           = 0.76,
#   "colsample_bytree"    = 0.95,
#   "alpha"               = 2e-05,
#   "lambda"              = 10
# )

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 5,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 1.0,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)


X     <- xgb.DMatrix( as.matrix( train_in %>% select(-reordered) ), label = train_in$reordered )
model <- xgboost(data = X, params = params, nrounds = 80)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance, train_in)
gc()

# predict baskets for train_ud --------------------------------------
X <- xgb.DMatrix(as.matrix(train_ud %>% select( -order_id, -product_id, -reordered )))
train_ud$reordered_hat <- predict(model, X)

hist( train_ud$reordered_hat )

#cutoff = 0.21

cutoff = seq(0.10, 0.23, by = 0.01)

meanF1score = sapply(cutoff, function( x ){
  
  train_ud$reordered_lab <- (train_ud$reordered_hat > x ) * 1
  
  metrics = train_ud %>% 
    group_by( order_id ) %>%
    summarise( 
      TP  = length( which(reordered * reordered_lab == 1 ) ), # number of True Positive
      CP  = sum( reordered ), # condition positive: number of ordered items
      PCP = sum( reordered_lab ), #predicted condition positive: predicted number of ordered items
      precision = ifelse( PCP == 0, 0, TP / PCP),
      recall    = ifelse(  CP == 0, 0, TP /  CP),
      f1score   = ifelse( any( c( precision, recall ) == 0), 0, 2 * precision * recall / ( precision + recall) )
    )
  #metrics_tmp = head( metrics, n = 100 )
  
  mean( metrics$f1score )
})

names( meanF1score ) = cutoff

meanF1score = as.data.frame( meanF1score )
colnames( meanF1score ) = paste( "max_depth", params$max_depth, sep = "_" )

plot( cutoff, meanF1score[, 1] )

meanF1score

# max_depth_6
# 0.1    0.3402069
# 0.11   0.3479049
# 0.12   0.3541398
# 0.13   0.3589328
# 0.14   0.3627650
# 0.15   0.3653806
# 0.16   0.3669857
# 0.17   0.3671211
# 0.18   0.3667456
# 0.19   0.3655384
# 0.2    0.3638240
# 0.21   0.3618351
# 0.22   0.3592490
# 0.23   0.3559091

# max_depth_6
# 0.1    0.3439991
# 0.11   0.3506081
# 0.12   0.3559743
# 0.13   0.3604589
# 0.14   0.3637906
# 0.15   0.3658986
# 0.16   0.3673791
# 0.17   0.3678763
# 0.18   0.3675389
# 0.19   0.3664969
# 0.2    0.3651815
# 0.21   0.3633048
# 0.22   0.3606603
# 0.23   0.3578941

# max_depth_5_lambda_10
# 0.1    0.3427718
# 0.11   0.3497660
# 0.12   0.3555940
# 0.13   0.3600113
# 0.14   0.3635354
# 0.15   0.3655204
# 0.16   0.3668564
# 0.17   0.3671419
# 0.18   0.3669872
# 0.19   0.3656256
# 0.2    0.3638076
# 0.21   0.3616907
# 0.22   0.3590538
# 0.23   0.3560371

# max_depth_5_lambda_6_eta_0.1
# 0.1    0.3430155
# 0.11   0.3500617
# 0.12   0.3559013
# 0.13   0.3603665
# 0.14   0.3636732
# 0.15   0.3658532
# 0.16   0.3674942
# 0.17   0.3678709
# 0.18   0.3674549
# 0.19   0.3661955
# 0.2    0.3643848
# 0.21   0.3622416
# 0.22   0.3599396
# 0.23   0.3570330

# max_depth_5_lambda_6_eta_0.3
# 0.1    0.3406844
# 0.11   0.3473349
# 0.12   0.3527421
# 0.13   0.3569525
# 0.14   0.3603944
# 0.15   0.3626488
# 0.16   0.3639017
# 0.17   0.3643383
# 0.18   0.3643278
# 0.19   0.3635641
# 0.2    0.3620901
# 0.21   0.3601053
# 0.22   0.3578439
# 0.23   0.3551498

# max_depth_4
# 0.1    0.3412091
# 0.11   0.3480324
# 0.12   0.3532126
# 0.13   0.3577682
# 0.14   0.3609189
# 0.15   0.3633011
# 0.16   0.3646763
# 0.17   0.3655233
# 0.18   0.3653750
# 0.19   0.3643997
# 0.2    0.3632505
# 0.21   0.3612474
# 0.22   0.3587185
# 0.23   0.3560231

# Apply model -------------------------------------------------------------
if(FALSE){
  X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
  test$reordered <- predict(model, X)
  
  test$reordered <- (test$reordered > 0.21) * 1
  
  submission <- test %>%
    filter(reordered == 1) %>%
    group_by(order_id) %>%
    summarise(
      products = paste(product_id, collapse = " ")
    )
  
  missing <- data.frame(
    order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
    products = "None"
  )
  
  submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
  write.csv(submission, file = "submit.csv", row.names = F)
}
