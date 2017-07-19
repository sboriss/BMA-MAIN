
###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################

# Add-on: 


cat("\014")  # clear console
rm(list=ls(all=TRUE))

library(data.table)
library(dplyr)
library(tidyr)

# functions
fnGetMetrics             <- function( mX ){
  
  metrics = mX %>% 
    group_by( user_id ) %>%
    summarise( 
      TP  = length( which( y_out * y_hat == 1 ) ), # number of True Positive
      CP  = sum( y_out ),     #           condition positive: number of re-ordered items
      PCP = sum( y_hat ), # predicted condition positive: predicted number of re-ordered items
      precision = ifelse( PCP == 0, 0, TP / PCP),
      recall    = ifelse(  CP == 0, 0, TP /  CP),
      f1score   = ifelse( any( c( precision, recall ) == 0), 0, 2 * precision * recall / ( precision + recall) )
    )
  return( metrics )
}


# Load Data ---------------------------------------------------------------

setwd('c:/BBB/PROPACAD/CAPSTONE/BASKET/SLIDE/')

path <- "./input"
#path <- "./DATA"

aisles      <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp      <- fread(file.path(path, "order_products__prior.csv"))
ordert      <- fread(file.path(path, "order_products__train.csv"))
orders      <- fread(file.path(path, "orders.csv"))
products    <- fread(file.path(path, "products.csv"))

orderp_tmp <- head( orderp, n = 100 )
orders_tmp <- head( orders, n = 100 )
ordert_tmp <- head( ordert, n = 100 )

# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)

products_tmp = head( products, n = 50 )

###assign user_id to orders in train
ordert$user_id = orders$user_id[match(ordert$order_id, orders$order_id)]
ordert         = ordert %>% group_by( user_id, product_id ) %>% arrange( user_id, product_id )

ordert_tmp1    <- head( ordert, n = 100 )

#retain only orders from prior
orderp_products     = orders %>% inner_join(orderp, by = "order_id")
orderp_products_tmp = head( orderp_products, n = 100 )

rm(orderp)
gc()

#get user_id in train set
user_id_train = orders %>% filter( eval_set == "train") %>%
                           select( user_id)
user_id_train = user_id_train$user_id

# repeat last order
repeat_last_order <- orderp_products %>% group_by( user_id ) %>% 
                             filter( order_number == max(order_number) ) %>%
                             arrange( user_id, product_id ) %>%
                             ungroup() %>% 
                             select( user_id, product_id ) %>%
                             mutate( y_hat = 1 ) %>% #                     
                             full_join( ordert,  by = c("user_id", "product_id") ) %>%
                             group_by( user_id, product_id ) %>%
                             arrange( user_id, product_id ) %>%
                             mutate( y_out = reordered ) %>%
                             filter( user_id %in% user_id_train )

repeat_last_order$y_out[ is.na(repeat_last_order$y_out) ] = 0


repeat_last_order_tmp = repeat_last_order %>% head( 100 )

fnGetMetrics( repeat_last_order_tmp )
                             
                             

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
prd$prod_reorder_times       <- 1 + prd$prod_reorders  / prd$prod_first_orders
prd$prod_reorder_ratio       <- prd$prod_reorders      / prd$prod_orders

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
data <- orderp_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))

rm(orderp_products)
rm(orders)
rm(ordert)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

data_tmp = data %>% head( n = 100 )
rm(ordert, prd, users)
gc()

colnames( data )

#plot histogram of average basket
hist( data$user_average_basket, main = "Average order size per user" )



if(FALSE){
# Train / Test datasets ---------------------------------------------------
train_all <- as.data.frame(data[data$eval_set == "train",])

#split train into train (in) and validation (ud) sets: random selection of user_id (one order_id per user)
user_id_train_all  = unique( train_all$user_id ) #select user_id in train

user_id_train_in   = c( user_id_train_all %>% as.data.frame %>% sample_frac(0.1) )$.
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

if(FALSE){
#reserve train for insample analysis
train_insample = as.data.frame(data[data$eval_set == "train",])

user_id_train  = unique( train_insample$user_id ) #select user_id in train
user_id_tmp    = head( user_id_train )
data_tmp       = head( data, n = 100 )

#retain all orders in train_insample by user_id_tmp
train_insample_tmp = train_insample %>% filter( user_id %in% user_id_tmp )
train_insample_tmp$eval_set  = NULL
train_insample_tmp$user_id   = NULL
train_insample_tmp$reordered[is.na(train_insample_tmp$reordered)] <- 0

#clean train_insample data
train_insample$eval_set  = NULL
train_insample$user_id   = NULL
train_insample$reordered[is.na(train_insample$reordered)] <- 0
}

rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)


#original setup
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
  "min_child_weight"    = 1,
  "gamma"               = 0.70,
  "subsample"           = 1,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 6
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

### identify re-ordering customers in train_ud
hist( train_ud$user_reorder_ratio )
hist( train_ud$up_order_rate )
train_ud_tmp = head( train_ud, n = 100 )

order_of_reordering_users = train_ud %>% 
                            group_by( order_id ) %>% 
                            summarise(
                              mean_up_order_rate = mean( up_order_rate )          
                            ) %>%
                            filter( mean_up_order_rate == 1)
                          
                                

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
}

if( FALSE ){
  subtrain <- train %>% sample_frac(0.1) #it randomly selects product_id not user_id
  X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered)), label = subtrain$reordered)
  model <- xgboost(data = X, params = params, nrounds = 80)
  
  importance <- xgb.importance(colnames(X), model = model)
  xgb.ggplot.importance(importance)
  
  #subtrain_tmp = head( subtrain, n = 100 )
  
  rm(X, importance, subtrain)
  gc()
  
  # predict baskets for train_insample --------------------------------------
  X <- xgb.DMatrix(as.matrix(train_insample_tmp %>% select( -order_id, -product_id, -reordered )))
  train_insample_tmp$reordered_hat <- predict(model, X)
  
  hist( train_insample_tmp$reordered_hat )
  
  cutoff = 0.21
  
  train_insample_tmp$reordered_lab <- (train_insample_tmp$reordered_hat > 0.21) * 1
  
  metrics = train_insample_tmp %>% 
            group_by( order_id ) %>%
            summarise( 
              TP  = length( which(reordered * reordered_lab == 1 ) ), # number of True Positive
              CP  = sum( reordered ), # condition positive: number of ordered items
              PCP = sum( reordered_lab ), #predicted condition positive: predicted number of ordered items
              precision = TP / PCP,
              recall    = TP / CP,
              f1score   = 2 * precision * recall / ( precision + recall)
            )
  metrics
  
  mean( metrics$f1score )
}


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
