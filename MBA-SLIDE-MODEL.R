
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
library(xgboost)

# Functions ----------------------------------------------------------------

fnGetMetrics             <- function( mX ){
  
  metrics = mX %>% 
    group_by( user_id ) %>%
    summarise( 
      TP  = length( which( y_act * y_hat == 1 ) ), # number of True Positive
      CP  = sum( y_act ),     #           condition positive: number of re-ordered items
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
path_subm <- "./SUBM"
#path <- "./DATA"

aisles      <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp      <- fread(file.path(path, "order_products__prior.csv"))
ordert      <- fread(file.path(path, "order_products__train.csv"))
orders      <- fread(file.path(path, "orders.csv"))
products    <- fread(file.path(path, "products.csv"))

# Load Data from MTEC-0373 ------------------------------------------------

# path_main = "C:/Users/sboriss/PROPACAD/"
# path_data = paste0(path_main, 'DATA/')
# path_subm = paste0(path_main, 'SUBM/')
# 
# orders      <- fread( paste0(path_data, 'orders.csv') ) 
# products    <- fread( paste0(path_data, 'products.csv'             ) )
# ordert      <- fread( paste0(path_data, 'order_products__train.csv') )
# orderp      <- fread( paste0(path_data, 'order_products__prior.csv') )
# aisles      <- fread( paste0(path_data, 'aisles.csv'               ) )
# departments <- fread( paste0(path_data, 'departments.csv'          ) )

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
orderp_products_tmp = head( orderp_products, n = 300 )

rm(orderp)
gc()

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
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T),
    user_mean_dow              = mean( order_dow ),
    user_mean_hod              = mean( order_hour_of_day)
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
         post_days_since_last_order = days_since_prior_order,
         post_dow                   = order_dow,
         post_hod                   = order_hour_of_day)

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

data_tmp = data %>% head( 100 )

rm(orderp_products)
gc()

# get user_id in train set -----------------------
user_id_train = orders %>% filter( eval_set == "train") %>% select( user_id)
user_id_train = user_id_train$user_id

user_order_test = orders %>% filter( eval_set == "test") %>% select( user_id, order_id )
user_id_test = user_order_test$user_id

rm(orders)
gc()

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id")) %>%
            replace_na( list( reordered = 0 )) %>%
            mutate( prod_orders = log(prod_orders) ) %>% 
            mutate( user_period = log(user_period) ) 

data_tmp = data %>% head( n = 100 )
rm(ordert, prd, users)
gc()

colnames( data )

# Train / Test datasets ---------------------------------------------------

test      =  data%>% filter(eval_set == "test")
dim( test )

train_all = data %>% filter(eval_set == "train") 
dim( train_all )

rm( data )
gc()

#split train into train (in) and validation (ud) sets: random selection of user_id (one order_id per user)
set.seed( 17 )
user_id_train_in   = c( user_id_train %>% as.data.frame %>% sample_frac(0.1) )$.
length( user_id_train_in )

#train train set
train_in = train_all %>% filter( user_id %in% user_id_train_in )

#validation train set
train_ud = train_all %>% filter( !(user_id %in% user_id_train_in ) )
dim( train_ud )

rm( train_all )
gc()


#remove unnecessary variables in train part
train_in$eval_set   <- NULL
train_in$user_id    <- NULL
train_in$product_id <- NULL
train_in$order_id   <- NULL

#remove unnecessary variables in validation part
train_ud$eval_set  = NULL
#train_ud$user_id   = NULL

#remove unnecessary variables in test part
test$eval_set  <- NULL
test$user_id   <- NULL
test$reordered <- NULL



# Model -------------------------------------------------------------------
cutoff = 0.17

cutoff_seq = seq( 0.21, 0.21, 0.01 )

mtrx_cutoff = lapply( cutoff_seq, function(cutoff){
    
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
    
    
    X     <- xgb.DMatrix( as.matrix( train_in %>% select(-reordered) ), label = train_in$reordered )
    model <- xgboost(data = X, params = params, nrounds = 90)
    
    importance <- xgb.importance(colnames(X), model = model)
    xgb.ggplot.importance(importance)
    
    rm(X, importance)
    gc()
    
    # predict baskets for train_ud --------------------------------------
    X <- xgb.DMatrix( as.matrix( train_ud %>% select( -order_id, -product_id, -reordered, -user_id ) ) )
    
    train_ud_ = train_ud
    
    train_ud_ = train_ud_ %>% mutate( reordered_hat = predict( model, X ) )
    train_ud_ = train_ud_ %>% mutate( y_hat         = ifelse( reordered_hat > cutoff, 1, 0 ) )
    train_ud_ = train_ud_ %>% mutate( y_act         = reordered ) %>% select( -reordered )
    
    train_ud_tmp = train_ud_ %>% head( 100 )
    
    mtrx_train_ud_ = fnGetMetrics( train_ud_ )
    
    mtrx_train_ud    = apply( mtrx_train_ud_, 2, summary ) %>% as.data.frame %>% select( -user_id ) %>% round( digits = 3 )
    print( mtrx_train_ud )
    mtrx_train_ud

})
names( mtrx_cutoff ) = paste0( "cutoff_", cutoff_seq*100 )
mtrx_cutoff

