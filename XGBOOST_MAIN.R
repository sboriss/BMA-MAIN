
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
library(xgboost)
library( ggplot2 )

# functions --------------

fnGetMetrics             <- function( mX ){
  
  metrics = mX %>% 
    group_by( user_id ) %>%
    summarise( 
      TP  = length( which( ordered * ordered_hat == 1 ) ), # number of True Positive
      CP  = sum( ordered ),     #           condition positive: number of ordered items
      PCP = sum( ordered_hat ), # predicted condition positive: predicted number of ordered items
      precision = ifelse( PCP == 0, 0, TP / PCP),
      recall    = ifelse(  CP == 0, 0, TP /  CP),
      f1score   = ifelse( any( c( precision, recall ) == 0), 0, 2 * precision * recall / ( precision + recall) )
    )
  return( metrics )
}


fnIdenifyReorderingUsers <- function(){
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
}                                      



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
    

# Reshape data ------------------------------------------------------------
aisles$aisle           <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set        <- as.factor(orders$eval_set)
products$product_name  <- as.factor(products$product_name)
    
products <- products %>% 
            inner_join(aisles) %>% inner_join(departments) %>% 
            select(-aisle_id, -department_id)
    
rm(aisles, departments)
    
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]
ordert$ordered <- 1

ordert_tmp = ordert %>% arrange( user_id ) %>% head( ordert, n = 100 )
    
#only orders from prior
orderp_products     = orders %>% inner_join(orderp, by = "order_id")
orderp_products_tmp = head( orderp_products, n = 100 )   

rm( orderp )

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
prd$prod_reorder_times       <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio       <- prd$prod_reorders / prd$prod_orders
    
prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)
  
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

users_tmp = head( users, n = 100 )
    
rm(us)
gc()
    

# Database ----------------------------------------------------------------
data <- orderp_products %>%
      group_by(user_id, product_id) %>% 
      summarise(
        up_orders = n(),
        up_first_order = min(order_number),
        up_last_order  = max(order_number),
        up_average_cart_position = mean(add_to_cart_order)
      )
    
data <- data %>% 
      inner_join(prd, by = "product_id") %>%
      inner_join(users, by = "user_id")

rm( prd, users )
    
data$up_order_rate                   <- data$up_orders / data$user_orders
data$up_orders_since_last_order      <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)
    
data_tmp = head( data, n = 100 )

data <- data %>% 
        left_join(ordert %>% select(user_id, product_id, reordered), 
                 by = c("user_id", "product_id"))

data_tmp1 = head( data, n = 100 )
  
#use full_join in order to include in the train/test those products that were ordered for the first time
# data <- data %>% 
#     full_join(ordert %>% select(user_id, product_id, reordered, ordered), by = c("user_id", "product_id")) %>%
#     arrange( user_id, product_id)
  
gc()
  

# Train / Test datasets ----------------------------------------
  
train_all <- as.data.frame(data[data$eval_set == "train",])
    
#split train into train (in) and validation (ud) sets: random selection of user_id (one order_id per user)
user_id_train_all  = unique( train_all$user_id ) #select user_id in train
    
set.seed( 17 )
user_id_train_in   = c( user_id_train_all %>% as.data.frame %>% sample_frac( 0.2 ) )$.
length( user_id_train_in )

user_id_train_ud   = user_id_train_all[ !(user_id_train_all %in% user_id_train_in ) ]
length( user_id_train_ud )

sort( user_id_train_all ) %>% head( n = 20 )
sort( user_id_train_in  ) %>% head( n = 10 )
sort( user_id_train_ud  ) %>% head( n = 10 )
    
#train train set
train_in = train_all %>% filter( user_id %in% user_id_train_in )
    
#validation train set
train_ud = train_all %>% filter( user_id %in% user_id_train_ud )

#keep only those user_id that are in train_ud
ordert_train_ud = ordert %>% filter( user_id %in% user_id_train_ud ) %>%
                             arrange( user_id, product_id )
    
rm( train_all )
    
#remove unnecessary variables in train part
train_in$eval_set   <- NULL
train_in$user_id    <- NULL
train_in$product_id <- NULL
train_in$order_id   <- NULL
train_in$reordered[is.na(train_in$reordered)] <- 0
    
#remove unnecessary variables in validation part
train_ud$eval_set  = NULL
#train_ud$user_id   = NULL
train_ud$reordered[is.na(train_ud$reordered)] <- 0
    
#remove unnecessary variables in test part
test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set  <- NULL
test$user_id   <- NULL
test$reordered <- NULL

train_in_tmp = head( train_in, n = 100)
train_ud_tmp = head( train_ud, n = 300)
test_tmp     = head( test    , n = 100)
    
rm(data)
gc()
    


### pull benchmarks together
if(FALSE){

list_bnch <- list()

# set benchmark: all ordered products -------------------------------------
bnch_all_ordered = train_ud %>% select( user_id, order_id, product_id ) %>% 
                             mutate( ordered_hat = 1 ) %>% 
                             full_join( ordert_train_ud, by = c("user_id", "product_id")) %>%
                             arrange( user_id, product_id )

bnch_all_ordered$ordered_hat[ is.na(bnch_all_ordered$ordered_hat) ] = 0
bnch_all_ordered$ordered[     is.na(bnch_all_ordered$ordered    ) ] = 0

bnch_all_ordered_tmp = head( bnch_all_ordered, n = 200)

metrics_bnch_all_ordered = fnGetMetrics( bnch_all_ordered )

summ_bnch_all_ordered = apply( metrics_bnch_all_ordered, 2, summary ) %>% as.data.frame %>% select( -user_id ) %>% round( digits = 3 )
summ_bnch_all_ordered 

list_bnch$all_ordered = summ_bnch_all_ordered

# set benchmark: TOP-N products according to up_order_rate_since_first_order
TOP_N_MAX = 10

list_TOP_N <- lapply( seq( TOP_N_MAX ), function(TOP_N){

    bnch_topN_ordered = train_ud %>% select( user_id, product_id, up_order_rate_since_first_order, up_order_rate ) %>% 
                                     group_by( user_id ) %>%
                                     mutate( rank = dense_rank( -up_order_rate_since_first_order) ) %>%
                                     filter( rank <= TOP_N ) %>%
                                     mutate( ordered_hat = 1 ) %>% 
                                     full_join( ordert_train_ud, by = c("user_id", "product_id")) %>%
                                     arrange( user_id, product_id )
    
    bnch_topN_ordered$ordered_hat[ is.na(bnch_topN_ordered$ordered_hat) ] = 0
    bnch_topN_ordered$ordered[     is.na(bnch_topN_ordered$ordered    ) ] = 0
    
    bnch_topN_ordered_tmp = bnch_topN_ordered %>% head( n = 100 )
               
    metrics_bnch_topN_ordered = fnGetMetrics( bnch_topN_ordered )
    
    summ_bnch_topN_ordered = apply( metrics_bnch_topN_ordered, 2, summary ) %>% as.data.frame %>% select( -user_id ) %>% round( digits = 3 )
    summ_bnch_topN_ordered

})
names( list_TOP_N ) <- paste0("TOP_N_",seq( TOP_N_MAX ))
list_bnch$TOP_N <- list_TOP_N  


# set benchmark: last ordered products -------------------------------------
bnch_last_order <- orderp_products %>% select(user_id, order_number, product_id) %>%
                   filter( user_id %in% user_id_train_ud ) %>% #retain only users that are in train_ud
                   arrange(user_id, order_number, product_id) %>%
                   group_by(user_id) %>%
                   filter( order_number == max( order_number ) ) %>% #retain last order
                   mutate( ordered_hat = 1) %>%
                   select( -order_number ) %>%
                   full_join( ordert_train_ud, by = c("user_id", "product_id"))  %>% 
                   arrange(user_id, product_id) 
                   
bnch_last_order$ordered[     is.na(bnch_last_order$ordered    ) ] = 0
bnch_last_order$ordered_hat[ is.na(bnch_last_order$ordered_hat) ] = 0
    
bnch_last_order_tmp = head( bnch_last_order, n = 100 )  

metrics_bnch_last_order = fnGetMetrics( bnch_last_order )

summ_bnch_last_order = apply( metrics_bnch_last_order, 2, summary ) %>% as.data.frame %>% select( -user_id ) %>% round( digits = 3 )
summ_bnch_last_order

list_bnch$last_order <- summ_bnch_last_order

# set benchmark: ordered products in last two orders
bnch_last_two_orders <- orderp_products %>% select(user_id, order_number, product_id) %>%
                        filter( user_id %in% user_id_train_ud ) %>% #retain only users that are in train_ud
                        arrange(user_id, order_number, product_id) %>%
                        group_by(user_id) %>%
                        filter( order_number == max( order_number ) | order_number == max( order_number - 1 ) ) %>% #retain last two orders
                        ungroup() %>%
                        group_by( user_id, product_id ) %>%
                        mutate(product_time = row_number()) %>%
                        filter( product_time == 1) %>% # retain unique product_id
                        arrange(user_id, product_id) %>%
                        mutate( ordered_hat = 1) %>%
                        select( -order_number ) %>%
                        full_join( ordert_train_ud, by = c("user_id", "product_id"))  %>% 
                        arrange(user_id, product_id) 

bnch_last_two_orders$ordered[     is.na(bnch_last_two_orders$ordered    ) ] = 0
bnch_last_two_orders$ordered_hat[ is.na(bnch_last_two_orders$ordered_hat) ] = 0

bnch_last_two_orders_tmp = head( bnch_last_two_orders, n = 100 ) 

metrics_bnch_last_two_orders = fnGetMetrics( bnch_last_two_orders )

summ_bnch_last_two_orders = apply( metrics_bnch_last_two_orders, 2, summary ) %>% as.data.frame %>% select( -user_id ) %>% round( digits = 3 )
summ_bnch_last_two_orders

list_bnch$last_two_orders <- summ_bnch_last_two_orders


list_bnch
}

rm(orderp_products, orders)
rm(ordert, prd, users)



#initial analysis
if( FALSE ){
#plot diestribution of number of unique products by user_id
    unique_products_per_user = orderp_products %>% group_by( user_id ) %>%
                                                   summarise(
                                                     unq = length( unique(product_id) ) 
                                                   )
    
    head( unique_products_per_user, n = 100 )
    summary( unique_products_per_user$unq )
    
    orderp_products %>%
      group_by(user_id) %>%
      summarize( n_items = length( unique(product_id) ) ) %>%
      ggplot(aes(x=n_items))+
      geom_histogram(stat="count",fill="red") +
      geom_rug() +
      coord_cartesian(xlim=c(0,200))
}




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
model <- xgboost(data = X, params = params, nrounds = 80, nthread = 8)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance, train_in)
gc()

# predict baskets for train_ud --------------------------------------
X <- xgb.DMatrix(as.matrix(train_ud %>% select( -order_id, -product_id, -reordered, -user_id )))
train_ud$reordered_hat <- predict(model, X)

#join those products that were not in the prior set
train_ud_eval <- train_ud %>% full_join( ordert_train_ud, by = c("user_id", "product_id")) %>%
                              arrange( user_id, product_id )

train_ud_eval$ordered[     is.na(train_ud_eval$ordered    ) ] = 0

train_ud_eval_tmp <- head( train_ud_eval, n = 100)

hist( train_ud$reordered_hat )

#cutoff = 0.21

cutoff = seq(0.10, 0.23, by = 0.01)

meanF1score = sapply(cutoff, function( x ){
  
  #x = 0.21
  train_ud_eval$ordered_hat <- (train_ud_eval$reordered_hat > x ) * 1
  train_ud_eval$ordered_hat[ is.na(train_ud_eval$reordered_hat) ] = 0

  metrics = fnGetMetrics( train_ud_eval )
  #metrics_tmp = head( metrics, n = 100 )
  summ_xgboost = apply( metrics, 2, summary ) %>% as.data.frame %>% select( -user_id ) %>% round( digits = 3 )
  summ_xgboost
  
  mean( metrics$f1score )
})

names( meanF1score ) = cutoff

meanF1score = as.data.frame( meanF1score )
colnames( meanF1score ) = paste( "max_depth", params$max_depth, sep = "_" )

plot( cutoff, meanF1score[, 1] )

meanF1score


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
