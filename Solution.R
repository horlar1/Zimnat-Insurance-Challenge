options(warn = -1)

library(plyr)
library(tidyverse)
library(caret)
library(SOAR)
library(data.table)
library(lubridate)

##### FEATURES
# policy_cnt = unqiue counts of policies
# product_cnt = unique counts of products per policy
# mean premium = average NPR PREMIUM per policy
# min premium  = minimum NPR PREMIUM
# principal_cnt = unique count of principals per policy: last on feature importance might not be useful
# family_cnt = counts of family per policy : NPH LASTNAME
# location = unique location per policy
# aegnt = agent location per policy
# type = max type per policy
# location_freq_encode = frequency encoding of location
# prem_cnt = count of unique mean_premium
# has_new_policy_flag = flag 1 if policy was effected in 2020 else 0
# in client flag = flag if policy is present in client data else 0
# premium_date flag = flag if policy has premium data from 2019 else 0
# policy_location cnt = counts of interaction of policy and  location
# umap 1&2 = umap dimemsion reudction features using tsne

#####
path.dir = getwd()
data.dir = paste0(path.dir,"/Data")

#dir.create(paste0(path.dir,"/subm"))
save.files.dir = paste0(path.dir,"/tmp")
subm.dir = paste0(path.dir,"/subm")
source("utils.R")


####
df = fread(paste0(data.dir,"/train.csv")) %>% as.data.frame()
client = fread(paste0(data.dir,"/client_data.csv")) %>% as.data.frame()
policy = fread(paste0(data.dir,"/policy_data.csv")) %>% as.data.frame()
payment = fread(paste0(data.dir,"/payment_history.csv")) %>% as.data.frame()



#### POLICY
policy = policy %>%  
  mutate(Date = as.Date(NP2_EFFECTDATE,format = '%d/%m/%Y'),
    new = ifelse(Date>='2020-01-01',1,0),
    policy_with_new_client = ifelse(new ==1,`Policy ID`,NA))  

##### PAYMENT
payment2 = payment %>% filter(PREMIUMDUEDATE >= '2019-01-01')

#### CLIENT : CLEANING CLIENT DATA
client = client %>% group_by(`Policy ID`) %>% 
  filter(!duplicated(NPH_BIRTHDATE))


#### MAIN FEATURE ENGINEERING
##### TAKING AGGREGATES PER POLICIES
policy_Aggregate = policy %>% 
  group_by(`Policy ID`) %>% 
  summarise(
    policy_cnt = n(),
    product_cnt= length(unique(PPR_PRODCD)),
    mean_premium = mean(NPR_PREMIUM,na.rm = T),
    min_premium = min(NPR_PREMIUM,na.rm = T),
    principal_cnt = length(unique(CLF_LIFECD==1)),
    family_cnt = length(unique(NPH_LASTNAME)),
    location = max(PCL_LOCATCODE),
    agent = max(AAG_AGCODE),
    type = max(NLO_TYPE)) %>% 
  mutate(
    location = as.numeric(as.factor(location)),
    agent= as.numeric(as.factor(agent)),
    type = as.numeric(as.factor(type)),
    location_freq_encode = freq.encode(location))


##### UMAP DIMENSION REDUCTION
load(paste0(save.files.dir,"/umap_data.RData"))
# data = rbind(df_train,df_test[,colnames(df_train)])
# um = umap::umap(data)
# colnames(um$layout) = paste0("umap_",1:2)


###### CREATING TRAIN DATA WITH ADDITIONAL FEATURES
df = df %>% left_join(policy_Aggregate,by = "Policy ID") %>% 
            add_count(mean_premium) %>% 
            rename(prem_cnt = n) %>% 
  mutate(
    has_new_policy_flag = ifelse(`Policy ID` %in% policy$policy_with_new_client,1,0),
    in_Client_flag = ifelse(`Policy ID` %in% client$`Policy ID`,1,0),
    premium_date_flag = ifelse(`Policy ID` %in% payment2$`Policy ID`,1,0),
    label = ifelse(df$Lapse==1,1,0)
  ) 
  
df$Policy_location_cnt = my.f2cnt(df,"policy_cnt","location")
df = df %>%   filter(!`Lapse Year` %in% c('2017','2018')) ## drop policies that lapsed in 2017 and 2018
df= cbind(df,um$layout[1:48714,])
df = df %>%  filter(in_Client_flag == 1) ## drop all poicies not in client data

#### SETTING TARGET AND ID'S
label = df$label
train.id = df$`Policy ID`
test.id = samp$`Policy ID`


######
df_train = df %>% select(-c(`Policy ID`,Lapse,`Lapse Year`,label))

##### CREATING TEST DATA WITH ADDITIONAL FEATURES 
df_test = samp %>% left_join(policy_Aggregate,by="Policy ID") %>% 
                   add_count(mean_premium) %>% 
                   rename(prem_cnt = n) %>% 
  mutate(
    has_new_policy_flag = ifelse(`Policy ID` %in% policy$policy_with_new_client,1,0),
    in_Client_flag = ifelse(`Policy ID` %in% client$`Policy ID`,1,0),
    premium_date_flag = ifelse(`Policy ID` %in% payment2$`Policy ID`,1,0)
  )
df_test$Policy_location_cnt = my.f2cnt(df_test,"policy_cnt","location")
df_test = cbind(df_test,um$layout[48715:nrow(um$layout),])



##### MODELLING 
## IT'S NOT OPTIMIZED, NO PARAMETER TUNING
### CAN BE IMPROVED
library(xgboost)

dtrain = xgb.DMatrix(as.matrix(df_train), label=label)
dtest = xgb.DMatrix(as.matrix(df_test[,colnames(df_train)]))

### PROBABLY NEEDS TUNING
params = list(booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.01,
  colsample_bytree = 0.8,
  max_depth = 3,
  min_child_weight = 1,
  nthread = 8,
  subsample = 0.8
)
Set.seed(1235)
fit_cv = xgb.cv(params = params,
  data = dtrain,
  nrounds = 1000,
  nfold = 5,
  print_every_n = 500,
  early_stopping_rounds = 10,
  #prediction = TRUE,
  maximize = F)
##
set.seed(1235)
mod.xgb = xgb.train(data = dtrain,params = params,nrounds = 1000)
imp = as.data.frame(xgb.importance(feature_names = colnames(df_train),model = mod.xgb))


pred= predict(mod.xgb,dtest)
sub = data.table(test.id,pred)
colnames(sub) = c("Policy ID","Lapse")
fwrite(sub,file = paste0(subm.dir,"/sub.csv"),row.names = F) 