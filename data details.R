options(warn = -1)

library(plyr)
library(tidyverse)
library(caret)
library(SOAR)
library(data.table)
library(lubridate)



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

train.id = df$`Policy ID`
test.id = samp$`Policy ID`
Store(train.id)
Store(test.id)
#label = df$label
#### POLICY
policy = policy %>%  
  mutate(Date = as.Date(NP2_EFFECTDATE,format = '%d/%m/%Y'),
         try = ifelse(Date>='2020-01-01',1,0),
         pol2 = ifelse(try ==1,`Policy ID`,NA)) 
pol = policy %>% 
  group_by(`Policy ID`) %>% 
  summarise(pol_cnt = n(),
         prod_cnt= length(unique(PPR_PRODCD)),
        # prod_cnt2= sum(length(unique(PPR_PRODCD))),
         mean_premium = mean(NPR_PREMIUM,na.rm = T),
         min_premium = min(NPR_PREMIUM,na.rm = T),
         #date_cnt = length(unique(NP2_EFFECTDATE)),
         principal_cnt = length(unique(CLF_LIFECD==1)),
        # prod_64QNIHM = sum(unique(PPR_PRODCD == "PPR_PRODCD_64QNIHM")),
         # sum_prem = sum(NPR_PREMIUM,na.rm = T),
         family = length(unique(NPH_LASTNAME)),
        # amount = mean(NLO_AMOUNT,na.rm = T),
         location = max(PCL_LOCATCODE),
         #category = max(CATEGORY),
         agent = max(AAG_AGCODE),
         type = max(NLO_TYPE),
         #type_cnt = sum(length(unique(NLO_TYPE))),
         occupation = max(OCCUPATION)) %>% 
  mutate(location = as.numeric(as.factor(location)),
         #category = as.numeric(as.factor(category)),
         agent= as.numeric(as.factor(agent)),
         type = as.numeric(as.factor(type)),
         #occupation = as.numeric(as.factor(occupation)),
         en = freq.encode(location))


df =df %>% left_join(pol,by = "Policy ID")
df = df %>% add_count(mean_premuim) %>% rename(prem_cnt = n)
df$en2 = my.f2cnt(df,"pol_cnt","location")
df$has_new = ifelse(df$`Policy ID` %in% policy$pol2,1,0)
df = df %>% filter(!`Lapse Year` %in% c('2017','2018'))
df$label = ifelse(df$Lapse==1,1,0)
label = df$label
Store(label)
df_train = df %>% select(-c(`Policy ID`,Lapse,`Lapse Year`,label))
#####
df_test = samp %>% left_join(pol,by="Policy ID")
df_test = df_test %>% add_count(mean_premuim) %>% rename(prem_cnt = n)
df_test$en2 = my.f2cnt(df_test,"pol_cnt","location")
df_test$has_new = ifelse(df_test$`Policy ID` %in% pol2,1,0)








###
m = policy %>% group_by(`Policy ID`) %>% summarise(bb = length(unique(CLF_LIFECD==1)))
summarise(cat=max(CATEGORY))
summarise(agent=max(AAG_AGCODE))
summarise(type=max(NLO_TYPE))
summarise(occu=max(OCCUPATION))


df_train$en2 = my.f2cnt(df_train,"pol_cnt","location")
en = freq.encode(df_train$loc)
length(unique(which(CLF_LIFECD==1)))



####PAYMENT
day= policy %>% arrange(`Policy ID`,NP2_EFFECTDATE) %>% 
      filter(!duplicated(`Policy ID`, fromLast = T)) %>% 
      mutate(Date = as.Date(NP2_EFFECTDATE,format = '%d/%m/%Y'),
             days_since_last = as.numeric(Sys.Date()-as.Date(Date))) %>% 
      select(`Policy ID`,Date,days_since_last) 
####
day= payment %>% arrange(`Policy ID`,POSTDATE) %>% 
  filter(!duplicated(`Policy ID`, fromLast = T)) %>% 
  mutate(days_since_last = as.numeric(Sys.Date()-as.Date(POSTDATE))) %>% select(`Policy ID`,days_since_last) 


pol2 = policy[which(policy$try==1),"Policy ID"]
pol2 = unique(pol2)
df$has_new = ifelse(df$`Policy ID` %in% pol2,1,0)
df_test$has_new = ifelse(df_test$`Policy ID` %in% pol2,1,0)

m = payment %>% filter(PREMIUMDUEDATE >= '2019-01-01')

df = df %>% add_count(mean_premuim) %>% rename(prem_cnt = n)


### CLIENT
df2 = client %>% 
  group_by(`Policy ID`) %>% 
  filter(!duplicated(NPH_BIRTHDATE))

m2 = m %>% group_by(`Policy ID`) %>% summarise(bbb = n())
m2 = df2 %>% group_by(`Policy ID`) %>% summarise(bb2 = n())
m2 = df %>% group_by(type) %>% summarise(tm = mean(label,na.rm = T))
m2 = m %>% group_by(`Policy ID`) %>% summarise(mmm2 = mean(AMOUNTPAID,na.rm = T))

m2 = policy %>% group_by(`Policy ID`) %>% summarise(nbb1= min(NPR_PREMIUM, na.rm = T),nbb2= max(NPR_PREMIUM,na.rm = T), ns = sd(NPR_PREMIUM,na.rm = T))





##No of days since last order
# last_30days = orders %>% filter(created_at>='2020-01-29') %>% 
#   group_by(vendor_id) %>% 
#   summarise(orders_last_30days = n()) %>% 
#   rename(vendor = vendor_id)
# 
# day= orders %>% filter(!duplicated(vendor_id, fromLast = T)) %>% 
#      mutate(days_since_last_order = as.numeric(Sys.Date()-as.Date(created_at))
#        ) %>% select(vendor_id,days_since_last_order) %>% 
#       rename(vendor = vendor_id)
# 
# df3 = df3 %>% left_join(last_30days, by = "vendor") %>% 
#               left_join(day, by = "vendor")





########
##
#######
df_train = df[1:length(train.id),]
df_test = df[(length(train.id)+1):nrow(df),]




ftrs = data.frame(
  type = unlist(lapply(df[1:length(train.id),],class)),
  n.unique = unlist(lapply(df[1:length(train.id),],function(x)length(unique(x)))),
  f.missing = unlist(lapply(df[1:length(train.id),],function(x)mean(is.na(x)))),
  spear.cor = unlist(lapply(df[1:length(train.id),],function(x){idx = !is.na(x);
  if(is.factor(x)) x = as.numeric(x);
  if(is.character(x)) x = as.numeric(as.factor(x));
  if(is.integer(x)) x = as.numeric(x);
  if(is.logical(x)) x = as.numeric(x);
  cor(x[idx],y = label[idx], method = "spearman")
  }))
)

ftrs$name = rownames(ftrs)
ftrs =ftrs %>% drop_na()
df = df[,names(df) %in% ftrs$name]











data3 = rbind(df_train,df_test[,colnames(df_train)])
um = umap::umap(data3)
colnames(um$layout) = paste0("umap_",1:2)


