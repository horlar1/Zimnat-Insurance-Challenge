
library(xgboost)

dtrain = xgb.DMatrix(as.matrix(df_train), label=label2)
dtest = xgb.DMatrix(as.matrix(df_test[,colnames(df_train)]))

# cauchyobj <- function(preds, dtrain) {
#   labels <- getinfo(dtrain, "label")
#   c <- 5000 
#   x <-  preds-labels
#   grad <- x / (x^2/c^2+1)
#   hess <- -c^2*(x^2-c^2)/(x^2+c^2)^2
#   return(list(grad = grad, hess = hess))
# }
params = list(booster = "gbtree",
             objective = "binary:logistic",
             eval_metric = "logloss",
             eta = 0.01,
             colsample_bytree = 0.8,
             max_depth = 3,
             min_child_weight = 1,
             # num_parallel_tree = 10,
             nthread = 8,
             #base_score = mean(label),
             #gamma = 1.5,
             subsample = 0.8
)

watchlist= list(train = dtrain)

set.seed(1235)
fit_cv = xgb.cv(params = params,
                data = dtrain,
                #watchlist = watchlist,
                nrounds = 1000,
                #eval_metric = eval_F1Score,
                nfold = 5,
                print_every_n = 500,
                early_stopping_rounds = 10,
                #prediction = TRUE,
                maximize = F)




set.seed(1235)
mod.xgb = xgb.train(data = dtrain,params = params,nrounds = 1000)
imp = as.data.frame(xgb.importance(feature_names = colnames(df_train),model = mod.xgb))


pred= predict(mod.xgb,dtest)
#pred = round(pred^2)
#pred = ifelse(pred2>=0.15,1,0)


sub = data.table(test.id,pred)
colnames(sub) = c("Policy ID","Lapse")
fwrite(sub,file = paste0(subm.dir,"/sub15.csv"),row.names = F)


#########
q = c("PID_GGSYHEK","PID_7G7GRVT","PID_AYQTF9H","PID_E6CZEQ4","PID_EA4PND8","PID_8WXLEQ4","PID_F3L60EV")

### Featire selection process
nfold = 5
fcrossv = function(param,nfold,vb=1){
  set.seed(1235)
  xgb.cv(param,
         dtrain,nrounds = 100000,
         nfold = nfold,
         maximize = F,
         early_stopping_rounds = 50,
         print_every_n = 1000,
         verbose = 1)
}

vimp = colnames(df_train)
crossv = fcrossv(param,nfold)
nrounds = crossv$best_ntreelimit

set.seed(1235)
mod0 = xgb.train(data = dtrain,
                 params = param,
                 nrounds = nrounds,
                 maximize =  F,
                 verbose = 1)

vmodel = colnames(dtrain)
imp = as.data.frame(xgb.importance(feature_names = vmodel,model = mod0))
imp$cumGain = cumsum(imp$Gain)
imp$n = 1:nrow(imp)
impgap = imp
gc()

s= Sys.time()
nfeat= length(impgap$Feature)
eval = 1700
best = -1
eps = 0
featbag = impgap$Feature[1]
for (idx in 2:nfeat) {
  vadd = imp$Feature[idx]
  vimp = c(featbag,vadd)
  dtrain = xgb.DMatrix(as.matrix(df_train[,vimp]), label = label)

  crossv = fcrossv(param,nfold,1)
  nrnds = crossv$best_ntreelimit

  tmpeval = crossv$evaluation_log$test_rmse_mean[nrnds]
  if((tmpeval+eps)<eval){eval = tmpeval; best = idx; featbag=vimp}

  print(c(idx,eval,best,length(featbag)));flush.console()
  rm(dtrain,crossv)
  gc()
}

Sys.time() 






### log loss trick
a = pred
for (i in 1:nrow(a)) {
  j = 1:9
  row = a[i,]
  max_val = max(row)
  preds = match(max_val, row)
  if(max_val > 0.9){
    a[i,preds] = 1
    a[i,j!= preds] = 0
  }
}

a = pred

for (i in 1:nrow(a)) {
  j = 1:9
  preds = 5
  if(a[i,preds] > 0.8){
    a[i,preds] = 1
    a[i,j != preds]=0
  }
}


library(h2o)

h2o.init(nthreads = 6)
tr = cbind(df_train,label2=as.factor(label))

htrain = as.h2o(tr)
dtest = as.h2o(df_test)

aml = h2o.automl(x = seq(1,ncol(htrain)),
                 y = ncol(htrain),
                 training_frame = htrain,
                 project_name = "auto1",
                 nfolds = 5,seed = 1235,
                 max_runtime_secs = 1800,
                 stopping_metric = "misclassification",
                 exclude_algos = "DeepLearning"
                 )
####
p = as.data.frame(h2o.predict(aml@leader,newdata = dtest))
pred = as.data.frame(h2o.predict(aml,newdata = dtest))




############
grid = expand.grid(committees = c(20,30,40), neighbors = c(0,9))
mod = caret::train(x = oof_data[,-1],y = label, method = "cubist",
                    metric = "RMSE",
                    trControl = trainControl(
                      method = "cv",
                      number = 5,
                      verboseIter = T#,
                    #  savePredictions = "final"
                    ),tuneGrid = grid)


dev.result <-  rep(0, nrow(df_train)) 
pred_te <- rep(0, nrow(df_test))

for (this.round in 1:length(folds)) {
  cat("model training",i," ",this.round,"\n")
  valid = c(1:length(label))[unlist(folds[this.round])]
  dev = c(1:length(label))[unlist(folds[1:length(folds)!= this.round])]
  
  dtrain<- xgb.DMatrix(data= as.matrix(df_train[dev,]),label= label[dev])
  #weight = w[dev])
dvalid <- xgb.DMatrix(data= as.matrix(df_train[valid,]),label= label[valid])
  valids <- list(val = dvalid)
  #### parameters are far from being optimal ####  
  param = list(booster = "gbtree",
               objective = "reg:linear",
               eval_metric = "rmse",
               eta = 0.01,
               colsample_bytree = 0.5,
               max_depth = 4,
               min_child_weight = 1,
               # num_parallel_tree = 10,
               nthread = 4,
               #base_score = mean(label),
               gamma = 0,
               subsample = 0.8
  )
  model<- xgb.train(data = dtrain,
                    params= param, 
                    nrounds = 100000, 
                    verbose = T, 
                    list(val1=dtrain , val2 = dvalid) ,       
                    early_stopping_rounds = 100 , 
                    print_every_n = 1000,
                    maximize = F
  )
  pred = predict(model,as.matrix(df_train[valid,]))
  dev.result[valid] = pred  
  pred_test  = predict(model,as.matrix(df_test[,colnames(df_train)]))
  pred_te = pred_te +pred_test
}




oof_data = data.frame(label, xgb, lgb)
colnames(oof_data)[2] = "xgb"
colnames(oof_data)[3] = "lgb"

pred_data = data.frame(xgbt ,lgbt)
colnames(pred_data)[1] = "xgb"
colnames(pred_data)[2] = "lgb"

lr = lm(label ~., data=oof_data)
summary(lr)

pred_stack = predict(lr, newdata=pred_data)
