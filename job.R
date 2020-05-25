
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


