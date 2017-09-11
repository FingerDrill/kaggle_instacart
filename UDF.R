strs <- function(x){
  str(x, strict.width = "cut")
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

xgb_eval_f1 <- function (yhat, dtrain) {
  require(ModelMetrics)
  y <-  getinfo(dtrain, "label")
  dt <- data.table(user_id=attr(dtrain, 'user_id'), purch=y, pred=yhat)
  f1 <- mean(dt[,.(f1score=f1Score(purch, pred, cutoff=0.2)), by=user_id]$f1score,
             na.rm=T)
  return (list(metric = "f1", value = f1))
}



