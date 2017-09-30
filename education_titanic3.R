rm(list = ls()); gc(reset = T)
# ------------------- #
# install.packages("xlsx")
# ------------------- #
library(xlsx)
# ------------------ #

### input ####

set = read.xlsx("C:/Users/roy/Documents/titanic3.xls", sheetName = "titanic3")
set_n = set[-1310, -c(8, 10, 12:14)]
count_na = apply(is.na(set_n), 2, sum)

### sampling ####

sampling_hist = function(data, size) {
  
  x = data; hist.x = hist(x, freq = F)
  bins = with(hist.x, sample(length(mids), size, p = density, replace = T))
  result = runif(length(bins), hist.x$breaks[bins], hist.x$breaks[bins + 1])
  return(result)
  
}

sampling_disc = function(data, size) {
  
  x = table(data); prob = x/sum(x)
  result = sample(dimnames(x)$data, size, prob = prob, replace = T)
  return(result)
  
}

random_sampling = function(count_na, set_n) {
  
  id_na = which(count_na > 0)
  len_na = count_na[id_na]
  loc1 = which(is.na(set_n[, id_na[1]])); loc2 = which(is.na(set_n[, id_na[2]]))
  loc3 = which(is.na(set_n[, id_na[3]]))
  
  set_n[loc1, id_na[1]] = sampling_hist(na.omit(set_n[, id_na[1]]), len_na[1])
  set_n[loc2, id_na[2]] = sampling_hist(na.omit(set_n[, id_na[2]]), len_na[2])
  set_n[loc3, id_na[3]] = sampling_disc(na.omit(set_n[, id_na[3]]), len_na[3])
  
  set_n$sex = ifelse(set_n$sex == "male", 1, 0)
  set_n$age = (set_n$age - 0)/100
  set_n$fare = (set_n$fare - 0)/550
  set_n$sibsp = (set_n$sibsp - min(set_n$sibsp))/max(set_n$sibsp)
  set_n$parch = (set_n$parch - min(set_n$parch))/max(set_n$parch)
  
  pclass.0 = ifelse(set_n$pclass == 1, 1, 0)
  pclass.1 = ifelse(set_n$pclass == 2, 1, 0)
  
  embarked.0 = ifelse(set_n$embarked == "C", 1, 0)
  embarked.1 = ifelse(set_n$embarked == "Q", 1, 0)
  
  set_m = data.frame(set_n[, -c(1, 3, 9)], pclass.0, pclass.1, embarked.0, embarked.1)
  
  return(set_m)
  
}

### accuracy ####

train_test_acc = function(model, x_train, x_test, y_train, y_test) {
  
  train_y = y_train
  train_pred = predict(model, newdata = x_train, type = "class")
  conf_train = table(train_y, train_pred)
  acc_train = sum(diag(conf_train))/sum(conf_train)
  
  pred.model = predict(model, newdata = x_test, type = "class")
  pred.value = y_test
  conf_test = table(pred.value, pred.model)
  acc_test = sum(diag(conf_test))/sum(conf_test)
  
  return(c(acc_train, acc_test))
  
}

### analysis models ####

randomforest = function(form, data_train, data_test, ntree) {
  
  library(randomForest); model = formula(form)
  
  rf_model = randomForest(model, data = data_train,  ntree = ntree, 
                          importance = TRUE, proximity = TRUE)
  
  acc = train_test_acc(rf_model, data_train[, -1], data_test[, -1], factor(data_train[, 1]), data_test[, 1])
  
  return(acc)
  
}

svm = function(form, data_train, data_test, gamma, cost, kernel) {
  
  library(e1071); model = formula(form)
  
  opt_parameter = tune.svm(model, data = data_train, 
                           kernel = kernel, gamma = gamma, cost = cost)
  
  gamma_op = opt_parameter$best.parameters[1]; cost_op = opt_parameter$best.parameters[2]
  
  svm_model = svm(model, data = data_train, kernel = kernel, gamma = gamma_op, cost = cost_op)
  
  acc = train_test_acc(svm_model, data_train[, -1], data_test[, -1], factor(data_train[, 1]), data_test[, 1])
  
  return(acc)
  
}

naivebayes = function(form, data_train, data_test) {
  
  library(e1071); model = model = formula(form)
  
  nb_model <- naiveBayes(model, data = data_train)
  
  acc = train_test_acc(nb_model, data_train[, -1], data_test[, -1], factor(data_train[, 1]), data_test[, 1])
  
  return(acc)
  
}

# -------------------- #

analysis_model = function(set_m, part, type, set_n) {
  
  l = dim(set_m)[1]; idx = sample(1:l, size = as.integer(l*part), replace = F)
  
  train_s = set_m[idx, ]; test_s = set_m[-idx, ]
  
  result = switch(type, 
                  randomforest("factor(survived) ~.", data_train = train_s, data_test= test_s, ntree = 1000), 
                  svm("factor(survived) ~.", data_train = train_s, data_test= test_s, 
                      gamma = 10^-3:10, cost = 10^-3:10, kernel = "radial"), 
                  naivebayes("factor(survived) ~.", data_train = train_s, data_test= test_s))
  
  models = switch(type, "RandomForest", "Support Vector Machine", "Naive Bayes")

  return(c(result, models))
  
}

#### execution ####

set_m = random_sampling(count_na, set_n)

simulation = 10

results = t(replicate(simulation, analysis_model(set_m, part = 0.8, type = 1, set_n))) 
