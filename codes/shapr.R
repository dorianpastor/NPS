library(Hmisc)
# File di diagnosi regressioni

# Errore in log($) al variare del prezzo
y_pred = predict(model_final,X_test)
plot(y_test, y_test-y_pred)
abline(h=0)


# Proviamo a vedere come varia l'errore al variare del prezzo delle case
n_windows = 100

data = data.frame(cbind(y_test, abs(10^y_test-10^y_pred)))
colnames(data) = c("y_test","abs_error")
data$group =  as.numeric(cut_number(data$y_test,n_windows))
datagg = data[,2:3] %>% 
  group_by(group) %>% 
  summarise(across(everything(), list(mean = mean)))
plot(datagg, pch=20, xlab="bins from 100k to 2M dollar houses", ylab="mae in each bin")
min(datagg[,2])  # Best performing window of prediction average error
max(datagg[,2])  # Worst performing window of prediction average error
# Distribution of pct error (having the mean error is not satisfying)
hist(as.numeric(unlist(unname(datagg[,2]))), breaks=20) 

data_pct = data.frame(cbind(y_test, 100*abs(10^y_test-10^y_pred)/(10^y_pred)))
colnames(data_pct) = c("y_test","abs_pct_error")
data_pct$group =  as.numeric(cut_number(data_pct$y_test,n_windows))
datagg_pct = data_pct[,2:3] %>% 
  group_by(group) %>% 
  summarise(across(everything(), list(mean = mean)))
plot(datagg_pct, pch=20, xlab="bins from 100k to 2M dollar houses", ylab="mape in each bin")
grid(nx=n_windows, ny=NULL)
min(datagg_pct[,2])  # Best performing window of prediction average pct error
max(datagg_pct[,2])  # Worst performing window of prediction average pct error
# Distribution of pct error (having the mean error is not satisfying)
hist(as.numeric(unlist(unname(datagg_pct[,2]))), breaks=20) 
# Where should our model for rich houses start working, where the one for poor?
datagg_pct[datagg_pct[,2]>20,1] 
# first 5 groups, last 4 (or 9) out of 100
groupsize = dim(x_test)[1]/n_windows
sorted_by_price = x_test[order(x_test$price),"price"]
max(sorted_by_price[1:5*groupsize])  # fino 210k
sorted_by_price = x_test[order(x_test$price, decreasing = T),"price"]
min(sorted_by_price[1:6*groupsize]) # 1.2M (or 900k) (to have 1M, last 6)


#########################################################################
# Experiments below
#########################################################################
library(shapr)
#save_X1 = X1
rm(X1_copy)
idxs = sample(1:dim(save_X1)[1],1000)
X1 = save_X1[idxs,]
X1_copy = X1
X1_copy$age_cut =  (X1$yr_old-80)*(X1$yr_old>80)
X1_copy$condition_cut = cut(X1$condition,breaks = c(min(X1$condition),3,max(X1$condition)),include.lowest = T, right=F)
X1_copy$age_inter = X1$yr_old*X1$has_ren
#X1_copy$bathfloors_ratio_s = ns(X1$bathfloors_ratio, df=2)
#X1_copy$geodist_index_s = bs(X1$geodist_index, degree=2)
X1_copy = subset(X1_copy, select = -c(geodist_index,bathfloors_ratio,condition,yr_old,has_ren)) 

#save_x1_test = x_test_1
idxs2 = sample(1:dim(save_x1_test)[1],500)
x_test_1 = x_test_1[idxs2,]
x_test_copy = x_test_1
x_test_copy$age_cut =  (x_test_1$yr_old-80)*(x_test_1$yr_old>80)
x_test_copy$condition_cut = cut(x_test_1$condition,breaks = c(min(x_test_1$condition),3,max(x_test_1$condition)),include.lowest = T, right=F)
x_test_copy$age_inter = x_test_1$yr_old*x_test_1$has_ren
#X1_copy$bathfloors_ratio_s = ns(X1$bathfloors_ratio, df=2)
#X1_copy$geodist_index_s = bs(X1$geodist_index, degree=2)
x_test_copy = subset(x_test_copy, select = -c(geodist_index,bathfloors_ratio,condition,yr_old,has_ren)) 

model_final = lm(y_train[idxs] ~ #bathfloors_ratio_s+
                      view + grade +
                      #condition_cut +
                      age_cut + age_inter + 
                      #geodist_index_s +
                      log10.sqm_living. +
                      log10.sqm_lot. +
                      log10.sqm_living15. +
                      log10.sqm_lot15. + is_rich
                    ,data=X1_copy
) 
summary(model_final)
explainer <- shapr(X1_copy, model_final)

p <- mean(y_train)
explanation <- explain(
  x_test_copy,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

