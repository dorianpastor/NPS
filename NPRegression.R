library(dplyr)
price = read.table('kc_house_data.csv', header=TRUE, sep=',')$price
df = read.table('house_data_1511.txt', header=TRUE, sep=' ')
attach(df)

keep = c("sqm_above", "sqm_basement", "sqm_living", "sqm_living15", "sqm_lot",
         "sqm_lot15", "yr_old", "renovate_index", "geo_dist") # grade looks like a parabola

par(mfrow=c(3,3))
par(mar=c(1,1,1,1))
for (i in keep){
  colnum <- which(colnames(df) %in% i)
  plot(df[,colnum], price,  main = i)
}

# TODO: compute param polynomial regr, univariate against price and think of a step as a consequence




#--------------------------------------------------------------------------
