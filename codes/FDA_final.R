################################################################################
####################      FUNCTIONAL  DATA  CREATION      ######################
####################                   &                  ######################
####################       TIME   SERIES   ANALYSIS       ######################
################################################################################
kc_cleaned = read.csv("kc_cleaned.csv")
attach(kc_cleaned)

n = 20347
ndays = 396

names(kc_cleaned)

price_sqm_ratio = price / sqm_living

################################################################################
############################ DAILY SEARCH ######################################
################################################################################
counter = numeric(ndays)
mean_daily_price_sqm_ratio = numeric(ndays)

for (i in 1:n){
  counter[ord_date[i]] = counter[ord_date[i]] +1
  mean_daily_price_sqm_ratio[ord_date[i]] = mean_daily_price_sqm_ratio[ord_date[i]]
                                            + price_sqm_ratio[i]
}

daily_means = NULL
for (i in 1:length(mean_daily_price_sqm_ratio))
  if (counter[i]!=0)
    daily_means = c(daily_means,mean_daily_price_sqm_ratio[i]/counter[i])

mean(daily_means) 

#weekend_days = NULL
#for (i in 0:56){
#  weekend_days = c(weekend_days,3+7*i,4+7*i)
#}

week_days = rep(1,ndays)
for (i in 0:56){
  week_days[3+7*i] = 0
  week_days[4+7*i] = 0
}

plot(1:ndays,mean_daily_price_sqm_ratio, col = ifelse(week_days,"red","lightblue"),pch=20)

#shapiro.test(mean_daily_price_sqm_ratio[week_days==1])
#shapiro.test(mean_daily_price_sqm_ratio[week_days==0])

#hist(mean_daily_price_sqm_ratio[week_days==1])
#hist(mean_daily_price_sqm_ratio[week_days==0])

plot(1:length(mean_daily_price_sqm_ratio[week_days==1]),mean_daily_price_sqm_ratio[week_days==1])

################################################################################
############################# WEEKLY SEARCH ####################################
################################################################################
nweeks = 57
week = numeric(n)

for (i in 1:n)
  week[i] = (ord_date[i]+2)%/%7+1

counter_weeks = numeric(nweeks)
price_sqm_ratio_weekly_mean = numeric(nweeks)

# useful gen
bedfloors_ratio_weekly_mean = numeric(nweeks)
bathfloors_ratio_weekly_mean = numeric(nweeks)
view_weekly_mean = numeric(nweeks)
condition_weekly_mean = numeric(nweeks)
grade_weekly_mean = numeric(nweeks)
is_rich_weekly_mean = numeric(nweeks)

# useful age
renovate_index_weekly_mean = numeric(nweeks)

# useful geo
geodist_index_weekly_mean = numeric(nweeks)

# useful sqm
sqm_lot_weekly_mean = numeric(nweeks)
sqm_living15_weekly_mean = numeric(nweeks)
sqm_lot15_weekly_mean = numeric(nweeks)

for (i in 1:n){
  counter_weeks[week[i]] = counter_weeks[week[i]] +1
  price_sqm_ratio_weekly_mean[week[i]] = price_sqm_ratio_weekly_mean[week[i]] + price_sqm_ratio[i]
  
  # useful gen 
  bedfloors_ratio_weekly_mean[week[i]] = bedfloors_ratio_weekly_mean[week[i]] +  bedfloors_ratio[i]
  bathfloors_ratio_weekly_mean[week[i]] = bathfloors_ratio_weekly_mean[week[i]] +  bathfloors_ratio[i]
  view_weekly_mean[week[i]] = view_weekly_mean[week[i]] +  view[i]
  condition_weekly_mean[week[i]] = condition_weekly_mean[week[i]] +  condition[i]
  grade_weekly_mean[week[i]] = grade_weekly_mean[week[i]] +  grade[i]
  is_rich_weekly_mean[week[i]] = is_rich_weekly_mean[week[i]] +  is_rich[i]
  
  # useful age
  renovate_index_weekly_mean[week[i]] = renovate_index_weekly_mean[week[i]] + renovate_index[i]
  
  # useful geo
  geodist_index_weekly_mean[week[i]] = geodist_index_weekly_mean[week[i]] + geodist_index[i]
  
  # useful sqm
  sqm_lot_weekly_mean[week[i]] = sqm_lot_weekly_mean[week[i]] + sqm_lot[i]
  sqm_living15_weekly_mean[week[i]] = sqm_living15_weekly_mean[week[i]] + sqm_living15[i]
  sqm_lot15_weekly_mean[week[i]] = sqm_lot15_weekly_mean[week[i]] + sqm_lot15[i]
 
}

price_sqm_ratio_weekly_mean = price_sqm_ratio_weekly_mean/counter_weeks

# useful gen
bedfloors_ratio_weekly_mean = bedfloors_ratio_weekly_mean/counter_weeks
bathfloors_ratio_weekly_mean = bathfloors_ratio_weekly_mean/counter_weeks
view_weekly_mean = view_weekly_mean/counter_weeks
condition_weekly_mean = condition_weekly_mean/counter_weeks
grade_weekly_mean = grade_weekly_mean/counter_weeks
is_rich_weekly_mean = is_rich_weekly_mean/counter_weeks

# useful age
renovate_index_weekly_mean = renovate_index_weekly_mean/counter_weeks

# useful geo
geodist_index_weekly_mean = geodist_index_weekly_mean/counter_weeks

# useful sqm
sqm_lot_weekly_mean = sqm_lot_weekly_mean/counter_weeks
sqm_living15_weekly_mean = sqm_living15_weekly_mean/counter_weeks
sqm_lot15_weekly_mean = sqm_lot15_weekly_mean/counter_weeks



# mean(price_sqm_ratio)    #2832.683
# mean(daily_means)        #2862.847, giusto perché i giorni cosniderati sono solo quelli in cui sono avvenute vendite
# mean(price_sqm_ratio_weekly_mean) # 2855.558, giusto perchéla settimana numero uno pesa di più rispetto alle altre

plot(1:nweeks,price_sqm_ratio_weekly_mean,pch=20)   # NB: da levare la prima settimana e le ultime 2 (guardare counter_weeks)
plot(2:(nweeks-2),price_sqm_ratio_weekly_mean[2:(nweeks-2)],pch=20)

################################################################################
############################# MONTHLY SEARCH ###################################
################################################################################
nmonths = 13
counter_months = numeric(nmonths)
mean_monthly_price_sqm_ratio = numeric(nmonths)

month = numeric(n)
for (i in 1:n) {
  if(ord_date[i]<=31)
    month[i] = 1
  else if(ord_date[i]<=61)
    month[i] = 2
  else if(ord_date[i]<=92)
    month[i] = 3
  else if(ord_date[i]<=123)
    month[i] = 4
  else if(ord_date[i]<=153)
    month[i] = 5
  else if(ord_date[i]<=184)
    month[i] = 6
  else if(ord_date[i]<=214)
    month[i] = 7
  else if(ord_date[i]<=245)
    month[i] = 8
  else if(ord_date[i]<=276)
    month[i] = 9
  else if(ord_date[i]<=304)
    month[i] = 10
  else if(ord_date[i]<=335)
    month[i] = 11
  else if(ord_date[i]<=365)
    month[i] = 12
  else if(ord_date[i]<=396)
    month[i] = 13
}

for (i in 1:n){
  counter_months[month[i]] = counter_months[month[i]]+1
  mean_monthly_price_sqm_ratio[month[i]] = mean_monthly_price_sqm_ratio[month[i]] + price_sqm_ratio[i]
}


mean_monthly_price_sqm_ratio = mean_monthly_price_sqm_ratio/counter_months

# mean(price_sqm_ratio)    #2832.683
# mean(daily_means)        #2862.847, giusto perché i giorni cosniderati sono solo quelli in cui sono avvenute vendite
# mean(price_sqm_ratio_weekly_mean) # 2855.558, giusto perchéla settimana numero uno pesa di più rispetto alle altre
# mean(mean_monthly_price_sqm_ratio)  #2835.358, giusto perché più il mese ha osservazioni, più quelle osservazioni pesano di meno


plot(1:nmonths,mean_monthly_price_sqm_ratio,pch=20)

################################################################################
########################      PLOT    ##########################################
################################################################################

week_days = factor(week_days)

x11()
par(mfrow=c(3,1))
plot(1:ndays,mean_daily_price_sqm_ratio/counter, col = ifelse(week_days==1,"red","lightblue")
     ,pch=16, xlab = "days", ylab = "$/(m^2)")
title("DAILY")
plot(1:nweeks,price_sqm_ratio_weekly_mean,pch=16, xlab = "weeks", ylab = "$/(m^2)")
title("WEEKLY")
plot(1:nmonths,mean_monthly_price_sqm_ratio,pch=16, xlab = "months", ylab = "$/(m^2)")  
title("MONTHLY")

plot(ord_date)


################################################################################
#########################      Tentativo mappe      ############################
################################################################################


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
library(maps)
library(usmap)
library(ggplot2)
library(mapdata)
library(ggmap)
library(mapdata)
library(rgdal)
library(tidyverse)

ggplot(data=`King_County_Political_Boundary_(no_waterbodies)___kingco_area`)

kc_bound <- readOGR(dsn="C:/Users/Messoanuovo.it/Desktop/TimeSeries/NPS - Time Series",layer="King_County_Political_Boundary_(no_waterbodies)___kingco_area")
kc_bound_df <- map_data(kc_bound)

map('county', 'washington', col = palette())


plot_usmap(include = c("WA")) +
  labs(title = "Washington (state)") +
  theme(panel.background = element_rect(color = "blue"))

washington <- subset(state, region=="washington")
counties <- map_data("county")
washington_county <- subset(counties, region=="washington")

sa <- map_data('usa')
state <- map_data("state")
washington <- subset(state, region=="washington")
counties <- map_data("county")
washington_county <- subset(counties, region=="washington")
ca_map <- ggplot(data=washington, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=washington_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  ggtitle('Washington Map with Counties') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ca_map

################################################################################
################################################################################
################################################################################

# TENTATIVO 1
library(ggmap)
qmplot(data = kc_cleaned, 
       x = long, 
       y = lat, 
       color = I("#342c5c"), 
       alpha = I(0.5))

# TENTATIVO 2
install.packages("sf")
library(sf)

address <- st_read("AddressShpParts/address.shp")
head(address)

ggplot(data = address) +
  geom_sf()


################################################################################
#########################       ZIP CLUSTERS        ############################
################################################################################


zipcode = factor(zipcode)
nzips = length(levels(zipcode))
long_lat_means = matrix(NA,nrow = nzips, ncol = 2)

for (i in 1:nzips)
  long_lat_means[i,] = colMeans(cbind(long,lat)[which(zipcode==levels(zipcode)[i]),])

plot(long_lat_means,pch= 16)
dimnames(long_lat_means)[[1]] = levels(zipcode)
dimnames(long_lat_means)[[2]] = c("Long","Lat")

# b = NULL
# w = NULL
# for(k in 1:10){
#   result.k = kmeans(long_lat_means, k)
#   w = c(w, sum(result.k$wit))
#   b = c(b, result.k$bet) }
# x11()
# matplot (1:10, w/(w+b), pch='', xlab='#clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
# lines(1:10, w/(w+b), type='b', lwd=2) 


k = 7
result.k = kmeans(long_lat_means, centers = k, nstart = 300 ) # centers = quanti cluster voglio avere
names(result.k)
result.k$iter
result.k$size

colors = ifelse(result.k$cluster==1,"red",
                ifelse(result.k$cluster==2 ,"blue",
                ifelse(result.k$cluster==3,"purple",
                ifelse(result.k$cluster==4,"lightgreen",
                ifelse(result.k$cluster==5,"darkgreen",
                ifelse(result.k$cluster==6,"lightblue","orange"))))))
x11()
# plot(long_lat_means, col = result.k$cluster+1,pch=16)
plot(long_lat_means, col = colors,pch=20)

#richest_by_Enri = c("98004","98006","98033","98039","98040","98053","98074","98075",
#           "98077","98101","98110","98112","98134","98164","98199")

richest_ind_by_Enri = c(4,6,22,25,26,30,38,39,40,49,70)

price_zip_mean = numeric(nzips)
price_sqm_zip_mean = numeric(nzips)
for (i in 1:nzips){
  price_zip_mean[i] = mean(price[which(zipcode==levels(zipcode)[i])])
  price_sqm_zip_mean[i] = mean(price_sqm_ratio[which(zipcode==levels(zipcode)[i])])
}

plot(1:nzips,price_zip_mean)
richest_by_Jahi = which(price_zip_mean>650000)

plot(1:nzips,price_sqm_zip_mean)
richest_sqm_by_Jahi = which(price_sqm_zip_mean>4000)

plot(long_lat_means, col = colors,pch=20)
title("Enri's richest")
points(long_lat_means[richest_ind_by_Enri,])
points(-122.33825,47.62617,col = "black",pch = 16)

plot(long_lat_means, col = colors,pch=20)
title("Jahi's richest ($)")
points(long_lat_means[richest_by_Jahi,])
points(-122.33825,47.62617,col = "black",pch = 16)

plot(long_lat_means, col = colors,pch=20)
title("Jahi's richest ($/m2)")
points(long_lat_means[richest_sqm_by_Jahi,])
points(-122.33825,47.62617,col = "black",pch = 16)

################################################################################
####################         ZIP FUNCTIONS         #############################
################################################################################
zip_clust = numeric(n)
for (i in 1:n)
  for (j in 1:nzips)
    if (zipcode[i] == levels(zipcode)[j])
      zip_clust[i] = result.k$cluster[j]    # non il massimo dell'efficienza ma funziona

price_sqm_ratio_zip_mean = numeric(k)
counter_zips = numeric(k)

for (i in 1:n){
  counter_zips[zip_clust[i]] = counter_zips[zip_clust[i]] + 1
  price_sqm_ratio_zip_mean[zip_clust[i]] = price_sqm_ratio_zip_mean[zip_clust[i]] + price_sqm_ratio[i]
}

price_sqm_ratio_zip_mean = price_sqm_ratio_zip_mean/counter_zips

mean(price_sqm_ratio_zip_mean) # 2621.452

################################################################################
############ Mean weekly price for sqm for each zip ############################
################################################################################

price_sqm_weekly_zip_mean = matrix(NA,nrow = nweeks,ncol = k)
#grade_weekly_zip_mean = matrix(NA,nrow = nweeks,ncol = k)
#geodist_index_weekly_zip_mean = matrix(NA,nrow = nweeks,ncol = k)

for (i in 1:nweeks)
  for (j in 1:k){
    price_sqm_weekly_zip_mean[i,j] = mean(price_sqm_ratio[which(week == i & zip_clust == j)])
#    grade_weekly_zip_mean[i,j] = mean(grade[which(week == i & zip_clust == j)])
#    geodist_index_weekly_zip_mean[i,j] = mean(geodist_index[which(week == i & zip_clust == j)])
  }


matplot(price_sqm_weekly_zip_mean,type='l',lwd= 3,
        xlab = "weeks",ylab="price/squaremeter mean",
        col = c("red","blue","purple","lightgreen","darkgreen","lightblue","orange"))

#matplot(grade_weekly_zip_mean,type='l',lwd= 3)
#matplot(geodist_index_weekly_zip_mean,type='l',lwd= 3,col = c("red","blue","purple","lightgreen","darkgreen","lightblue","orange"))

x11()
plot(long_lat_means, col = colors,pch=16,asp =1)
points(-122.33825,47.62617,col = "black",pch = 16)

################################################################################
####################        GRIGLIA per PANZ          ##########################
################################################################################

help("expand.grid")


model_final = lmrob(y_train~ns(bathfloors_ratio, df=2)+
                      view + grade +
                      cut(condition,breaks = c(min(condition),3,max(condition)),include.lowest = T, right=F)+
                      I((yr_old-80)*(yr_old>80)) + yr_old:has_ren + 
                      bs(geodist_index, degree=2) +
                      log10.sqm_living. +
                      log10.sqm_lot.+
                      log10.sqm_living15.+
                      log10.sqm_lot15.+is_rich)
model_gam=gam(prestige ~ s(education,bs='cr') + s(income,bs='cr'))

bathfloors_ratio.grid = seq(range(bathfloors_ratio)[1],range(bathfloors_ratio)[2],length.out = 100)
view.grid = levels(as.factor(view))
grade.grid = levels(as.factor(grade))
condition.grid = levels(as.factor(condition))
yr_old.grid = seq(range(yr_old)[1],range(yr_old)[2],length.out = 100)
has_ren.grid = levels(as.factor(has_ren))
geodist_index.grid = seq(range(geodist_index)[1],range(geodist_index)[2],length.out = 100)
log10.sqm_living.grid = seq(range(log10.sqm_living.)[1],range(log10.sqm_living.)[2],length.out = 100)
log10.sqm_lot.grid = seq(range(log10.sqm_lot.)[1],range(log10.sqm_lot.)[2],length.out = 100)
log10.sqm_living15.grid = seq(range(log10.sqm_living15.)[1],range(log10.sqm_living15.)[2],length.out = 100)
log10.sqm_lot15.grid = seq(range(log10.sqm_lot15.)[1],range(log10.sqm_lot15.)[2],length.out = 100)

grid=expand.grid(bathfloors_ratio.grid,view.grid,grade.grid,condition.grid,yr_old.grid,
                 has_ren.grid,geodist_index.grid,log10.sqm_living.grid,log10.sqm_lot.grid,
                 log10.sqm_living15.grid,log10.sqm_lot15.grid)

names(grid)=c('bathfloors_ratio','view','grade','condition','yr_old','has_ren',
              'geodist_index','log10.sqm_living.','log10.sqm_lot.',
              'log10.sqm_living15.','log10.sqm_lot15.')

pred=predict(...,newdata=grid)

################################################################################
#######################       MA for weekly price       ########################
################################################################################

price_sqm_weekly_zip_mean_no_last = price_sqm_weekly_zip_mean[-c(56,57),]

# smoothing utilizzando ultime 4 osservazioni:
smo1 = matrix(NA,nrow = 55,ncol = k)
smo1[1:3,]= price_sqm_weekly_zip_mean_no_last[1:3,]
for ( i in 4:55){
  smo1[i,] = colMeans(price_sqm_weekly_zip_mean_no_last[(i-3):i,])
}

matplot(smo1,type="l")

# smoothing utilizzando le 5 osservazioni più vicine:
smo2 = matrix(NA,nrow = 55,ncol = k)
smo2[c(1,2,54,55),] = price_sqm_weekly_zip_mean_no_last[c(1,2,54,55),]
for ( i in 3:53){
  smo2[i,] = colMeans(price_sqm_weekly_zip_mean_no_last[(i-2):(i+2),])
}

matplot(smo2,type="l")

# Conformal Prediction
ber_m = t(smo2)
alpha=.1
n=nrow(ber_m)
i1=sample(1:n,n/2)
t_set=ber_m[i1,]
c_set=ber_m[-i1,]
mu=colMeans(t_set)
res=c_set-mu
ncm=apply(res,2,max)
ncm_sort=c(sort(ncm),Inf)
d=ncm_sort[ceiling((n/2 + 1)*(1-alpha))]

matplot(cbind(mu,mu+d,mu-d),type='l')
# FINE Conformal Prediction


# EVITEREI QUESTI METODI
# smoothing utilizzando FDA methods:
library(fda)
library(fields)

n = dim(price_sqm_weekly_zip_mean_no_last)[1]
N = dim(price_sqm_weekly_zip_mean_no_last)[2]
time = 1:n	# ed eventuali modifiche rispetto allo step temporale
matplot(price_sqm_weekly_zip_mean_no_last, type = 'l', main = 'p-w-lin', xlab = 'weeks', ylab = 'mean price/surf')

basis= create.bspline.basis(rangeval = c(1,n), nbasis = 13)	# nbasis 

data_fd = Data2fd(y = price_sqm_weekly_zip_mean_no_last, argvals = time, basisobj = basis)	# rangeval segue le eventuali modifiche di time
plot.fd(data_fd, main="B-splines", xlab = 'weeks', ylab = 'mean price/surf')
title("smooth")
lines(mean.fd(data_fd),lwd=2)		# media
