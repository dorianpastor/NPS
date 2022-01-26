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

################################################################################
# TENTATIVO FDA: andamento del prezzo al metro quadro per ogni zip per ognuno dei
# primi 12 mesi. Alla fine si ottengono 4 plot degli andamenti per gli zip appartenenti
# ad ognuno dei quattro cluster: MA e poi media + confidence interval?

zipcode = factor(zipcode)
nzips = length(levels(zipcode))
table(zipcode)

ll_meanzip = matrix(NA,nrow = nzips, ncol = 2)
for (i in 1:nzips)
  ll_meanzip[i,] = colMeans(cbind(long,lat)[which(zipcode==levels(zipcode)[i]),])

plot(ll_meanzip)

nmonths = 13

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

zip = numeric(n)
for (i in 1:n) {
  found = FALSE
  z = 1
  while(!found){
    if(zipcode[i]==levels(zipcode)[z]){
      zip[i] = z
      found = TRUE
    }
    z =z+1
  }
}

psr_month_zip_mean = matrix(0, nrow = nmonths, ncol = nzips)
counter_months = matrix(0, nrow = nmonths, ncol = nzips)
for (i in 1:n)
  counter_months[month[i],zip[i]] = counter_months[month[i],zip[i]] + 1
matplot(counter_months,type="l")
lines(1:13,rep(0,13))

# eliminiamo l'ultimo mese:

matplot(counter_months[-13,],type="l")
which(counter_months[-13]==0)

for(i in 1:n)
  psr_month_zip_mean[month[i],zip[i]] = psr_month_zip_mean[month[i],zip[i]] + psr[i]

psr_mz_mean = psr_month_zip_mean/counter_months
which(psr_mz_mean[-13,] == 'NaN')

# correzione dei NaN attraverso osservazione appena prima precedente e post:
psr_mz_mean[-13,] == 'NaN' # psr_mz_mean[10,15], psr_mz_mean[8,25], psr_mz_mean[9,25]

psr_mz_mean[10,15] = (psr_mz_mean[9,15]+psr_mz_mean[11,15])/2
psr_mz_mean[8,25] = (2*psr_mz_mean[7,15]+psr_mz_mean[10,25])/3
psr_mz_mean[9,25] = (psr_mz_mean[7,15]+2*psr_mz_mean[10,25])/3

matplot(psr_mz_mean[-13,],type='l')
x11()

# cluster delle zip:

clust = kmeans(ll_meanzip, centers = 4 ,nstart=10)
clust$iter
clust$size
sum(clust$size)

names(clust)
clust$cluster

matplot(psr_mz_mean[-13,which(clust$cluster==1)],type = 'l')
matplot(psr_mz_mean[-13,which(clust$cluster==2)],type = 'l')
matplot(psr_mz_mean[-13,which(clust$cluster==3)],type = 'l')
matplot(psr_mz_mean[-13,which(clust$cluster==4)],type = 'l')

p_val = numeric()
B=1000
for(i in 1:3)
  for(j in (i+1):4){
    tot = rbind(t(psr_mz_mean[-13,which(clust$cluster==i)]),t(psr_mz_mean[-13,which(clust$cluster==j)]))
    n_1=clust$size[i]
    n_2=clust$size[j]
    nr = nrow(tot)
    meandiff=(colMeans(t(psr_mz_mean[-13,which(clust$cluster==i)]))-colMeans(t(psr_mz_mean[-13,which(clust$cluster==j)])))
    T0=sum(meandiff^2)
    T0_perm=numeric(B)
    for(perm in 1:B){
      permutazione <- sample(nr)
      tot_perm=tot[permutazione,]
      perm_1 = tot_perm[1:n_1,] 
      perm_2 = tot_perm[(n_1+1):nr,] 
      T0_perm[perm]=sum(((colMeans(perm_1)-colMeans(perm_2)))^2)
    }
    p_val = c(p_val,sum(T0_perm >= T0)/B)
  }

# no statistical evidence for saying SW-NE are different!
