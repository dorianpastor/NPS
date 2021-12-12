#setwd("C:/Users/panze/Desktop/NONPARAM STATISTICS/PROGETTO")

library(MASS)
library(rgl)
library(DepthProc)
library(gam)
library(plot3D)
library(lme4)
library(nlme)
library(packagefinder)
library(aplpack)
library(robustbase)

DATA<-read.csv("kc_house_data.csv", header=TRUE, sep=",")
n <- dim(DATA)[1]
attach(DATA)


#################################################################
# EXPLORATORY DATA ANALYSIS
#################################################################
#PRICE EXPLORATION
price_mean = mean(price)
price_sd = sd(price)
x11()
par(mfrow=c(1,3))
plot(price,main='occurencies in DATA') 
abline(a=price_mean,b=0,col='green')
abline(a=price_mean+price_sd,b=0,col='red')
abline(a=price_mean-price_sd,b=0,col='red')
hist(price,main = 'distribution of occurencies',prob=T)
boxplot(price)
#very unbalanced distribution, try log10() transform

x11()
par(mfrow=c(1,3))
plot(log10(price),main='occurencies in DATA') 
hist(log10(price),main = 'distribution of occurencies',prob=T)
boxplot(log10(price))
#clearly better
#we actually decided to work on a log10 scale for price variable

#BEDROOMS EXPLORATION
bedrooms_mean = mean(bedrooms)
bedrooms_sd = sd(bedrooms)
x11()
par(mfrow=c(1,3))
plot(bedrooms,main='occurencies in DATA') 
abline(a=bedrooms_mean,b=0,col='green')
abline(a=bedrooms_mean+bedrooms_sd,b=0,col='red')
abline(a=bedrooms_mean-bedrooms_sd,b=0,col='red')
hist(bedrooms,main = 'distribution of occurencies',prob=T)
boxplot(bedrooms)

#clearly presence of an outlier
which(bedrooms>30) #15871
#presence of a group of outliers per bedrooms>=8 
which(bedrooms>=8) #1661  4036  4068  4097  4236  6080  6175  8547  8758  9078 9453 10959 12886 13315 15071 15162 15671 15871 16845 17236 18444 18478 19255 19303
#presence of a group of outliers per bedrooms=0
which(bedrooms==0) #876  3120  3468  4869  6995  8478  8485  9774  9855 12654 14424 18380 19453

#BATHROOMS EXPLORATION
bathrooms_mean = mean(bathrooms)
bathrooms_sd = sd(bathrooms)
x11()
par(mfrow=c(1,3))
plot(bathrooms,main='occurencies in DATA') 
abline(a=bathrooms_mean,b=0,col='green')
abline(a=bathrooms_mean+bathrooms_sd,b=0,col='red')
abline(a=bathrooms_mean-bathrooms_sd,b=0,col='red')
hist(bathrooms,main = 'distribution of occurencies',prob=T)
boxplot(bathrooms)

#presence of a group of outliers per bathrooms>=6
which(bathrooms>=6) #1316  4025  4036  7253  8093  8547  9255 12371 12778 14557 18303 19149 20579 21051 21345 21507

#FLOORS EXPLORATION
floors_mean = mean(floors)
floors_sd = sd(floors)
x11()
par(mfrow=c(1,3))
plot(floors,main='occurencies in DATA') 
abline(a=floors_mean,b=0,col='green')
abline(a=floors_mean+floors_sd,b=0,col='red')
abline(a=floors_mean-floors_sd,b=0,col='red')
hist(floors,main = 'distribution of occurencies',prob=T)
boxplot(floors)
#it appears completely useless

#BEDFLOORS_RATIO EXPLORATION
bedfloors_ratio = bedrooms/floors
bedfloors_ratio_mean = mean(bedfloors_ratio)
bedfloors_ratio_sd = sd(bedfloors_ratio)
x11()
par(mfrow=c(1,3))
plot(bedfloors_ratio,main='occurencies in DATA') 
abline(a=bedfloors_ratio_mean,b=0,col='green')
abline(a=bedfloors_ratio_mean+bedfloors_ratio_sd,b=0,col='red')
abline(a=bedfloors_ratio_mean-bedfloors_ratio_sd,b=0,col='red')
hist(bedfloors_ratio,main = 'distribution of occurencies',prob=T)
boxplot(bedfloors_ratio)

#clearly presence of an outlier
which(bedfloors_ratio>30) #15871 lo stesso di bedrooms
#presence of a group of outliers per bedfloors_ratio>=8 
which(bedfloors_ratio>=8) #4068  6175  9453 10959 13315 15871 19303 tutti contenuti nel gruppo di outliers di bedrooms
#presence of a group of outliers per bedfloors_ratio=0
which(bedfloors_ratio==0) #876  3120  3468  4869  6995  8478  8485  9774  9855 12654 14424 18380 19453 tutti gli stessi di quelli di bedrooms

#quindi nell'outliers detection uso solo una variabile tra bedrooms e bedfloors_ratio 

#BATHFLOORS_RATIO EXPLORATION
bathfloors_ratio = bathrooms/floors
bathfloors_ratio_mean = mean(bathfloors_ratio)
bathfloors_ratio_sd = sd(bathfloors_ratio)
x11()
par(mfrow=c(1,3))
plot(bathfloors_ratio,main='occurencies in DATA') 
abline(a=bathfloors_ratio_mean,b=0,col='green')
abline(a=bathfloors_ratio_mean+bathfloors_ratio_sd,b=0,col='red')
abline(a=bathfloors_ratio_mean-bathfloors_ratio_sd,b=0,col='red')
hist(bathfloors_ratio,main = 'distribution of occurencies',prob=T)
boxplot(bathfloors_ratio)

#presence of a group of outliers per bathfloors_ratio>=5
which(bathfloors_ratio>=5) #301  1922  2237  3021  3101  7036  8093 13315 15829 diversi da quelli di bathrooms

#quindi nell'outliers detection uso entrambe le variabili bathrooms e bathfloors_ratio (?)

#SQM_LIVING EXPLORATION
sqft_to_sqm = 0.09290304
sqm_living = sqft_living*sqft_to_sqm
sqm_living = round(sqm_living)

sqm_living_mean = mean(sqm_living)
sqm_living_sd = sd(sqm_living)
x11()
par(mfrow=c(1,3))
plot(sqm_living,main='occurencies in DATA') 
abline(a=sqm_living_mean,b=0,col='green')
abline(a=sqm_living_mean+sqm_living_sd,b=0,col='red')
abline(a=sqm_living_mean-sqm_living_sd,b=0,col='red')
hist(sqm_living,main = 'distribution of occurencies',prob=T)
boxplot(sqm_living)
#unbalanced distribution, try log10() transform

x11()
par(mfrow=c(1,3))
plot(log10(sqm_living),main='occurencies in DATA') 
hist(log10(sqm_living),main = 'distribution of occurencies',prob=T)
boxplot(log10(sqm_living))
#it seems better

#presence of a group of outliers per log10(sqm_living)>=2.9
which(log10(sqm_living)>=2.9) #3915  4412  7253  8093  9255 12778 14557
#presence of a group of outliers per log10(sqm_living)<=1.6
which(log10(sqm_living)<=1.6) #861  4869  8624 11501 14467 15382 18380 19453 21333

#SQM_ABOVE EXPLORATION
sqm_above = sqft_above*sqft_to_sqm
sqm_above = round(sqm_above)

sqm_above_mean = mean(sqm_above)
sqm_above_sd = sd(sqm_above)
x11()
par(mfrow=c(1,3))
plot(sqm_above,main='occurencies in DATA') 
abline(a=sqm_above_mean,b=0,col='green')
abline(a=sqm_above_mean+sqm_above_sd,b=0,col='red')
abline(a=sqm_above_mean-sqm_above_sd,b=0,col='red')
hist(sqm_above,main = 'distribution of occurencies',prob=T)
boxplot(sqm_above)
#unbalanced distribution, try log10() transform

x11()
par(mfrow=c(1,3))
plot(log10(sqm_above),main='occurencies in DATA') 
hist(log10(sqm_above),main = 'distribution of occurencies',prob=T)
boxplot(log10(sqm_above))
#it seems better

#log10(sqm_above) comportamento quasi identico a log10(sqm_living) 

#SQM_BASEMENT EXPLORATION
sqm_basement = sqft_basement*sqft_to_sqm
sqm_basement = round(sqm_basement)

sqm_basement_mean = mean(sqm_basement)
sqm_basement_sd = sd(sqm_basement)
x11()
par(mfrow=c(1,3))
plot(sqm_basement,main='occurencies in DATA') 
abline(a=sqm_basement_mean,b=0,col='green')
abline(a=sqm_basement_mean+sqm_basement_sd,b=0,col='red')
abline(a=sqm_basement_mean-sqm_basement_sd,b=0,col='red')
hist(sqm_basement,main = 'distribution of occurencies',prob=T)
boxplot(sqm_basement)
#sqm_basement comportamento brutto (troppi zeri)

#sqm_living = sqm_above + sqm_basement
#quindi tengo solo log10(sqm_living) nell'outliers detection

#SQM_LOT EXPLORATION
sqm_lot = sqft_lot*sqft_to_sqm
sqm_lot = round(sqm_lot)

sqm_lot_mean = mean(sqm_lot)
sqm_lot_sd = sd(sqm_lot)
x11()
par(mfrow=c(1,3))
plot(sqm_lot,main='occurencies in DATA') 
abline(a=sqm_lot_mean,b=0,col='green')
abline(a=sqm_lot_mean+sqm_lot_sd,b=0,col='red')
abline(a=sqm_lot_mean-sqm_lot_sd,b=0,col='red')
hist(sqm_lot,main = 'distribution of occurencies',prob=T)
boxplot(sqm_lot)
#very unbalanced distribution, try log10() transform

x11()
par(mfrow=c(1,3))
plot(log10(sqm_lot),main='occurencies in DATA') 
hist(log10(sqm_lot),main = 'distribution of occurencies',prob=T)
boxplot(log10(sqm_lot))
#clearly better

#presence of a group of outliers per log10(sqm_lot)>=4.7
which(log10(sqm_lot)>=4.7) #1720  3950  4442  4541  6692  7078  7295  7648  7770  9715 13007 13478 16189 17320 17826 20453

#TOTAL_SQM EXPLORATION
total_sqm = sqm_living+sqm_lot

total_sqm_mean = mean(total_sqm)
total_sqm_sd = sd(total_sqm)
x11()
par(mfrow=c(1,3))
plot(total_sqm,main='occurencies in DATA') 
abline(a=total_sqm_mean,b=0,col='green')
abline(a=total_sqm_mean+total_sqm_sd,b=0,col='red')
abline(a=total_sqm_mean-total_sqm_sd,b=0,col='red')
hist(total_sqm,main = 'distribution of occurencies',prob=T)
boxplot(total_sqm)
#very unbalanced distribution, try log10() transform

x11()
par(mfrow=c(1,3))
plot(log10(total_sqm),main='occurencies in DATA') 
hist(log10(total_sqm),main = 'distribution of occurencies',prob=T)
boxplot(log10(total_sqm))
#clearly better

#log10(total_sqm) comportamento quasi identico a log10(sqm_lot) 

#presence of a group of outliers per log10(total_sqm)>=4.7
which(log10(total_sqm)>=4.7) #1720  3950  4442  4541  6692  7078  7295  7648  7770  9715 13007 13478 16189 17320 17826 20453 gli stessi identici di log10(sqm_lot)

#quindi tengo log10(sqm_living) e log10(sqm_lot) nell'outliers detection, mentre non considero log10(total_sqm)

#SQM_LIVING15 EXPLORATION
sqm_living15 = sqft_living15*sqft_to_sqm
sqm_living15 = round(sqm_living15)

sqm_living15_mean = mean(sqm_living15)
sqm_living15_sd = sd(sqm_living15)
x11()
par(mfrow=c(1,3))
plot(sqm_living15,main='occurencies in DATA') 
abline(a=sqm_living15_mean,b=0,col='green')
abline(a=sqm_living15_mean+sqm_living15_sd,b=0,col='red')
abline(a=sqm_living15_mean-sqm_living15_sd,b=0,col='red')
hist(sqm_living15,main = 'distribution of occurencies',prob=T)
boxplot(sqm_living15)
#a bit unbalanced distribution, try log10() transform

x11()
par(mfrow=c(1,3))
plot(log10(sqm_living15),main='occurencies in DATA') 
hist(log10(sqm_living15),main = 'distribution of occurencies',prob=T)
boxplot(log10(sqm_living15))
#it seems better

#presence of a group of outliers per log10(sqm_living15)>2.7
which(log10(sqm_living15)>2.7) #1124  1531  5452 10374 11872 12714 16431 19859 20564 20831 21541
#presence of a group of outliers per log10(sqm_living15)<1.8
which(log10(sqm_living15)<1.7) #12107 13442 17287

#SQM_LOT15 EXPLORATION
sqm_lot15 = sqft_lot15*sqft_to_sqm
sqm_lot15 = round(sqm_lot15)

sqm_lot15_mean = mean(sqm_lot15)
sqm_lot15_sd = sd(sqm_lot15)
x11()
par(mfrow=c(1,3))
plot(sqm_lot15,main='occurencies in DATA') 
abline(a=sqm_lot15_mean,b=0,col='green')
abline(a=sqm_lot15_mean+sqm_lot15_sd,b=0,col='red')
abline(a=sqm_lot15_mean-sqm_lot15_sd,b=0,col='red')
hist(sqm_lot15,main = 'distribution of occurencies',prob=T)
boxplot(sqm_lot15)
#very unbalanced distribution, try log10() transform

x11()
par(mfrow=c(1,3))
plot(log10(sqm_lot15),main='occurencies in DATA') 
hist(log10(sqm_lot15),main = 'distribution of occurencies',prob=T)
boxplot(log10(sqm_lot15))
#clearly better

#log10(sqm_lot15) comportamento quasi identico a log10(sqm_lot) 

#presence of a group of outliers per log10(sqm_lot15)>4.5
which(log10(sqm_lot15)>4.5) #1720  3802  6692  7295  8665  9715 11184 11565 13465 15621 17660 19157 20453 21432 molti sono gli stessi di log10(sqm_lot)

#TOTAL_SQM15 EXPLORATION
total_sqm15 = sqm_living15+sqm_lot15

total_sqm15_mean = mean(total_sqm15)
total_sqm15_sd = sd(total_sqm15)
x11()
par(mfrow=c(1,3))
plot(total_sqm15,main='occurencies in DATA') 
abline(a=total_sqm15_mean,b=0,col='green')
abline(a=total_sqm15_mean+total_sqm15_sd,b=0,col='red')
abline(a=total_sqm15_mean-total_sqm15_sd,b=0,col='red')
hist(total_sqm15,main = 'distribution of occurencies',prob=T)
boxplot(total_sqm15)
#very unbalanced distribution, try log10() transform

x11()
par(mfrow=c(1,3))
plot(log10(total_sqm15),main='occurencies in DATA') 
hist(log10(total_sqm15),main = 'distribution of occurencies',prob=T)
boxplot(log10(total_sqm15))
#clearly better

#log10(total_sqm15) comportamento quasi identico a log10(sqm_lot15) 

#presence of a group of outliers per log10(total_sqm15)>4.5
which(log10(total_sqm15)>4.5) #1720  3537  3802  6692  7295  8665  9715 11184 11565 13465 15621 17660 19157 20453 21432 gli stessi identici di log10(sqm_lot15)

#quindi tengo log10(sqm_living15) nell'outliers detection, mentre non considero log10(sqm_lot15) e log10(total_sqm15)

#RENOV_INDEX EXPLORATION
yr_old = 2015-yr_built  #per? alcune sono state vendute nel 2014 (?)
yr_lastrenov = 2015-yr_renovated
yr_lastrenov[which(yr_lastrenov==2015)] = NaN

renov_index = c(ifelse(yr_renovated==0,yr_old,yr_lastrenov))

renov_index_mean = mean(renov_index)
renov_index_sd = sd(renov_index)
x11()
par(mfrow=c(1,3))
plot(renov_index,main='occurencies in DATA') 
abline(a=renov_index_mean,b=0,col='green')
abline(a=renov_index_mean+renov_index_sd,b=0,col='red')
abline(a=renov_index_mean-renov_index_sd,b=0,col='red')
hist(renov_index,main = 'distribution of occurencies',prob=T)
boxplot(renov_index)
#it appears completely useless

#GEODIST_INDEX EXPLORATION 
geodist <- function(Lat,Lon){
  lats <- 47.62617*pi/180
  lons <- -122.33825*pi/180
  R <- 6371
  lat <- Lat*pi/180
  lon <- Lon*pi/180
  fi <- abs(lon-lons)
  p <- acos(sin(lat)*sin(lats)+cos(lat)*cos(lats)*cos(fi))
  d <- p*R
  return(d)
}
geodist_index = geodist(lat,long)

geodist_index_mean = mean(geodist_index)
geodist_index_sd = sd(geodist_index)
x11()
par(mfrow=c(1,3))
plot(geodist_index,main='occurencies in DATA') 
abline(a=geodist_index_mean,b=0,col='green')
abline(a=geodist_index_mean+geodist_index_sd,b=0,col='red')
abline(a=geodist_index_mean-geodist_index_sd,b=0,col='red')
hist(geodist_index,main = 'distribution of occurencies',prob=T)
boxplot(geodist_index)

#presence of a group of outliers per geodist_index>=60
which(geodist_index>=60) #2590  2928  3296  4204  4849  5868  6090 10096 10899 13073 13250 13995 14616 16843 16942 19651 19981 21387

#ORDINAL_DATE EXPLORATION
library(dplyr)
library(stringr)

left_date = numeric(dim(DATA)[1])
for (i in 1:dim(DATA)[1])
  left_date[i] = as.numeric(strsplit(toString(date[i]),"T")[[1]][1])

ord_date <- function(d){
  if(d<20150000){ #siamo nel 2014
    if(d<20140600){ # siamo a maggio
      r = d-20140500
    }
    else if(d<20140700){ #siamo a giugno
      r = d-20140600 + 31
    }
    else if(d<20140800){ #siamo a luglio
      r = d-20140700 + 61
    }
    else if(d<20140900){ #siamo a agosto
      r = d-20140800 + 92
    }
    else if(d<20141000){ #siamo a settembre
      r = d-20140900 + 123
    }
    else if(d<20141100){ #siamo a ottobre
      r = d-20141000 + 153
    }
    else if(d<20141200){ #siamo a novembre
      r = d-20141100 + 184
    }
    else if(d<20141300){ #siamo a dicembre
      r = d-20141200 + 214
    }
  }
  else{ #siamo nel 2015
    if(d<20150200){ # siamo a gennaio
      r = d-20150100 + 245
    }
    else if(d<20150300){ #siamo a febbraio
      r = d-20150200 + 276
    }
    else if(d<20150400){ #siamo a marzo
      r = d-20150300 + 304
    }
    else if(d<20150500){ #siamo a aprile
      r = d-20150400 + 335
    }
    else if(d<20150600){ #siamo a maggio
      r = d-20150500 + 365
    }
  }
}

ordinal_date = numeric(21613)
for (i in 1:21613){
  ordinal_date[i] = ord_date(left_date[i])
}
min(left_date)
min(ordinal_date)
max(left_date)
max(ordinal_date)

ordinal_date_mean = mean(ordinal_date)
ordinal_date_sd = sd(ordinal_date)
x11()
par(mfrow=c(1,3))
plot(ordinal_date,main='occurencies in DATA') 
abline(a=ordinal_date_mean,b=0,col='green')
abline(a=ordinal_date_mean+ordinal_date_sd,b=0,col='red')
abline(a=ordinal_date_mean-ordinal_date_sd,b=0,col='red')
hist(ordinal_date,main = 'distribution of occurencies',prob=T)
boxplot(ordinal_date)
#it appears completely useless



#################################################################
# DATA WRANGLING
#################################################################
# Adding new variables
# Not added for now, but to consider: bedr_bathr_ratio, living_lot_ratio, log_total_sqm

# Replace
DATA$sqm_living = round(sqft_living*sqft_to_sqm)
DATA$sqm_lot = round(sqft_lot*sqft_to_sqm)
DATA$sqm_above = round(sqft_above*sqft_to_sqm)
DATA$sqm_basement = round(sqft_basement*sqft_to_sqm)
DATA$sqm_living15 = round(sqft_living15*sqft_to_sqm)
DATA$sqm_lot15 = round(sqft_lot15*sqft_to_sqm)
DATA$date = left_date
# Add
DATA$yr_old = 2015-yr_built
DATA$renovate_index =  c(ifelse(yr_renovated==0,DATA$yr_old,2015-yr_renovated))
DATA$has_ren = as.integer(as.logical(yr_renovated!=0))  # If it was renovated
DATA$has_bas = as.integer(as.logical(sqft_basement!=0))  # If it has basement, since many don't have it
DATA$ord_date = ordinal_date  # To take into account the market evolution

DATA <- cbind(DATA,log10(price), bedfloors_ratio, bathfloors_ratio,log10(sqm_living),log10(sqm_lot),log10(sqm_living15),log10(sqm_lot15),geodist_index)
# Delete
DATA = subset(DATA, select=-c(sqft_above,sqft_basement,sqft_living,sqft_living15,sqft_lot,sqft_lot15)) # Drop useless columns


#################################################################
#OUTLIERS DETECTION
#################################################################
#Variabili da usare nell'outliers detection: log10(price), bedrooms, bathrooms, 
# bathfloors_ratio, bedfloors_ratio, log10(sqm_living), log10(sqm_lot), geodist_index
# More to try..

# Insert here all the column-couples to put in the bivariate bagplot
coll_columns = rbind(c("bedrooms","log10(price)"),c("bathrooms","log10(price)"),
                     c("bedfloors_ratio","log10(price)"), c("bathfloors_ratio","log10(price)"),
                     c("log10(sqm_living)","log10(price)"),c("log10(sqm_lot)","log10(price)"),
                     c("geodist_index","log10(price)"))
conta = 0  # Here I will count how many outliers we have
indexes = c()  # Here I will save all the indexes to discard
for (j in 1:dim(coll_columns)[1]){  # Iterate over all column-couples                
  columns = coll_columns[j,]
  print(columns)
  out = distinct(data.frame(bagplot(DATA[columns])$pxy.outlier))  # distinct to remove duplicates
  for (i in 1:dim(out)[1]){  # it can be optimized: loop to search for the outlier in the whole dataset
    row = out[i,]
    get_idx = which(DATA[columns[1]]==as.integer(row["x"]) & DATA[columns[2]]==as.numeric(row["y"]))  # get the indexes where the outlier is found
    count_indexes = length(indexes)
    indexes = union(indexes,get_idx)
    conta = conta + length(indexes) - count_indexes
  }
  print(conta)
}
conta
indexes

df_clean <- DATA[-indexes,]
write.csv(df_clean, "kc_cleaned.csv", row.names = FALSE)








