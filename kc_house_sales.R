setwd("C:/Users/usube/Documents/UNI/Laurea Magistrale/Nonparametric Statistics/Nonparametric_project")

library(MASS)
library(rgl)
library(DepthProc)
library(gam)
library(plot3D)
library(lme4)
library(nlme)
library(hexbin)
library(packagefinder)
library(aplpack)
library(robustbase)

DATA<-read.csv("kc_house_data.csv", header=TRUE, sep=",")
n <- dim(DATA)[1]
attach(DATA)

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
yr_old = 2015-yr_built  #però alcune sono state vendute nel 2014 (?)
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
#trial = strsplit(date[1], "T")
#trial[[1]][1]
#trial1= as.numeric(trial[[1]][1])
left_date = numeric(21613)
for (i in 1:21613)
  left_date[i] = as.numeric(strsplit(date[i],"T")[[1]][1])

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

#trial3 = ord_date(left_date[2])
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


#OUTLIERS DETECTION

#Variabili da usare nell'outliers detection: log10(price), bedrooms, bathrooms, bathfloors_ratio (?), log10(sqm_living), log10(sqm_lot), log10(sqm_living15), geodist_index

DATA2 <- DATA
drop <- c("id","date","price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","grade","sqft_above","sqft_basement","yr_built","yr_renovated","zipcode","lat","long","sqft_living15","sqft_lot15")
DATA2 = DATA2[,!(names(DATA2) %in% drop)]
DATA2 <-  cbind(DATA2,log10(price),bedrooms,bathrooms,bathfloors_ratio,log10(sqm_living),log10(sqm_lot),log10(sqm_living15),log10(sqm_lot15),geodist_index)

x11()
aplpack::bagplot(DATA2[c("bedrooms","log10(price)")])
bagplot_cont_normals_bedrooms <- bagplot(DATA2[c("bedrooms","log10(price)")])
outlying_obs_bedrooms <- bagplot_cont_normals_bedrooms$pxy.outlier

x11()
aplpack::bagplot(DATA2[c("bathrooms","log10(price)")])
bagplot_cont_normals_bathrooms <- bagplot(DATA2[c("bathrooms","log10(price)")])
outlying_obs_bathrooms <- bagplot_cont_normals_bathrooms$pxy.outlier

x11()
aplpack::bagplot(DATA2[c("bathfloors_ratio","log10(price)")])
bagplot_cont_normals_bathfloorsratio <- bagplot(DATA2[c("bathfloors_ratio","log10(price)")])
outlying_obs_bathfloorsratio <- bagplot_cont_normals_bathfloorsratio$pxy.outlier

x11()
aplpack::bagplot(DATA2[c("log10(sqm_living)","log10(price)")])
bagplot_cont_normals_log10sqmliving <- bagplot(DATA2[c("log10(sqm_living)","log10(price)")])
outlying_obs_log10sqmliving <- bagplot_cont_normals_log10sqmliving$pxy.outlier

x11()
aplpack::bagplot(DATA2[c("log10(sqm_lot)","log10(price)")])
bagplot_cont_normals_log10sqmlot <- bagplot(DATA2[c("log10(sqm_lot)","log10(price)")])
outlying_obs_log10sqmlot <- bagplot_cont_normals_log10sqmlot$pxy.outlier

x11()
aplpack::bagplot(DATA2[c("log10(sqm_living15)","log10(price)")])
bagplot_cont_normals_log10sqmliving15 <- bagplot(DATA2[c("log10(sqm_living15)","log10(price)")])
outlying_obs_log10sqmliving15 <- bagplot_cont_normals_log10sqmliving15$pxy.outlier

x11()
aplpack::bagplot(DATA2[c("geodist_index","log10(price)")])
bagplot_cont_normals_geodistindex <- bagplot(DATA2[c("geodist_index","log10(price)")])
outlying_obs_geodistindex <- bagplot_cont_normals_geodistindex$pxy.outlier

#CREATION OF CLEANED DATASET
Y = price
LogY = log10(price)
Log_sqm_living = log10(sqm_living)
Log_sqm_lot = log10(sqm_lot) 
Log_sqm_living15 = log10(sqm_living15) 
Log_sqm_lot15 = log10(sqm_lot15) 
DATA3 <- DATA
drop <- c("id","date","price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","grade","sqft_above","sqft_basement","yr_built","yr_renovated","zipcode","lat","long","sqft_living15","sqft_lot15")
DATA3 = DATA3[,!(names(DATA3) %in% drop)]
DATA3 <-  cbind(DATA3,Y,LogY,bedrooms,bathrooms,floors,bedfloors_ratio,bathfloors_ratio,sqm_above,sqm_basement,sqm_living,Log_sqm_living,sqm_lot,Log_sqm_lot,sqm_living15,Log_sqm_living15,sqm_lot15,Log_sqm_lot15,yr_built,yr_renovated,renov_index,geodist_index,ordinal_date,waterfront,view,condition,grade)

#DATA_CLEAN <- dplyr::anti_join(x = DATA3, y = outlying_obs_bedrooms, by = c("bedrooms" = "x", "LogY" = "y"),copy = TRUE)
#DATA_CLEAN <- dplyr::anti_join(x = DATA_CLEAN, y = outlying_obs_bathrooms, by = c("bathrooms" = "x", "LogY" = "y"),copy = TRUE)
#DATA_CLEAN <- dplyr::anti_join(x = DATA_CLEAN, y = outlying_obs_bathfloorsratio, by = c("bathfloors_ratio" = "x", "LogY" = "y"),copy = TRUE)
#DATA_CLEAN <- dplyr::anti_join(x = DATA_CLEAN, y = outlying_obs_log10sqmliving, by = c("Log_sqm_living" = "x", "LogY" = "y"),copy = TRUE)
#DATA_CLEAN <- dplyr::anti_join(x = DATA_CLEAN, y = outlying_obs_log10sqmlot, by = c("Log_sqm_lot" = "x", "LogY" = "y"),copy = TRUE)
#DATA_CLEAN <- dplyr::anti_join(x = DATA_CLEAN, y = outlying_obs_log10sqmliving15, by = c("Log_sqm_living15" = "x", "LogY" = "y"),copy = TRUE)
#DATA_CLEAN <- dplyr::anti_join(x = DATA_CLEAN, y = outlying_obs_geodistindex, by = c("geodist_index" = "x", "LogY" = "y"),copy = TRUE)
#D <- DATA_CLEAN 

#ind_outlying_obs_bedrooms <- which(apply(DATA3[c("bedrooms","LogY")],1,function(x) all(x %in% outlying_obs_bedrooms)))
#df_clean <- DATA3[-ind_outlying_obs,]

detach(DATA)
#non uso più attach o non si capisce più un cazzo


#TESTING

#Gaussian LogY test

B<-1000
shapiro.pvalue<-numeric(B)
for (b in 1:B){
  Yobs<-sample(D$LogY,5000)
  shapiro.pvalue[b]<-shapiro.test(Yobs)$p.value
}
sum(shapiro.pvalue>=0.1)/B    
#refuse normality of LogY


#H0: C(D$Log_sqm_living) == C(D$Log_sqm_living15) vs H1: C(D$Log_sqm_living) != C(D$Log_sqm_living15)

x11()
par(mfrow=c(1,3))
plot(D$Log_sqm_living,main='occurencies in DATA') 
hist(D$Log_sqm_living,main = 'distribution of occurencies',prob=T)
boxplot(D$Log_sqm_living)

x11()
par(mfrow=c(1,3))
plot(D$Log_sqm_living15,main='occurencies in DATA') 
hist(D$Log_sqm_living15,main = 'distribution of occurencies',prob=T)
boxplot(D$Log_sqm_living15)

t1 <- cbind(D$Log_sqm_living)
t2 <- cbind(D$Log_sqm_living15)
p  <- dim(t1)[2]
n1 <- dim(t1)[1]
n2 <- dim(t2)[1]
n <- n1+n2
t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)
delta.0 <- 0
diff <- t1-t2
diff.mean <- colMeans(diff)

T20 <- as.numeric((diff.mean-delta.0)  %*% (diff.mean-delta.0))

T2 <- numeric(B)
for(perm in 1:B)
{
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  T2[perm] <- as.numeric((diff.mean_perm-delta.0) %*% (diff.mean_perm-delta.0))
}

x11()
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)
x11()
plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)
p_val <- sum(T2>=T20)/B
p_val #0.628
#we don't reject H0
#I can argue that the Log_sqm_living is equal to the Log_sqm_living15
#quindi le dimensioni della casa venduta sono uguali alle dimensioni medie delle case dei 15 vicini più vicini,
#per es. la casa venduta è in un insieme di villette a schiera o in un insieme di ville, non una villa con casette piccoline vicine


#H0: D_OBBR$LogY =d= D_RBNR$LogY) vs H1: D_OBBR$LogY =!d!= D_RBBR$LogY

D_OBBR <- D[which(D$yr_built <= 1990 & D$yr_renovated != 0),]
D_RBNR <- D[which(D$yr_built > 1990 & D$yr_renovated == 0),]

T0<-abs(mean(D_OBBR$LogY)-mean(D_RBNR$LogY))
DataPooled<-c(D_OBBR$LogY,D_RBNR$LogY)
n<-length(DataPooled)
B<-10000
tstat<-numeric(B)
for(perm in 1:B){
  permutation<-sample(1:n)
  x_perm<-DataPooled[permutation]
  x1_perm<-x_perm[1:length(D_OBBR$LogY)]
  x2_perm<-x_perm[(length(D_RBNR$LogY)+1):n]
  tstat[perm]<-abs(mean(x1_perm)-mean(x2_perm))
}

x11()
hist(tstat,xlim=range(c(tstat,T0)),breaks = 20)
abline(v=T0,col='red')
x11()
plot(ecdf(tstat))
pvalue<-sum(tstat>=T0)/B
pvalue #0
#we reject H0 
#we can say, with a level of confidence higher than 99%, that LogY in old built but renovated houses is significantly different from LogY in recently built not renovated houses


# verify effect of waterfront on LogY

x11()
boxplot(D$LogY ~ D$waterfront, col=rainbow(8),main='Effect of waterfront on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

watfront <- factor(D$waterfront, levels=c('0','1'))
i1 <- which(watfront=='1')
i2 <- which(watfront=='0')
n1 <- length(i1)
n2 <- length(i2)
n  <- n1+n2
g  <- length(levels(watfront))

fit <- aov(D$LogY ~ watfront)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D$LogY[permutation]
  fit_perm <- aov(LogY_perm ~ watfront)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor waterfront has significantly effect on LogY


#








