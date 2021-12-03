setwd("C:/Users/Messoanuovo.it/Desktop/Progetto NPS/House")
getwd()
DATA = read.table('kc_house_data.csv', header=TRUE, sep=',')
attach(DATA)

library(pastecs)
summary_df <- stat.desc(DATA)


model0 = lm(price~., data=DATA[,4:21])
# Modifica dataset/creazioni nuove variabili:
which(yr_built==2015)
which(yr_renovated==2015)

yr_old = 2015-yr_built
plot(yr_old) # le ultime case sono pi? giovani
yr_last_ren = 2015-yr_renovated
yr_last_ren[which(yr_last_ren==2015)] = NaN
plot(yr_last_ren) # pochissime tra le ultime sono state ristrutturate

sqft_to_sqm = 0.09290304

sqm_living = sqft_living*sqft_to_sqm
sqm_living = round(sqm_living)
plot(sqm_living) # gi? qua inizia a vedersi leggermente un comportamento anomalo delle ultime 2000 osservazioni circa.

sqm_lot = sqft_lot*sqft_to_sqm
sqm_lot = round(sqm_lot)
plot(sqm_lot) # continua comportamento anomalo, seppur meno evidente

sqm_above = sqft_above*sqft_to_sqm
sqm_above = round(sqm_above)
plot(sqm_above) # idem con patate

sqm_basement = sqft_basement*sqft_to_sqm
sqm_basement = round(sqm_basement)
plot(sqm_basement) # stesso discorso

sqm_living15 = sqft_living15*sqft_to_sqm
sqm_living15 = round(sqm_living15)
plot(sqm_living15) # qua no

sqm_lot15 = sqft_lot15*sqft_to_sqm
sqm_lot15 = round(sqm_lot15)
plot(sqm_lot15) # qua poco evidente

which(bedrooms==0) #876 3120 3468 4869 6995 8478 8485 9774 9855 12654 14424 18380 19453
which(bathrooms==0) #876 1150 3120 5833 6995 9774 9855 10482 14424 19453
which(bedrooms==0 & bathrooms==0) #876  3120  6995  9774  9855 14424 19453: li eliminiamo dal proje?
bedr_bathr_ratio = bedrooms/bathrooms # contains Nan and Inf -> allora skippiamo
plot(bedr_bathr_ratio) # evidente roba dell'ultime 2000 unit?
plot(bedrooms) # non presente
plot(bathrooms) # presente: sembrano avere pi? bagni le ultime 2000
log_bedbathratio = log10(bedr_bathr_ratio)
plot(log_bedbathratio) #chiaramente ancora pi? evidente
grade[which(bedrooms==0 & bathrooms==0)]
condition[which(bedrooms==0 & bathrooms==0)]

log_sqm_living = log10(sqm_living)
plot(log_sqm_living)

which(sqm_living==0)
which(sqm_lot==0)
living_lot_ratio = sqm_living/sqm_lot
plot(living_lot_ratio) #perch? pi? alto di 1 per alcuni? 
# molto pi? alto per le unit? ultime 

renovate_index = c(ifelse(yr_renovated==0,yr_old,yr_last_ren))
plot(renovate_index) # si vede come le ultime 2000 siano case giovani giovani

log_sqmliving15 = log10(sqm_living15)
plot(log_sqmliving15)

library(dplyr)
library(stringr)
#trial = strsplit(date[1], "T")
#trial[[1]][1]
#trial1= as.numeric(trial[[1]][1])
left_date = numeric(21613)
for (i in 1:21613)
  left_date[i] = as.numeric(strsplit(toString(date[i]),"T")[[1]][1])
plot(left_date)

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
  return(r)
}

#trial3 = ord_date(left_date[2])
ordinal_date = numeric(21613)
for (i in 1:21613){
  ordinal_date[i] = ord_date(left_date[i])
}
plot(ordinal_date)
min(left_date)
min(ordinal_date)
max(left_date)
max(ordinal_date)

log_total_sqm = log10(sqm_living+sqm_lot)
plot(log_total_sqm) # mediamente inferiore nelle ultime osservazioni

###GEODIST
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
Geo_dist_index = geodist(lat,long)
plot(Geo_dist_index) # non particolarmente rilevante per e ultime 2000 unit?

DATA = DATA[,1:21]
new_dataset = cbind(DATA,
                    bedr_bathr_ratio, # interesting
                    log_bedbathratio, # no sense
                    living_lot_ratio, # think about it
                    log_total_sqm,   # I would discard this but we need price per sqm to pop up somewhere(?)..
                    sqm_living,log_sqm_living, # we got all these and the following
                    sqm_lot,sqm_above,sqm_basement,
                    yr_old,yr_last_ren,renovate_index,
                    left_date,ordinal_date,Geo_dist_index,
                    sqm_living15,log_sqmliving15,sqm_lot15)

write.table(new_dataset, "house_data.txt", sep = " ")


model1 = lm(price~., data=new_dataset[,4:39])
summary(model0) # R2adj = 0.6995
summary(model1)
