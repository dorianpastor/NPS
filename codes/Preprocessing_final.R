rm(list=ls())
library(DepthProc)
library(aplpack)

DATA<-read.csv("../deprecated/kc_house_data.csv", header=TRUE, sep=",")
attach(DATA)

#################################################################
#Useful functions
#################################################################
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

# Richest Zipcodes in King's County
# source: https://www.zipdatamaps.com/economics/income/agi/metro/wealthiest-zipcodes-in-metro-seattle-tacoma
richest = c("98039","98040","98004","98112","98164","98134","98033","98075",
            "98074","98077","98101","98053","98110","98006","98199")
# Conversion sqft to sqm
sqft_to_sqm = 0.09290304

#################################################################
# VARIABLE CREATION
#################################################################

sqm_living = sqft_living*sqft_to_sqm
sqm_living = round(sqm_living)
sqm_above = sqft_above*sqft_to_sqm
sqm_above = round(sqm_above)
sqm_basement = sqft_basement*sqft_to_sqm
sqm_basement = round(sqm_basement)
sqm_lot = sqft_lot*sqft_to_sqm
sqm_lot = round(sqm_lot)

sqm_living15 = sqft_living15*sqft_to_sqm
sqm_living15 = round(sqm_living15)
sqm_lot15 = sqft_lot15*sqft_to_sqm
sqm_lot15 = round(sqm_lot15)

yr_old = 2015-yr_built
yr_lastrenov = 2015-yr_renovated
yr_lastrenov[which(yr_lastrenov==2015)] = NaN
renov_index = c(ifelse(yr_renovated==0,yr_old,yr_lastrenov))
left_date = numeric(dim(DATA)[1])
for (i in 1:dim(DATA)[1])
  left_date[i] = as.numeric(strsplit(toString(date[i]),"T")[[1]][1])
ordinal_date = numeric(21613)
for (i in 1:21613){
  ordinal_date[i] = ord_date(left_date[i])
}

geodist_index = geodist(lat,long)

bedfloors_ratio = bedrooms/floors
bathfloors_ratio = bathrooms/floors

#################################################################
# DATA WRANGLING
#################################################################
# Adding new variables

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
DATA$is_rich = as.integer(DATA$zipcode %in% richest)
DATA <- cbind(DATA,log10(price), bedfloors_ratio, bathfloors_ratio,log10(sqm_living),log10(sqm_lot),log10(sqm_living15),log10(sqm_lot15),geodist_index)

# Delete
DATA = subset(DATA, select=-c(sqft_above,sqft_basement,sqft_living,sqft_living15,sqft_lot,sqft_lot15)) # Drop useless columns


#################################################################
#OUTLIERS DETECTION
#################################################################
# Insert here all the column-couples to put in the bivariate bagplot
coll_columns = rbind(c("bedrooms","log10(price)"),c("bathrooms","log10(price)"),
                     c("bathfloors_ratio","log10(price)"),c("bedfloors_ratio","log10(price)"),
                     c("log10(sqm_living)","log10(price)"),
                     c("geodist_index","log10(price)"),c("log10(sqm_lot)","log10(price)"))
conta = 0  # Here I will count how many outliers we have
indexes = c()  # Here I will save all the indexes to discard
for (j in 1:dim(coll_columns)[1]){  # Iterate over all column-couples                
  columns = coll_columns[j,]
  print(columns)
  out = distinct(data.frame(bagplot(DATA[columns])$pxy.outlier))  # distinct to remove duplicates
  for (i in 1:dim(out)[1]){  # it can be optimized: loop to search for the outlier in the whole dataset
    row = out[i,]
    get_idx = which(DATA[columns[1]]==as.numeric(row["x"]) & DATA[columns[2]]==as.numeric(row["y"]))  # get the indexes where the outlier is found
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








