DATA = read.table('kc_house_data.csv', header=TRUE, sep=',')
attach(DATA)


df_ = subset(DATA, select=-c(id,date,price)) # Drop useless columns. 
mod0 = lm(price~., data=df_) # R2adj = 0.6995
summary(mod0)

# Constants
sqft_to_sqm = 0.09290304

# Crafting features
left_date = numeric(dim(DATA)[1])
for (i in 1:dim(DATA)[1])
  left_date[i] = as.numeric(strsplit(toString(date[i]),"T")[[1]][1])
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

# Adding new variables
# Not added for now: ordinal_date, bedr_bathr_ratio, log_bedbathratio, living_lot_ratio
# log_total_sqm, yr_last_ren (NAs... maybe categorical, renovated yes/no?)
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
DATA$geo_dist = Geo_dist_index
# Delete
DATA = subset(DATA, select=-c(yr_built,yr_renovated,sqft_above,sqft_basement,sqft_living,sqft_living15,sqft_lot,sqft_lot15)) # Drop useless columns

df = subset(DATA, select=-c(id,date,price)) # Drop useless columns.
mod = lm(price~., data=df) # R2adj = 0.7367
summary(mod)

write.table(df, "house_data_1511.txt", sep = " ")
