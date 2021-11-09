# Setup
set.seed(911)
setwd("C:\\Users\\Enrico\\Desktop\\IngMTM\\NPS\\Wine quality project\\")
file = "winequality.txt"
wines = read.table(file, header = TRUE, sep = "", dec = ".")
quality = wines$quality
hist(quality)  # How quality looks like

x = wines[,1:11]
model = lm(quality~., data=x) # Original regression Rsquared is 0.29
summary(model)

# Price generation
const = 5  # Base cost of wine. Set higher for less noise (in proportion) and higher price
multiplier = 6  # To compute the price. Set higher for less noise (in proportion) and higher price
k = 2  # To compute the mean of the exponential. Set higher for less noise and smaller price
threshold = 7  # Threshold for high-quality
jittered_quality = jitter(quality)  # quality jittering (it's a Gaussian noise).
noise = rexp(n = length(quality), rate = k/sd(quality)) # Exponential noise with mean: sd(quality)/k
hist(noise)  # How noise looks like
price_base = const+multiplier*log(jittered_quality)  # price after transformation but before exp noise.
# It features a small logNormal noise
price = round(price_base + noise, 2)  # Final price, rounded

# Visualization
x11()
hist(price, breaks=40)  # How final price looks like
# x$q = quality>=threshold #  uncomment to add quality as regressor
model = lm(price~., data=x)
summary(model)  # Rsquared is 0.23. Objective: obtain>0.29 with non-parametric methods. 
# The meaningful variables are more or less the same as lm
dev.off()

# Output
file = "winequality0911.txt"
wines$price = price
write.table(wines, file = file, sep = " ", row.names = FALSE)