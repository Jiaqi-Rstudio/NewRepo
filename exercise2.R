Keeling_Data <- read.csv(file = "co2_mm_mlo.csv", header = T)
colnames(Keeling_Data)
head(Keeling_Data)
tail(Keeling_Data)
head(Keeling_Data,3)
tail(Keeling_Data,4)
Keeling_Data$co2
min(Keeling_Data$co2)
max(Keeling_Data$co2)
range(Keeling_Data$co2)
mean(Keeling_Data$co2)
median(Keeling_Data$co2)
hist(Keeling_Data$co2)
summary(Keeling_Data$co2)
Keeling_Data$co2[1:10]
Keeling_Data$year[200:210]
Keeling_Data$co2[0]
plot(Keeling_Data$decimal_date, Keeling_Data$co2)
plot(Keeling_Data$decimal_date, Keeling_Data$co2, ylim=c(300,420), type="l")
Year     <- Keeling_Data$decimal_date
CO2_ppm  <- Keeling_Data$co2 
CO2_ppm[which(CO2_ppm == -99.99)]  <- NA
plot(Year, CO2_ppm, type="l")
mean(CO2_ppm)
mean(CO2_ppm, na.rm=T)
Keeling_Data$co2[1:10] * 1e3
Keeling_Data$month[1:10] + Keeling_Data$co2[1:10]
class(Keeling_Data)
vector_1 <- vector(length = 3)
vector_1
vector_2 <- vector(mode='character', length=3)
vector_2
str(vector_2)
str(Keeling_Data$co2)
vector_3 <- c(1,2,3)
vector_3
vector_4 <- c(1,2,3,"4")
vector_4
vector_5 <- c("a", TRUE)
vector_5
vector_6 <- c(0, TRUE)
vector_6
vector_7 <- c('0','2','4')
vector_7
str(vector_7)
vector_8 <- as.numeric(vector_7)
vector_8
str(vector_8)
vector_9 <- as.logical(vector_8)
vector_9
str(vector_9)
Keeling_Data$quality[1:10]
Keeling_Data$quality <- as.logical(Keeling_Data$quality)
Keeling_Data$quality[1:10]
length(Keeling_Data$quality)
x <- 1:26
x <- x * 2
names(x) <- LETTERS
x
str(Keeling_Data$month)
Keeling_Data$month <- factor(Keeling_Data$month)
class(Keeling_Data$month)
str(Keeling_Data$month)
typeof(Keeling_Data$month)
list_1 <- list(1, "a", TRUE, 1+4i)
list_1
list_2 <- list(title = "Numbers", numbers = 1:10, data = TRUE )
list_2
typeof(Keeling_Data)




typeof(Keeling_Data$co2)
vector1 <- vector(length = 3)
vector2 <- vector(mode="chaoracter", length=3)
str(Keeling_Data$month)
class(Keeling_Data$month)
factor(Keeling_Data$month)
list_1 <- list(1, "a", TRUE, 1+4i)
list_1



Keeling_Data[2]
Keeling_Data[[2]]
Keeling_Data$month
Keeling_Data["month"]
Keeling_Data[2, 2]
Keeling_Data[, 2]
Keeling_Data[2, ]


matrix <- matrix(1:50, ncol=5, nrow=10)
matrix




Airport_Data <- read.csv(file = "2281305.csv", header = T)
names(Airport_Data)
Airport_Data$TMP
typeof(Airport_Data$TMP)
BaoAn_T <- Airport_Data$TMP
head(BaoAn_T)
tail(BaoAn_T)
typeof(BaoAn_T)
Obs_Time <- Airport_Data$DATE
head(Obs_Time)
tail(Obs_Time)
typeof(Obs_Time)
BaoAn_T_value <- substr("BaoAn_T", 1, 5)
BaoAn_T_flag <- substr("BaoAn_T", 7,7)
BaoAn_T_value2 <- as.numeric(BaoAn_T_value)


#add a line
plot(x, y)

#add a new line
plot(a, b)
