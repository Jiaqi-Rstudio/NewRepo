permutations(4, 2, c("red", "green", "blue", "black"), set=TRUE, repeats.allowed = FALSE)
combinations(4, 2, c("red", "green", "blue", "black"), set=TRUE, repeats.allowed = FALSE)

# Define the function FindTop()
FindTop        <- function(N){
  MoviePage  <- readLines('https://movie.douban.com/top250', encoding='UTF-8')
  Pattarn    <- '<span class=\"title\">&nbsp;/&nbsp;([^<]*)</span>'
  Lines      <- grep(Pattarn, MoviePage, value=TRUE)
  Lines      <- sub(Pattarn,  '\\1', Lines )
  Names      <- sub('^ *', '', Lines)
  Names      <- sub('&#39;', "'", Names)
  print(Names[1:N])
}
# Call the function with arguement 5 and 10
FindTop(5)
FindTop(10)

# Define the function FindTop_new()
FindTop_new    <- function(N){
  MoviePage  <- readLines('https://movie.douban.com/top250', encoding='UTF-8')
  Pattarn    <- '<span class=\"title\">&nbsp;/&nbsp;([^<]*)</span>'
  Lines      <- grep(Pattarn, MoviePage, value=TRUE)
  Lines      <- sub(Pattarn,  '\\1', Lines )
  Names      <- sub('^ *', '', Lines)
  Names      <- sub('&#39;', "'", Names)
  return(Names[1:N])
}
FindTop_new(10)

MovieList <- as.vector(FindTop_new(10), mode= "any")


Keeling_Data <- read.csv(file = "co2_mm_mlo.csv", header = T)
Data_Year  <- Keeling_Data$year
Data_CO2   <- Keeling_Data$co2
Data_Month <- Keeling_Data$month

# Clean data
Data_CO2[which(Data_CO2 == -99.99)]  <- NA
Anual_mean <- c()
for ( i in Data_Year) {
  Anual <- Data_Year[which(Data_Year==i)]
  DATE_CO2A <- Data_CO2[which(Data_Year==i)]
  Anual_mean <- mean(DATE_CO2A)
}
CO2_annual_mean <- c()
for(iYear in unique(Data_Year)){
  # unique() function, returns unique elements of the vector
  ThisYear      <- which(Data_Year == iYear)
  ThisYear_Mean <- mean(Data_CO2[ThisYear], na.rm=T)
  CO2_annual_mean <- c(CO2_annual_mean, ThisYear_Mean)
}