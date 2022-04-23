library(ggplot2)

#setwd("~/Desktop/Prigramming in R/Project")
df <- read.csv("SSDSE-B-2021.csv", header=TRUE, row.names = NULL, stringsAsFactors=FALSE, fileEncoding = "shift-jis")
print(df)

#extraction based on columnList
columnList <- c("SSDSE.B.2021", "Code", "Prefecture", "A1101", "A1301", "E6502") #make columnList
data.selected <- df[, columnList] #access data
data.selected <- data.selected[-1,]
colnames(data.selected) <- c("year", "code of prefecture", "prefecture", "total population", "population younger than 15 yesrs old", "popularion graduated from bachelor")
print(data.selected)

#make a plot of number of people who graduate from the bachelor by year
years <- unique(data.selected$year)
total_people_with_bachelor = data.frame(matrix(ncol = 1, nrow = length(years)))
rownames(total_people_with_bachelor) <- years

count <- 0
for (i in years) {
  data_year <- data.selected[data.selected$year == i, ]
  print(data_year)
  #total_people_with_bachelor[count + 1 , 1] <- i
  total_people_with_bachelor[count + 1, 1] <- sum(as.numeric(data_year[, "popularion graduated from bachelor"]))
  count <- count + 1
  print(count)
}    
barplot(total_people_with_bachelor[,1], main = "Numbers of those who graduated from the bachelor")

#Summary of Statistics

data.selected