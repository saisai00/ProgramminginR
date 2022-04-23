setwd("~/Desktop/Prigramming in R/Project")
df <- read.csv("SSDSE-B-2021.csv", header=TRUE, row.names = NULL, stringsAsFactors=FALSE, fileEncoding = "shift-jis")
options(digits=2) #change digit
print(df)

#extraction based on columnList
columnList <- c("SSDSE.B.2021", "Code", "Prefecture", "A1101", "A1301", "E6502") #make columnList
data.selected <- df[, columnList] #access data
data.selected <- data.selected[-1,]
colnames(data.selected) <- c("year", "code of prefecture", "prefecture", "total population", "population younger than 15 yesrs old", "popularion guraduated from bachelor")
#columnListtoNumeric <- c("total population", "population younger than 15 yesrs old", "popularion guraduated from bachelor")
#as.numeric(data.selected[, columnListtoNumeric])
print(data.selected)

#make a plot of number of people who graduate from the bachelor by year
years <- unique(data.selected$year)
total_people_with_bachelor <- data.frame(matrix(rep(NA, 2), nrow=1))[numeric(0), ]
colnames(total_people_with_bachelor) <- c("year", "total_people_with_bachelor")

#is.vector(years)
count <- 0
for (i in years) {
  data_year <- data.selected.total[data.selected$year == i, ]
  print(data_year)
  total_people_with_bachelor[count + 1 , 1] <- i
  total_people_with_bachelor[count + 1 , 2] <- sum(as.numeric(data_year[, "popularion guraduated from bachelor"]))
  count <- count + 1
  print(count)
  }    


print(total_people_with_bachelor)

barplot(total_people_with_bachelor[, 2])

#summary of statistics


