
#set directory
setwd("/Users/wanyingzhao/Desktop/CS 544/Final Project")
dir()

#Read CSV into R
ChocolateBars <- read.csv(file = "cacao.csv", header=TRUE)
ChocolateBars
View(ChocolateBars)

ChocolateBars$Cocoa.Percent <- as.numeric(gsub("[%]", "", ChocolateBars$Cocoa.Percent))

# check NA data
is.na(ChocolateBars)

# subsetting data
ChocolateBars$REF <- NULL
#cocoa <- na.omit(Chocolate)
cocoa <- ChocolateBars


View(cocoa)
summary(cocoa)

#save the data into R project
save(cocoa, file = "cocoa.RData")

##########data analysis for categorical and numerical data################
#### country ####
table.country <- table(cocoa$Company.Location)
table.country
head(sort(table.country,decreasing=TRUE), n = 10)

# frequency of countries
country.frame <- as.data.frame(table.country)
colnames(country.frame) <- c("table.country", "NumberOfBars")

#descending order
country.frame[order(-country.frame$NumberOfBars),]

# barplot for countries
country.plot <- barplot(head(sort(table.country, decreasing = TRUE), 10), 
                      main="Bar chart of top 10 countries", xaxt = "n", 
                 col = "blue", ylim = c(0,800), ylab = "number of bars")

labs <- names(head(sort(table.country, decreasing = TRUE), 10))
text(country.plot, par()$usr[3], labels = labs, srt = 75, 
     adj = c(1.1,1.1), xpd = TRUE, cex=.9)

####Company####

company <- table(cocoa$Company..Maker.if.known.)
company
# frequency of companies
company.df <- as.data.frame(company)
colnames(company.df) <- c("company","ratingFrequency")

#descending order
company.df[order(-company.df$ratingFrequency),]
head(sort(company, decreasing = TRUE), 10)

# barplot for company
company.plot <- barplot(head(sort(company, decreasing = TRUE), 10), 
          main = "Number of ratings in top companies", xaxt = "n",
          ylim = c(0,50), ylab = "Rating Frequency")
labs <- names(head(sort(company, decreasing = TRUE), 10))
text(company.plot, par()$usr[3], labels = labs, srt = 45, 
     adj = c(1.1,1.1), xpd = TRUE, cex=.9)

####Bean Type####

bean <- table(cocoa$Bean.Type)
bean
# frequency of bean typy
bean.df <- as.data.frame(bean)
colnames(bean.df) <- c("bean","NumbersOfRating")

#descending order
bean.df[order(-bean.df$NumbersOfRating),]
head(sort(bean, decreasing = TRUE), 10)

# barplot for bean type
bean.plot <- barplot(head(sort(bean, decreasing = TRUE), 10),  
         main = "Top 10 Bean Type", col = "red", xaxt = "n", 
         ylim = c(0,650), ylab = "Rating Frequency")
labs <- names(head(sort(bean, decreasing = TRUE), 10))
text(bean.plot, par()$usr[3], labels = labs, srt = 45, 
     adj = c(1.1,1.1), xpd = TRUE, cex=.9)

####Origin####

origin <- table(cocoa$Specific.Bean.Origin.or.Bar.Name)
origin
# frequency of origin
origin.df <- as.data.frame(origin)
colnames(origin.df) <- c("origin","NumbersOfBars")

#descending order
origin.df[order(-origin.df$NumbersOfBars),]
head(sort(origin, decreasing = TRUE), 10)

# barplot for origin
origin.plot <- barplot(head(sort(origin, decreasing = TRUE), 10),  
     main = "Top 10 Regions to Produce Cocoa", col = "chocolate", 
     xaxt = "n", ylim = c(0,60), ylab = "Number of Bars")
labs <- names(head(sort(origin, decreasing = TRUE), 10))
text(origin.plot, par()$usr[3], labels = labs, srt = 45, 
     adj = c(1.1,1.1), xpd = TRUE, cex=.9)

####Rating####

rating <- table(cocoa$Rating)
rating
# barplot for rating
rating.plot <- barplot(head(sort(rating, decreasing = TRUE), 11), 
      main="Bar Chart of Rating", col = "cyan", ylim = c(0,400), 
      xlab = "Rating", ylab = "Count")
#five numbers
a <- fivenum(cocoa$Rating)
a
# box plot for rating
boxplot(cocoa$Rating, horizontal = TRUE, xaxt = "n",
                      col = "cyan", main = "Rating")
axis(side = 1, at = fivenum(cocoa$Rating), labels = TRUE)
# outliers
c(a[2] - 1.5*(a[4] - a[2]),
  a[4] + 1.5*(a[4] - a[2]))
# average rating
pop.mean <- mean(cocoa$Rating)
pop.mean

####CocoaPercent####
percent <- table(cocoa$Cocoa.Percent)
percent

# barplot for CocoaPercent
percent.plot <- barplot(sort(percent, decreasing = TRUE), 
      main="Bar Chart of Cocoa Percent", col = "pink", 
      ylim = c(0,700), xlab = "Rating", ylab = "Count")

# average CocoaPercent
mean(cocoa$Cocoa.Percent)

##########data distribution of Coccoa Percent################
p <- hist(cocoa$Cocoa.Percent, ylim = c(0,800),
          main="Histogram of Cocoa Percent", 
          xlab="Cocoa Percent", col="blue", breaks=20)
p$breaks
p$counts

xfit<-seq(min(cocoa$Cocoa.Percent),max(cocoa$Cocoa.Percent),length=40) 
yfit<-dnorm(xfit,mean=mean(cocoa$Cocoa.Percent),sd=sd(cocoa$Cocoa.Percent)) 
yfit <- yfit*diff(p$mids[1:2])*length(cocoa$Cocoa.Percent) 
lines(xfit, yfit, col="red", lwd=2)

##########Draw	various	random	samples	of	the	data & the	Central	Limit	Theorem	for	this	variable##############
####Draw random sample of cocoa percent####

par(mfrow = c(2,2))
set.seed(150)
samples <- 1000
sample.size <- c(10, 20, 30, 40)

xbar <- numeric(samples)

for (size in sample.size) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(cocoa$Cocoa.Percent, size = size))
    
  }
  
  hist(xbar, prob = TRUE, 
       breaks = 15, col = "#53869B", ylim = c(0,0.5),
       main = paste("Sample Size =", size))
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

par(mfrow = c(1,1))

##########various	sampling	methods################
library(sampling)
### Simple Random Sampling ###
### srswr
set.seed(200)

s <- srswr(100, nrow(cocoa))
s[s != 0]

rows <- (1:nrow(cocoa))[s!=0]
rows <- rep(rows, s[s != 0])
rows

sample.1 <- cocoa[rows, ]
sample.1

#frequencies of countries
table(sample.1$Company.Location)

#pie chart
data <- head(sort(table(sample.1$Company.Location), decreasing = TRUE), 4)
slice.labels <- names(data)
slice.percents <- round(data/sum(data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
pie(data, labels = slice.labels, 
main = "Top 4 Countries With Replacement",
radius=1,col=hcl(c(0, 60, 120, 180)))

### srswor
set.seed(200)

s <- srswor(100, nrow(cocoa))
s[s != 0]

rows <- (1:nrow(cocoa))[s!=0]
rows

sample.2 <- cocoa[rows, ]
sample.2

#frequencies of countries
table(sample.2$Company.Location)

#pie chart
data <- head(sort(table(sample.2$Company.Location), decreasing = TRUE), 4)
slice.labels <- names(data)
slice.percents <- round(data/sum(data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
pie(data, labels = slice.labels, 
main = "Top 4 Countries Without Replacement",
radius=1,col=hcl(c(0, 60, 120, 180)))

### systematic sampling

N <- nrow(cocoa)
N
n <- 100
k <- floor( N / n)
k

# select every kth item
index <- seq(sample(k,1),by = k, length = n )
index
sample.systematic <- cocoa[index,]
sample.systematic

#frequencies of percent
table(sample.systematic$Cocoa.Percent)

#histogram
hist(sample.systematic$Cocoa.Percent, prob = TRUE,
     main="Histogram of Ccoca Percent",
     ylim = c(0,0.06),
     xlab="Percent", col="skyblue", breaks=6)
mean(sample.systematic$Cocoa.Percent, na.rm = TRUE)
mean(cocoa$Cocoa.Percent)

##########Analysis of Confidence Levels	of	80	and	90################
sample.size <- 20
sample.data <- sample(cocoa$Cocoa.Percent, size=sample.size)
sample.data

n <- length(sample.data)
n

sd.sample.means <- sd(sample.data)/sqrt(n)
sd.sample.means

xbar <- mean(sample.data)
xbar

pop.mean <- mean(cocoa$Cocoa.Percent)
pop.mean

conf <- c(80,90)
conf

alpha <- 1 - conf/100
alpha

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), t: %.2f , %.2f",
                 100*(1-i), i, 
                 qt(i/2, df = n-1),
                 qt(1 - i/2, df = n-1))
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qt(1 - i/2, df = n-1) * sd.sample.means,
                 xbar + qt(1 - i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qt(1-i/2, df = n-1) * sd.sample.means)
  cat(str,"\n")
}


