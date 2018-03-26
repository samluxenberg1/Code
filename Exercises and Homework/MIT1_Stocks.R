#MIT Analytics Edge Week 1 - Stocks
setwd('C:/Users/Samuel/Documents/Slide Rule/Data/MIT1_Stocks')
library(ggplot2)
temp <- list.files(pattern = "*.csv")
myfiles <- lapply(temp, read.csv)
str(myfiles)
Boeing <- as.data.frame(myfiles[1])
CocaCola <- as.data.frame(myfiles[2])
GE <- as.data.frame(myfiles[3])
IBM <- as.data.frame(myfiles[4])
ProctorGamble <- as.data.frame(myfiles[5])

Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
ProctorGamble$Date <- as.Date(ProctorGamble$Date, "%m/%d/%y")
GE$Date <- as.Date(GE$Date, "%m/%d/%y")
IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")

min(year(Boeing$Date))
min(year(CocaCola$Date))
min(year(ProctorGamble$Date))
min(year(GE$Date))
min(year(IBM$Date))

max(year(Boeing$Date))
max(year(CocaCola$Date))
max(year(ProctorGamble$Date))
max(year(GE$Date))
max(year(IBM$Date))

names(IBM)
summary(IBM$StockPrice)
summary(Boeing)
summary(GE)
summary(ProctorGamble)
summary(CocaCola)
sd(ProctorGamble$StockPrice)

PGC <- rbind(ProctorGamble, CocaCola)
PGC$grp <- rep(factor(1:2), times = c(nrow(ProctorGamble), nrow(CocaCola)))
ggplot(aes(x = Date, y = StockPrice, color = grp), data = PGC) +
  geom_line()

#Alternative method
plot(CocaCola$Date, CocaCola$StockPrice,
     type = "l",
     col = "red",
     lty = 2)
lines(ProctorGamble$Date, ProctorGamble$StockPrice,
      col = "blue")
abline(v = as.Date(c("2000-03-01")), lwd = 1)
abline(v = as.Date(c("1983-01-01")), lwd = 3)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432],
     type = "l",
     col = "red",
     ylim = c(0, 210))
lines(ProctorGamble$Date[301:432], ProctorGamble$StockPrice[301:432],
      col = "blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],
      col = "pink")
lines(GE$Date[301:432], GE$StockPrice[301:432],
      col = "green")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],
      col = "orange")
abline(v = as.Date(c("1997-09-01")))
abline(v = as.Date(c("1997-11-01")))

#Alt. Method
All <- rbind(ProctorGamble, CocaCola, IBM, GE, Boeing)
All$grp <- rep(factor(1:5), 
               times = c(nrow(ProctorGamble),
                         nrow(CocaCola),
                         nrow(IBM),
                         nrow(GE),
                         nrow(Boeing)))
ggplot(aes(x = Date, y = StockPrice, color = grp), data = All) +
  geom_line() +
  coord_cartesian(xlim = c(as.Date("1995-01-01"), as.Date("2005-12-31"))) +
  geom_vline(xintercept = as.numeric(as.Date("1997-09-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("1997-11-01")))

#From 2004 to 2006, which company is performing the best?
match(as.Date("2006-01-01"), GE$Date)
match(as.Date("2004-01-01"), GE$Date)

GE[433, 2]/GE[409, 2]-1
ProctorGamble[433, 2]/ProctorGamble[409, 2]-1
CocaCola[433, 2]/CocaCola[409, 2]-1
IBM[433, 2]/IBM[409, 2]-1
Boeing[433, 2]/Boeing[409, 2]-1

#Are stocks higher or lower in certain months?
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(ProctorGamble$StockPrice, months(ProctorGamble$Date), mean)
tapply(IBM$StockPrice, months(IBM$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
