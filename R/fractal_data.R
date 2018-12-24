# checks similar to the papers of Corsi and müller, if my data is fractal

# mueller p. 3: check structural break by plotting log price agains log time scale
# mueller p. 5: plot cdf and check if tail behavior changes with different sampling frequencies
# mueller p. 6: check seasonality by plotting acf
# mueller p. 9: plot hyperbolic and exponential curve to acf, hyperbolic should fit better and there is a "heat wave"

# check for the stylilzed facts in corsi that are represented by the cascade model

Time5min <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1000), by = "5 min"),
                    "%Y%m%dH%M", tz="GMT")
length(Time5min)

set.seed(1)
t <- 0:288000  # time
## first, simulate a set of random deviates
x <- rnorm(n = length(t) - 1, 0,0.1)
## now compute their cumulative sum
x <- c(0, cumsum(x))
plot(t, x, type = "l", ylim = c(-30,30))

Data <- cbind(t,x) %>% as.data.frame()
Data <- cbind(Time5min, Data)
Data <- Data[,c(1,3)]
colnames(Data) <- c("Date", "Return")
Data$Date <- as.Date(Data$Date, format = "%Y%m%dH%M")
DataTime <- as.xts(Data[,-1], order.by = Data$Date)
colnames(DataTime) <- c("Return")
DataTime$Return <- Return.calculate(DataTime$Return, method = "log")

DataTime$Return %>% head()
Data <- DataTime %>% as.data.frame()

plot(Data$Return, type = "l", ylim = c(-0.6,0.6))
head(Data)
Data[1:300,]
head(DataTime)
tail(Data)
tail(DataTime)

DataTime$RVday <- rollapply(DataTime$Return, width = 288, FUN = sum) %>% sqrt()
length(DataTime$RVday)


RVt = sqrt()
