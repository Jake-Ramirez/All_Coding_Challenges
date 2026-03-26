#First coding challenge !!

z <- c(1:200)
mean(z)
sd(z)
zlog <- z > 30
head(zlog)
tail(zlog)
# seems like it works
zdf <- data.frame(z = z, zlog = zlog)
head(zdf)

#change col names
colnames(zdf) <- c("zvec", "zlogic")
head(zdf)

#creat squared col
zdf$zsquared <- zdf$zvec^2
head(zdf)

#zsquared from 10 to 100
zdf_subset <- subset(zdf, zsquared > 10 & zsquared < 100)
head(zdf_subset)

#row 26
zdf_row26 <- zdf[26, ]
head(zdf_row26)

#row 180 col zsquared
zdf_row18 <- zdf[180, "zsquared"]
head(zdf_row18)

#reading the csv from my wd and setting periods as NA's
tips <- read.csv("TipsR.csv", na.strings = ".")
head(tips)

