#reading 1999 data
pm0 <- read.csv('annual_conc_by_monitor_1999.csv', header = FALSE, na.strings = '')

cnames <- readLines('annual_conc_by_monitor_1999.csv', 1)
cnames <- strsplit(cnames, '\",\"', fixed = TRUE)
cnames <- unlist(cnames)
colnames(pm0) <- cnames
pm0 <- pm0[-c(1),]
colnames(pm0)[1] <- "State.Code"
colnames(pm0)[2] <- "County.Code"
colnames(pm0)[3] <- "Site.ID"
colnames(pm0)[28] <- "Sample.Value"
pm0 <- subset(pm0, pm0['Parameter Code'] == '88101')

#reading 2012 data
pm1 <- read.csv('annual_conc_by_monitor_2012.csv', header = FALSE, na.strings = '')

cnames1 <- readLines('annual_conc_by_monitor_2012.csv', 1)
cnames1 <- strsplit(cnames1, '\",\"', fixed = TRUE)
cnames1 <- unlist(cnames1)
colnames(pm1) <- cnames1
pm1 <- pm1[-c(1),]
colnames(pm1)[1] <- "State.Code"
colnames(pm1)[2] <- "County.Code"
colnames(pm1)[3] <- "Site.ID"
colnames(pm1)[28] <- "Sample.Value"
pm1 <- subset(pm1, pm1['Parameter Code'] == '88101')

#getting arithmetic mean values as integer vectors
x0 <- as.numeric(unlist(pm0['Sample.Value']))
summary(x0)

x1 <- as.numeric(unlist(pm1['Sample.Value']))
summary(x1)

#visualizing data
boxplot(x0, x1)

#subsetting based on state
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))

site0 <- paste(site0[,1], site0[,2], sep = '.')
site1 <- paste(site1[,1], site1[,2], sep = '.')

#determining where the two datasets intersect based on monitor
#Both returns monitors that are present in 99 and '12
both <- intersect(site0, site1)
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = '.'))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = '.'))

#creates new dataframes for NY where both 99 and 12 have the same monitors
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

#creating two dataframes of the same monitor at two different dates
pm0sub <- subset(pm0, State.Code == 36 & County.Code == '063' & Site.ID == '2008')
pm1sub <- subset(pm1, State.Code == 36 & County.Code == '063' & Site.ID == '2008')

#creating boxplots for above monitor
x0sub <- as.numeric(unlist(pm0sub['Sample.Value']))
x1sub <- as.numeric(unlist(pm1sub['Sample.Value']))
boxplot(x0sub,x1sub)

#comparing pm values state by state
mn0 <- with(pm0, tapply(as.numeric(unlist(Sample.Value)), 
                        as.numeric(unlist(State.Code)), 
                        mean, na.rm = T))
mn1 <- with(pm1, tapply(as.numeric(unlist(Sample.Value)), 
                        as.numeric(unlist(State.Code)), 
                        mean, na.rm = T))

d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)

mrg <- merge(d0, d1, by = "state")

par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 51), mrg[,2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012, 51), mrg[,3]))
segments(rep(1999,51), mrg[,2], rep(2012, 51), mrg[,3])





