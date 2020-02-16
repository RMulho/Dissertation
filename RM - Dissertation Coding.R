
######## INVESTIGATING RIVER WATER QUALITY IN ENGLAND ######

#### Author: Rachel Mulholland


##### 1 - SETTING UP ####
## Loading in libraries
require("rgdal")
require("ggplot2")
require("gridExtra")
require("NADA")
require("mgcv")
require("geoR")
require("zoo")
require("fda")

## Loading in ggmap with API key
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library("ggmap")
register_google(key = "AIzaSyDVjdvmM9r3ziRA1GL4I6XSeof0FLX1Hn8")


## ggplot colour replicator
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
rainbow <- gg_color_hue(7)




##### 2 - READING IN THE DATA #####
## Annual Data
# Loading it in and preparing for log(TRP)
TRPdata <- read.csv("TRP_data.csv")
TRPdata$log.trp <- log(TRPdata$Pconc)

# Look at what the data looks like and it's summary statistics
View(TRPdata)
summary(TRPdata)

# Assign each x co-ordinate a unique number (in this case all x co-ordinates were unique)
locations <- unique(cbind(unique(TRPdata$xcoord), c(1:500)))

# For each row in the Annual TRP dataset, assign these unique locations to the locations within the Annual
# dataset
TRPdata$location <- 1
for(i in 1:nrow(TRPdata)){
  for(j in 1:500){
    if(TRPdata$xcoord[i]==locations[j,1]){
      TRPdata$location[i] <- locations[j,2]
      
    }
  }
  
}

# For easiness, just take the information we need i.e. the unique location number and the x and y co-ords
monitor.stations <- subset(TRPdata, select=c("location","xcoord", "ycoord"))

# Transform the x and y co-ordinates into a lat and long
monitor.stations.sp <- SpatialPointsDataFrame(monitor.stations[,2:3], data=data.frame(monitor.stations$location), proj4string = CRS("+init=epsg:27700"))
monitor.stations.ll <- spTransform(monitor.stations.sp, CRS("+init=epsg:4326"))
monitor.stations.ll

# Put these transformed co-ordinates into the Annual TRP dataset
colnames(monitor.stations.ll@coords)[colnames(monitor.stations.ll@coords) == "xcoord"] <- "Longitude"
colnames(monitor.stations.ll@coords)[colnames(monitor.stations.ll@coords) == "ycoord"] <- "Latitude"
TRPdata$long <- monitor.stations.ll@coords[,1]
TRPdata$lat <- monitor.stations.ll@coords[,2]



## Monthly Data
# Loading it in and preparing for log(TRP)
monthly_TRPdata <- read.csv("HNWS_data.csv")
monthly_TRPdata$log.trp <- log(monthly_TRPdata$MEAS_RESULT)

# Look at what the data looks like and it's summary statistics
View(monthly_TRPdata)
summary(monthly_TRPdata)

# Assign each x and y co-ordinate a unique number
locations <- data.frame(unique(cbind(monthly_TRPdata$SMPT_EASTING,monthly_TRPdata$SMPT_NORTHING)))
locations$number <- 1:nrow(locations)

# For each row in the Monthly TRP dataset, assign these unique locations
monthly_TRPdata$location <- 0
for(i in 1:nrow(monthly_TRPdata)){
  n <- which(locations$X1 %in% monthly_TRPdata$SMPT_EASTING[i]  & locations$X2 %in% monthly_TRPdata$SMPT_NORTHING[i])
  monthly_TRPdata$location[i] <- locations[n,3]
}

# For easiness, just take the information we need i.e. the unique location number and the x and y co-ords
monitor.stations <- subset(monthly_TRPdata, select=c("location","SMPT_EASTING", "SMPT_NORTHING"))

# Transform the x and y co-ordinates into a lat and long
monitor.stations.sp <- SpatialPointsDataFrame(monitor.stations[,2:3], data=data.frame(monitor.stations$location), proj4string = CRS("+init=epsg:27700"))
monitor.stations.ll <- spTransform(monitor.stations.sp, CRS("+init=epsg:4326"))
monitor.stations.ll

# Put these transformed co-ordinates into the Monthly TRP dataset
colnames(monitor.stations.ll@coords)[colnames(monitor.stations.ll@coords) == "xcoord"] <- "Longitude"
colnames(monitor.stations.ll@coords)[colnames(monitor.stations.ll@coords) == "ycoord"] <- "Latitude"
monthly_TRPdata$long <- monitor.stations.ll@coords[,1]
monthly_TRPdata$lat <- monitor.stations.ll@coords[,2]

# Transform the monthly date readings into 'Date' format
monthly_TRPdata$SAMP_SAMPLE_DATE <- as.Date(monthly_TRPdata$SAMP_SAMPLE_DATE, "%d/%m/%Y")
monthly_TRPdata$year <- as.numeric(format(monthly_TRPdata$SAMP_SAMPLE_DATE, "%Y"))
monthly_TRPdata$month <- as.numeric(format(monthly_TRPdata$SAMP_SAMPLE_DATE, "%m"))
monthly_TRPdata$year.month <- monthly_TRPdata$year+(monthly_TRPdata$month-1)/12






##### 3 - Geographically plotting the Data ####
## Plot of all monitoring stations
# Since the monitoring stations were around the North-West of England, Chester was a fitting centre for the
#map. The colour scheme was chosen to be black and white so that the overlayed data could be clearly seen
eng <- get_map(location="chester, england", zoom=7, maptype = "terrain", color="bw")
eng.map <- ggmap(eng, base_layer = ggplot(aes(x=long, y=lat), data=TRPdata))

# Annual monitoring stations
p1 <- eng.map+
  geom_point(data=TRPdata, aes(x=long, y=lat, pch=RBD), col=rainbow[1])+
  labs(x="Longitude", y="Latitude",
       title="Annual mean monitoring stations")+
  theme(legend.position="bottom")

# Monthly monitoring stations
p2 <- eng.map+
  geom_point(data=monthly_TRPdata, aes(x=long, y=lat, pch=RBD), col=rainbow[6])+
  labs(x="Longitude", y="Latitude",
       title="Monthly mean monitoring stations")+
  theme(legend.position="bottom")

# Plot side by side
grid.arrange(p1,p2,ncol=2)


##### 4 - Analysis on Annual Data #####

#### 4.1 - EXPLORATORY ANALYSIS  ####

## Histograms of TRP and log(TRP)
# Histogram of the normal TRP (P concentration)
p1 <- ggplot(data=TRPdata, aes(x=Pconc))+
  geom_histogram(bins = 50, fill="limegreen", color="black", alpha=0.5)+
  labs(x="TRP (mg/l)", y="Count",
       title="Histogram of TRP")+theme_classic()

# Histogram of the log (TRP)
p2 <- ggplot(data=TRPdata, aes(x=log.trp))+
  geom_histogram(bins = 50, fill="dodgerblue4", color="black", alpha=0.5)+
  labs(x="Log(TRP) (mg/l)", y="Count",
       title="Histogram of Log(TRP)")+theme_classic()

# Plot side by side
grid.arrange(p1, p2, ncol=2)




### Getting low values 
# Subset to low values (log(TRP) < -7) or LOD's to do further investigation
TRP.lowvalues <- TRPdata[which(TRPdata$log.trp<(-7)),]

# Find the number of times each unique monitoring station has a low annual TRP value or LOD
TRP.lowvalues.count <- as.data.frame(table(TRP.lowvalues$location))
colnames(TRP.lowvalues.count) <- c("location", "Count")

# Add in the lat and long so that we can plot it later
TRP.lowvalues.count$lat <- 0
TRP.lowvalues.count$long <- 0
for(i in 1:nrow(TRP.lowvalues.count)){
  TRP.lowvalues.count$lat[i] <- unique(TRPdata$lat[which(TRPdata$location==TRP.lowvalues.count[i,1])])
  TRP.lowvalues.count$long[i] <- unique(TRPdata$long[which(TRPdata$location==TRP.lowvalues.count[i,1])])
  
}

# Create a loop which inserts the number of times flagged into the 'Annual' dataset according to the 
#latitude (unique across all locations)
TRPdata$times.flagged.low <- 0 
for(i in 1:6){
  lats.count <- TRP.lowvalues.count$lat[which(TRP.lowvalues.count$Count==i)]
  
  TRPdata$times.flagged.low[which(TRPdata$lat %in% lats.count)] <- i
}


# Create dataset of number of times flagged and a summary of the number of times monitoring stations were 
#flagged. Divide this by 6 for the number of years and divide it by 500 for the number of total monitoring
#stations
prop.times.flagged <- data.frame(cbind(as.factor(0:6),summary(as.factor(TRPdata$times.flagged.low))/6/500))
colnames(prop.times.flagged) <- c("No.times", "Prop")
prop.times.flagged$No.times <- factor(0:6)

# Plot  this summary into a bar plot
ggplot(data=prop.times.flagged, aes(x=No.times, y=Prop))+
  geom_bar(stat="identity", position = "dodge", fill=rainbow)+theme_light()+
  geom_text(aes(label=round(Prop,2)),vjust=-0.5, size=3,position = position_dodge(width=0.5))+
  labs(x="Number of times flagged", y="Proportion of monitoring stations", 
       title="Proportion of monitoring stations flagged for LOD values (Log(TRP)<-7)")



## Map of times flagged
# Extract the number of times each location was flagged and their lat and log
TRP.no.times.flagged <- data.frame(unique(cbind(TRPdata$lat, TRPdata$long, TRPdata$times.flagged.low)))
colnames(TRP.no.times.flagged) <- c("lat", "long", "times.flagged.low")

# Plot this in a map
eng <- get_map(location="chester, england", zoom=7, maptype = "terrain", color="bw")
eng.map <- ggmap(eng, base_layer = ggplot(aes(x=long, y=lat), data=TRP.lowvalues.count))
eng.map+
  geom_point(data=TRP.no.times.flagged, aes(x=long, y=lat, col=as.factor(times.flagged.low)))+
  labs(x="Longitude", y="Latitude", color="Count of Log(TRP) < -7",
       title="Number of times locations are flagged for Low Log(TRP) (<-7)")




## Distributions of times flagged for catchment variables
# Bar plot of 'Altitude' per number of times flagged
p1 <- ggplot(TRPdata, aes(y=altitude,x=as.factor(times.flagged.low), fill=as.factor(times.flagged.low)))+
  geom_boxplot(alpha=0.5)+theme_light()+
  labs(x="Times locations was flagged for Low TRP", y="Altitude (m)", title="Altitude",
       fill="Count of Location\n with Log(TRP) < -7")

# Bar plot of 'Alkalinity' per number of times flagged
p2 <- ggplot(TRPdata, aes(y=alkalinity,x=as.factor(times.flagged.low), fill=as.factor(times.flagged.low)))+
  geom_boxplot(alpha=0.5)+theme_light()+
  labs(x="Times locations was flagged for Low TRP", y="Alkalinity (mg/l)", title="Alkalinity",
       fill="Count of Location\n with Log(TRP) < -7")


# Finding the proportion each times a 'Strahler' category is flagged for LOD
strahler.highlow.count <- as.data.frame(table(as.factor(TRPdata$strahler), TRPdata$times.flagged.low))
colnames(strahler.highlow.count) <- c("strahler", "group", "count")
strahler.highlow.count$prop <- 0
totals.count <- summary(as.factor(TRPdata$times.flagged.low))
for(i in 1:nrow(strahler.highlow.count)){
  n <- which(names(totals.count)==strahler.highlow.count$group[i])
  strahler.highlow.count$prop[i] <- strahler.highlow.count$count[i]/totals.count[n]
  
}
# Plot proportions in barplot
p3 <- ggplot(strahler.highlow.count, aes(x=strahler, fill=group, y=prop))+
  geom_bar(stat="identity", position = "dodge", alpha=0.5,color="black")+theme_light()+
  labs(x="Strahler", y="Proportion", title="Strahler",
       fill="Count of Location\n with Log(TRP) < -7")


# Finding the proportion each times a 'Geology' category is flagged for LOD
geology.highlow.count <- as.data.frame(table(TRPdata$geology, TRPdata$times.flagged.low))
colnames(geology.highlow.count) <- c("geology", "group", "count")
geology.highlow.count$prop <- 0
totals.count <- summary(as.factor(TRPdata$times.flagged.low))
for(i in 1:nrow(geology.highlow.count)){
  n <- which(names(totals.count)==geology.highlow.count$group[i])
  geology.highlow.count$prop[i] <- geology.highlow.count$count[i]/totals.count[n]
}
# Plot proportions in barplot
p4 <- ggplot(geology.highlow.count, aes(x=geology, fill=group, y=prop))+
  geom_bar(stat="identity", position = "dodge", alpha=0.5,color="black")+theme_light()+
  labs(x="Geology", y="Proportion", title="Geology",
       fill="Count of Location\n with Log(TRP) < -7")

# Plot in a 2x2 format
grid.arrange(p1,p2,p3,p4, ncol=2)


## Plot of altitude vs alkalinity for rock type
# Plot 'Altitude' vs 'Alkalinity' and colour the points by 'Geology'.
ggplot(data=TRPdata, aes(x=altitude, y=alkalinity))+
  geom_point(data=TRPdata, aes(col=geology))+theme_light()+
  labs(x="Altitude (m)", y="Alkalinity (mg/l)", title="Alkalinity vs Altitude coloured by Rock Type", col="Geology")



### Subsetting to new population
# Subset to locations with 'low' altitudes and 'high' alkalinities
TRPdata.final <- subset(TRPdata, altitude<80 & alkalinity>50)

## Distribution of log(TRP) in new dataset
# Plot histogram of log(TRP) of new subset
ggplot(data=TRPdata.final, aes(x=log.trp))+
  geom_histogram(bins = 50, fill="dodgerblue4", color="black", alpha=0.5)+
  labs(x="Log(TRP) (mg/l)", y="Count",
       title="Histogram of Log(TRP)", 
       subtitle = "Subsetted monitoring stations with Altitude<80m and Alkalinity>50mg/l")+theme_classic()

## Map of subsetted locations
# Geographically plot the monitoring stations of this new population
eng <- get_map(location="chester, england", zoom=7, maptype = "terrain")
eng.map <- ggmap(eng, base_layer = ggplot(aes(x=long, y=lat), data=TRPdata.final))
eng.map+
  geom_point(data=TRPdata.final, aes(x=long, y=lat, col=mycols[1], pch=RBD),col=mycols[1])+
  labs(x="Longitude", y="Latitude",
       subtitle = "Subsetted monitoring stations with Altitude<80m and Alkalinity>50mg/l")+
  theme(legend.position="bottom")



#### 4.2 - DEALING WITH LOD's ####

## ROS generated LODs
# You must perform ROS on non-transformed data
obs <- TRPdata.final$Pconc
# Create a vector the same length as the TRP with TRUE for in the LOD and FALSE otherwise
LOD <- vector(length=nrow(TRPdata.final))
LOD[which(TRPdata.final$Pconc<(exp(-5)))] <- T
# Perform ROS
myros <- ros(obs, LOD)
# Set seed so we get same results for generating a random normal distribution from ROS mean and standard devation
set.seed(90)
sub <- rnorm(100000, mean(myros), sd(myros))
# Subset to only positive TRP values and ones below the LOD
subnew <- sub[sub>0 & sub<exp(-5)]

# Create temporary vector for the TRP levels to be replaced with the new ones (later)
TRPnew <- TRPdata.final$Pconc
# Find number of LOD values
n <- length(TRPdata.final$Pconc[LOD==TRUE])
# Set seed so we get same results for generating random samples
set.seed(90)
# Take a random sample of the ROS generated LOD values from subnew and replace the vector where LOD was TRUE
TRPnew[LOD==TRUE] <- sample(subnew, n)
# Take log of the new ROS generated TRP values
TRPdata.final$log.trp.new <- log(TRPnew)


## Plot Before and After LOD replacement
# Random locations for two per number of times flagged
location.subset <- c(236, 279, 90, 50, 439, 9, 54, 424, 183, 386, 75, 444, 450, 18)

# Preparing for plots
col.frmt <- rep(1:7, each=2)
par(mfrow=c(1,2))

# Plotting 'Before' - Includes LOD's
plot(1,1, type="l", xlim=c(2006,2011),
     ylim=c(min(TRPdata.final$log.trp,TRPdata.final$log.trp.new), max(TRPdata.final$log.trp,TRPdata.final$log.trp.new)),
     ylab="Log(TRP) (mg/l)", xlab="Year", col=rainbow[1])

for(i in 1:length(location.subset)){
  TRPdata.final.loc <- subset(TRPdata.final, location==location.subset[i])
  lines(TRPdata.final.subset.loc1$year, TRPdata.final.subset.loc1$log.trp, col=rainbow[col.frmt[i]])
  
  
}
mtext("Before", font=2, line=1, cex=1)
mtext("(Including original LODs)", cex=0.85, line=0.25)


# Plotting 'After' - ROS generated values for LODs
plot(1,1, type="l", xlim=c(2006,2011),
     ylim=c(min(TRPdata.final.subset$log.trp,TRPdata.final.subset$log.trp.new), max(TRPdata.final.subset$log.trp,TRPdata.final.subset$log.trp.new)),
     ylab="Log(TRP) (mg/l)", xlab="Year", col=rainbow[1])

for(i in 1:length(location.subset)){
  TRPdata.final.subset.loc1 <- subset(TRPdata.final.subset, location==location.subset[i])
  lines(TRPdata.final.subset.loc1$year, TRPdata.final.subset.loc1$log.trp.new, col=rainbow[col.frmt[i]])
  
  
}
mtext("After", font=2, line=1, cex=1)
mtext("(ROS generated values for LODs)", cex=0.85, line=0.25)

# Overall titles
par(mfrow=c(1,1))
mtext("Time Series of 12 Random Locations", font=2, line=2.25, cex=1.25)
plot(1,1)
legend("bottom", col=rainbow, lty=rep(1,times=7), legend=0:6, horiz = T, bty="n")
text(1,0.7, "Times Flagged:")




#### 4.3 - SPATIOTEMPORAL MODELLING ####

### GAM models for each year
# Creating table to hold info from models
spatial.table <- data.frame(matrix(ncol=6, nrow=8))
colnames(spatial.table) <- 2006:2011
rownames(spatial.table) <- c("lambda1", "lambda2", "GCV","AIC", "R-sq.Ad", "edf","min","max")
range <- c(-11,6)

# Create a plot and insert values from GAM tensors into the table for each year
par(mfrow=c(2,3))
for(i in 1:ncol(spatial.table)){
  # Subset to each year
  TRP.year <- subset(TRPdata.final, year==colnames(spatial.table)[i])
  # Fit the model for lat and long
  gam.tensor <- gam(log.trp.new~te(long,lat,bs="cr", fx=F, k=c(7,7)),
                    data=TRP.year)
  # Store goodness of fit statistic in table
  spatial.table[1,i] <- round(gam.tensor$sp[1],2)
  spatial.table[2,i] <- round(gam.tensor$sp[2],2)
  spatial.table[3,i] <- gam.tensor$gcv.ubre.dev
  spatial.table[4,i] <- AIC(gam.tensor)
  spatial.table[5,i] <- summary(gam.tensor)$r.sq
  spatial.table[6,i] <- summary(gam.tensor)$edf
  spatial.table[7,i] <- min(gam.tensor$linear.predictors)
  spatial.table[8,i] <- max(gam.tensor$linear.predictors)
  
  # Plot the linear predictors
  vis.gam(gam.tensor, contour.col="black",plot.type="contour",
          color="topo", zlim=range,ticktype = "detailed", zlab="",
          xlab="Longitude", ylab="Latitude", main="")
  mtext(colnames(spatial.table)[i], line=1.25, font=2)
  mtext(paste("EDF=", round(spatial.table[6,i],2), ", R-sq (adj)=", round(spatial.table[5,i],2)), line=0.25, cex=0.75)
  
  # Overlap monitoring stations
  points(TRPdata.final$long, TRPdata.final$lat, pch=20, col="red", cex=1.25)
  
  
}

### Difference between 2006 and 2011
# Calculating the difference between the years for each location
TRP.year06 <- subset(TRPdata.final, year==colnames(spatial.table)[1])
TRP.year11 <- subset(TRPdata.final, year==colnames(spatial.table)[6])
diff.06.11 <- data.frame(cbind(TRP.year06$lat, TRP.year06$long, TRP.year06$log.trp.new, TRP.year11$log.trp.new))
colnames(diff.06.11) <- c("lat", "long", "log.trp.06", "log.trp.11")

diff.06.11$diff <- diff.06.11$log.trp.11-diff.06.11$log.trp.06

# Fit GAM model using tensor functions for latitude and longitude 
par(mfrow=c(1,1))
gam.tensor.diff <- gam(diff~te(long,lat,bs="cr", fx=F, k=c(7,7)),
                       data=diff.06.11)
range <- c(round(min(gam.tensor.diff$linear.predictors))-3,
           round(max(gam.tensor.diff$linear.predictors))+3)

# Plot the linear predictions and overlap monitoring stations
vis.gam(gam.tensor.diff, contour.col="black",plot.type="contour",color="topo", main="",
        ylab="Latitude", xlab="Longitude",zlim=range, cex=1)
mtext("Difference between 2006 and 2011", cex=1.25, font = 2, line=1)
mtext(paste("EDF=", round(summary(gam.tensor.diff)$edf,2), ", R-sq (adj)=", round(summary(gam.tensor.diff)$r.sq,2)),
      line=0.25,cex=0.85)
points(TRPdata.final$long, TRPdata.final$lat, pch=20, col="red")



### Treating year as a factor
#Fit the GAM model model with year as a factor
gam.tensor.yr.factor <- gam(log.trp.new~te(long,lat,bs="cr", fx=F, k=c(7,7))+as.factor(year),
                            data=TRPdata.final)

# Summarise and check ANOVA for the factor year
summary(gam.tensor.yr.factor)
anova(gam.tensor.yr.factor)

# Plot linear predictors
range <- c(round(min(gam.tensor.yr.factor$linear.predictors))-3,
           round(max(gam.tensor.yr.factor$linear.predictors))+3)

par(mfrow=c(1,1))
year <- sort(unique(as.factor(TRPdata.final$year)))

vis.gam(gam.tensor.yr.factor, view=c("long", "lat"), cond=list(year==2011),
        color="topo", plot.type="contour", contour.col = "black", main="",
        zlim=range, ylab="Latitude", xlab="Longitude")
points(TRPdata.final$long, TRPdata.final$lat, pch=20, col="red")
mtext("Spatial surface with year as factor", cex=1.25, font = 2, line=1)
mtext(paste("EDF=", round(summary(gam.tensor.yr.factor)$edf,2), ", R-sq (adj)=", round(summary(gam.tensor.yr.factor)$r.sq,2)),
      line=0,cex=0.85)


### Treating year as an interaction with the catchment variables
# Fit a GAM with year as an interaction with the catchment variables
gam.tensor.yr.final <- gam(log.trp.new~te(long,lat,bs="cr", fx=T, k=c(6,6),
                                          by=as.factor(year))+RBD+as.factor(strahler)+
                             geology,data=TRPdata.final)

# Summarise and check ANOVA
summary(gam.tensor.yr.final)
anova(gam.tensor.yr.final)

# Plot the linear predictors
range <- c(round(min(gam.tensor.yr.final$linear.predictors))-3,
           round(max(gam.tensor.yr.final$linear.predictors))+3)
par(mfrow=c(2,3))
for(i in 1:6){
  vis.gam(gam.tensor.yr.final, view=c("long", "lat"),cond=list(year=year[i]),
          theta=-30, phi=30, color="topo",
          zlim=range, plot.type="contour",
          contour.col = "black", main="", xlab="Longitude", ylab="Latitude") 
  
  mtext(year[i], line=.25, font=2)
  
  
  points(TRPdata.final$long, TRPdata.final$lat, pch=20, col="red", cex=1.25)
}

## Diagnostic plots
par(mfrow=c(1,3))
# QQ-Plot of residuals
qqnorm(gam.tensor.yr.final$residuals, main="")
qqline(gam.tensor.yr.final$residuals)
mtext("Normal QQ Plot", font=2, line=0.5)

# Histogram of residuals
hist(gam.tensor.yr.final$residuals, main="", xlab="Residuals", col="dodgerblue4")
mtext("Histogram of Residuals", font=2, line=0.5)

# Residuals vs fitted values
plot(gam.tensor.yr.final$fitted.values, gam.tensor.yr.final$residuals, xlab="Fitted Values", ylab="Residuals")
abline(h=0)
mtext("Residuals vs Fitted Values", font=2, line=0.5)



## Checking for spatial correlation
par(mfrow=c(2,3))
# Extract residuals from final model with year as an interaction and the catchment variables
TRPdata.final$residuals <- residuals(gam.tensor.yr.final)
years <- sort(unique(TRPdata.final$year))

# Create a semi-variogram per annual model
for(i in 1:6){
  # Extract the lat and long for the residuals
  TRPdata.final.year <- subset(TRPdata.final, year==years[i])
  resid.data <- data.frame(residuals=TRPdata.final.year$residuals,
                           long=TRPdata.final.year$long, lat=TRPdata.final.year$lat)
  # Transform into a geodata
  resid.geodata <- as.geodata(resid.data, coords.col = 2:3, data.col=1, borders=T)
  # Generate semi-variogram
  vari <- variog(resid.geodata)
  # Generate MC bands of independence
  vari.mc <- variog.mc.env(resid.geodata, obj.variog=vari)
  # Plot
  plot(vari, envelope.obj=vari.mc, main="", pch=20, cex=1.25)
  mtext(paste("Semivariogram of Residuals for", years[i]), font=2, line=0.5)
  
  
}


##### 5 - ANALYSIS OF MONTHLY DATA #####

#### 5.1 - Monthly Means  ####

## Making monthly data into time series friendly
# Create a dataset for each column as a location and each row being a year and month combination
trp.location.ts <- as.data.frame(matrix(ncol=length(unique(monthly_TRPdata$location)),
                                        nrow=length(unique(monthly_TRPdata$year.month))))
colnames(trp.location.ts) <- sort(unique(monthly_TRPdata$location))
rownames(trp.location.ts) <- sort(unique(monthly_TRPdata$year.month))
head(trp.location.ts[,c(1:10)])
year.month.vector <- sort(unique(monthly_TRPdata$year.month))

# Insert the mean year and month log(TRP) values for each of the locations
for(i in 1:nrow(trp.location.ts)){
  
  for(j in 1:ncol(trp.location.ts)){
    year.subset <- subset(monthly_TRPdata, year.month==year.month.vector[i]
                          &  location==j) 
    m <- mean(year.subset$log.trp)
    if(is.nan(m)){
      trp.location.ts[i,j] <- NA
      
    } else
      trp.location.ts[i,j] <- m
    
  }
  
}


## FINDING LOCATIONS FROM JAN 2006 TO JAN 2010
# Locations with values on Jan 2006 (1 for T and 0 for F)
na.count.jan06.full <- vector(length=ncol(trp.location.ts))
names(na.count.jan06.full) <- colnames(trp.location.ts)
for(i in 1:length(na.count.jan06.full)){
  location0607 <- trp.location.ts[1,i]
  na.count.jan06.full[i] <-length(which(is.na(location0607)==F))
}
na.count.jan06.full
length(names(na.count.jan06.full[(which(na.count.jan06.full>0))]))
names(na.count.jan06.full[(which(na.count.jan06.full>0))])

# Locations with values on Jan 2010 (1 for T and 0 for F)
na.count.jan10.full <- vector(length=ncol(trp.location.ts))
names(na.count.jan10.full) <- colnames(trp.location.ts)
for(i in 1:length(na.count.jan10.full)){
  location1011 <- trp.location.ts[49,i]
  na.count.jan10.full[i] <-length(which(is.na(location1011)==F))
}

# Finding locations which have values in both Jan 2006 and Jan 2010
full.06.10 <- intersect(names(na.count.jan06.full[(which(na.count.jan06.full>0))]),
                        names(na.count.jan10.full[(which(na.count.jan10.full>0))]))
full.06.10


## Subsetting monthly data for these values from Jan 06 to Jan 10
# Create a new dataset of the lat and long of these subsetted monitoring stations
lat.full <- monthly_TRPdata$lat[which(monthly_TRPdata$location %in% as.numeric(c(full.06.10)))]
long.full <- monthly_TRPdata$long[which(monthly_TRPdata$location %in% as.numeric(c(full.06.10)))]
monthly.jan.06.10 <- data.frame(unique(cbind(lat.full, long.full)))

# Add in the location number of RBD into the new dataset
monthly.jan.06.10$location.no <- 0 
monthly.jan.06.10$RBD <- 0 
for(i in 1:nrow(monthly.jan.06.10)){
  location.subset <- subset(monthly_TRPdata, lat==monthly.jan.06.10$lat.full[i] & long==monthly.jan.06.10$long.full[i])
  monthly.jan.06.10$location.no[i] <- unique(location.subset$location)
  monthly.jan.06.10$RBD[i] <- as.character(unique(location.subset$RBD))
  
}




## PLOT OF ANNUAL AND MONTHLY LOCATIONS
eng <- get_map(location="chester, england", zoom=7, maptype = "terrain")
eng.map <- ggmap(eng, base_layer = ggplot(aes(x=long, y=lat), data=TRPdata.final))

eng.map+
  geom_point(data=monthly.jan.06.10, aes(x=long.full, y=lat.full, col=rainbow[6], pch=RBD))+ # Monthly
  geom_point(data=TRPdata.final, aes(x=long, y=lat, col=mycols[1], pch=RBD))+ # Annual
  labs(x="Longitude", y="Latitude", 
       subtitle = "Annual mean monitoring stations vs Monthly mean monitoring stations (subsetted)")+
  scale_color_manual(labels=c("Monthly Mean", "Annual Mean"),
                     values=c(rainbow[6],rainbow[1]),name="Monitoring Station Type")


## HISTOGRAM
# The layout for a histogram rquires all the values in a column - subset original dataset for the monitoring 
# stations of interest (subsetted for Jan 2006 to Jan 2010)
monthly_TRPdata_hist <- subset(monthly_TRPdata, location %in% full.06.10)

# Plot the histogram
ggplot(data=monthly_TRPdata_hist, aes(x=log.trp))+
  geom_histogram(bins = 50, fill="dodgerblue4", color="black", alpha=0.5)+
  labs(x="Log(TRP) (mg/l)", y="Count",
       title="Histogram of Log(TRP)", subtitle = "Monthly mean values from Jan 06 to Jan 10")+theme_classic()


#### 5.2 - Temporal Trends ####

## INTERPOLATING SUBSETTED DATA
# Finding all of the year and months between Jan 2006 and Jan 2010
year.month.0610 <- year.month.vector[1:which(year.month.vector==2010)]
#Subset the time series monthly dataset of the mean log(TRP) to the time period only
trp.location.ts.full <- trp.location.ts[1:length(year.month.0610),c(full.06.10)]

# Fill in the NA's with the interpolated values
trp.location.ts.full.na.approx <- trp.location.ts.full
for(i in 1:ncol(trp.location.ts.full.na.approx)){
  time.series <- trp.location.ts.full.na.approx[,i]
  trp.location.ts.full.na.approx[,i] <-  na.approx(time.series, rule=3)
  
}

## Plots of raw data - Original and interpolated
# Mat plot of the original data
matplot(year.month.0610, trp.location.ts.full, type="l", xlab="Time", ylab="Log(TRP) (mg/l)")
mtext("Raw monthly data for Log(TRP)", font=2, line=0.25, cex=1)

# Mat plot for the interpolated data
matplot(year.month.0610, trp.location.ts.full.na.approx, type="l", xlab="Time", ylab="Log(TRP) (mg/l)")
mtext("Linearly interpolated data for Log(TRP)", font=2,
      cex=1, line=0.25)

#### 5.3 - Functional Data Analysis  ####

## B-splines
# Let the knots = each time points
knots <- c(year.month.0610)
# Order = 4 which means we are using cubic b-splines
norder <- 4
# Create the basis
nbasis <- length(knots)+norder-2
# Fit the data uing the b-splines with these smoothing parameters
bbasis <- create.bspline.basis(c(2006,2010), nbasis, norder,knots)

# Plot these B-Splines
par(mfrow=c(1,2))
plot(bbasis, xlab="Time")
mtext("B-Splines", font=2, line=0.25, cex=1)

## Choosing smoothing parameter from GCV
# Convert 2 to Linear Differential Operator
curv.Lfd <- int2Lfd(2)
# Set initial lambda
lambda <- 0.1
# Define the B-Splines asa functional parameter
curv.fdPar <- fdPar(bbasis, curv.Lfd, lambda)
# Get list of possible lambdas as vector
lambdas <- 10^seq(-4,4,by=0.1)
# Create empty vector to store the mean GCVs
mean.gcv <- rep(0,length(lambdas))

# Find the mean GCV for each lambda
for(i in 1:length(lambdas)){
  #  Set lambda
  curv.fdPari = curv.fdPar
  curv.fdPari$lambda = lambdas[i]
  
  #  Smooth
  trpsmoothi = smooth.basis(year.month.0610, as.matrix(trp.location.ts.full.na.approx), curv.fdPari)
  
  #  Record mean GCV
  mean.gcv[i] = mean(trpsmoothi$gcv)
}

# Plot the Mean GCV against the lambdas
plot(lambdas,mean.gcv,type='o',log='x', pch=20, xlab=expression(lambda), ylab="Mean GCV")
mtext("The Mean GCV for different values of lambda", font=2, line=1.05, cex=1)
mtext("Saturated Cubic B-Splines Models", line=0.25, cex=0.85)

# Find the lambda with the lowest mean GCV
best <- which.min(mean.gcv)
lambdabest <- lambdas[best]
# Replace our smoothing parameter with the 'best' lambda
curv.fdPar$lambda <- lambdabest

# Fit the final smoothing basis using these parameters 
trpsmooth.final <- smooth.basis(year.month.0610, as.matrix(trp.location.ts.full.na.approx), curv.fdPar)


## Plot the smooth functions
# Smooth functions for all monitoring stations in subsetted population
par(mfrow=c(1,1))
plot(trpsmooth.final$fd, xlab="Time", ylab="Log(TRP) (mg/l)")
mtext("Smooth Functions for Log(TRP)", font=2,
      cex=1, line=.25)
# Overlay the mean smoothing function
lines(mean(trpsmooth.final$fd), lwd=3, col=1)
legend("topleft", col=1, lwd=3, legend=c("Mean Function"), cex=0.85)

## Covariance
# Finding covariance
trp.cor <- cor.fd(knots,trpsmooth.final$fd)
# Plot the covariance
filled.contour(knots,knots,trp.cor, xlab="Time", ylab="Time")
mtext("Functional Covariance of Time Points", font=2,
      cex=1, line=0.75,adj=0.3)


## FDA ANOVA for RBD

# Put each RBD into a coloured group
rbd.cols <- vector(length=length(trpsmooth.final$fd$fdnames$reps))
names(rbd.cols) <- trpsmooth.final$fd$fdnames$reps
for(i in 1:length(rbd.cols)){
  location <- names(rbd.cols)[i]
  rbd.cols[i] <- (unique(monthly_TRPdata$RBD[which(monthly_TRPdata$location==location)]))
  
  
}

# Plot the smoothing functions by the colour of the RBD
par(mfrow=c(1,2))
plot(trpsmooth.final$fd, col=rbd.cols+1, lty=1)
mtext("Smooth Functions for Log(TRP)", font=2,
      cex=1.25, line=1.25)
mtext(paste("Coloured via River Basin District"), font=2, line=0.25, cex=1)

# Plot of the mean functions per RBD
plot(mean(trpsmooth.final$fd), ylim=c(-3,0), lwd=2, xlab="Time", ylab="Mean Log(TRP)")

# RBD = Humber
humber.locations <- names(which(rbd.cols==1))
humber.fn <- smooth.basis(year.month.0610, as.matrix(trp.location.ts.full.na.approx[,humber.locations]), curv.fdPar)
humber.mean <- mean(humber.fn$fd)
lines(humber.mean, col="red")
# RBD = North West
northwest.locations <- names(which(rbd.cols==2))
northwest.fn <- smooth.basis(year.month.0610, as.matrix(trp.location.ts.full.na.approx[,northwest.locations]), curv.fdPar)
northwest.mean <- mean(northwest.fn$fd)
lines(northwest.mean, col="green")
# RBD = Severn
severn.locations <- names(which(rbd.cols==3))
severn.fn <- smooth.basis(year.month.0610, as.matrix(trp.location.ts.full.na.approx[,severn.locations]), curv.fdPar)
severn.mean <- mean(severn.fn$fd)
lines(severn.mean, col="blue")

legend("topleft", lty=rep(1, times=4), col=c(1:4), lwd=c(2,1,1,1),cex=0.85, legend=c("Overall","Humber", "North-West", "Severn"))

mtext("Mean Smooth Functions for the River Basin Districts", font=2,
      cex=1.25, line=0.25)

## Functional Principal Component Analysis (FPCA)
# Fit the fPCAs for 6 possible PCs
trp.PCA <- pca.fd(trpsmooth.final$fd, nharm=6, centerfns = T)

# Scree plot of the variance explained by the number of PCs
par(mfrow=c(1,1))
plot(trp.PCA$varprop, type="b", xlab="Number of Principal Components", ylab="Variance explained")
mtext("Scree plot of variance explained by each Principal Component", font=2,
      cex=1.25, line=0.25)

## Mean centred functions
# Create new dataset for new mean centred functions
fda.nomean <- data.frame(trpsmooth.final$fd$coefs)
rownames(fda.nomean) <- seq(2006, 2011, length.out = nrow(fda.nomean))
# Find the mean centred function
for(i in 1:ncol(fda.nomean)){
  fda.nomean[,i] <- fda.nomean[,i]-mean(trpsmooth.final$fd)$coefs

}
# Plot
par(mfrow=c(1,2))
matplot(rownames(fda.nomean), fda.nomean, type="l", xlab="Time", ylab="Log(TRP) (mg/l)")
abline(h=0, lwd=2, lty=2)
mtext("Centered Smoothing Funtions for Log(TRP)", font=2, line=0.25)

## Plot the 2 PC's
plot(trp.PCA$harmonics[1:2], xlab="Time", ylab="PCs")
mtext("Principal Component Functions", font=2,
      cex=1, line=0.25)
legend("topright",cex = 0.85, col=c(1,2), lty=c(1,2), legend=c("PC1", "PC2"), bty="n")

## Plot the scores from fPCA
# Save the 2 PCs
trp.PCA <- pca.fd(trpsmooth.final$fd, nharm=2)
# Fnd their scores and save into dataset
trp.PCA.scores <- as.data.frame(cbind(trp.PCA$scores[,1:2]))
colnames(trp.PCA.scores) <- c("PC1", "PC2")
rownames(trp.PCA.scores) <- colnames(trp.location.ts.full.na.approx)

# Add RBD of PC scores
trp.PCA.scores$RBD <- 0
for(i in 1:nrow(trp.PCA.scores)){
  location <- rownames(trp.PCA.scores)[i]
  trp.PCA.scores$RBD[i] <- (unique(monthly_TRPdata$RBD[which(monthly_TRPdata$location==as.numeric(location))]))
  
}

# Plot the 2 PC scores against each other and colour by the RBDs
par(mfrow=c(1,1))
plot(trp.PCA.scores$PC1, trp.PCA.scores$PC2, pch=20, xlab="PC1 Scores", ylab="PC2 Scores", col=(trp.PCA.scores$RBD)+1)
mtext("PC1 Scores vs. PC2 Scores", font=2,
      cex=1, line=0.25)
legend("bottomleft", cex=0.85, pch=rep(20, times=3), col=c(1:3)+1, legend=c("Humber", "North-West", "Severn"))
abline(v=0, lty=2)
abline(h=0, lty=2)

