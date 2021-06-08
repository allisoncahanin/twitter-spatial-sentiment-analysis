getwd()
setwd("C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project")
library(tools)
library(ggplot2)
library(splancs)
library(mapproj)
library(rgdal)
library(ggspatial)
library(USAboundaries)
library(sf)
library(sp)
library(raster)
library(maps)
library(tidyr)
library(tidyverse)
library(ggforce)
options(scipen = 999)

#import data
pos.points <- read.csv("positiveTrumpGEO.csv", header = F)
neg.points <- read.csv("negativeTrumpGEO.csv", header = F)
#spatial transform using rgdal
pos.coord <- SpatialPoints(cbind(pos.points$V2, pos.points$V1), proj4string=CRS("+proj=longlat"))
pos.projected <- spTransform(pos.coord, CRS("+init=EPSG:2163"))
neg.coord <- SpatialPoints(cbind(neg.points$V2, neg.points$V1), proj4string=CRS("+proj=longlat"))
neg.projected <- spTransform(neg.coord, CRS("+init=EPSG:2163"))


################################################
###        READ SHAPEFILE FOR ANALYSIS       ###
################################################

# https://hub.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0

boundary <- readOGR("C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/world_countries/World_Countries__Generalized_.shp")
boundary =boundary[boundary@data$COUNTRY == "United States",]
boundary <- spTransform(boundary, CRS("+init=EPSG:2163"))
outline <- boundary@polygons[[1]]@Polygons[[1]]@coords
plot(outline, type = "l")


################################################
###              CLIPPING POINTS             ###
################################################

#subsetting to remove AK, HI, and PR
usb <- us_boundaries()
usb.48 <- subset(usb, usb$stusps != "HI")
usb.48 <- subset(usb.48, usb.48$stusps != "AK")
usb.48 <- subset(usb.48, usb.48$stusps != "PR")
plot(st_geometry(usb.48))
usb.sf <- as_Spatial(usb.48)
usb <- spTransform(usb.sf, CRS("+init=EPSG:2163")) #reprojecting
#subset method to clip points
plot(usb)
plot(pos.projected, add=T)
pos.projected <- pos.projected[usb]
plot(usb) #boundary for analysis
plot(pos.projected, add=T) #clipped positive point data
#also for negative points
plot(usb)
plot(neg.projected, add=T)
neg.projected <- neg.projected[usb]
plot(usb) #boundary for analysis
plot(neg.projected, add=T) #clipped positive point data


################################################
###        REPROJECTING FOR GGPLOT           ###
################################################

#transforming the US boundaries from lat long to meters for visualization in ggplot
#usb is not the right type of object for plotting in ggplot? use combined for plotting
state <- map_data("state")
state.latlong <- cbind(state[1], state[2]) #extract lat long from data frame
latlong.pts <- SpatialPoints(cbind(state.latlong$long, state.latlong$lat), 
                             proj4string=CRS("+proj=longlat")) #create spatial points
trans.state <- spTransform(latlong.pts, CRS("+init=EPSG:2163")) #transform points to UTM
t.state.coords <- trans.state@coords #extract values from transformed points
combined <- cbind(t.state.coords[,1],t.state.coords[,2],state[3],state[4],state[5],state[6])
combined <-combined %>% #renaming columns for mapping
  rename(
    xvalue = `t.state.coords[, 1]`,
    yvalue = `t.state.coords[, 2]`
  )


################################################
###   EXTRACT COORDS FROM SPATIAL POINTS     ###
################################################

#extracting the coords from spatial points for ggplot
#also need this for analysis since .projected objects are lists not data frames
neg.coord <- neg.projected@coords
neg.coord <- as.data.frame(neg.coord)
neg.coord <-neg.coord %>%
  rename(
    x = coords.x1,
    y = coords.x2
  )
pos.coord <- pos.projected@coords
pos.coord <- as.data.frame(pos.coord)
pos.coord <-pos.coord %>% 
  rename(
    x = coords.x1,
    y = coords.x2
  )


################################################
###     CALCULATE MEAN CENTER AND SD         ###
################################################

# NEG mean center and standard distance
n.mean.ctr <- c(mean(neg.coord[,1]), mean(neg.coord[,2]))
n.point.sd <- sqrt(sum((neg.coord[,1] - n.mean.ctr[1])^2 + (neg.coord[,2] - n.mean.ctr[2])^2) / nrow(neg.coord))
n.ctr.pt <- as.data.frame(cbind(n.mean.ctr[1], n.mean.ctr[2]))

# POS mean center and standard distance
p.mean.ctr <- c(mean(pos.coord[,1]), mean(pos.coord[,2]))
p.point.sd <- sqrt(sum((pos.coord[,1] - p.mean.ctr[1])^2 + (pos.coord[,2] - p.mean.ctr[2])^2) / nrow(pos.coord))
p.ctr.pt <- as.data.frame(cbind(p.mean.ctr[1], p.mean.ctr[2]))


################################################
###        GGPLOT MEAN CENTER AND SD         ###
################################################

ggplot() + 
  ggtitle("Mean Center and Standard Distance of \n Positive and Negative Tweets about Donald Trump") +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("Easting") +
  ylab("Northing") +
  geom_polygon(data = combined, aes(x=xvalue, y=yvalue, group=group),
               color="gray90", fill="gray80") +
  geom_point(data = neg.coord, aes(y = y, x = x), size = .5, 
             shape = 1, col = "#e06666") +
  geom_point(data = n.ctr.pt, aes(y = V2, x = V1), 
             col = "#e06666", shape = 16, size = 1.7) +
  geom_circle(aes(x0 = n.mean.ctr[1], y0 = n.mean.ctr[2], r = n.point.sd), 
              col = "#e06666", lwd = 1.1) +
  geom_point(data = pos.coord, aes(y = y, x = x), size = .5, 
             shape = 1, col = "#5f7d95") +
  geom_point(data = p.ctr.pt, aes(y = V2, x = V1), 
             col = "#5f7d95", shape = 16, size = 1.7) +
  geom_circle(aes(x0 = p.mean.ctr[1], y0 = p.mean.ctr[2], r = p.point.sd), 
              col = "#5f7d95", lwd = 1.1) +
  theme(
    panel.background = element_rect(fill = "#edf2f4", colour = "#2b2d42",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +
  geom_text() +
  annotate("text", label = "Negative", 
           x = -2000000, y = -2100000, 
           size = 5, colour = "#e06666",
           hjust = 0) +
  annotate("text", label = "Positive", 
           x = -2000000, y = -1800000, 
           size = 5, colour = "#5f7d95",
           hjust = 0)
  

################################################
###            NEAREST NEIGHBOR              ###
################################################

library(tidyverse)
library(hrbrthemes)

# Positive
p.pts <- as.points(pos.coord)
p.nnd <- nndistG(p.pts)
hist(p.nnd$dists)
summary(p.nnd$dists)

p.dataframe <- as.data.frame(p.nnd$dists)

p.gg <- p.dataframe %>%
  ggplot( aes(p.nnd$dists)) +
  geom_histogram(fill="#5f7d95", color="#5f7d95", alpha=0.7) +
  ggtitle("Positive Tweet Nearest Neighbor Distribution") +
  xlab("Nearest Neighbor Distance (m)") +
  ylab("Frequency") +
  theme(
    panel.background = element_rect(fill = "#edf2f4", colour = "#2b2d42",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +
  annotate("text", label = "Mean: 3064 m", 
           x = 225000, y = 1000, 
           size = 5, colour = "#5f7d95",
           hjust = 1)
p.gg

# Negative
n.pts <- as.points(neg.coord)
n.nnd <- nndistG(n.pts)
hist(n.nnd$dists)
summary(n.nnd$dists)

n.dataframe <- as.data.frame(n.nnd$dists)

n.gg <- n.dataframe %>%
  ggplot( aes(n.nnd$dists)) +
  geom_histogram(fill="#e06666", color="#e06666", alpha=0.7) +
  ggtitle("Negative Tweet Nearest Neighbor Distribution") +
  xlab("Nearest Neighbor Distance (m)") +
  ylab("Frequency") +
  theme(
    panel.background = element_rect(fill = "#edf2f4", colour = "#2b2d42",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +
  annotate("text", label = "Mean: 11178 m", 
           x = 280000, y = 650, 
           size = 5, colour = "#e06666",
           hjust = 1)
n.gg

# Expected Nearest Neighbor Distributions
mean.nndist <- function(x.points, x.poly, ntrials)
{
out.vector <- mean(nndistG(x.points)$dists)
num.pts <- npts(x.points)
for(i in 1:ntrials)
{
  rnd.pts <- gen(x.poly, num.pts)
  mean.nndist <- mean(nndistG(rnd.pts)$dists)
  out.vector <- c(out.vector, mean.nndist)
}
out.vector
}

# NEGATIVE expected histogram
neg.exp.nnd <- mean.nndist(n.pts, outline, 999)
hist(neg.exp.nnd)
neg.exp.nnd <- as.data.frame(neg.exp.nnd)
neg.pval <- length(neg.exp.nnd[neg.exp.nnd<=mean(n.nnd$dists)]+1)/(999+1)

n.expected.hist <- neg.exp.nnd %>%
  ggplot( aes(neg.exp.nnd)) +
  geom_histogram(fill="#e06666", color="#e06666", alpha=0.6) +
  labs(title = "Observed and Expected Mean \nNearest Neighbor Distances",
       subtitle = "Negative Tweets",
       caption = "p-value: 0.001") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  xlab("Mean Nearest Neighbor Distance (m)") +
  ylab("Frequency") +
  theme(
    panel.background = element_rect(fill = "#edf2f4", colour = "#2b2d42",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +
  geom_segment(aes(x = mean(n.nnd$dists), y = 0, xend = mean(n.nnd$dists), yend = 500),
               color = "#e06666", size = 1.2, alpha = 0.7) +
  geom_segment(aes(x = mean(neg.exp.nnd), y = 0, xend = mean(neg.exp.nnd), yend = 500),
               color = "#e06666", size = 1.2, alpha = 0.8)

n.expected.hist

# POSITIVE expected histogram
pos.exp.nnd <- mean.nndist(p.pts, outline, 999)
hist(pos.exp.nnd)
pos.exp.nnd <- as.data.frame(pos.exp.nnd)
pos.pval <- length(pos.exp.nnd[pos.exp.nnd<=mean(p.nnd$dists)]+1)/(999+1)

p.expected.hist <- pos.exp.nnd %>%
  ggplot( aes(pos.exp.nnd)) +
  geom_histogram(fill="#5f7d95", color="#5f7d95", alpha=0.6) +
  labs(title = "Observed and Expected Mean \nNearest Neighbor Distances",
       subtitle = "Positive Tweets",
       caption = "p-value: 0.001") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  xlab("Mean Nearest Neighbor Distance (m)") +
  ylab("Frequency") +
  theme(
    panel.background = element_rect(fill = "#edf2f4", colour = "#2b2d42",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +
  geom_segment(aes(x = mean(p.nnd$dists), y = 0, xend = mean(p.nnd$dists), yend = 700),
               color = "#5f7d95", size = 1.2, alpha = 0.7) +
  geom_segment(aes(x = mean(pos.exp.nnd), y = 0, xend = mean(pos.exp.nnd), yend = 700),
               color = "#5f7d95", size = 1.2, alpha = 0.8)
p.expected.hist


# Testing overlaid histograms of pos + neg
testdata <- data.frame(
  type = c( rep("Negative", 1245), rep("variable 2", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=4) )
)

NEW.Ndatafram <- cbind(rep("Negative", 1245), n.dataframe)
NEW.Ndatafram <-NEW.Ndatafram %>%
  rename(
    type = `rep("Negative", 1245)`,
    value = `n.nnd$dists`
  )
NEW.Pdatafram <- cbind(rep("Positive", 1412), p.dataframe)
NEW.Pdatafram <-NEW.Pdatafram %>%
  rename(
    type = `rep("Positive", 1412)`,
    value = `p.nnd$dists`
  )
allNND <- rbind(NEW.Ndatafram, NEW.Pdatafram)

overlay.hist <- allNND %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram(alpha=0.6, position = 'identity') +
  scale_color_manual(values = c("Negative"="#e06666", "Positive"="#5f7d95")) +
  scale_fill_manual(values=c("#e06666", "#5f7d95")) +
  theme_ipsum() +
  labs(fill="") +
  ggtitle("Nearest Neighbor Distribution") +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("Nearest Neighbor Distance (m)") +
  ylab("Frequency") +
  theme(legend.position = c(0.8, 0.8)) +
  theme(
    panel.background = element_rect(fill = "#edf2f4", colour = "#2b2d42",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +
  annotate("text", label = "Mean: 11178 m", 
           x = 280000, y = 650, 
           size = 5, colour = "#e06666",
           hjust = 1)
overlay.hist

################################################
###        GHAT FIRST ORDER NEIGHBORS        ###
################################################

ghat.sim <- function(poly, pts, dist, ntrials)
{
  hold.mat <- matrix(nrow = length(dist), ncol = ntrials, NA)
  for(i in 1:ntrials)
  {
    t.rand <- splancs::gen(poly, npts(pts))
    hold.mat[,i] <- Ghat(t.rand, dist)
  }
  t.rand.max <- apply(hold.mat, 1, max)
  t.rand.min <- apply(hold.mat, 1, min)
  t.rand.mean <- apply(hold.mat, 1, mean)
  cbind(t.rand.min, t.rand.mean, t.rand.max)
}

#POSITIVE
pos.ghat <- Ghat(p.pts, seq(0,max(p.nnd$dists),max(p.nnd$dists)/30))
pos.gsim <- ghat.sim(outline, p.pts, seq(0,max(p.nnd$dists),max(p.nnd$dists)/30), 99)

# Positive tweet plot
plot(seq(0,max(p.nnd$dists),max(p.nnd$dists)/30), pos.ghat, type = "l",
     main = paste("Positive Tweet Observed Ghat \n and Significance Envelope"),
     xlab= "distance", 
     ylab= "Ghat",
     col = 3,
     lwd = 2)
lines(seq(0,max(p.nnd$dists),max(p.nnd$dists)/30),
      pos.gsim[,1], 
      lwd = 2,
      lty = 2,
      col = 1)
lines(seq(0,max(p.nnd$dists),max(p.nnd$dists)/30),
      pos.gsim[,3], 
      lwd = 2,
      lty = 2,
      col = 1)
lines(seq(0,max(p.nnd$dists),max(p.nnd$dists)/30),
      pos.gsim[,2], 
      lwd = 2,
      lty = 1,
      col = 4)
legend("bottomright", c("Simulated Envelope", "Expected Random", "Observed"),
       cex = 0.8,
       lty = c(2,1,1),
       col = c(1,4,3))

#NEGATIVE
neg.ghat <- Ghat(n.pts, seq(0,max(n.nnd$dists),max(n.nnd$dists)/30))
neg.gsim <- ghat.sim(outline, n.pts, seq(0,max(n.nnd$dists),max(n.nnd$dists)/30), 99)

#Negative tweet plot
plot(seq(0,max(n.nnd$dists),max(n.nnd$dists)/30), neg.ghat, type = "l",
     main = paste("Negative Tweet Observed Ghat \n and Significance Envelope"),
     xlab= "distance", 
     ylab= "Ghat",
     col = 3,
     lwd = 2)
lines(seq(0,max(n.nnd$dists),max(n.nnd$dists)/30),
      neg.gsim[,1], 
      lwd = 2,
      lty = 2,
      col = 1)
lines(seq(0,max(n.nnd$dists),max(n.nnd$dists)/30),
      neg.gsim[,3], 
      lwd = 2,
      lty = 2,
      col = 1)
lines(seq(0,max(n.nnd$dists),max(n.nnd$dists)/30),
      neg.gsim[,2], 
      lwd = 2,
      lty = 1,
      col = 4)
legend("bottomright", c("Simulated Envelope", "Expected Random", "Observed"),
       cex = 0.8,
       lty = c(2,1,1),
       col = c(1,4,3))


################################################
###        LHAT HIGHER ORER NEIGHBORS        ###
################################################

Lhat <- function (pts, poly, s)
{
  khat.out <- khat(pts, poly, s)
  lhat.out <- sqrt(khat.out/pi)-s
  lhat.out
}

Lenv.csr <- function (nptg, poly, nsim, s, quiet = F)
{
  
  tlist <- Kenv.csr(nptg, poly, nsim, s, quiet = T)
  tlist$lower <- sqrt(tlist$lower/pi)-s
  tlist$upper <- sqrt(tlist$upper/pi)-s
  tlist
}

# bounding box
points <- rbind(p.pts, n.pts)
min.x <- min(points[,1])
max.x <- max(points[,1])
min.y <- min(points[,2])
max.y <- max(points[,2])
points.bb <- cbind(c(min.x,max.x,max.x,min.x),c(max.y,max.y,min.y,min.y))

# POSITIVE lhat plot
pos.lhat <- Lhat(p.pts, points.bb, seq(0, 3000000, 1000))
psim.khat <- Lenv.csr(npts(p.pts), points.bb, nsim = 99, seq(0,3000000,1000), quiet = T)
plot(seq(0, 3000000, 1000), pos.lhat, type = "l", col = "#5f7d95",
     main = "Positive Tweet Observed Lhat \n and Significance Envelope",
     xlab = "Distance Classes",
     ylab = "Estimated Lhat",
     bg = "#edf2f4",
     ylim = c(-300000, 300000))
lines(seq(0,3000000,1000), psim.khat$upper, lty = 2, lwd = 2, col = "#2b2d42")
lines(seq(0,3000000,1000), psim.khat$lower, lty = 2, lwd = 2, col = "#2b2d42")
legend("bottomleft", c("Simulated Envelope", "Observed Lhat"),
       bty = "n",
       cex = .9,
       y.intersp = .8,
       lty = c(2,1),
       col = c("#2b2d42","#5f7d95"))

# NEGATIVE lhat plot
neg.lhat <- Lhat(n.pts, points.bb, seq(0, 3000000, 1000))
nsim.khat <- Lenv.csr(npts(n.pts), points.bb, nsim = 99, seq(0,3000000,1000), quiet = T)
plot(seq(0, 3000000, 1000), neg.lhat, type = "l", col = "#e06666",
     main = "Negative Tweet Observed Lhat \n and Significance Envelope",
     xlab = "Distance Classes",
     ylab = "Estimated Lhat",
     ylim = c(-300000, 300000))
lines(seq(0,3000000,1000), nsim.khat$upper, lty = 2, lwd = 2, col = "#2b2d42")
lines(seq(0,3000000,1000), nsim.khat$lower, lty = 2, lwd = 2, col = "#2b2d42")
legend("bottomleft", c("Simulated Envelope", "Observed Lhat"),
       bty = "n",
       cex = .9,
       y.intersp = .8,
       lty = c(2,1),
       col = c("#2b2d42","#e06666"))

################################################
###      L12HAT SPATIAL AUTOCORRELATION      ###
################################################

L12hat <- function (pts1, pts2 ,poly, s)
{
  kest <- k12hat(pts1, pts2, poly, s)
  L12hat.out <- sqrt(kest/pi)-s
  L12hat.out
}

Lenv.tor <- function (pts1, pts2, poly, nsim, s, quiet = F)
{
  tlist <- Kenv.tor(pts1, pts2, poly, nsim, s, quiet = T)
  tlist$lower <- sqrt(tlist$lower/pi)-s
  tlist$upper <- sqrt(tlist$upper/pi)-s
  tlist
}

# Toroidal shifts to look for spatial auto correlation
tweet.L12hat <- L12hat(p.pts, n.pts, points.bb, seq(0, 3000000, 1000))
tweet.Lenv.tor <- Lenv.tor(p.pts, n.pts, points.bb, nsim = 99, seq(0, 3000000, 1000))

plot(seq(0, 3000000, 1000), tweet.L12hat, xlab = "distance", ylab = "L12Hat", type = "l",
     main = "Simulation Envelopes From Random Toroidal Shifts",
     ylim = c(-500000, 500000))
lines(seq(0, 3000000, 1000), tweet.Lenv.tor$upper, lty = 2, lwd = 2, col = 2)
lines(seq(0, 3000000, 1000), tweet.Lenv.tor$lower, lty = 2, lwd = 2, col = 2)
legend("bottomleft", inset = .02, bg = 'transparent', c("Simulated Envelope", "Observed"),
       lty = c(2,1),
       col = c(2,1),
       cex = .8,
       box.lty = 0)


################################################
###       D(d) RANDOM SUBSET SIMULATION      ###
################################################

# is positive a random subset of overall points
pos.khat <- khat(p.pts, points.bb, seq(0, 2000000, 1000))
neg.khat <- khat(n.pts, points.bb, seq(0, 2000000, 1000))
k.diff <- pos.khat-neg.khat
env.lab <- Kenv.label(p.pts, n.pts, points.bb, nsim = 99, seq(0, 2000000, 1000), quiet = T)
plot(seq(0, 2000000, 1000), k.diff, 
     xlab = "Distance", 
     ylab = expression (hat(K)[1] - hat(K)[2]),
     ylim = c(-200000000000, 200000000000),
     type = "l",
     main = bquote("Observed" ~ hat(K)[1] - hat(K)[2] ~ "and Random Permutation Significance Envelope"),
     col = "#5f7d95")
lines(seq(0, 2000000, 1000), env.lab$upper, col = "#2b2d42", lty = 2)
lines(seq(0, 2000000, 1000), env.lab$lower, col = "#2b2d42", lty = 2)
legend("topright", c("Simulated Envelope", "Observed Lhat"),
       bty = "n",
       cex = .9,
       y.intersp = .8,
       lty = c(2,1),
       col = c("#2b2d42","#5f7d95"))

# is negative a random subset of overall points
k.diff2 <- neg.khat-pos.khat
env.lab <- Kenv.label(n.pts, p.pts, points.bb, nsim = 99, seq(0, 3000000, 1000), quiet = T)
plot(seq(0, 3000000, 1000), k.diff2, 
     xlab = "distance", 
     ylab = expression (hat(K)[1] - hat(K)[2]),
     ylim = c(-400000000000, 400000000000),
     type = "l",
     main = "Simulation Envelopes using Random Labelling")
lines(seq(0, 3000000, 1000), env.lab$upper, col = 3, lty = 2)
lines(seq(0, 3000000, 1000), env.lab$lower, col = 3, lty = 2)
legend("bottomleft", c("Simulated Envelope", "Observed"),
       lty = c(2,1),
       col = c(3,1))

################################################
###          SPATIAL AUTOCORRELATION         ###
################################################
library(GISTools)
library(spdep)
library(classInt)

### POSITIVE ANALYSIS ###

# Neighbor counts and color selection
pos.pt.counts <- poly.counts(pos.projected, usb)
usb.neigh <- poly2nb(usb)
n.cl <- 8
colorset <- brewer.pal(n.cl, "Blues")
p.class <- classIntervals(pos.pt.counts, n.cl, style = "quantile")
coded.cols <- findColours(p.class, colorset)

# Positive Choropleth
plot(usb, col = coded.cols, bg = "#edf2f4", main = "Choropleth of Positive Tweets")
legend("left", legend = names(attr(coded.cols, "table")), fill = attr(coded.cols, "palette"),
       cex = 0.6, bty = "n", title = "Number of Tweets")

# Positive Spatial Correlation Tests
pos.wt <- nb2listw(usb.neigh, style = "W", zero.policy = FALSE)
moran.test(pos.pt.counts, pos.wt)
geary.test(pos.pt.counts, pos.wt)
pos.wtB <- nb2listw(usb.neigh, style = "B", zero.policy = FALSE)
globalG.test(pos.pt.counts, pos.wtB)

# Positive Correlogram
pos.lag5 <- sp.correlogram(usb.neigh, pos.pt.counts, order = 5, method = "I")
plot(pos.lag5, main = "Moran's I Test using Queen's Case Adjacency and Row Standardized Weight Matrix \nCorrelogram of Positive Tweets")
title(main = "Correlogram of Positive Tweets",
     sub = "Moran's I Test using Queen's Case Adjacency and Row Standardized Weight Matrix")
print(pos.lag5)


### NEGATIVE ANALYSIS ###

# Neighbor counts and color selection
neg.pt.counts <- poly.counts(neg.projected, usb)
usb.neigh <- poly2nb(usb)
n.cl <- 8
colorset <- brewer.pal(n.cl, "Blues")
n.class <- classIntervals(neg.pt.counts, n.cl, style = "quantile")
n.coded.cols <- findColours(n.class, colorset)

# Negative Choropleth
plot(usb, col = n.coded.cols, bg = "#edf2f4", main = "Choropleth of Negative Tweets")
legend("left", legend = names(attr(n.coded.cols, "table")), fill = attr(n.coded.cols, "palette"),
       cex = 0.6, bty = "n", title = "Number of Tweets")

# Negative correlation tests
neg.wt <- nb2listw(usb.neigh, style = "W", zero.policy = FALSE)
moran.test(neg.pt.counts, neg.wt)

# Negative Correlogram
neg.lag5 <- sp.correlogram(usb.neigh, neg.pt.counts, order = 5, method = "I")
plot(neg.lag5,  main = "Moran's I Test using Queen's Case Adjacency and Row Standardized Weight Matrix \nCorrelogram of Negative Tweets")
print(DMV.lag5)
