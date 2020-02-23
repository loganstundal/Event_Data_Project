#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          January 27, 2020                                                 
# Purpose:       Initial SPDE toy modeling following Python's Boko Haram script                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#            on meshes 
#               https://haakonbakka.bitbucket.io/btopic126.html                                                           #             
#                                                                             
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory   
#---------------------------#
rm(list = ls())


#---------------------------#
# Load required libraries   
#---------------------------#
library(INLA)
library(maptools)
library(foreign)
library(sp)
library(lattice)
library(maps)
library(spam)
library(spatstat)
library(fields)
library(raster)

#---------------------------#
# Set working directory     
#---------------------------#
# setwd()

#---------------------------#
# Load data                 
#---------------------------#
load('data/colombia.rdata')

# writeOGR(col, 'Data/Shapefile_Data/colombia.shp', 'col', driver = 'ESRI Shapefile')
# st_write(colombia, 'Data/Shapefile_Data/colombia.shp', 'col', driver = 'ESRI Shapefile')

#---------------------------#
# Load functions            
#---------------------------#


# CODE FROM STbin_final.R
# MESH ------------------------------------------------------------------------
colcoord <- cbind(colombia$centroid_mun_long, colombia$centroid_mun_lat)
border   <- inla.mesh.segment(colcoord)
mesh     <- inla.mesh.2d(loc.domain = border$loc, max.edge = c(1.6))

# plot(mesh)

# ANALYSIS --------------------------------------------------------------------
# ICEWS - CONTINUOUS MODEL FOR EVENT SCALE
y      <- colombia$icews_farc_scale
forest <- colombia$google_ee_forest_per
pop    <- log(colombia$google_ee_pop_sum)
tri    <- colombia$google_terrain_ri_mean_m


nv   <- mesh$n
A    <- inla.spde.make.A(mesh = mesh,
                         loc  = as.matrix(cbind(colombia$centroid_mun_long,colombia$centroid_mun_lat)));dim(A)
spde <- inla.spde2.matern(mesh, alpha=2)

formula <- y ~ -1 + intercept + forest + pop + tri + f(spatial.field, model=spde)

stk <- inla.stack(data    = list(y = y),
                  A       = list(A,1,1,1), 
                  effects = list(c(list(intercept = rep(1,nv)),
                                   inla.spde.make.index("spatial.field", spde$n.spde)),
                                 forest = forest,
                                 pop    = pop,
                                 tri    = tri),
                  tag='testing')

res0<-inla(formula, 
           data = inla.stack.data(stk,
                                  spde = spde),
           family = 'gaussian',
           control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
           control.compute   = list(waic = TRUE))
summary(res0)


# ICEWS - BINOMIAL MODEL FOR INCIDENCE OF EVENT
y      <- ifelse(colombia$icews_farc>=1,1,0)
forest <- colombia$google_ee_forest_per
pop    <- log(colombia$google_ee_pop_sum)
tri    <- colombia$google_terrain_ri_mean_m

  
nv   <- mesh$n
A    <- inla.spde.make.A(mesh = mesh,
                         loc  = as.matrix(cbind(colombia$centroid_mun_long,colombia$centroid_mun_lat)));dim(A)
spde <- inla.spde2.matern(mesh, alpha=2)

formula <- y ~ -1 + intercept + forest + pop + tri + f(spatial.field, model=spde)

stk <- inla.stack(data    = list(y = y),
                  A       = list(A,1,1,1), 
                  effects = list(c(list(intercept = rep(1,nv)),
                                   inla.spde.make.index("spatial.field", spde$n.spde)),
                                 forest = forest,
                                 pop    = pop,
                                 tri    = tri),
                  tag='testing')

res1<-inla(formula, 
           data = inla.stack.data(stk,
                                  spde = spde),
           family = 'binomial',
           control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
           control.compute   = list(waic = TRUE), verbose = TRUE)
summary(res1)

# --------------------------------------------------------------------------- #
# CODE FROM STbin_total_graphics.R
require(sp)
library(spatstat)
library(fields)
library(maptools)
library(fields)
library(lattice)

# ------------------------------------- #
# Logan modification
library(sf)
studyarea <- st_transform(colombia, crs = "+proj=longlat +datum=WGS84 +no_defs+towgs84=0,0,0+ellps=WGS84")
studyarea <- as_Spatial(studyarea)

# studyarea <- as_Spatial(colombia)
proj4string(studyarea)<-CRS("+proj=longlat +datum=WGS84 +no_defs+towgs84=0,0,0+ellps=WGS84")
# ------------------------------------- #

resfinal<-res0#to be adjusted if necessary

# ------------------------------------- #

#create points in location of mesh vertices
meshvert<-rbind(c(mesh$loc[,1],mesh$loc[,2]))
#extract values for each year of the randomfield mean
xmean<-resfinal$summary.random$spatial.field$mean#update file location according to model selection
xsd<-resfinal$summary.random$spatial.field$sd#update file location according to model selection

#plot on 2d (projection)
proj = inla.mesh.projector(mesh, projection = "longlat",dims=c(1444,724))
proj = inla.mesh.projector(mesh, projection = "longlat")


# ------------------------------------- #
# LOGAN - PROELEMT WITH PLOT SURFACE AND BREAKS LIKELY OCCURRING HERE.
win<-as.owin(studyarea)
library(mgcv)
e<-expand.grid(proj$x,proj$y)
# ------------------------------------- #

ins<-inside.owin(e[,1],e[,2],win)
ins<-matrix(ins,nrow=length(proj$y))

xmean<-inla.mesh.project(proj,resfinal$summary.random$spatial.field$mean)#update file location according to model selection
xmean[!ins]<-NA

#extract values for each year of the probability of lethal attack
probsurf<-binomial(link='logit')$linkinv(xmean+resfinal$summary.fix[1,1]+resfinal$summary.fix[2,1]+resfinal$summary.fix[3,1]
                                         +resfinal$summary.fix[4,1]
                                         # +resfinal$summary.fix[5,1]
                                         #+resfinal$summary.fix[6,1]
)#update file location according to model selection 

#plot prob. surface for y
dev.off()
plot.new()
breaks<-seq(min(probsurf,na.rm=TRUE),max(probsurf,na.rm=TRUE),by=0.0025)
#breaks<- quantile(probsurf,na.rm=TRUE,c( .1,.2,.3,.4,.5,.6,.7,.8,.9))

#GTD <- subset(GTD, latitude>4.23 & latitude<13.94 & longitude>2.61 & longitude<14.73) #longitude cut to avoid points out of Tajikistan

n<-length(breaks)-1
par(cex=3,mar=c(5, 4, 2, 4.5) + 0.1)#mar: c(bottom,left,top,right)
image.plot(proj$x,proj$y,probsurf,col=viridis::viridis(n),xlab="Longitude",
           main= "prob attack" ,ylab="Latitude",
           # xlim = c(2.55, 14.75), ylim = c(4.20, 14.0),
           zlim=c(min(probsurf,na.rm=TRUE),max(probsurf,na.rm=TRUE)),#zlim creates identical scale
           breaks=breaks)#should have one more break than color
dev.off()

# Latitude Min:   36.67   Max:   41.04   Longitude   Min:   67.39   Max:   75.14 with 0.05 tolerance
dev.off()
plot.new()
mypath=paste("C:/Users/apython/Documents/Andre/Education/StAndrews/PhD/Conferences/BayesianYoungresearcher/submittedJuly31/probsurf.eps")
postscript(file=mypath,horiz=FALSE,onefile=FALSE,width=21,height=8,paper="a4") 
par(cex=3,mar=c(2.7, 1, 1.75, 5.1) + 0.1)#mar: c(bottom,left,top,right)
#par(cex=3,mar=c(1.75, 1, 1.75, 4.4) + 0.1)#mar: c(bottom,left,top,right)
image.plot(proj$x,proj$y,probsurf,col=Llinpal(n),xlab=NA,ylab=NA,
           main=NA,axes=FALSE,
           xlim = c(2.55, 14.75), ylim = c(4.20, 14.0),zlim=c(min(probsurf,na.rm=TRUE),max(probsurf,na.rm=TRUE)),#zlim creates identical scale
           breaks=breaks
           #,legend.shrink=1.1
)
dev.off()





save()
rm(list = ls())