#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          January 01, 2020                                                 
# Purpose:       Note                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#                                                                             
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
library(ProbitSpatial)
library(raster)

#---------------------------#
# Set working directory     
#---------------------------#
setwd('c:/users/logan/googledrive/umn/research/ra_john/event_data_project')

#---------------------------#
# Load data                 
#---------------------------#


#---------------------------#
# Load functions            
#---------------------------#



# SIMULATE DATA ---------------------------------------------------------------

n <- 10
nneigh <- 3
rho <- 0.5
beta <- c(4,-2,1)
W <- generate_W(n,nneigh)
X <- cbind(1,rnorm(n,2,2),rnorm(n,0,1))
colnames(X) <- c('Intercept','X1','X2')
# y <- sim_binomial_probit(W,X,beta,rho,model="SAR") #SAR model
y <- sim_binomial_probit(W,X,beta,rho,model="SEM") #SEM model

d <- as.data.frame(cbind(y, X))


W_map <- raster(as.matrix(W))
plot(W_map)

values(W_map) <- y
# M <- generate_W(n,nneigh,seed=1)
# lambda <- -0.5
# y <- sim_binomial_probit(W,X,beta,rho,model="SARAR",M=M,lambda=lambda) #SARAR 

# ANALYSIS --------------------------------------------------------------------

mod <- SpatialProbitFit(y ~ X1 + X2, data = d, W = W, DGP = 'SEM',
                        method = 'conditional', varcov = 'varcov')

summary(mod)


nb <- mat2listw(W)

# MAP TESTING -----------------------------------------------------------------

library(spdep)
data(nc.sids)

# function converts nb object to a data.frame
nb_to_df <- function(nb, coords){
  x <- coords[, 1]
  y <- coords[, 2]
  n <- length(nb)
  
  cardnb <- card(nb)
  i <- rep(1:n, cardnb)
  j <- unlist(nb)
  return(data.frame(x=x[i], xend=x[j],
                    y=y[i], yend=y[j]))
}

# create distance-based neighbors
sids_nb <- dnearneigh(sidscents, d1=0, d2=80, longlat=T)
nb_df <- nb_to_df(sids_nb, sidscents)

# create data frame of coordinates
coord_df <- data.frame(sidscents)
names(coord_df) <- c("lon", "lat")

# plot results with ggmap
library(ggmap)
basemap <- get_map("North Carolina", zoom=6)

ggmap() +
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend),
               data=nb_df) +
  geom_point(aes(x=lon, y=lat), data=coord_df) +
  ylab("Latitude") +
  xlab("Longitude")

# https://www.r-bloggers.com/plotting-spatial-neighbors-in-ggmap/
#




# BAYESIAN PROBIT -------------------------------------------------------------

# https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html

mod <- "data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0,upper=1> y[N];
}
parameters {
  real alpha;
  real beta;
}
model {
   y ~ bernoulli(Phi_approx(alpha + beta * x));
}"

library(rstan)

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

setwd('c:/users/logan/googledrive/umn/research/ra_john/event_data_project')
fit <- stan(file = 'scripts/8schools.stan', data = schools_dat)

  # https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started


# https://discourse.mc-stan.org/t/running-rstan-with-latest-r-3-6-0-fails-with-error-in-compilecode/8632/21


cat('Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")',
     file = file.path(Sys.getenv("HOME"), ".Rprofile"),
     sep = "\n", append = TRUE)

file.path(Sys.getenv("HOME"), ".R", "Makevars")


# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR)) dir.create(dotR)
# M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
# if (!file.exists(M)) file.create(M)
# cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
#     if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else 
#       if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=corei7 -mtune=corei7" else
#         "CXX14FLAGS += -fPIC",
#     file = M, sep = "\n", append = TRUE)

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) 
  dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) 
  file.create(M)
cat("\nCXX14FLAGS=-O3 -Wno-unused-variable -Wno-unused-function",
    "CXX14 = $(BINPREF)g++ -m$(WIN) -std=c++1y",
    "CXX11FLAGS=-O3 -Wno-unused-variable -Wno-unused-function",
    file = M, sep = "\n", append = TRUE)
# SAVE ------------------------------------------------------------------------
# save()
# rm(list = ls())