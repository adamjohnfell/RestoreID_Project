
#################################################################################################

#########################
###                   ###
### Set-up - Packages ###
###                   ###
#########################

### Covariate packages

#download worldclim data
library(geodata) 
#download reforestation and deforestation data
library(gfcanalysis) 

### Spatial mapping and plotting packages

library(ggplot2)
library(rasterVis)
library(maptools)
library(maps)
library(terra)
library(plyr)
library(dplyr)
library(raster)
library(rgeos)
library(rgdal)
library(sf)
library(tmap)
library(doParallel)
library(foreach)
library(coda)

### Spatial modeling packages

#generation of background points biased on population density
library(seegSDM)      
library(dismo)
#MaxENT
library(rJava)        
library(hSDM)

#################################################################################################

#################################################################################
###                                                                           ###
### Ensemble model - binomial regression with hierarchical Bayesian framework ###
###                                                                           ###
#################################################################################

### Stack covariates

deforestation <- raster("treeloss.tif")
ppt <- raster("ppt_m.tif")
tmin <- raster("tmin_m.tif")
pop <- raster("pop.tif")
alt <- raster("alt.tif")
mining <- raster("mining.tif")
mining  <- resample(mining, ppt,  method="bilinear")
mammal <- raster("all_mammals.tif")
mammal  <- resample(mammal, ppt,  method="bilinear")
cropland <- raster("cropland.tif")
acc_city <- raster("acc_50k.tif")
acc_city  <- resample(acc_city, ppt,  method="bilinear")
bushmeat <- raster("bushmeat_global.tif")
landuse <- raster("lulc.tif")
raster_list= list(deforestation, ppt,tmin,pop,alt,
                  mining,mammal, cropland, acc_city,
                  bushmeat,landuse)
extents <- lapply(raster_list,extent)
print(extents)
covars= stack(deforestation, ppt,tmin,pop,alt,
              mammal, cropland, acc_city,
              bushmeat,landuse)
names(covars) = c("deforestation", "ppt","tmin","pop","alt",
                  "mammal", "cropland","acc_city",
                  "bushmeat","landuse")

### Extract data

obs.data <- read.csv("coords_litreview.csv", header = TRUE)
obs.data <- na.omit(obs.data)
coordinates(obs.data)<-~x+y
p <- obs.data
presence = p@coords
presvals <- raster::extract(covars, presence, cellnumber=TRUE)
prevals_coords =cbind(presence,presvals)

### Create the pseudo-absences

pop_d <- raster("pop_density.tif")
Sys.setenv(GITHUB_PAT = "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")

GITHUB_PAT="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"
remotes::install_version("rgeos", version = "0.6-4")
remotes::install_version("rgdal", version = "1.6-7")

remotes::install_github("SEEG-Oxford/seegSDM")
set.seed(2024)
bg <- bgSample(pop_d,
               n = 1000,
               prob = TRUE,
               replace = TRUE,
               spatial = FALSE)

colnames(bg) <- c('x', 'y')
absence<- data.frame(bg)
absvals <- raster::extract(covars, absence, cellnumber=TRUE)
absvals_coords =cbind(absence,absvals)
pb <- c(rep(1, nrow(prevals_coords)), rep(0, nrow(absvals_coords)))
pa <- data.frame(cbind(pb, rbind(prevals_coords, absvals_coords)))

### Extract environmental values and cell number for observations

pa$Presences <- pa$pb
pa$Trials <- c(1)

### Omit rows with missing data

pa.cc=na.omit(pa)

### Normalized continuous covariates

pa.norm <- pa.cc
Mean <- vector()
Sd <- vector()
for (i in c(5:14)) {
  m <- mean(pa.cc[,i],na.rm=TRUE)
  s <- sd(pa.cc[,i],na.rm=TRUE)
  Mean <- c(Mean,m)
  Sd <- c(Sd,s)
  pa.norm[,i] <- (pa.cc[,i]-m)/s
}
### Data-frame with mean and sd for each variable

df.mean.sd <- as.data.frame(rbind(Mean,Sd))
names(df.mean.sd) <- names(pa.norm)[c(5:14)]

### Raster stack for predictions (with normalized covariates)

env <- covars

for (i in c(5:14)) {
  var.name <- names(pa.norm)[i] ## Variable name
  w <- which(names(env)==var.name) ## Position in the stack 
  m <- df.mean.sd[1,var.name] ## Mean
  s <- df.mean.sd[2,var.name] ## Sd
  orig <- values(subset(env,w)) ## Original values
  trans <- (orig-m)/s ## Transformed values
  env[[w]][] <- trans
}

### Select only grid cells with no NA

env.df.pred <- as.matrix(env)
w <- complete.cases(env.df.pred) ## Note: will be used to obtain the cell identifier for predictions in iCAR model
env.df.pred.complete <- as.data.frame(env.df.pred[w,])

### Make a cluster for parallel MCMCs

nchains <- 2
ncores <- nchains ## One core for each MCMC chains
cores<-detectcores()
clust <- makeCluster(2)
registerDoParallel(clust)

### Starting values and random seed

seed <- 1234
set.seed(seed)
beta.start <- runif(nchains,-1,1)
gamma.start <- runif(nchains,-1,1)
Vrho.start <- runif(nchains,0,10)
seed.mcmc <- round(runif(nchains,0,1e6))
pa.norm$Trials <- c(1)
pa.norm$Presences <- pa.norm$pb

#########################################################
###                                                   ###
### 5.2. Binomial with iCAR (spatial autocorrelation) ### 
###                                                   ###
#########################################################

### Landscape and neighbors

ncells <- ncell(covars)
neighbors.mat <- adjacent(covars, cells=c(1:ncells), directions=8, pairs=TRUE, sorted=TRUE)
n.neighbors <- as.data.frame(table(as.factor(neighbors.mat[,1])))[,2]
adj <- neighbors.mat[,2]
cells.pred <- which(w) ## Vector indicates the cells with environmental information (without NA)

### Binomial icar model
### hSDM model using Binomial icar for perfect detection

mod.binomial.icar <- foreach (i=1:nchains, .packages="hSDM") %dopar% {
  mod <- hSDM.binomial.iCAR(presences=pa.norm$Presences,
                            trials=pa.norm$Trials,
                            suitability=~ deforestation+ ppt+tmin+pop+alt+
                              mammal+ cropland+ acc_city+
                              bushmeat+landuse,
                            data=pa.norm,
                            ## Spatial structure
                            spatial.entity=pa.norm$cells,
                            n.neighbors=n.neighbors,
                            neighbors=adj,
                            suitability.pred=env.df.pred.complete,
                            spatial.entity.pred=cells.pred,
                            burnin=1000,
                            mcmc=1000, thin=5,
                            beta.start=beta.start[i],
                            Vrho.start=Vrho.start[i],
                            ## Priors
                            priorVrho="Uniform",
                            mubeta=0, Vbeta=1.0E6,
                            Vrho.max=10,
                            seed=seed.mcmc[i], verbose=1,
                            save.p=1) ## save the post. distributions for each pixel to calculate uncertainty
  return(mod)
}

### Extract list of MCMCs from output

binomial.icar.mcmc <- mcmc.list(lapply(mod.binomial.icar,"[[","mcmc"))

### Outputs summary

bionomial.icar.stat <- summary(binomial.icar.mcmc)$statistics
sink(file="binomial.icar_mcmc_summary.txt")
summary(binomial.icar.mcmc)
cat(rep("\n",3))
gelman.diag(binomial.icar.mcmc)
sink()

bionomial.icar.stat <- summary(binomial.icar.mcmc)$statistics

### Deviance

deviance.bionomial.icar <- bionomial.icar.stat["Deviance","Mean"]

### Plot trace and posterior distributions

pdf("bionomial.icar_mcmc_trace.pdf")
plot(binomial.icar.mcmc)
dev.off()

### Spatial random effects

rho <- subset(covars,1) ## create a raster
values(rho) <- mod.binomial.icar[[1]]$rho.pred
pdf(file="binomial.iCAR_random_effects.pdf")
plot(rho)
dev.off()

### Prediction on the landscape

prob.p.b <- subset(covars,1) ## create a raster for predictions
values(prob.p.b)[w] <- apply(mod.binomial.icar[[1]]$theta.pred,2,mean) ## assign predicted values
#values(prob.p.b)[w] <- mod.binomial.icar[[1]]$theta.pred
values(prob.p.b)[!w] <- NA ## set NA where no environmental data

### Plot the predictions

plot(prob.p.b)
plot(pa.norm[pa.norm$pb==0,],pch=".",col=grey(0.5),add=TRUE)
plot(pa.norm[pa.norm$pb>0,],pch=3,add=TRUE)

### Export the results as GeoTIFF

writeRaster(prob.p.b,filename="binomial_icar_pred_LIT.tif",overwrite=TRUE)

### Matrix with CI for each pixel

#prob.p.quant <- apply(mod.binomial.icar[[1]]$theta.pred,2,quantile, c(0.025,0.975))
#prob.p.mean <- apply(mod.binomial.icar[[1]]$theta.pred,2,mean)
prob.p.sd <- apply(mod.binomial.icar[[1]]$theta.pred,2,sd)

### Map uncertainty

prob.p.stdev <- subset(covars,1)

### Assign values

values(prob.p.stdev)[w]<- prob.p.sd
uncertainty <- prob.p.stdev
plot(uncertainty)

writeRaster(uncertainty,filename="uncertainty.tif",overwrite=TRUE)

#################################################################################################

###################
###             ###
### NS Map code ###
###             ###
###################

### Defining weights

Wmammala <- raster("covars_5k/all_mammals.tif")
Wpop <- raster("covars_5k/pop_density.tif")

### Load uncertainty raster

uncertainty <-  raster("uncertainty2.tif")
e<- raster::extent(uncertainty)
Wpop <- crop(Wpop, e)
Wpop <- resample(Wpop, uncertainty)
Wmammala <- resample(Wmammala, uncertainty)
#Wpop_log <- log10(Wpop)
#plot(Wpop_log)
#pop_log[Wpop_log==-Inf] <- NA

### Calculate necessity for additional surveillance (standardized from 0 to 1)

NS <- (uncertainty *Wpop*Wmammala)
NS <-climateStability::rescale0to1(NS)
NS<- (NS*20)/10
plot(NS)
writeRaster(NS, "NS.tif",format = 'GTiff', overwrite = T)

### Loop following for 100 (4*100) iterations

idx = which.max(NS)
xy1 = xyFromCell(NS,idx)
colnames(xy1) <- c('x', 'y')
xy1 <- data.frame(xy1)
coordinates(xy1) <-~x+y
extract(NS, xy1@coords, buffer=100000, fun=mean)
extract(NS, xy1@coords)
b = circles(xy1, d=100000, lonlat=T)
m <- polygons(b)
b1 = circles(xy1, d=75000, lonlat=T)
m1 <- polygons(b1)
b2 = circles(xy1, d=50000, lonlat=T)
m2 <- polygons(b2)
b3 = circles(xy1, d=25000, lonlat=T)
m3 <- polygons(b3)
d1 <- gDifference(m, m1)
d2 <- gDifference(m1, m2)
d3 <- gDifference(m2, m3)


r <- rasterize(d1, NS, mask = TRUE)
r<- r*1
NS <-merge(r,NS)

r1 <- rasterize(d2, NS, mask = TRUE)
r1 <-r1*0.75
NS <-merge(r1, NS)

r2 <- rasterize(d2, NS, mask = TRUE)
r2 <-r2*0.50
NS <-merge(r2,NS)

r3 <- rasterize(d3, NS, mask = TRUE)
r3 <- r3*0.25
NS <-merge(r3,NS)

r4 <- rasterize(m3, NS, mask = TRUE)
r4 <- r4*0
NS <-merge(r4,NS)
xy1@coords














































