# from Lipsey et al. 2017, Diversity and Distributions
# https://onlinelibrary.wiley.com/doi/abs/10.1111/ddi.12567

###### Appendix S3: Annotated code for spatially hierarchical models in Program R ###### using R2Jags

###### Spatially hierarchical multi-scale habitat selection model with four nested
###### levels (broad, intermediate, fine, survey)

###### Created by Dr. Marisa Lipsey and Dr. Josh Nowak
###### Wildlife Biology Program, the University of Montana
###### August 3, 2015

##### Workspace and data preparation
setwd()
require (R2jags)
require (raster)
require (rgdal)


#### Step 1: Prepare species occurrence data ####

occ.data <- read.csv(“file path”,header=TRUE,sep=",", na.strings = "NA")# Read in data 

# NOTE: occurrence data must be formatted in a hierarchical, spatially-replicated 
# fashion. It should include a unique ID for broad-scale units (broad.id), and a 
# repeating id for units at successively finer scales (intermediate.id; fine.id). See 
# Table S3.1, example occurrence data and Figure S3.1 hypothetical hierarchical 
# landscape.

sapply(occ.data, data.class) 			# Checks that all variables are numeric
summary(is.na(occ.data)) 				# Checks for missing values

## Set up dimension variables
broad.names <- levels(as.factor(occ.data$broad.id)) # List of unique broad-scale units
broad = length(broad.names) 		# Number of broad-scale units in study area
intermediate = 4 		# Number of intermediate-scale units per broad-scale unit
fine = 4 			# Number of fine-scale units per intermediate-scale unit
survey = 3 			# Maximum number of surveys per fine-scale unit

broad*intermediate*fine*survey ; nrow(occ.data) 	# these numbers should be the same!

## Format hierarchically-nested multi-scale arrays for occurrence of a single species
Species <- "species1" # Specifies column name for species in occurrence data matrix

# Create four-dimensional array for survey-level occupancy
Z <- array(occ.data[,paste(species)],dim=c(broad,intermediate,fine,survey)) 

# Create three-dimensional array for fine-scale occupancy
R <-	tapply(occ.data[,paste(species)], INDEX=list(occ.data$broad.id, 
                                                  occ.data$intermediate.id,occ.data$fine.id), FUN=max, na.rm=TRUE)
R[R=="-Inf"]<-"NA" 				# Replace “-Inf” values with missing data
class(R) <- "numeric"			# Set data class to numeric

# Create two-dimensional matrix for intermediate- and broad-scale occupancy
Y <-	tapply(occ.data[,paste(species)], INDEX=list(occ.data$broad.id, 
                                                  occ.data$intermediate.id), FUN=max, na.rm=TRUE)
Y[Y=="-Inf"]<-"NA"				# Replace “-Inf” values with missing data
class(Y) <- "numeric"			# Set data class to numeric

#### Step 2: Prepare habitat covariate data ####

# Read in original raster data for habitat covariates
var1 <- raster("file path variable 1 raster")
var2 <- raster("file path variable 2 raster")
# Read in template nested hierarchical grids (formatted as raster grids in ArcGIS)
broad= raster("file path broad grid")
intermediate = raster("file path intermediate grid")
fine= raster("file path fine grid")

# Define cell sizes
broad_cell	<- 38624.16 		# Cell size of broad-scale grid
int_cell	<- 9656.06		# Cell size of intermediate-scale grid
fine_cell	<- 1609.34		# Cell size of fine-scale grid
var1_cell	<- 30			# Cell size of original habitat variable 1
var2_cell	<- 231.6564 		# Cell size of original habitat variable 2

### Broad-scale variable resampling: 
cell_factor <-(broad_cell/var1_cell)   
#numerator is cell size of new, denominator is cell size of original in m
var1_broad <- aggregate(var1, fact=cell_factor, fun=mean, na.rm=T) # Aggregate data
var1_broad <- resample(var1_broad, broad) # Resample to align with template
writeRaster(var1_broad, filename="file path", format="GTiff", overwrite=T)

cell_factor <-(broad_cell/var2_cell)   
#numerator is cell size of new, denominator is cell size of original in m
var2_broad <-aggregate(var2, fact=cell_factor, fun=mean, na.rm=T) # Aggregate data
var2_broad <- resample(var2_broad, broad) # Resample to align with template
writeRaster(var2_broad, filename="file path", format="GTiff", overwrite=T)

### Intermediate-scale variable resampling:
cell_factor <-(int_cell/var1_cell)   
#numerator is cell size of new, denominator is cell size of original in m
var1_int <- aggregate(var1, fact=cell_factor, fun=mean, na.rm=T) # Aggregate data
var1_int <- resample(var1_int, intermediate) # Resample to align with template
writeRaster(var1_int, filename="file path", format="GTiff", overwrite=T)

cell_factor <-(int_cell/var2_cell)   
#numerator is cell size of new, denominator is cell size of original in m
var2_int <- aggregate(var2, fact=cell_factor, fun=mean, na.rm=T) # Aggregate data
var2_int <- resample(var2_int, intermediate) # Resample to align with template
writeRaster(var2_int, filename="file path", format="GTiff", overwrite=T)

# Section resampling
cell_factor <-(fine_cell/var1_cell)   
#numerator is cell size of new, denominator is cell size of original in m
var1_fine <- aggregate(var1, fact=cell_factor, fun=mean, na.rm=T) # Aggregate data
var1_fine <- resample(var1_fine, fine) # Resample to align with template
writeRaster(var1_fine, filename="file path", format="GTiff", overwrite=T)

cell_factor <-(fine_cell/var2_cell)   
#numerator is cell size of new, denominator is cell size of original in m
var2_fine <- aggregate(var2, fact=cell_factor, fun=mean, na.rm=T) # Aggregate data
var2_fine <- resample(var2_fine, fine) # Resample to align with template
writeRaster(var2_fine, filename="file path", format="GTiff", overwrite=T)

### Data extraction from raster stacks to hierarchical data frame

# Create raster stacks of variables at each scale
broad_stack <- stack(broad,var1_broad,var2_broad)
int_stack <- stack(intermediate,var1_int,var2_int)
fine_stack <- stack(fine,var1_fine,var2_fine)

# Read in point shapefile of finest-scale grid centroids
Spts <- readOGR(dsn="file path", layer="fine_centroids", pointDropZ=TRUE) 

# Extract multi-scale data to points
broad_ext <- data.frame(extract(broad_stack, spts))
int_ext <- data.frame(extract(int_stack, spts))
fine_ext <- data.frame(extract(fine_stack, spts))

data_ext <- cbind(broad_ext, int_ext, fine_ext) # Combines scales into single df
data_ext2 <- data_ext[order(data_ext$fine, data_ext$intermediate, data_ext$broad),]
# orders data frame by hierarchical grids

colnames(data_ext2)<-("broad.id", "var1_broad", "var2_broad", "intermediate.id", "var1_int","var2_int", "fine.id”,”var1_fine”,”var2_fine”)    # names columns                   
                      
                      write.csv(data_ext2, file="file path covariate data")
                      
                      #### Step 3: Format covariate data for use in model ####
                      
                      Covariates <- read.csv("file path covariate data ")	# Read in data
                      
                      ## Define and scale broad-scale covariates as one-dimensional vectors. Repeat for any 
                      ## number of covariates (two shown).
                      
                      var1_broad <- as.vector(scale(covariates[,"var1_broad"], center=TRUE))
                      var2_broad <- as.vector(scale(covariates[,"var2_broad"], center=TRUE))
                      ## Define intermediate-scale covariates as two-dimensional matrices
                      
                      var1_int <-	tapply(covariates$var1_int, INDEX=list(covariates$broad.id, 
                      covariates$intermediate.id), FUN=max, na.rm=TRUE)
                      var2_int <-	tapply(covariates$var2_int, INDEX=list(covariates$broad.id, 
                      covariates$intermediate.id), FUN=max, na.rm=TRUE)
                      
                      ## Scale intermediate-scale covariates using mean and sd.
                      
                      # Intermediate-scale variable 1
                      mean.var1_int <- mean(var1_int,na.rm=TRUE)
                      sd.var1_int <- sqrt(var(var1_int[1:length(var1_int)],na.rm=TRUE))
                      var1_int <-(var1_int-mean.var1_int)/sd.var1_int
                      
                      # Intermediate-scale variable 2
                      mean.var2_int <- mean(var2_int,na.rm=TRUE)
                      sd.var2_int <- sqrt(var(var2_int[1:length(var2_int)],na.rm=TRUE))
                      var2_int <-(var1_int-mean.var2_int)/sd.var2_int
                      
                      ## Define fine-scale covariates as three-dimensional array. Repeat for any number of 
                      ## covariates (two shown).
                      
                      var1_fine <- array(covariates[,"var1_fine"], dim=c(broad, intermediate, fine))
                      var2_fine <- array(covariates[,"var2_fine"], dim=c(broad, intermediate, fine))
                      
                      ## Scale fine-scale covariates using mean and sd.
                      
                      # Fine-scale variable 1
                      mean.var1_fine <- mean(var1_fine,na.rm=TRUE)
                      sd.var1_fine <- sqrt(var(var1_fine[1:length(var1_fine)],na.rm=TRUE))
                      var1_fine <- (var1_fine-mean.var1_fine)/sd.var1_fine
                      
                      # Fine-scale variable 2
                      mean.var2_fine <- mean(var2_fine,na.rm=TRUE)
                      sd.var2_fine <- sqrt(var(var2_fine[1:length(var2_fine)],na.rm=TRUE))
                      var2_fine <- (var2_fine-mean.var2_fine)/sd.var2_fine
                      
                      ## As an additional fine-scale covariate, this example uses the number of surveys 
                      ## conducted within each fine-scale unit.
                      
                      Nsurvey <- tapply(occ.data[,"N"], INDEX=list(occ.data$broad.id, 
                      occ.data$intermediate.id,occ.data$fine.id), FUN=max, na.rm=TRUE)
                      
                      ## Clean up and scale survey effort variable
                      nsurvey[nsurvey=="-Inf"]<-0	# Replace undefined values with zero (no surveys)
                      class(nsurvey) <- "numeric" 			# Define class as numeric
                      mean.nsurvey<-mean((nsurvey), na.rm=TRUE)
                      sd.nsurvey<-sqrt(var(nsurvey[1:length(nsurvey)], na.rm=TRUE))
                      nsurvey<-((nsurvey-mean.nsurvey)/sd.nsurvey)
                      
                      
                      #### Step 4: Specify hierarchical model in JAGS ####
                      ## Function to calculate initial values for z:
                      z.init <- function(Y, R, Z){
                      y.val <- apply(Y, 1, function(x){ ifelse(any(x == 1, na.rm = T), 1, 0) })
                      r.val <- apply(R, 1, function(x){ ifelse(any(x == 1, na.rm = T), 1, 0) })
                      z.val <- apply(Z, 1, function(x){ ifelse(any(x == 1, na.rm = T), 1, 0) })
                      out <- ifelse(y.val|r.val|z.val == 1, 1, 0)
                      return(as.numeric(out))}
                      
                      ## Set initial values
                      zz <- z.init(Y, R, Z)
                      
                      ## Create list of names for objects that will be used in model
                      data <- list ( "Y","R","Z","zz", "broad", "intermediate", "fine","survey", "var1_broad","var2_broad", "var1_int","var2_int",  "var1_fine","var2_fine", “nsurvey”)
                      
                      ## Set initial values
                      inits <- function()
                      list ( z=zz,
                      psi0=runif(1), q1= runif(1,-1,1),q2= runif(1,-1,1),q3= runif(1,-1,1),
                      theta0=runif(1),t1=runif(1,-1,1),t2= runif(1,-1,1),t3= runif(1,-1,1),
                      gy0=runif(1), s1= runif(1, -1, 1),s2= runif(1,-1,1),s3= runif(1,-1,1), s4=runif(1,-1,1),
                      pi0=runif(1))
                      
                      sink("model.txt")
                      cat("
                      model { 
                        # Set prior distributions for parameters
                        psi0 ~ dunif(0,1)      
                        q0 <- log(psi0/(1-psi0))
                        q1 ~ dunif(-10, 10)
                        q2 ~ dunif(-10,10)
                        q3 ~ dunif(-10,10)
                        
                        theta0 ~ dunif(0,1)
                        t0 <- log(theta0/(1-theta0))
                        t1 ~ dunif(-10, 10)
                        t2 ~ dunif(-10,10)
                        t3 ~ dunif(-10,10)
                        
                        gy0 ~ dunif(0,1)
                        s0<- log(gy0/(1-gy0))
                        s1  ~ dunif(-10,10)
                        s2 ~ dunif(-10,10)
                        s3 ~ dunif(-10,10)
                        s4 ~ dunif(-10,10)
                        
                        pi0 ~ dunif(0,1)
                        p0<- log(pi0/(1-pi0))
                        
                        
                        
                        
                        # Broad-scale process model with two interacting variables
                        for(i in 1:broad){
                          z[i] ~ dbern(psi[i])   
                          logit(psi[i]) <- q0 + q1*var1_broad[i] + q2*var2_broad[i] + 
                            q3*var1_broad[i]*var2_broad[i]
                          
                          # Intermediate-scale observation model with two interacting variables
                          for(j in 1:intermediate){
                            logit(theta[i,j]) <- t0 + t1*var1_int[i,j] + t2*var2_int[i,j] + 
                              t3*var1_int[i,j]*var2_int[i,j]
                            muy[i,j] <- z[i] * theta[i,j]
                            Y[i,j] ~ dbern(muy[i,j]) 
                            
                            
                            # Fine-scale observation model with additional survey effort variable (s4)
                            for (k in 1:fine){
                              logit(gy.tmp[i,j,k])<- s0 + s1*var1_fine[i,j,k] + s2*var2_fine[i,j,k] + 
                                s1*var1_fine[i,j,k] *var2_fine[i,j,k] + 
                                s4*nsurvey[i,j,k]
                              gy[i,j,k]<-Y[i,j]*gy.tmp[i,j,k]
                              R[i,j,k]~dbern(gy[i,j,k])   
                              
                              
                              # Survey-scale observation model to estimate availability for detection
                              for (l in 1:survey){
                                logit(pi.tmp[i,j,k,l])<-p0 
                                pi[i,j,k,l]<-R[i,j,k]*pi.tmp[i,j,k,l]
                                Z[i,j,k,l]~dbern(pi[i,j,k,l]) 
                              }}}}}
                      ",fill=TRUE)
                      sink()
                      
                      # Set parameters for output
                      parameters <- c("psi0","q0","q1","q2","q3", "theta0", "t0","t1","t2","t3", "gy0", "s0", "s1", "s2", "s3", "s4", "pi0", "p0")
                      
                      # Run model (potentially long run times; use fewer iterations when testing)
                      out <- jags.parallel(data,
                      inits=inits,
                      parameters,
                      "model.txt",
                      n.thin=1,
                      n.chains=3,
                      n.burnin=2000,
                      n.iter=100000)
                      
                      jag.sum<-out$BUGSoutput$summary # Calls output summary
                      write.csv(jag.sum, file="file path") # Saves output summary
                      
                      out$BUGSoutput$DIC # Calls DIC
                      
                      # To save a model object
                      save(out, file="name.Rdata")
                      