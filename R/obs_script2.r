read.csv('./data/wilcox2017.csv', header=TRUE) -> d17
library(dplyr)
source('./R/spselect.r')
spselect(code = "EATO", df = d17)

str(SP.EATO)
# SP.EATO$D30[is.na(SP.EATO$D30)] <- 0
# SP.EATO$D30_60[is.na(SP.EATO$D30_60)] <- 0
# apply(SP.EATO[,12:13],1,sum) -> in60
#dim(as.data.frame(in60))
<<<<<<< HEAD
which(in60>0) -> nearobs
SP.EATO[nearobs,2:11]

# get sites that were surveyed where the species was not observed
setdiff(unique(na.omit(d17$Point)), unique(SP.EATO$Point)) -> not.obs
matrix(0, nrow=length(not.obs), ncol=3) -> nodata 
nodata <- as.data.frame(nodata)
names(nodata) <- c("Point", "Ob1", "Ob2")
nodata[,1] <- not.obs # copy in point identifier



# need to get list of sites and observers
table(d17$Point, d17$Observer) -> p.ob
table(d17$Point)

# need to melt/cast data

=======
# which(in60>0) -> nearobs  # index for observations within specified distance

for(i in 7:16){
  SP.EATO[,i] -> tmp
  tmp[is.na(tmp)] <- 0
  tmp -> SP.EATO[,i]
}

# SP.EATO[,7:11] -> tmp
# 
# tmp[is.na(tmp)] <- 0
# tmp[-nearobs,] * 0 -> tmp[-nearobs,]
# 
# SP.EATO[-nearobs,7:11] * 0 -> SP.EATO[-nearobs,7:11] 
# 
# SP.EATO


distobs <- function(sp.mat, dc){
  # function distobs: parses bird data that was recorded as being in different distance classes, recodes things to exclude observations from far away
  # input: species matrix with observations, distances, and other stuff
  # processing: get index values for stuff that was far away
  #             zero out observations that have the index values
  # output: new species matrix with observations zeroed out if they were too far away
  
  # NEED TO FIX FOR 30
#   if(dc==30){   
#     dcol <- 12
#     }
  if(dc==60){
    dcol <- 13
    }
  if(dc==90){
    dcol <- 14
    }
  if(dc==91){
    dcol <- 15
    }
  
  
  apply(sp.mat[,12:dcol],1,max) -> obs
  which(obs>0) -> nearobs
  sp.mat[-nearobs,7:11]*0 -> sp.mat[-nearobs,7:11]
  return(sp.mat)
} 

distobs(sp.mat=SP.EATO, dc=60) -> SP.EATO60

# need to join with overall list of sites
# need to get list of unique points that were surveyed
# need to write observation data into a matrix
# some possibly helpful information: https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html
# after getting the data together, melt and cast to get final matrix - row names are points, column names are observers

# join: do inner join, then do left outer join, take difference



unique(SP.EATO60$Point) -> p60
unique(na.omit(d17$Point)) -> pall
match(p60, pall) -> pmatch
length(pall) - length(pmatch) -> pext
matrix(0, nrow = pext, ncol = 5) -> exob
cbind(pall[-pmatch], exob) -> no.obs
as.data.frame(no.obs) -> no.obs

# rbind no.obs and the selected stuff from SP.EATO60
incl.ind <- c(4,7:11)
SP.EATO60[,incl.ind] -> b.obs
names(no.obs) <- names(b.obs)
rbind(b.obs, no.obs) -> obs.dat # gives observation data, need to bring along the observers and the dates too.....

# match the no.obs points with the original d17
match(no.obs$Point, d17$Point) -> d17.no.obs
>>>>>>> dffe67adf85f14f8fbf340780cce4a7ba63ead3e

