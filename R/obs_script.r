read.csv('./data/wilcox2017.csv', header=TRUE) -> d17
library(dplyr)
source('./R/spselect.r')
spselect(code = "EATO", df = d17)

str(SP.EATO)
SP.EATO$D30[is.na(SP.EATO$D30)] <- 0
SP.EATO$D30_60[is.na(SP.EATO$D30_60)] <- 0
apply(SP.EATO[,12:13],1,sum) -> in60
#dim(as.data.frame(in60))
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


