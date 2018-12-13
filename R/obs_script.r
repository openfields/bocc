source('./R/spselect.r')
spselect(code = "EATO", df = d17)

str(SP.EATO)
SP.EATO$D30[is.na(SP.EATO$D30)] <- 0
SP.EATO$D30_60[is.na(SP.EATO$D30_60)] <- 0
apply(SP.EATO[,12:13],1,sum) -> in60
#dim(as.data.frame(in60))
which(in60>0) -> nearobs
SP.EATO[nearobs,2:11]
