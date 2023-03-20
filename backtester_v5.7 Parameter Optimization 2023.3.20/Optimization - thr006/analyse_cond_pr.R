

dataList <- getData(directory="PART1") 

condPr <- matrix(NA,nrow=10,ncol=4)
colnames(condPr) <- c("P(U|U)","P(D|U)","P(U|D)","P(D|D)")
for (i in 1:10) {
    d <- dataList[[i]][1:500,]; len <- nrow(d)
    ud  <- as.numeric((d$Close - d$Open) > 0)
    ud2 <- as.data.frame(cbind(ud[1:(len-1)],ud[2:len]))
    nUU <- length(which(ud2$V1 + ud2$V2 == 2))
    nUD <- length(which(ud2$V1 == 1 & ud2$V2 == 0))
    nDU <- length(which(ud2$V1 == 0 & ud2$V2 == 1))
    nDD <- length(which(ud2$V1 + ud2$V2 == 0))
    cat(nUU,nUD,nDU,nDD,'\n')
    condPr[i,] <- c(nUU/sum(ud),nUD/sum(ud),
                        nDU/(len - sum(ud)),
                        nDD/(len - sum(ud)))
}
condPr <- round(condPr,2); print(condPr)

resp_to_up <- function(x, thr) {
    if (x["P(U|U)"] > thr) return(1)
    else if(x["P(D|U)"] > thr) return(-1)
    else return(0)
}
resp_to_dn <- function(x, thr) {
    if (x["P(U|D)"] > thr) return(1)
    else if(x["P(D|D)"] > thr) return(-1)
    else return(0)
}
get_params <- function(thr) { 
    lst <- vector("list", 2) # initialize list
    lst[[1]] <- apply(condPr, 1, function(x) resp_to_up(x, thr))
    lst[[2]] <- apply(condPr, 1, function(x) resp_to_dn(x, thr))
    names(lst) <- c("response_to_up", "response_to_dn")
    return(lst)
}
