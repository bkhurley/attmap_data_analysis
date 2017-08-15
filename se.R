# calculate standard error of the mean
#
# 08 OCT 2015 - BH

se <- function(x) {
        se.out <- sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
        return(se.out) # return SE value as output
}