#' @title Cut gaps from inundation time/dry time periods
#' @name CutExtendedGaps
#' @param tij water level time series
#' @param gaps Dataframe generated with \code{\link{gapsts}} containing gaps in waterlevel time series
#' @param ivals Intervals that have to be corrected for gaps
#' @export CutExtendedGaps


CutExtendedGaps <- function(tij= tij, gaps=gaps, ivals = DTs){
  
    # run over intervals 'ivals' (eg. DTs or ITs) and remove intervals in gaps
    
    # lookup starttimes of gaps-1N and endtimes of gaps+1N
    # starttimes of gaps-1N:
    gmin1 <- tij[is.element(tij$N, replace(gaps$N-1, gaps$N-1==0,1)),] # all GapsN-1 phases  (if N-1 = 0, replace by 1)
    t1 <- lapply(unique(gmin1$N), function(x){min(gmin1$time[gmin1$N==x])}) # use lapply, otherwise POSIXct is converted to numeric/char
    # endtimes of  gaps+1N
    gplus1 <- tij[is.element(tij$N,gaps$N+1),] # all GapsN+1 phases 
    t2 <- lapply(unique(gplus1$N), function(x){max(gplus1$time[gplus1$N==x])})
    
    # 3 possible scenario's while looping over the extended gap times:
    # A. extended gap is totally within interval-> make two intervals after cutting by extended gap time.
    # B. extended gap is partially or fully overlapping interval -> check if it's start or endtime, and if so replace start/endtime by end/starttime of extended gap.
    # if extended gap is fully overlapping interval, ivals$t1 will take gap$t2 and ivals$t2 will take gap$t2, and dt will be negative.
    # C. extended gap is not part of interval -> nothing happens
    
    for (i in 1:length(t1)){ # use for-loop to loop over gaps, because gaps can adjust DTs/ITs consecutively
      j <- 1
      while(j <= nrow(ivals)){ # use while-loop, because nrow might change during loop
        # A.# if extended gap is totally within interval
        if(ivals$t1[j]<t1[[i]] & t2[[i]]<ivals$t2[j]){ 
          # add additional interval and cut by extended gap time.
          #ivals 
          ivals <-  rbind(ivals[1:j-1, -3], 
                          data.frame(t1 = ivals$t1[j], t2= t1[[i]] ), # start ivals until start extended gaps
                          data.frame(t1 = t2[[i]], t2= ivals$t2[j] ), # end extended gaps until end of ivals
                          ivals[j+1:nrow(ivals), -3]) # if j is last row of ivals, NA row introduced
          ivals <- na.omit(ivals) # remove NA rows
        }
        
        # B. if extended gap is partially or fully overlapping interval
        if(t1[[i]]<=ivals$t1[j] & ivals$t1[j]<t2[[i]]) ivals$t1[j] <- t2[[i]] # if starttime is within gap+-1N time, replace starttime
        if(t1[[i]]<=ivals$t2[j] & ivals$t2[j]<t2[[i]]) ivals$t2[j] <- t1[[i]] # if endtime is within gap+-1N time, replace endtime
        
        j <- j+1
      }
    }
    
    # calculate new dt
    if (!inherits(ivals$t1,"POSIXt")){
      ivals$dt <- ivals$t2 - ivals$t1
    } else {
      ivals$dt <- difftime(ivals$t2,ivals$t1,units=attributes(gaps$dt)$units) # gaps has the units attribute
    }
    
    ivals <- ivals[!ivals$dt<=0,] # delete negative (if ivals completely within gap) or 0 dt's
    ivals$n <- 1:nrow(ivals) # give number
    
    if (!any(ivals$dt > 0)) ivals <- NULL # if no valid dt value, return NULL
  return(ivals)
} # end function
