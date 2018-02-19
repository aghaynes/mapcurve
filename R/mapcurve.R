# Functions for mapcurves


mapcurve <- function(group1, group2, area){
  
  if(sum(is.na(group1)) > 0) warning("group1 contains NAs. Those regions cannot be included in calculations")
  if(sum(is.na(group2)) > 0) warning("group2 contains NAs. Those regions cannot be included in calculations")
  
  mc <- function(g1, g2, area){
    # this is the real work horse
    L <- length(unique(g1))
    S <- numeric(length = L)
    for(I in 1:L){
      l <- unique(g1)[I]
      
      g2i <- g2[g1 == l & !is.na(g2)]
      Ti <- sum(area[g1 == l])
      
      K <- length(unique(g2i))
      s <- numeric(length = K)
      for(J in 1:K){
        j <- unique(g2i)[J]
        # j <- "BE05"
        # tmpj <- x[x[, g2] == j, ]
        Tj <- sum(area[g2 == j])
        
        c <- sum(area[g1 == l & g2 == j & !is.na(g1) & !is.na(g2)])
        s[J] <- (c/Ti)*(c/Tj)
      }
      S[I] <- sum(s)
    }
    
    S1 <- S[order(S, decreasing = TRUE)]
    return(S1)
  }
  
  S1 <- mc(group1, group2, area)
  S2 <- mc(group2, group1, area)
  
  
  s <- seq(0,1,.01)
  p1 <- numeric(length = length(s))
  p2 <- numeric(length = length(s))
  for(l in 1:length(s)){
    p1[l] <- length(S1[S1>s[l]])/length(S1)
    p2[l] <- length(S2[S2>s[l]])/length(S2)
  }
  
  aucs <- c(sum(diff(s)*zoo::rollmean(p1,2))*100, sum(diff(s)*zoo::rollmean(p2,2))*100)
  
  w <- which(aucs == max(aucs))
  
  out <- list(S = get(paste0("S", w)),
              P = get(paste0("p", w)),
              AUC = aucs[w],
              S1 = S1,
              P1 = p1,
              S2 = S2,
              P2 = p2,
              cuts = s, 
              AUCs = aucs)
  
  class(out) <- "mapcurve"
  return(out)
  
}



plot.mapcurve <- function(object, which = "max"){
  
  match.arg(as.character(which), c(1,2,"max"))
  if(as.character(which) == "max") which <- which(object$AUC == max(object$AUC)) 
  
  v <- object[[paste0("P", which)]]*100
  cuts <- object$cuts*100
  
  plot(v ~ cuts, 
       xlim = c(0,100), 
       ylim = c(0,100), 
       col = "black", 
       pch = 16, 
       xlab = "Goodness of Fit (GOF)", 
       cex = 0.65, 
       ylab = "% overlapping regions above GOF", 
       bty = "l", 
       cex.axis = 0.8, 
       asp = 1)
  lines(y = v,
        x = cuts)
  
}

