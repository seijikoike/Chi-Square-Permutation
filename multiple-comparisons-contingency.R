chi.perm <- function(data, trials){
  N <- sum(data)
  r <- nrow(data)
  c <- ncol(data)
  
  expected.counts.obs <- as.data.frame(matrix(NA, nrow = r, ncol = c))
  
  for(i in 1:r){
    expected.counts.obs[i,] <- rowSums(data)[i] * colSums(data) / N
  }
  
  chi.obs <- sum((data-expected.counts.obs)^2 / expected.counts.obs)
  #Calculating the observed chi-square statistic
  
  
  chis <- rep(0,trials)
  for(i in 1:trials){
    lab <- vector() #Sets labels for the cases according to column
    
    for(k in 1:c){
      lab <- append(lab, rep(colnames(data)[k], colSums(data)[k]))
    }
    
    p.data <- as.data.frame(matrix(0, nrow = r, ncol = c))
    #creates an empty permutation matrix
    
    for(j in 1:r){
      samp <- sample(lab, rowSums(data)[j])
      
      for(l in 1:c){
        p.data[j,l] <- c(sum(samp == colnames(data)[l]))
      }
      #Assigns number to column and row
      
      lab <- vector()
      for(g in 1:c){
        lab <- append(lab, rep(colnames(data)[g], 
                               colSums(data)[g]-colSums(p.data)[g]))
      }
    }
    #This ensures the integrity of row and column sums are same throughout
    
    expected.counts <- as.data.frame(matrix(NA, nrow = r, ncol = c))
    
    for(m in 1:r){
      expected.counts[m,] <- rowSums(p.data)[m] * colSums(p.data) / N
    }
    chis[i] <- sum(((p.data-expected.counts)^2) / expected.counts)
  }
  p <- sum(chis >= chi.obs)/trials
  return(p)
}

M <- as.data.frame(matrix(sample(1:10, 4, replace = TRUE), nrow = 2, ncol = 2))
