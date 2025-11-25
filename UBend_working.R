##U-Bend v2 with fixed swap
#David Robert Grimes, July 2025

writeit <- 0 #make 1 if you want the output in excel format.

#Use this function if you want to find a particular extreme U value. Otherwise, use #SIMRANK if you're just
#producing an array of p-values! 

simulate_ranks <- function(n1, n2, U_target, max_iter = 100000) {
  total_ranks <- 1:(n1 + n2)
  R1_target <- U_target + n1 * (n1 + 1) / 2

  for (i in 1:max_iter) {
    group1_ranks <- sort(sample(total_ranks, n1, replace = FALSE))
    if (sum(group1_ranks) == R1_target) {
      group2_ranks <- setdiff(total_ranks, group1_ranks)
      return(list(
        group1_ranks = group1_ranks,
        group2_ranks = group2_ranks,
        U = U_target
      ))
    }
  }
  stop("Failed to find a matching rank configuration within iteration limit.")
}




#Simrank is quicker for making arrays - use this generally! 

simrank <- function(n1, n2, U_target, max_iter = 100000) {
  total_ranks <- 1:(n1 + n2)
  R1_target <- U_target + n1 * (n1 + 1) / 2
  
  for (i in 1:max_iter) {
    group1_ranks <- sort(sample(total_ranks, n1, replace = FALSE))
    if (sum(group1_ranks) == R1_target) {
      group2_ranks <- setdiff(total_ranks, group1_ranks)
      return(list(
        group1_ranks = group1_ranks,
        group2_ranks = group2_ranks,
        U = U_target
      ))
    }
  }
  
  # Return NA instead of stopping with an error
  return(NA)
}


#Here enter in your n1 and n2
n1 <- 18
n2 <- 60

#Expected stats (Ties and no ties)
uexp <- (n1*n2)/2
seu <- sqrt(n1*n2*(n1+n2+1)/12)
seu_tie <- sqrt(n1*n2*(n1+n2+1)/12 - n1*n2*(7)/(12*(n1 + n2)*(n1 + n2 - 1)) )

#range of p-values of interest
pvals <- seq(0.01, 0.10, by = 0.005)
zscores <- qnorm(1 - pvals/2)

#conversion (no ties)
upred <- seu*zscores + uexp  #has higher max value
upredtie <- seu_tie*zscores + uexp #has lower min value 
vals <- seq(floor(upredtie[length(upredtie)]),ceiling(upred[1]) , by = 0.5)
exactval <- rep(NA_real_, length(vals))
spssval <- rep(NA_real_, length(vals))




#Let it run 
i <- 1
while (i <= length(vals)) {
  k <- vals[i]
  j <- simrank(n1, n2, U_target = round(k))

  # Skip iteration if simulation failed
  if (is.na(j[1])) {
    exactval[i] <- NA
    spssval[i] <- NA
    i <- i + 1
    next
  }

  # If k is an integer
  if (round(k) == k) {
    m1 <- wilcox.test(j$group1_ranks, j$group2_ranks)
    m2 <- wilcox.test(j$group1_ranks, j$group2_ranks, correct = FALSE, exact = FALSE)
  } else {
    # Adjust one value to simulate a fractional rank
    w <- which(diff(j$group2_ranks) == 2)[1]
    if (!is.na(w)) {
      mv <- j$group2_ranks[w]
      j$group2_ranks[w] <- mv + 0.5

      w2 <- which(j$group1_ranks == mv + 1)
      if (length(w2) > 0) {
        j$group1_ranks[w2] <- mv + 0.5
      }
    }

    m1 <- wilcox.test(j$group1_ranks, j$group2_ranks)
    m2 <- wilcox.test(j$group1_ranks, j$group2_ranks, correct = FALSE, exact = FALSE)
  }

  exactval[i] <- m1$p.value
  spssval[i] <- m2$p.value

  i <- i + 1
}

n1v <- rep(n1,i-1)
n2v <- rep(n2,i-1)

df <- data.frame("U_Values" = vals, "P_Exact" = exactval, "Approx P" = spssval)
df$Diff <- abs(df$Approx.P - df$P_Exact)
df$n1 <- n1v
df$n2 <- n2v

#Optional - extract to EXCEL

if(writeit == 1){

library(openxlsx)

# Create a new workbook
wb <- createWorkbook()
addWorksheet(wb, "P-Value table")
writeData(wb, "P-Value table", df)

filename <- "P-value Table.xlsx"

# Save workbook
saveWorkbook(wb, filename, overwrite = TRUE)
}



