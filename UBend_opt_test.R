# change here to try different results
random_seed <- 43799

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

simulate_ranks_opt <- function(n1, n2, U_target, max_iter = 100000) {
  total_ranks <- 1:(n1 + n2)
  R1_target <- U_target + n1 * (n1 + 1) / 2

  for (i in 1:max_iter) {
    group1_ranks <- sample(total_ranks, n1, replace = FALSE, )
    if (sum(group1_ranks) == R1_target) {
      group2_ranks <- setdiff(total_ranks, group1_ranks)
      return(list(
        group1_ranks = sort(group1_ranks),
        group2_ranks = group2_ranks,
        U = U_target
      ))
    }
  }
  stop("Failed to find a matching rank configuration within iteration limit.")
}

# unoptimized

set.seed(random_seed)
start.time <- Sys.time()

simulate_ranks(10, 12, 7)

end.time <- Sys.time()
time.taken <- end.time - start.time
unopt_time <- time.taken

#optimized

set.seed(random_seed)
start.time <- Sys.time()

simulate_ranks_opt(10, 12, 7)

end.time <- Sys.time()
time.taken <- end.time - start.time
opt_time <- time.taken

# display difference in runtime
as.double(unopt_time) / as.double(opt_time)
