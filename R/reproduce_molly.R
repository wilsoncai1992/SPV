library(ggplot2)
library(ggpubr)

estimate_variance <- function(x, order) {
  variance_n <- var(x)
  acf_n <- acf(x, lag.max = order, plot = FALSE)
  acf_fitted <- acf_n$acf[,1,1]
  return(2 * sum(acf_fitted) * variance_n - variance_n)
}
compute_one_gg <- function(n, order_grid = 0:20) {
  x <- arima.sim(model = list(ma = beta), n = n)
  df_ls <- list()
  for (order in order_grid) {
    sigma_n <- estimate_variance(x, order = order)
    df_ls <- c(df_ls, list(data.frame(order = order, sigma_n = sigma_n)))
  }
  df_ls <- do.call(rbind, df_ls)
  gg <- ggplot(df_ls, aes(x = order, y = sigma_n)) +
    geom_line() +
    geom_hline(yintercept = sigma_true, lty = 3) +
    ggtitle(paste("MA order =", length(beta))) +
    theme_bw()
  return(gg)
}

beta_list <- list(
  c(),
  c(.9),
  c(.9, .5, .1),
  c(.9, .7, .5, .3, .1),
  seq(.9, .1, length.out = 10),
  seq(.9, .1, length.out = 20),
  seq(.9, .1, length.out = 50),
  seq(.9, .1, length.out = 1e2),
  seq(.9, .1, length.out = 5e2)
)
for (beta in beta_list) {
  sigma_w <- 1
  variance <- (sigma_w ^ 2) * (1 + sum(beta ^ 2))
  if (length(beta) == 0) {
    acf_true <- 1
  } else {
    acf_true <- ARMAacf(ma = beta, lag.max = 1e2)
  }
  sigma_true <- 2 * sum(acf_true) * variance - variance
  sigma_true

  n <- 1e3
  gg_ls <- list()
  for (i in 1:10) {
    gg_ls <- c(
      gg_ls,
      list(compute_one_gg(n = n, order_grid = 0:1e2))
    )
  }
  panel1 <- ggarrange(plotlist = gg_ls)
  ggpubr::ggexport(
    gg_ls,
    filename = paste("./order_", length(beta), ".pdf", sep = ""),
    width = 5,
    height = 5
  )
}


# out <- lapply(1:10000, function(i) mean(arima.sim(model=list(ma=beta), n=n)))
# hist(unlist(out))
# var(unlist(out)) * n

