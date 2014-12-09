sim <- function(x, nrep, with.replacement=TRUE, p=1/20) {
  ## First, focus on cases where we have a valid estimate of the
  ## fraction of species that are woody (i.e., at least one known
  ## species).
  ok <- !is.na(x$p)

  w <- matrix(NA, nrow(x), nrep)

  ## A: genera with any known species
  if (with.replacement)
    w[ok,] <- x$W[ok] + rbinom(sum(ok), x$N[ok]-x$K[ok], x$W[ok]/x$K[ok])
  else
    w[ok,] <- t(sapply(which(ok), function(i)
                       rhyper2(nrep, x$H[i], x$W[i], x$N[i])))

  ## B: genera with no known species
  n.unk <- sum(!ok)
  w[!ok,] <- apply(w[ok,,drop=FALSE] / x$N[ok], 2, function(y)
                   rbinom(n.unk, x$N[!ok], quantile(y, runif(n.unk))))
  
  rownames(w) <- x$genus

  summarise_sim(w, x[c("order", "family", "genus",
                       "W", "V", "H", "N", "K")])
}


## Named to avoid a conflict with dplyr::summarise
summarise1 <- function(x, p=1/20) {
  structure(c(mean(x), quantile(x, c(p/2, 1-p/2))),
            names=c("mean", "lower", "upper"))
}

summarise_sim <- function(w, info) {
  
  order <- info$order[[1]]

  info.cols <- c("W", "V", "H", "N", "K")

  ## Genus is easy;
  w_g <- cbind(info, t(apply(w, 1, summarise1)))

  ## Family is a pain:
  w_f <- do.call(rbind,
                 lapply(split(as.data.frame(w), info$family), colSums))
  w_f <- t(apply(w_f, 1, summarise1))
  w_f <- data.frame(order=order,
                    aggregate(info[info.cols], info["family"], sum),
                    w_f, stringsAsFactors=TRUE)
  rownames(w_f) <- NULL
  
  ## Order is easy; we are guaranteed to have just one order here, so:
  w_o <- data.frame(order=order,
                    as.data.frame(t(colSums(info[info.cols]))),
                    t(summarise1(colSums(w))), stringsAsFactors=FALSE)

  ret <- list(genus=w_g, family=w_f, order=w_o)
  attr(ret, "total") <- colSums(w)
  ret
}

rhyper2 <- function(nn, s0, s1, xn, fraction=FALSE) {
  x1 <- seq(s1, xn - s0)
  x0 <- xn - x1
  p1 <- dhyper(s1, x1, x0, s0+s1)
  p1 <- p1 / sum(p1)
  x1[sample(length(p1), nn, TRUE, p1)]
}

do_simulation <- function(dat_g, with.replacement) {
  nrep <- 1000
  set.seed(1) # for absolute reproducibility
  f <- function(level) {
    ret <- do.call(rbind, lapply(res, "[[", level))
    rownames(ret) <- NULL
    ret[c("p_mean", "p_lower", "p_upper")] <-
      ret[c("mean", "lower", "upper")] / ret[["N"]]
    ret
  }

  res <- lapply(split(dat_g, dat_g$order),
                sim, nrep, with.replacement)
  total <- rowSums(sapply(res, attr, "total"))
  overall <- summarise1(total)
  overall_p <- overall / sum(dat_g$N)

  list(genus=f("genus"), family=f("family"), order=f("order"),
       overall=overall, overall_p=overall_p, total=total)
}

do_simulation_b <- function(dat_g) {
  do_simulation(dat_g, TRUE)
}
do_simulation_h <- function(dat_g) {
  do_simulation(dat_g, FALSE)
}
