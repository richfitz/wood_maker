## Colours used throughout:
cols_methods <- c(binomial="#a63813",
                  hypergeometric="#4d697f")
cols_tree <- c(Monilophytes="#a63813",
               Gymnosperms="#21313b",
               BasalAngiosperms="#eeb911",
               Monocots="#204d14",
               Eudicots="#4d697f",
               Rest="gray15")
cols_woody <- c(Woody="#533d0c",
                Herbaceous="#799321")
cols_woody <- c(Woody="black",
                Herbaceous="white")
cols_shading <- "#eeb911"
cols_tropical <- c(tropical="#ea7518",
                   temperate="#62a184")


fig_distribution_raw <- function(res_b, res_h) {
  n_spp <- sum(res_b$order$N)
  p_b <- res_b$total / n_spp * 100
  p_h <- res_h$total / n_spp * 100
  
  r <- range(p_b, p_h)
  br <- seq(r[1], r[2], length.out=30)

  h_b <- hist(p_b, br, plot=FALSE)
  h_h <- hist(p_h, br, plot=FALSE)

  xlim <- c(42, 50)
  ylim <- range(h_b$density, h_h$density)

  cols <- cols_methods
  
  op <- par(mar=c(4.1, 4.1, .5, .5))
  on.exit(par(op))
  plot(h_b, col=cols[1], xlim=xlim, ylim=ylim, freq=FALSE, yaxt="n",
       ylab="",
       xlab="Percentage of woody species among all vascular plants",
       main="")
  box(bty="l")
  lines(h_h, col=cols[2], freq=FALSE)
  mtext("Probability density", 2, line=.5)
}

fig_distribution_raw_errors <- function(res_b,   res_h,
                                        res_b_w, res_h_w,
                                        res_b_h, res_h_h) {
  n_spp <- sum(res_b$order$N)
  res <- list(b  =res_b,   h  =res_h,
              b_w=res_b_w, h_w=res_h_w,
              b_h=res_b_h, h_h=res_h_h)
  p <- lapply(res, function(x) x$total / n_spp * 100)

  r <- range(unlist(p))
  br <- seq(r[1], r[2], length.out=30)

  h <- lapply(p, hist, br, plot=FALSE)

  xlim <- c(42, 50)
  ylim <- range(unlist(lapply(h, function(x) x$density)))

  cols <- cols_methods
  cols_fill <- mix(cols, "white", 0.5)

  op <- par(mar=c(1.1, 0.5, .5, .5), mfrow=c(2, 1), oma=c(3.1, 2, 0, 0))
  on.exit(par(op))
  plot(NA, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", bty="l",
       xlab="", ylab="", main="")
  axis(1, labels=FALSE)
  for (i in c(3, 5))
    hist_fill(h[[i]], border=cols[[1]], col=cols_fill[[1]])
  lines(h[["b"]], col=cols[1], freq=FALSE)
  label(.02, .96, 1)

  plot(NA, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", bty="l",
       xlab="", ylab="", main="")
  axis(1, labels=TRUE)
  for (i in c(4, 6))
    hist_fill(h[[i]], border=cols[[2]], col=cols_fill[[2]])
  lines(h[["h"]], col=cols[2], freq=FALSE)
  mtext("Probability density", 2, line=.5, outer=TRUE)
  mtext("Percentage of woody species among all vascular plants", 1,
        line=2.5, xpd=NA)
  label(.02, .96, 2)
}

fig_fraction_by_group <- function(res_b, res_h, dat_g, level="genus") {
  op <- par(mfrow=c(2, 1), mar=c(2, 2, .5, .5), oma=c(2, 0, 0, 0))
  on.exit(par(op))
  lwd <- 1.5

  n_br <- c(genus=50, family=40, order=30)[[level]]
  tmp <- aggregate(dat_g[c("W", "K")], dat_g[level], sum)
  tmp <- tmp[tmp$K >= 10,] # at least 10 records per group
  h <- hist(100 * tmp$W / tmp$K, n_br, plot=FALSE)

  plot(NA, xlim=c(0, 100), ylim=range(0, h$density),
       xaxt="n", yaxt="n", bty="l", xlab="", ylab="")
  mtext("Probability density", 2, line=.5)
  axis(1, tick=TRUE, label=FALSE)
  label(.02, .96, 1)
  hist_outline(h, col="black", lwd=lwd)
  
  cols <- cols_methods

  x_b <- res_b[[level]]
  x_h <- res_h[[level]]
  h.b <- hist(100*x_b$p_mean[x_b$N >= 10], n=n_br, plot=FALSE)
  h.h <- hist(100*x_h$p_mean[x_h$N >= 10], n=n_br, plot=FALSE)
  ylim <- range(h.b$density, h.h$density)
  plot(NA, xlim=c(0, 100), ylim=ylim,
       xlab="", ylab="", yaxt="n", bty="n", bty="l")
  mtext("Probability density", 2, line=.5)
  mtext(paste("Percentage of woody species in", level), 1, outer=TRUE,
        line=.5)

  hist_outline(h.b, col=cols[1], lwd=lwd)
  hist_outline(h.h, col=cols[2], lwd=lwd)
  
  legend("topleft", c("Strong prior (binomial)",
                      "Weak prior (hypergeometric)"),
         col=cols, lty=1, bty="n", cex=.85, inset=c(.1, 0), lwd=lwd)
  label(.02, .96, 2)
}

## Wrappers for maker:
fig_fraction_by_genus <- function(...) {
  fig_fraction_by_group(..., level="genus")
}
fig_fraction_by_family <- function(...) {
  fig_fraction_by_group(..., level="family")
}
fig_fraction_by_order <- function(...) {
  fig_fraction_by_group(..., level="order")
}

fig_survey_results <- function(d_survey, res_b, res_h) {
  ci <- 100*cbind(res_b$overall_p, res_h$overall_p)
  cols <- cols_methods
  cols_tr <- add.alpha(cols, .5)

  op <- par(no.readonly=TRUE)
  on.exit(par(op))

  layout(rbind(1:2), widths=c(4, 5))
  par(mar=c(6.5, 2, .5, .5), oma=c(0, 2, 0, 0))
  plot(Estimate ~ Familiarity, d_survey, col=cols_shading, axes=FALSE,
       xlab="", ylab="", bty="l",
       ylim=c(0, 100))
  axis(2, las=1)
  text(1:4, -5, levels(d_survey$Familiarity),
       srt=-55, xpd=NA, adj=c(0, NA), cex=.85)
  mtext("Estimate of percentage woodiness", 2, line=2.75)
  label(.02, .96, 1)

  usr <- par("usr")
  rect(usr[1], ci["lower",], usr[2], ci["upper",], col=cols_tr,
       border=NA)
  abline(h=ci["mean",], col=cols)

  plot(Estimate ~ Training, d_survey, col=cols_shading, axes=FALSE,
       xlab="", ylab="", bty="l", ylim=c(0, 100))
  axis(2, las=1)
  xl <- c("Postgrad","Part postgrad","Undergrad","Part undergrad", "None")
  text(1:5, -5, xl,
       srt=-55, xpd=TRUE, adj=c(0, NA), cex=.85) 
  label(.02, .96, 2) 

  usr <- par("usr")
  rect(usr[1], ci["lower",], usr[2], ci["upper",], col=cols_tr,
       border=NA)
  abline(h=ci["mean",], col=cols)
}

fig_survey_distribution <- function(d_survey, res_b, res_h) {
  op <- par(mfrow=c(2, 1), mar=c(2, 4, .5, .5), oma=c(2, 0, 0, 0))
  on.exit(par(op))
  lwd <- 1.5

  ci <- 100*cbind(res_b$overall_p, res_h$overall_p)
  hist(d_survey$Estimate, xlim=c(0, 100), las=1, col=cols_shading,
       xaxt="n", xlab="", ylab="Number of responses", main="")
  box(bty="l")
  axis(1, label=FALSE)
  label(.02, .96, 1)

  usr <- par("usr")
  rect(ci["lower",], usr[3], ci["upper",], usr[4],
       col=diversitree:::add.alpha(cols_methods, .5), border=NA)
  abline(v=ci["mean",], col=cols_methods)

  h_tropical <- hist(d_survey$Estimate[d_survey$Tropical], plot=FALSE)
  h_temperate <- hist(d_survey$Estimate[!d_survey$Tropical], plot=FALSE)

  ylim <- range(h_tropical$counts, h_temperate$counts)
  plot(NA, xlim=c(0, 100), ylim=ylim, las=1, xlab="",
       ylab="Number of responses", bty="n", bty="l")
  mtext("Estimate of percentage woodiness", 1, outer=TRUE, line=.5)

  hist_outline(h_tropical,  col=cols_tropical[1], lwd=lwd, density=FALSE)
  hist_outline(h_temperate, col=cols_tropical[2], lwd=lwd, density=FALSE)

  usr <- par("usr")
  rect(ci["lower",], usr[3], ci["upper",], usr[4],
       col=diversitree:::add.alpha(cols_methods, .5), border=NA)
  abline(v=ci["mean",], col=cols_methods)

  label(.02, .96, 2)

  legend("topright", c("Tropical", "Temperate"), lwd=lwd,
         col=cols_tropical, bty="n", cex=.75)
}

fig_variability <- function(dat_g) {
  dat_g$p_rare <- (0.5 - abs(dat_g$p - 1/2)) * 2
  dat_g$variable <- dat_g$p_rare > 0

  sub <- dat_g[!is.nan(dat_g$p),]

  ## Breaks for the moving average:
  br_N <- log_seq_range(sub$N, 20)
  br_K <- log_seq_range(sub$K, 15)

  ## Classify points:
  i_N <- findInterval(sub$N, br_N, all.inside=TRUE)  
  i_K <- findInterval(sub$K, br_K, all.inside=TRUE)  

  ## Midpoints for plotting.
  mid_N <- (br_N[-1] + br_N[-length(br_N)])/2
  mid_K <- (br_K[-1] + br_K[-length(br_K)])/2

  m_N <- tapply(sub$p_rare, i_N, mean)
  m_K <- tapply(sub$p_rare, i_K, mean)
  p_N <- tapply(sub$variable, i_N, mean)
  p_K <- tapply(sub$variable, i_K, mean)
  f_N <- tapply(sub$p, i_N, mean)
  f_K <- tapply(sub$p, i_K, mean)

  pch <- 19
  cex <- 0.5
  col <- "#00000066"

  op <- par(oma=c(4.1, 4.1, 0, 0),
            mar=c(1.1, 1.1, .5, .5),
            mfrow=c(3, 2))
  
  plot(p_rare ~ N, sub, pch=pch, cex=cex, col=col, log="x",
       axes=FALSE)
  lines(m_N ~ mid_N, col="red")
  axis(1, labels=FALSE) 
  axis(2, c(0, 1), c("Single type", "50:50"), las=1)
  mtext("Variability", 2, 3)
  box(bty="l")
  label(.02, .96, 1)

  plot(p_rare ~ K, sub, pch=pch, cex=cex, col=col, log="x",
       axes=FALSE)
  lines(m_K ~ mid_K, col="red")
  axis(1, labels=FALSE) 
  axis(2, c(0, 1), labels=FALSE)
  box(bty="l")
  label(.02, .96, 2)

  plot(variable ~ N, sub, pch=pch, cex=cex, col=col, log="x",
       bty="l", las=1, axes=FALSE)
  lines(p_N ~ mid_N, col="red")
  axis(1, labels=FALSE)
  axis(2, las=1)
  mtext("Probability genus is variable", 2, 3) 
  box(bty="l") 
  label(.02, .96, 3)

  plot(variable ~ K, sub, pch=pch, cex=cex, col=col, log="x",
       bty="l", yaxt="n", las=1, axes=FALSE)
  lines(p_K ~ mid_K, col="red")
  axis(1, labels=FALSE)
  axis(2, labels=FALSE) 
  box(bty="l") 
  label(.02, .96, 4)

  plot(p ~ N, sub, pch=pch, cex=cex, col=col, log="x",
       bty="l", las=1)
  lines(f_N ~ mid_N, col="red")
  mtext("Number of species in genus", 1, 3)
  mtext("Proportion of species woody", 2, 3)
  label(.02, .96, 5)

  plot(p ~ K, sub, pch=pch, cex=cex, col=col, log="x",
       bty="l", yaxt="n", las=1)
  lines(f_K ~ mid_K, col="red")
  axis(2, labels=FALSE)
  mtext("Number of species with known state", 1, 3)
  label(.02, .96, 6)
}

fig_graphical_abstract <- function(res_b, res_h, dat_g, d_survey) {
  p_raw <- sum(dat_g$W) / sum(dat_g$K)
  p_survey <- mean(d_survey$Estimate) / 100
  p_data <- mean(c(res_b$overall_p[["mean"]], res_h$overall_p[["mean"]]))
  
  f <- function(p, title) {
    pie(c(p, 1-p), c("Woody", "Herbaceous"), col=cols_woody)
    text(0, par("usr")[2], title, adj=c(0.5, 0), cex=1.2)
  }
  par(mfrow=c(1, 3), mar=rep(1, 4))
  f(p_raw,    "Raw data")
  f(p_survey, "Survey estimate")
  text(0, 1.5, "How much of the world is woody?", cex=1.5, xpd=NA)
  f(p_data,   "Bias corrected")
}
