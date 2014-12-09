fig_fraction_on_phylogeny <- function(phy_o, res) {
  hlt <- read.csv("data/high-level-taxonomy.csv", stringsAsFactors=FALSE)
  phy.group <- hlt$Group[match(phy_o$tip.label, hlt$Order)]
  tmp <- 
    lapply(seq_len(max(phy_o$edge)), function(x)
           if ( x <= length(phy_o$tip.label) ) phy.group[[x]] else
           unique(phy.group[diversitree::get.descendants(x, phy_o, TRUE)]))
  grp <- sapply(tmp, function(x) if (length(x) == 1) x else "Rest")

  col <- unname(cols_tree[grp])
  col2 <- col[match(phy_o$edge[,2], seq_along(grp))]

  p <- structure(res$order[["p_mean"]], names=res$order$order)
  p <- p[phy_o$tip.label]

  t <- max(branching.times(phy_o))
  offset <- .15

  op <- par(no.readonly=TRUE)
  on.exit(par(op))

  ## Drop orders with < 100 species, execpt for a couple we can fit
  ## in.
  drop <- c("Isoetales",
            "Psilotales",
            "Ophioglossales",
            "Equisetales",
            "Osmundales",
            "Salviniales",
            "Ginkgoales",
            "Welwitschiales",
            "Gnetales",
            "Ephedrales",
            "Nymphaeales",
            "Amborellales",
            "Austrobaileyales",
            "Chloranthales",
            "Ceratophyllales",
            # "Canellales",    # keep
            # "Acorales",      # keep
            # "Petrosaviales", # keep
            "Trochodendrales", # drop } could keep 1/2
            "Gunnerales",      # drop }
            "Crossosomatales", # drop
            "Picramniales",    # drop
            "Huerteales",      # drop
            "Berberidopsidales", # drop (borderline)
            "Paracryphiales", # drop
            "Escalloniales",  # drop
            "Bruniales",      # drop
            #"Garryales"      # keep
            "Schizeales"       # also drop
            )
  tip.color <- ifelse(phy_o$tip.label %in% drop, "#ffffff00", "black")
  plt <- diversitree:::plot2.phylo(phy_o, type="fan", cex=.5, no.margin=TRUE,
                                   label.offset=t * .15, font=1,
                                   edge.col=col2, tip.color=tip.color,
                                   n.taxa=sqrt(phy_o$n.taxa)/10)

  xy <- plt$xy

  r <- max(xy$r)*(1+offset)
  n.tip <- length(phy_o$tip.label)
  xy <- plt$xy[seq_len(n.tip),]
  xy.lab <- data.frame(x=cos(xy$theta)*r,
                       y=sin(xy$theta)*r)
  xrad <- .5 * diff(par("usr")[1:2])/50
  pie <- cbind(p, 1 - p)
  pie.col <- cols_woody

  r <- 3/4
  r0 <- max(xy$r) * (1 + offset * (1-r)/2)
  r2 <- max(xy$r) * (1 + offset * (1 - (1-r)/2))
  r1 <- r0 * p + r2 * (1-p)

  w <- 3

  xx1 <- c(rbind(r0 * cos(xy$theta) + w * cos(xy$theta + pi/2),
                 r0 * cos(xy$theta) - w * cos(xy$theta + pi/2),
                 r1 * cos(xy$theta) - w * cos(xy$theta + pi/2),
                 r1 * cos(xy$theta) + w * cos(xy$theta + pi/2),
                 NA))
  yy1 <- c(rbind(r0 * sin(xy$theta) + w * sin(xy$theta + pi/2),
                 r0 * sin(xy$theta) - w * sin(xy$theta + pi/2),
                 r1 * sin(xy$theta) - w * sin(xy$theta + pi/2),
                 r1 * sin(xy$theta) + w * sin(xy$theta + pi/2),
                 NA))
  
  xx2 <- c(rbind(r2 * cos(xy$theta) + w * cos(xy$theta + pi/2),
                 r2 * cos(xy$theta) - w * cos(xy$theta + pi/2),
                 r1 * cos(xy$theta) - w * cos(xy$theta + pi/2),
                 r1 * cos(xy$theta) + w * cos(xy$theta + pi/2),
                 NA))
  yy2 <- c(rbind(r2 * sin(xy$theta) + w * sin(xy$theta + pi/2),
                 r2 * sin(xy$theta) - w * sin(xy$theta + pi/2),
                 r1 * sin(xy$theta) - w * sin(xy$theta + pi/2),
                 r1 * sin(xy$theta) + w * sin(xy$theta + pi/2),
                 NA))

  polygon(xx1, yy1, border="black", col=pie.col[2], lwd=.3)
  polygon(xx2, yy2, border="black", col=pie.col[1], lwd=.3)

  cex.legend <- 2/3
  str <- str.leg <- setdiff(names(cols_tree), "Rest") # drop backbone
  str.leg[str.leg == "BasalAngiosperms"] <- '"Basal Angiosperms"'
  legend("topleft", str.leg, fill=cols_tree[str],
         cex=cex.legend, bty="n", border=NA)
  legend("topright", names(cols_woody), fill=cols_woody,
         cex=cex.legend, bty="n", border="black")
}
