make_phylogeny_order <- function(phy, dat_g, genus_order_lookup) {
  ## Try this in the simplest possible way.  We'll take one
  ## representative per order.
  phy_order <- genus_order_lookup$order[match(sub("_.+$", "", phy$tip.label),
                                              genus_order_lookup$genus)]
  phy_order[grepl("^UnknownOrder-", phy_order)] <- NA

  keep <- data.frame(tip=phy$tip.label, order=phy_order,
                     stringsAsFactors=FALSE) %>%
                       filter(!is.na(order)) %>%
                         group_by(order) %>%
                           summarise(tip=tip[[1]])

  ## Update this for a few cases:
  keep.change <- c("Cyatheales"="Dicksonia_antarctica",
                   "Ophioglossales"="Ophioglossum_lusitanicum")
  keep$tip[match(names(keep.change), keep$order)] <- unname(keep.change)

  ## This can use ape's drop tip.  Some versions will shuffle node
  ## labels though, in which case diversitree:::drop.tip or
  ## geiger:::.drop.tip may be preferable.
  phy_o <- drop.tip(phy, setdiff(phy$tip.label, keep$tip))
  
  phy_o$tip.label <- keep$order[match(phy_o$tip.label, keep$tip)]
  phy_o <- ladderize(phy_o)

  ## Get counts by order:
  n <- dat_g %>% group_by(order) %>% summarise(n=sum(N))

  phy_o$n.taxa <- structure(n$n[match(phy_o$tip.label, n$order)],
                            names=phy_o$tip.label)
  phy_o
}
