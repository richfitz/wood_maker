## This is a big bit of hassle in here:
make_genus_order_lookup <- function(tpl, genus_order_lookup_zae) {
  tpl_full <- tpl
  tpl <- unique(tpl[c("genus", "family", "major.clade")])
  rownames(tpl) <- NULL

  lookup1 <- genus_order_lookup_zae
  ## Augmented bits from Tank:
  ## TODO: Could depend on the filename here too...
  lookup2 <- read.csv("data/genus_order_lookup_extra.csv",
                    stringsAsFactors=FALSE)
  lookup <- rbind(lookup1, lookup2)
  rownames(lookup) <- NULL

  duplicates <- sort(tpl$genus[duplicated(tpl$genus)])

  info <- tpl_full                %.%
    filter(genus %in% duplicates) %.%
    group_by(family, genus, major.clade) %.%
    summarise(n=length(species))  %.%
    arrange(genus, desc(n))

  ## We'll generally keep just the largest group:
  keep <- as.data.frame(info)     %.%
    group_by(genus)               %.%
    summarise(family=family[[1]], major.clade=major.clade[[1]]) %.%
    arrange(genus)

  ## These are the species that we are ignoring (28).  This is tiny
  ## compared with the other taxonomic errors in the database.
  drop <- as.data.frame(info) %.% group_by(genus) %.%
    summarise(family=family[-1], n=n[-1]) %.% arrange(genus)
  sum(drop$n)

  ## But rewrite a couple of these anyway:
  ## * Malocarpus we have is not a cactus:
  keep$family[keep$genus == "Malocarpus"] <- "Zygophyllaceae"
  ## * Ericaceae record is a tropicos error
  keep$family[keep$genus == "Oreocallis"] <- "Proteaceae"
  ## * I camped under a Washingtonia filifera once (WCK)
  keep$family[keep$genus == "Washingtonia"] <- "Arecaceae"

  ## Join this back in with the plant list, removing the duplicates.
  tpl <- rbind(tpl[!(tpl$genus %in% duplicates),], keep)
  
  ## Assign order to the Plant List families using this lookup:
  tpl$order <- lookup$order[match(tpl$family, lookup$family)]
  ## There are encoding issues here:
  tpl$order[tpl$family == "Isoëtaceae"] <- "Isoetales"

  ## There are a handful of essentially unplaced families.  For now,
  ## these get their own pseudo-family
  i <- tpl$order == ""
  tpl$order[i] <- paste0("UnknownOrder-", tpl$family[i])

  tpl[c("genus", "family", "order", "major.clade")]
}
