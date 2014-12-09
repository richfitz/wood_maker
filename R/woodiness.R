## Check the classification by pulling apart the count.
parse_count <- function(x) {
  res <- t(sapply(strsplit(x, ";", fixed=TRUE), as.integer))
  colnames(res) <- c("H", "V", "W")
  drop(res)
}
summarise_count <- function(x, extreme=FALSE) {
  if (identical(extreme, FALSE)) {
    ans <- ifelse(x[,"W"] > x[,"H"], "W", "H")
    ans[(x[,"W"] == 0 & x[,"H"] == 0 & x[,"V"] > 0) |
        x[,"W"] == x[,"H"]] <- "V"
  } else if (identical(extreme, "woody")) {
    ans <- ifelse(x[,"W"] > 0 | x[,"V"] > 0, "W", "H")
  } else if (identical(extreme, "herbaceous")) {
    ans <- ifelse(x[,"H"] > 0 | x[,"V"] > 0, "H", "W")
  } else {
    stop("Invalid argument for 'extreme'")
  }
  ans
}

merge_counts <- function(x) {
  paste(colSums(parse_count(x)), collapse=";")
}


make_woodiness <- function(woodiness_zae, tpl) {
  dat <- woodiness_zae
  dat$woodiness[dat$woodiness == "variable"] <- "V"
  names(dat)[names(dat) == "gs"] <- "species"
  dat$species <- sub(" ", "_", dat$species)

  if (!identical(dat$woodiness,
                 summarise_count(parse_count(dat$woodiness.count)))) {
    stop("Database classification failure")
  }

  ## Map some species names to synonyms:
  syn <- read.csv("data/synonyms.csv", stringsAsFactors=FALSE)

  idx <- match(dat$species, syn$synonym)
  message(sprintf("Resolving synonomy for %d species", sum(!is.na(idx))))
  i <- !is.na(idx)
  dat$species[i] <- syn$species[idx[i]]

  ## Read in The Plant List
  keep <- dat$species %in% tpl$gs
  message(sprintf("Dropping %d species not in Plant List", sum(!keep)))
  dat <- dat[keep,]

  ## And look to see which species have now got duplicated records due
  ## to synonomy resolution:
  dups <- unique(sort(dat$species[duplicated(dat$species)]))
  message(sprintf("After synonym correction, %d duplicated entries",
                  length(dups)))
  
  ## Merge the counts across the different instances of these names:
  dups_i <- which(dat$species %in% dups)
  dups_count <- tapply(dat$woodiness.count[dups_i],
                       dat$species[dups_i], merge_counts)
  dups_woodiness <- summarise_count(parse_count(dups_count))

  dups_merged <- data.frame(species=names(dups_woodiness),
                            woodiness=unname(dups_woodiness),
                            woodiness.count=unname(dups_count),
                            stringsAsFactors=FALSE, row.names=NULL)

  ## Drop the duplicated records from the original vector, and add in
  ## the resolved entries here:
  dat <- rbind(dat[-dups_i,], dups_merged)
  
  ## Tidy up, and we're done
  dat <- dat[order(dat$species),
             c("species", "woodiness", "woodiness.count")]
  names(dat)[1] <- "gs"
  rownames(dat) <- NULL

  dat
}

## Build the per genus-data set.
make_woodiness_genus <- function(dat, lookup, tpl, extreme=FALSE) {
  if (!identical(extreme, FALSE)) {
    dat$woodiness <-
      summarise_count(parse_count(dat$woodiness.count), extreme)
  }

  res <-
    tpl %.% left_join(dat, "gs") %.%
    group_by(genus, family) %.%
    summarise(W=sum(woodiness == "W", na.rm=TRUE),
              V=sum(woodiness == "V", na.rm=TRUE),
              H=sum(woodiness == "H", na.rm=TRUE),
              N=length(woodiness)) %.%
    mutate(K = W + H) %.%
    mutate(p = W / K) %.%
    left_join(lookup, c("genus", "family"))

  cols <- c("genus", "family", "order", "W", "V", "H", "N", "K", "p")
  
  message(sprintf("Final set: %d genera, %d with data, %d species known",
                  nrow(res), sum(res$K > 0), sum(res$K)))

  res[cols]
}

make_woodiness_genus_h <- function(dat, lookup, tpl) {
  make_woodiness_genus(dat, lookup, tpl, "herbaceous")
}

make_woodiness_genus_w <- function(dat, lookup, tpl) {
  make_woodiness_genus(dat, lookup, tpl, "woody")
}
