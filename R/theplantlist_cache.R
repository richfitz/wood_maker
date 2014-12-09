download_theplantlist_cache <- function(filename) {
  url <-
    #"https://github.com/richfitz/wood/releases/download/v1.0/theplantlist-cache.tar.gz"
    "http://www.zoology.ubc.ca/~fitzjohn/files/cache.zip"
  download_file(url, filename)
}

## TODO: This will become a phoney rule, and then the dots disappear
## from read_names_accepted
unpack_theplantlist_cache <- function(filename) {
  unzip(filename)
}

## This bit is quite slow, because there are a lot of files to read
## and the resulting data object is 316143 rows, 5 columns of text.
read_names_accepted <- function(filename) {
  files <- unzip(filename)
  files <- files[grepl("\\.csv$", files)]
  
  dat <- do.call(rbind, lapply(files, load_names))
  dat$gs <- paste(dat$genus, dat$species, sep="_")
  dat_unique <- dat[!duplicated(dat),]
  dat_accepted <- dat_unique[dat_unique$status == "Accepted",]

  ## For ease later, we prefer Asteraceae/Fabaceae to
  ## Compositae/Leguminosae and fix a spelling mistake for
  ## Dryopteridaceae:
  tr <- c("Compositae"     = "Asteraceae",
          "Leguminosae"    = "Fabaceae",
          "Dryopteridacae" = "Dryopteridaceae")
  i <- match(dat_accepted$family, names(tr))
  j <- !is.na(i)
  dat_accepted$family[j] <- unname(tr[i[j]])
  dat_accepted[names(dat_accepted) != "status"]
}
