## TODO:
## Organise downloading the whole plant list and arranging into the
## zip file that can later be unpacked.  maker does not yet support
## targets that evaluate arbitrary functions, and because we're
## looking at two targets that go to the same place this is a problem.
##
## For now, running
##   library(RCurl)
##   source("R/theplantlist.R")
##   plant.list.rebuild()
## will work.
plant.list.rebuild <- function() {
  path <- "data/theplantlist"
  dir.create(file.path(path, "acceptedNames1.1"), FALSE)
  plant.list.get.group("gymnosperm")
  plant.list.get.group("pteridophyte")
  plant.list.get.group("angiosperm")
  owd <- setwd("data/theplantlist")
  on.exit(setwd(owd))
  zip("cache.zip", "acceptedNames1.1")
}

plant.list.groups <- function()
  c("angiosperm", "gymnosperm", "pteridophyte")

plant.list.url <- function(family, group) {
  group <- match.arg(tolower(group), plant.list.groups())
  sprintf("http://www.theplantlist.org/1.1/browse/%s/%s/%s.csv",
          toupper(substr(group, 1, 1)), family, family)
}

plant.list <- function(family, group) {
  getURLContent(plant.list.url(family, group))  
}

plant.list.csv <- function(family, path="data/theplantlist") {
  file.path(path, "acceptedNames1.1", paste0(family, ".csv"))
}

plant.list.get <- function(family, group, regenerate=FALSE) {
  file.out <- plant.list.csv(family)
  if (!regenerate && file.exists(file.out)) {
    invisible(FALSE)
  } else {
    message(sprintf("Fetching %s (%s)", family, group))
    dat <- plant.list(family, group)
    writeLines(dat, file.out)
  }
}

plant.list.get.group <- function(group, ..., path="data/theplantlist") {
  group <- match.arg(tolower(group), plant.list.groups())
  families <- readLines(file.path(path, sprintf("families/%s.txt", group)))
  for (f in families) {
    plant.list.get(f, group, ...)
  }
}

load_names <- function(filename) {
  keep <- c(genus="Genus", species="Species", family="Family",
            major.clade="Major group", status="Taxonomic status in TPL")
  dat <- read.csv(filename, stringsAsFactors=FALSE, check.names=FALSE)
  dat <- dat[dat$Genus != "", keep]
  names(dat) <- names(keep)
  
  dat$species <- gsub('"', "", dat$species, fixed=TRUE)
  dat
}

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
