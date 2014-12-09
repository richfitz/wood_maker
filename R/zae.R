## Simply download the files:
zae_dryad_url <- function(handle) {
  paste0('http://datadryad.org/bitstream/handle/10255/', handle)
}

download_zae_tree <- function(filename) {
  url <- zae_dryad_url('dryad.55548/PhylogeneticResources.zip?sequence=1')
  download_file(url, filename)
}

download_zae_woodiness <- function(filename) {
  url <- zae_dryad_url('dryad.55447/GlobalWoodinessDatabase.csv?sequence=1')
  download_file(url, filename)
}

download_zae_taxonomy <- function(filename) {
  url <- zae_dryad_url('dryad.55304/Spermatophyta_Genera.csv?sequence=2')
  download_file(url, filename)
}

read_zae_tree <- function(filename) {
  path <- tempdir()
  treefile <- "PhylogeneticResources/Vascular_Plants_rooted.dated.tre"
  read.tree(unzip1(filename, treefile))
}

read_zae_woodiness <- function(filename) {
  read.csv(filename, stringsAsFactors=FALSE)
}

read_zae_lookup <- function(filename) {
  lookup <- read.csv(filename, stringsAsFactors=FALSE)
  names(lookup)[1] <- "genus"
  lookup <- lookup[c("genus", "family", "order")]
  # Two genera need families set:
  lookup$family[lookup$genus == "Peltanthera"] <- "Gesneriaceae"
  lookup$family[lookup$genus == "Brachynema" ] <- "Olacaceae"
  lookup
}

## A little helper function
unzip1 <- function(zipfile, file, path=tempdir()) {
  unzip(zipfile, file, junkpaths=TRUE, exdir=path)
  file.path(path, basename(file))
}
