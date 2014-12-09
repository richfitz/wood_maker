make_survey <- function(filename_survey, filename_coords) {
  d <- read.csv(filename_survey, stringsAsFactors=FALSE)
  names(d) <- c("Time", "Estimate", "Familiarity", "Training",
                "Continent", "Country")

  ## Drop the Time and Continent columns:
  d <- d[!(names(d) %in% c("Time", "Continent"))]

  ## Here are the different familiarity and training categories from
  ## "best" to "worst".
  lvl_familiarity <- c("Very Familiar", "Familiar", "Somewhat Familiar",
                       "What's a Plant?")
  d$Familiarity <- factor(d$Familiarity, lvl_familiarity, ordered=TRUE)
  
  lvl_training <-
    c("Postgraduate degree in botany or a related field",
      "Partially complete postgraduate degree in botany or a related field",
      "Undergraduate degree in botany or a related field",
      "Some botany courses at either an undergraduate or postgraduate level",
      "No formal training in botany")             
  d$Training <- factor(d$Training, lvl_training, ordered=TRUE)

  ## Standardise the country names:
  countries <- read.csv(filename_coords,stringsAsFactors=FALSE)
  d$Country <- cleanup_country_names(d$Country)
  
  idx <- match(d$Country, countries$Country)
  mssg <- na.omit(d$Country[is.na(idx)])
  if (length(mssg) > 0)
    warning("Dropped countries %s", paste(mssg, collapse=", "))
  d <- cbind(d, countries[idx,c("Long", "Lat")])
  d$Tropical <- abs(d$Lat) < 23 + 26/60

  rownames(d) <- NULL
  d
}

cleanup_country_names <- function(x) {
  ## In cases where multiple countries are given, take the first one:
  x <- sub("( and |, | / | & ).+", "", x)
  ## Trim trailing non-alphabetic characters
  x <- sub("[^A-Za-z]+$", "", x)
  ## Translate inconsistent names:
  translate <- list(France="france",
                    "United States"=c("US", "USA"),
                    "United Kingdom"=c("UK", "Scotland"),
                    "Brazil"="Brasil")
  tr <- cbind(to=rep(names(translate), sapply(translate, length)),
              from=unlist(translate))
  i <- match(x, tr[,"from"])
  x[!is.na(i)] <- unname(tr[i[!is.na(i)],"to"])
  x[x == ""] <- NA
  x
}
