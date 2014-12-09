## # How much of the world is woody?
## 
## [Richard G. FitzJohn](http://www.zoology.ubc.ca/~fitzjohn),
## [Matthew W. Pennell](http://mwpennell.wordpress.com),
## [Amy E. Zanne](http://phylodiversity.net/azanne/),
## [Peter F. Stevens](http://www.missouribotanicalgarden.org/plant-scence/research-staff-article/487/stevens-p-f.aspx),
## [David C. Tank](http://www.phylodiversity.net/dtank/), 
## [William K. Cornwell](http://www.phylodiversity.net/wcornwell/)

## This is the full analysis that underlies our paper.    

### Set knitr options
##+ echo=FALSE,results=FALSE
knitr::opts_chunk$set(tidy=FALSE)
### Suppress a warning about incompatibility with results from R < 2.2.0
##+ echo=FALSE,results=FALSE
invisible(suppressWarnings(sample(1:250, 1, pr=rep(1, 250), replace=TRUE)))

## # Preliminaries

## This file is designed to be run by maker.
##
## In the main version of the analysis, we organised things so that
## objects were explicitly stored in .rds objects and we read them in
## via some functions.  The main analysis was carried out within this
## knitr document and this generated all the figures.
##
## Here, we've moved the focus 


## The 'load.woodiness.data.genus' function hides a reasonable amount
## of data cleaning required to load the data.  This mostly involves
## matching the woodiness data set from Zanne et al. to our species
## list (derived from The Plant List), cleaning up synonomies, and
## then collapsing down to genus.
## 
## The final object has columns
## * genus, family, order -- taxonomic information
## * W, V, H              -- number of species scored as woody,
##                           variable, herbaceous (respectively)
## * N                    -- number of species in the genus
## * K                    -- number of species with known state,
##                           after dropping all "variable" species
## * p                    -- fraction of known species that are woody,
##                           after dropping all "variable" species
head(dat_g)

## # Imputing the state of missing species

## For each genus:

## A. If the genus has a valid fraction of species (i.e. K > 0 so p is
## defined), then sample the number of species that are woody from
## either
##    - the binomial distribution (strong prior; assuming known
##      species were sampled with replacement from the pool of
##      species); or
##    - the hypergeometric distribution (weak prior; assuming that
##      known species were sampled without replacement from the pool
##      of species).

## B. If the genus has no valid fraction of species (i.e., K == 0 so p
## is undefined), then sample from the emperical distribution of
## per-genus fractions.  We're going to feed data into this by
## taxonomic order, so this will come from the per-order distribution.
sim

## # Distribution of estimates of woodiness

## This is the raw distribution; i.e., our estimate of the fraction of
## species that are woody and its estimate.

##+ distribution_raw,fig.cap="Distribution of simulated woodiness percentage"
fig_distribution_raw(res_b, res_h)

## Next, expand that plot to include the more extreme estimates
## (variable species as woody and variable species as herbaceous).
## This shows how much the classification errors affect the analysis.

##+ distribution_errors,fig.cap="Effect of recoding on woodiness estimates"
fig_distribution_raw_errors(res_b, res_h, res_b_w, res_h_w, res_b_h, res_h_h)

## Woodiness is structured among genera and other taxonomic groups.
## Make a plot of per genus/family/order estimates of woodiness:


##+ fraction_by_genus,fig.cap="Fraction of woodiness by genus"
fig_fraction_by_group(res_b, res_h, dat_g, "genus")
##+ fraction_by_family,fig.cap="Fraction of woodiness by family"
fig_fraction_by_group(res_b, res_h, dat_g, "family")
##+ fraction_by_order,fig.cap="Fraction of woodiness by order"
fig_fraction_by_group(res_b, res_h, dat_g, "order")

## Woodiness also varies over the tree; plot the per-order estimate of
## woodiness around the tree (the code to do this is not very pretty,
## so is kept separately in `R/plot-tree.R`)

## Phylogeny at the level of order:

##+ fraction_phy_binomial,fig.cap="Woodiness percentage by order"
fig_fraction_on_phylogeny(phy_o, res_b)

##+ fraction_phy_hypergeometric,fig.cap="Woodiness percentage by order"
fig_fraction_on_phylogeny(phy_o, res_h)

## # Survey:

## Raw results of the survey:

## Convert estimates to normal using logit transformation:
survey$Estimate_logit <- boot::logit(survey$Estimate / 100)

## Model with training and familiarity as factors:
res <- lm(Estimate_logit ~ Training + Familiarity, survey)
summary(res)
anova(res)

## Regression against |latitude|:
res_lat <- lm(Estimate_logit ~ abs(Lat), survey)
anova(res_lat)
summary(res_lat)

## Here is the fitted result:
##+ survey_by_latidude,fig.cap="Fitted latitude survey regression"
plot(Estimate_logit ~ abs(Lat), survey)
abline(res_lat)

## As a categorical tropical/non-tropical variable:
res_tro <- lm(Estimate_logit ~ Tropical, survey)
anova(res_tro)
summary(res_tro)

## Distribution of estimates vs different levels of botanical
## familiarity and education, with the estimates from the database
## overlaid.


##+ survey_results,fig.cap="Survey results by predictor"
fig_survey_results(survey, res_b, res_h)

## And the raw distribution of survey results, showing the general
## tendency for relatively low estimates, again overlaid with the
## estimates from the database:

##+ survey_distribution,fig.cap="Distribution of survey results"
fig_survey_distribution(survey, res_b, res_h)

## # Variability of a genus vs size

## These plots look at the how variable a genus is vs its size (left
## column) or the number of species in a known state (right column).
## The top row looks at how variable the genus is (a single type or a
## mixed type), the middle row at the probability that a genus is
## varaible.  The bottom row looks at the proportion of species that
## are woody in a genus vs its size, testing if woody genera are
## relatively larg or relatively small (woody genera are relatively
## smaller).

##+ variability,fig.cap="Variability by genus size"
fig_variability(dat_g)

## # Write out partly processed data sets

## To save having to rerun everything, these are the estimates by
## genus, family and order for the two different sampling approaches
## and the three different treatment of variable species (so 3 x 2 x 3
## = 18 files)
write_output <- function(d, type) {
  for (tax in c("genus", "family", "order")) {
    write.csv(d[[tax]],
              file.path("output/results", sprintf("%s-%s.csv", tax, type)),
              row.names=FALSE)
  }
}

## Core data sets:
dir.create("output/results", FALSE, TRUE)
write_output(res_b, "binomial-strong-prior")
write_output(res_h, "hypergeometric-weak-prior")

## Extra data sets:
write_output(res_b_w, "binomial-strong-prior-wood-biased")
write_output(res_h_w, "hypergeometric-weak-prior-wood-biased")
write_output(res_b_h, "binomial-strong-prior-herb-biased")
write_output(res_h_h, "hypergeometric-weak-prior-herb-biased")

## Meta data for interpreting these files.
metadata <- 
  list(order="Taxonomic order",
       family="Taxonomic family",
       genus="Taxonomic genus",
       W="Number of species known to be woody",
       V="Number of species known to be variable",
       H="Number of species known to be herbaceous",
       N="Estimate of the number of species in the genus/family/order",
       K="Number of species with known state (W + H)",
       mean="Mean estimated number of woody species",
       lower="0.025 quantile of estimated number of woody species",
       upper="0.975 quantile of estimated number of woody species",
       p_mean="Mean estimated fration of woody species",
       p_lower="0.025 quantile of estimated fraction of woody species",
       p_upper="0.975 quantile of estimated fraction of woody species")
metadata <- data.frame(column=names(metadata),
                       description=unlist(metadata),
                       stringsAsFactors=FALSE)
write.csv(metadata, "output/results/metadata.csv", row.names=FALSE)

## # Graphical abstract:

##+ graphical_abstract,fig.height=3.5
fig_graphical_abstract(res_b, res_h, dat_g, survey)

## # Version information:

sessionInfo()
