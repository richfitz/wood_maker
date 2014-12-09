RSCRIPT_PKGS := $(shell Rscript -e 'library(methods);writeLines(Sys.getenv("R_DEFAULT_PACKAGES"))')
RSCRIPT = Rscript --default-packages="${RSCRIPT_PKGS},methods"

all: wood.html doc/wood-ms.pdf

data-raw: ${DATA_RAW}
data-processed: ${DATA_PROCESSED}

wood.Rmd: wood.R
	${RSCRIPT} -e "library(sowsear); sowsear('$<', 'Rmd')"
wood.md: wood.Rmd ${DATA_PROCESSED}
	${RSCRIPT} -e "library(knitr); knit('$<')"
wood.html: wood.md
	${RSCRIPT} -e "library(markdown);\
	 opts <- setdiff(markdownHTMLOptions(TRUE), 'base64_images');\
	 markdownToHTML('$<', '$@', options=opts, stylesheet='stylesheet.css', template='.template.html')"

doc/wood-ms.pdf: wood.md
	make -C doc

# This target will never run because it depends on nothing.  But if
# data/geo/country_coords.csv is deleted then this will regenerate
# that file.  It's here for reference only, really.
data/geo/country_coords.csv:
	${RSCRIPT} make/data-geo-country_coords.csv.R

DATA_GENUS_DEPS = output/genus_order_lookup.csv output/woodiness.rds \
	R/load.R R/build.R

# Cache downloads
RELEASE=v1.0

THEPLANTLIST_CONTENTS = data/theplantlist/acceptedNames1.1
THEPLANTLIST_CACHE = .theplantlist-cache.tar.gz
THEPLANTLIST_URL = https://github.com/richfitz/wood/releases/download/${RELEASE}/theplantlist-cache.tar.gz

DRYAD_CONTENTS = data/zae/Spermatophyta_Genera.csv \
  data/zae/PhylogeneticResources.zip \
  data/zae/GlobalWoodinessDatabase.csv
DRYAD_CACHE = .dryad-cache.tar.gz
DRYAD_URL = https://github.com/richfitz/wood/releases/download/${RELEASE}/dryad-cache.tar.gz

ARCHIVES = wood-supporting.tar.gz wood-analysis.tar.gz

# Packrat support.
PACKRAT_SOURCES_URL = https://github.com/richfitz/wood/releases/download/${RELEASE}/packrat.sources.tar.gz
include make/packrat.mk

release-files: ${ARCHIVES} ${DRYAD_CACHE} ${THEPLANTLIST_CACHE}
	rm -rf release
	mkdir -p release
	cp ${THEPLANTLIST_CACHE} release/theplantlist-cache.tar.gz
	cp ${DRYAD_CACHE} release/dryad-cache.tar.gz
	mv ${ARCHIVES} release
	cp ${PACKRAT_SOURCES_ARCHIVE} release

wood-supporting.tar.gz: ./make/wood-supporting.tar.gz.sh doc/wood-ms.pdf
	$<
wood-analysis.tar.gz: ./make/wood-analysis.tar.gz.sh doc/wood-ms.pdf
	$<

clean:
	rm -f data/theplantlist/names_accepted.csv
	rm -f data/zae/Vascular_Plants_rooted.dated.tre \
		data/zae/genus_order_lookup.csv
	rm -rf data/geo/raw
	rm -f output/*.rds output/*.csv
	rm -rf output/results
	rm -rf ${REPORT} figure cache
	make -C doc clean
	rm -f doc/figs/[a-z]*.pdf
	rm -f ${ARCHIVES}
	rm -rf release

purge: clean
	rm -rf ${THEPLANTLIST_CONTENTS}
	rm -f ${DRYAD_CONTENTS}

deps:
	${RSCRIPT} make/dependencies.R

# Lots of phony targets not listed...
.PHONY: all clean purge data-raw data-processed deps
