all: clean initc docs test check

clean:
	rm -rf man/*
	rm -rf data/*
	rm -rf docs/*
	rm -rf inst/doc/*

docs: man readme vigns site

man:
	R --slave -e "devtools::document()"

readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

vigns:
	R --slave -e "devtools::build_vignettes()"
	cp -R doc inst/

quicksite:
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"

site:
	R --slave -e "pkgdown::clean_site()"
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)"

quickcheck:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1
	cp -R doc inst/

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1
	cp -R doc inst/

wbcheck:
	R --slave -e "devtools::check_win_devel()"
	cp -R doc inst/

solarischeck:
	R --slave -e "rhub::check(platform = 'solaris-x86-patched', email = 'jeffrey.hanson@uqconnect.edu.au', show_status = FALSE)"

maccheck:
	R --slave -e "devtools::check_mac_release()"

build:
	R --slave -e "devtools::build()"
	cp -R doc inst/

install:
	R --slave -e "devtools::install_local(force = TRUE)"

spellcheck:
	echo "\n===== SPELL CHECK =====\n" > spell.log 2>&1
	R --slave -e "devtools::spell_check()" >> spell.log 2>&1

urlcheck:
	R --slave -e "devtools::document();urlchecker::url_check()"

purl_vigns:
	R --slave -e "lapply(dir('vignettes', '^.*\\\\.Rmd$$'), function(x) knitr::purl(file.path('vignettes', x), gsub('.Rmd', '.R', x, fixed = TRUE)))"
	rm -f Rplots.pdf

examples:
	R --slave -e "devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE);warnings()"  >> examples.log
	rm -f Rplots.pdf

paper: paper/paper.pdf paper/paper.docx

paper/paper.pdf: paper/paper.Rmd paper/paper.bib paper/carbon.png
	rm -f paper/paper.pdf
	rm -f paper/paper.md
	R --slave -e "rmarkdown::render('paper/paper.Rmd', output_file = 'paper.pdf')"
	rm -f paper/paper.knit.md
	rm -f paper/paper.log
	rm -f paper/paper.tex
	rm -f paper/paper.pdf
	docker run --rm \
    --volume $(PWD)/paper:/data \
    --user $(id -u):$(id -g) \
    --env JOURNAL=joss \
    openjournals/inara
	rm -f paper/paper.jats

paper/paper.docx: paper/paper.pdf
	R --slave -e "rmarkdown::render('paper/paper.Rmd', output_file = 'paper.docx', output_format = 'word_document')"

paper_case_study:
	R CMD BATCH --no-restore --no-save paper/case-study.R
	mv case-study.Rout paper/case-study.Rout
	rm -f Rplots.pdf

wdpa_global:
	R CMD BATCH --no-restore --no-save inst/scripts/global-example-script.R

.PHONY: initc vigns clean data docs readme site test check checkwb build  install man spellcheck examples wdpa_global paper purl_vigns paper_case_study
