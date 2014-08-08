# The BoutrosLab.utilities package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

help.BL <- function(pattern, packages = NULL, search.code = FALSE) {

	# define BL packages
	if (is.null(packages)) {
		packages <- unlist(strsplit(gsub("\t", "",c(
		"BoutrosLab.benchmarking
		BoutrosLab.datasets.breast.cancer
		BoutrosLab.datasets.cervix.cancer
		BoutrosLab.datasets.colon.cancer
		BoutrosLab.datasets.gbm
		BoutrosLab.datasets.hnscc
		BoutrosLab.datasets.hnscc.cnv
		BoutrosLab.datasets.leiomyosarcoma
		BoutrosLab.datasets.nsclc
		BoutrosLab.datasets.ovarian.cancer
		BoutrosLab.datasets.prostate.cancer
		BoutrosLab.datasets.prostate.cancer.aCGH
		BoutrosLab.datasets.prostate.cancer.methylation
		BoutrosLab.dist.overload
		BoutrosLab.OMIM
		BoutrosLab.package.utilities
		BoutrosLab.pipeline.affymetrix
		BoutrosLab.pipeline.beadarray
		BoutrosLab.pipeline.limma
		BoutrosLab.pipeline.LTR
		BoutrosLab.pipeline.TLDA
		BoutrosLab.plotting.general
		BoutrosLab.plotting.genome
		BoutrosLab.plotting.survival
		BoutrosLab.prognosticsignature.general
		BoutrosLab.sequencing.coverage
		BoutrosLab.statistics.general
		BoutrosLab.statistics.survival
		BoutrosLab.survivalsignature.algorithms
		BoutrosLab.utilities
		NanoStringNorm
		SequenceAnalysis
		snp.methylation
		ternary.continuous
		VennDiagram")), split = "\n"));
		}

	# double check they are installed
	packages <- packages[packages %in% row.names(installed.packages())];

	if (length(packages) == 0) stop("No specified packages are installed");

	# search documentation
	help.results <- help.search(pattern = pattern, apropos = pattern, fields =  c("name", "title", "alias", "concept",  "keyword"), verbose = FALSE, types = c("help", "vignette"), package = packages);

	help.results <- help.results$match;

	code.results <- NULL;

	# search code
	if (search.code) {

		for (package in packages) {
			cat(paste("searching package: ", package, ".....\n", sep = ""));

			already.attached <- paste("package:",package,sep="") %in% search();

			if (!already.attached) {
				library(package, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE);
				}

			# get package env using first exported function... clunky!
			# this ensures finding all the non exported functions i.e. pkg:::

			# get the first exported function in order to get the package environment
			objs <- ls(paste("package:", package, sep=""));
			for (obj in objs) {
				first.function <- get(obj);
				if (class(first.function) == "function") break;
				}

			# get the function env
			package.env <- environment(first.function);

			# list all exported and internal functions in package
			funcs <- ls(package.env);
			funcs.exported <- ls(paste("package:", package, sep = ""));
			funcs.not.exported <- setdiff(funcs, funcs.exported);

			for(func in funcs) {
				code <- capture.output(get(func, envir=package.env));

				line.num <- grep(pattern, code, ignore.case = TRUE, value = FALSE);
				line.code <- grep(pattern, code, ignore.case = TRUE, value = TRUE);
				if (length(line.code) > 0) {
					code.results <- rbind(code.results, data.frame(package, func, line.num, line.code));
					}
				}


			# detach tmp loaded package
			if (!already.attached) {
				detach(paste("package:", package, sep = ""), character.only = TRUE, unload = TRUE);
				}
			}
		}
	return(list(help = help.results, code = code.results));
	}
