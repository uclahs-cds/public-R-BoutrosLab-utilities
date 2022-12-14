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

strsplit.to.matrix <- function(x, split, fixed = FALSE, perl = FALSE, select.cols = "max") {

	# convert factors to character
	if(is.factor(x)) {x <- as.character(labels(x));}

	x <- strsplit(x, split = split, fixed = fixed, perl = perl);

	n.splits <- sapply(x, length);

	if (length(unique(n.splits)) != 1) {
		max.length <- max(n.splits);
		min.length <- min(n.splits);
		if (select.cols[1] == "max") {
			x <- lapply(x, function(x, n) {c(x, rep(NA, n - length(x)))}, max.length);
			}
		else if (select.cols[1] == "min") {
			x <- lapply(x, function(x, n) {c(x[1:n])}, min.length);
			}
		else if (all(is.numeric(select.cols))) {
			x <- lapply(x, mixed.index, select.cols);
			}
		else {
			stop("select.cols value not recognized");
			}
		}
	else if (all(is.numeric(select.cols))) {
		x <- lapply(x, mixed.index, select.cols);
		} 

	x <- as.data.frame(do.call("rbind", x), stringsAsFactors=FALSE);
	return(x);
	}
