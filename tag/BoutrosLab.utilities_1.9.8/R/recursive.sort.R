# The BoutrosLab.utilities package is copyright (c) 2011 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

recursive.sort <- function(x, by = "col", na.last = TRUE, decreasing = FALSE, skip = NULL, reverse = FALSE, group = NULL, alternate.dim.order.func = NULL, alternate.dim.order.func.rev = FALSE) {

	# apply an initial sort in the alternate dimension
	if(!is.null(alternate.dim.order.func)) {
		if (by == "col") {
			func.order <- apply(x, 2, alternate.dim.order.func);
			if (alternate.dim.order.func.rev) {
				x <- x[order(-func.order),];
			} else {
				x <- x[order(func.order),];
				}
			}
		if (by == "row") {
			func.order <- apply(x, 1, alternate.dim.order.func);
			if (alternate.dim.order.func.rev) {
				x <- x[,order(-func.order)];
			} else {
				x <- x[,order(func.order)];
				}
			}

		}

	# check group is right length
	if (!is.null(group)) {
		if (by == "col") {
			if (sum(!rownames(x) %in% skip) != length(group)) {stop("group is wrong length");}
			}
		if (by == "row") {
			if (sum(!colnames(x) %in% skip) != length(group)) {stop("group is wrong length");}
			}
		}
	
	if (by == "col") {
		if (reverse) {
			x = x[do.call(order, c(as.data.frame(group), rev(x[,!colnames(x) %in% skip]), na.last = na.last, decreasing = decreasing)),];
		} else {
			x = x[do.call(order, c(as.data.frame(group), x[,!colnames(x) %in% skip], na.last = na.last, decreasing = decreasing)),];
			}	
	} else if (by == "row") {
		if (reverse) {
			x = x[,do.call(order, c(as.data.frame(group), rev(as.data.frame(t(x[!rownames(x) %in% skip,]))), na.last = na.last, decreasing = decreasing))];
		} else {
			x = x[,do.call(order, c(as.data.frame(group), as.data.frame(t(x[!rownames(x) %in% skip,])), na.last = na.last, decreasing = decreasing))];
			}
		}

	return(x);
	}
