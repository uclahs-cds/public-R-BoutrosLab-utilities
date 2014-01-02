recursive.sort <- function(x, by="col",na.last = TRUE, decreasing = FALSE, skip = NULL, reverse = FALSE, group = NULL, alternate.dim.order.func = NULL, alternate.dim.order.func.rev = FALSE) {

	# apply an initial sort in the alternate dimension
	if(!is.null(alternate.dim.order.func)) {
		if (by == "col") {
			func.order <- apply(x, 2, alternate.dim.order.func);
			if (alternate.dim.order.func.rev) {
				x <- x[order(-func.order),]
			} else {
				x <- x[order(func.order),]
				}
			}
		if (by == "row") {
			func.order <- apply(x, 1, alternate.dim.order.func);
			if (alternate.dim.order.func.rev) {
				x <- x[,order(-func.order)]
			} else {
				x <- x[,order(func.order)]
				}
			}

		}

	# check group is right length
	if (!is.null(group)) {
		if (by == "col") {
			if (sum(!rownames(x) %in% skip) != length(group)) {stop("group is wrong length")}
			}
		if (by == "row") {
			if (sum(!colnames(x) %in% skip) != length(group)) {stop("group is wrong length")}
			}
		}
	
	if (by == "col") {
		if (reverse) {
			x = x[do.call(order, c(as.data.frame(group), rev(x[,!colnames(x) %in% skip]), na.last = na.last, decreasing = decreasing)),];
		} else {
			x = x[do.call(order, c(as.data.frame(group), x[,!colnames(x) %in% skip], na.last = na.last, decreasing = decreasing)),];
			}	
	} else if(by == "row") {
		if (reverse) {
			x = x[,do.call(order, c(as.data.frame(group), rev(as.data.frame(t(x[!rownames(x) %in% skip,]))), na.last = na.last, decreasing = decreasing))];
		} else {
			x = x[,do.call(order, c(as.data.frame(group), as.data.frame(t(x[!rownames(x) %in% skip,])), na.last = na.last, decreasing = decreasing))];
			}
		}

	return(x);
	}
