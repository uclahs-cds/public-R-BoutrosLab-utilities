recode.vector <- function(x, lookup, drop = FALSE) {

	if (!drop) {
		names(x) <- x;
		lookup <- c(as.list(x),lookup);
		}
	x <- factor(x,exclude="");
	levels(x) <- lookup;
	x <- as.character(x);
	x;
	}
