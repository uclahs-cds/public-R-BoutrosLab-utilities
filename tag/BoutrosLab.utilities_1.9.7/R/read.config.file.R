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

read.config.file <- function(config.file = "config.txt", convert.path = FALSE, 
	old.path = if(.Platform$OS.type == "unix") "I:" else "/mnt/isilon/private", 
	new.path = if(.Platform$OS.type == "unix") "/mnt/isilon/private" else "I:") {

	if (!file.exists(config.file)) {
		stop("Cannot open config file");
		}
	
	config <- read.table(
		file = config.file, 
		sep = "=", 
		as.is = TRUE,
		strip.white = TRUE,
		comment.char = "#",
		header = FALSE,
		row.names = 1
		);
	
	cfg <- list();
	cfg[rownames(config)] <- suppressWarnings(as.numeric(config[,1]));
	cfg[is.na(cfg)] <- config[is.na(cfg),1];

	if(convert.path == TRUE) {
		cat(convert.path, old.path, new.path)
		cfg <- lapply(cfg, function(x) gsub(old.path, new.path, x) );
		}

	return(cfg);
		
	}
