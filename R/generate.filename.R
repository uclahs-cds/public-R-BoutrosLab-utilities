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

#' Generate a filename in the lab standard
#'
#' This function creates a filename according to the date_project_core.extension lab standard
#'
#' @param project.stem Name of the project
#' @param file.core Main part of the filename
#' @param extension File extension
#' @param file.date Optional date of filename. (defaults to today; FALSE or NULL turns off the date-stamp)
#'
#' @return
#' @export
#'
#' @examples
#' # generate a filename to screen
#' generate.filename('NSCLC', 'StatisticalAnalysis', 'txt');
#'
# generate a filename in context of another function
#' save.session.profile( generate.filename('Prostate', 'StatisticalAnalysisSessionProfile', 'txt') );
#'
# generate a filename without a date
#' generate.filename('NSCLC', 'StatisticalAnalysis', 'txt', FALSE);
generate.filename <- function(project.stem, file.core, extension, file.date = Sys.Date()) {
	if(is.null(file.date) || (is.logical(file.date) && ! file.date)) {
		date.prefix <- '';
		} else {
		# Check that given date conforms to YYYY-MM-DD
		valid.file.date <- try(as.Date(as.character(file.date), format = "%Y-%m-%d"), silent = TRUE)
		if(is.na(valid.file.date) || (! "Date" %in% class(valid.file.date))) {
			stop(file.date, ' is not is yyyy-mm-dd format or valid date.');
		}
		date.prefix <- paste0(valid.file.date, '_')
		}

	file.name <- sprintf('%s%s_%s.%s', date.prefix, project.stem, file.core, extension);

	return(file.name);
	}
