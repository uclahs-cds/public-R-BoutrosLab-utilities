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

string.capitalize <- function(input.string, all.words = FALSE) {

	# split the input string into its component words
        output.string <- strsplit(input.string, ' ')[[1]];

	# capitalize just the first word or all the words as needed
        if (FALSE == all.words) {
                output.string <- paste(
                                        paste(toupper(substring(output.string[1], 1, 1)), substring(output.string[1], 2), sep = ''),
                                        paste(output.string[-1], collapse = ' '),
                                        sep = ' '
                                        );
                }
        else {
                output.string <- paste(
					toupper(substring(output.string, 1, 1)), substring(output.string, 2),
					collapse = ' ', sep = ''
					);
                }

        return(output.string);
        }

