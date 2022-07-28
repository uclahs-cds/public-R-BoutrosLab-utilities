### calculate.mcc.R ###############################################################################
# Description: calculates matthew's correlation coefficient based on predicted class and true 
# 		class
# NOTE: currently only supports binary classification
# INPUT:
#	predicted.class: vector of predicted classes
#	true.class: vector of true classes
# OUTPUT: 
#	mcc: Matthew's correlation coefficient
#
### FUNCTION ######################################################################################
calculate.mcc <- function(predicted.class, true.class) {


	confusion.matrix <- table(true.class, predicted.class);

	TP <- as.numeric(confusion.matrix[2, 2]);
	TN <- as.numeric(confusion.matrix[1, 1]);
	FP <- as.numeric(confusion.matrix[1, 2]);
	FN <- as.numeric(confusion.matrix[2, 1]);
	
	mcc <- ((TP*TN) - (FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN));
	
	return(mcc);
	
	}
