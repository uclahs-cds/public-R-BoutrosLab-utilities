confusion.matrix <- function(prediction = list(), truth = list(), positive.value = NA) {
	prediction <- as.factor(prediction);
	truth <- as.factor(truth);
	if (length(levels(prediction)) > 2) {
		stop("Your prediction data has more than two levels.");
		}
	if (length(levels(truth)) > 2) {
		stop("Your truth data has more than two levels.");
		}
	if (!setequal(levels(prediction), levels(truth))) {
		stop("Your prediction and truth data do not have the same levels.");
		}
	if (length(prediction) != length(truth)) {
		stop("Your prediction and truth data do not have the same length.");
		}
	if (setequal(levels(truth), c(TRUE, FALSE)) & is.na(positive.value)) {
		positive.value = TRUE;
		}
	if (is.na(positive.value)) {
		warning(paste("positive.value not set.", levels(truth)[1], "used for positive.value."));
		positive.value = levels(truth)[1];
		}
	TP <- length(prediction[prediction == truth & prediction == positive.value]);
	TN <- length(prediction[prediction == truth & prediction != positive.value]);
	FP <- length(prediction[prediction != truth & prediction == positive.value]);
	FN <- length(prediction[prediction != truth & prediction != positive.value]);

	TPR <- TP / (TP + FN); # true positive rate, sensitivity, recall
	TNR <- TN / (FP + TN); # true negative rate, specificity
	PPV <- TP / (TP + FP); # positive predictive value, precision
	NPV <- TN / (TN + FN); # negative predictive value
	FPR <- FP / (FP + TN); # false positive rate, fall-out
	FDR <- FP / (FP + TP); # false discovery rate
	FNR <- FN / (FN + TP); # false negative rate, miss rate
	FOR <- FN / (TN + FN); # false omission rate
	
	ACC <- (TP + TN) / (TP + FN + FP + TN); # accuracy
	F1 <- 2*TP / (2*TP + FP + FN); # F1 score
	MCC <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)); # Matthews correlation coefficient
	
	LRP <- TPR / FPR; # positive likelihood ratio
	LRN <- FNR / TNR; # negative likelihood ratio
	DOR <- LRP / LRN; # diagnostic odds ratio

	base.matrix <- data.frame(observed.positive = c(TP, FN), observed.negative = c(FP, TN));
	row.names(base.matrix) <- c("predicted.positive", "predicted.negative");

	attr(base.matrix, "Sensitivity/Recall") <- TPR;
	attr(base.matrix, "Specificity") <- TNR;
	attr(base.matrix, "Precision") <- PPV;
	attr(base.matrix, "False Discovery Rate") <- FDR;
	attr(base.matrix, "Miss Rate") <- FNR;
	attr(base.matrix, "Fallout") <- FNR;
	attr(base.matrix, "Accuracy") <- ACC;
	attr(base.matrix, "Prevalence") <- (TP + FN) / (TP + FN + FP + TN);
	attr(base.matrix, "F1 Score") <- F1;
	attr(base.matrix, "Matthew's correllation coefficient") <- MCC;
	attr(base.matrix, "Diagnostic Odds Ratio") <- DOR;
	attr(base.matrix, "Negative Predictive Value") <- NPV;
	attr(base.matrix, "False Omission Rate") <- FOR;
	attr(base.matrix, "Positive Likelihood Ratio") <- LRP;
	attr(base.matrix, "Negative Likelihood Ratio") <- LRN;
	attr(base.matrix, "Informedness") <- TPR + TNR - 1;
	attr(base.matrix, "Markedness") <- PPV + NPV - 1;
	return(base.matrix)
	}
