# Create the template of scores for each (sub-)criterion, including
# the underlying output variable and the condition for the scoring

crit_scores_tmpl <- tibble::tibble(
	crit_id = c( rep(1, 2), rep(2, 8), rep(3, 10), rep(4, 3) ),
	crit = c( rep("C8", 2), rep("C9", 8), rep("C10", 10), rep("C11", 3) ),
	subcrit_id =  c( rep(0, 2), rep(1, 4), rep(2, 4), rep(1, 3),
																	rep(2, 2), rep(3, 3), rep(4, 2), rep(0, 3) ),
	subcrit = c( rep("C8", 2), rep("C9_1", 4), rep("C9_2", 4), rep("C10_1", 3),
												 rep("C10_2", 2), rep("C10_3", 3), rep("C10_4", 2), rep("C11", 3) ),
	definition = c( rep("reflect change", 2), rep("sensitivity: strength of IND response to pressure", 4),
			             rep("sensitivity: pressure range of IND response", 4), rep("robustness: expectation of relationship", 3),
												     rep("robustness: non-linearity of IND response", 2), rep("robustness: predictive model performance (nrsme)", 3),
		               rep("robustness: pressure interaction (thresh_gam)", 2),
		               rep("management link", 3) ),
	score_explanation = c( "not met", "yes, met",
	    "p > 0.05", "r_sq < 20%", "r_sq < 40%", "r_sq >= 40%",
    	"< 25% of range", "< 50% of range", "< 75% of range", ">=75% of range",
	    "no", "neutral (or no expectation)", "yes",
    	"yes (edf >= 1.5)", "no (edf < 1.5)",
    	"nrmse > 2", "nrmse <= 2", "nrmse <= 1",
	    "yes","no",
	    "response to > 2 pressure types",
	    "response to 2 pressure types",
	    "response to 1 pressure type"),
	score = c( 0,1,0,1,2,3,0,1,2,3,0,1,2,0,1,0,1,2,0,1,0,1,2),
	weight = rep(1, 23),
	score_pressure_specific = c( rep("no", 2), rep("yes", 18), rep("no", 3)),
	condition = list("x > 0.05", "x <= 0.05",
						FALSE, "x < 0.2", "x >= 0.2 & x < 0.4", "x > 0.4",
						"x < 0.25", "x >= 0.25 & x < 0.5", "x >= 0.5 & x < 0.75", "x >= 0.75",
						"identical(FALSE, x)", "identical(TRUE, x)", "identical(FALSE, x)",
						"x >= 1.5", "x < 1.5",
						"x > 2", "x > 1 & x <= 2", "x <= 1",
						"identical(TRUE, x)", "identical(FALSE, x)",
						"x > 2", "x == 2", "x == 1"),
	condition_var = c( rep("p_val",2), rep("r_sq", 4), rep("prop", 4),
																			 rep("expect", 3), rep("edf", 2), rep("nrmse", 3),
																			 rep("interaction", 2), rep(NA, 3) ),
	func_name = c( rep("model_trend", 2),
															 rep("model_gam/model_gamm", 4), rep("calc_deriv", 4),
															 rep("expect_resp", 3), rep("model_gam/model_gamm", 2),
															 rep("model_gam/model_gamm", 3), rep("test_interaction", 2),
															 rep(NA, 3) )
)


save(crit_scores_tmpl, file = "data/crit_scores_tmpl.rda")
