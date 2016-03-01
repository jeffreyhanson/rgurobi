#' Solve an LP, QP, or MIP using the Gurobi Optimizer
#'
#' Interface to the Gurobi Optimizer, which solves linear, quadratic, and mixed integer programming problems.
#'
#' The interface can be used to solve optimization problems of the following form:
#'
#' minimize	x'Qx + c'x	
#' subject to	Ax = b	(linear constraints)
#' l <= x <= u	(bound constraints)
#' some xj integral	(integrality constraints)
#' some xk lie within second order cones	(cone constraints)
#' Many of the model components listed here are optional.
#'
#' @param model \code{list} object with the model specification (see details).
#' @param params \code{list} object with Gurobi parameters. Defaults to \code{NULL} and uses default Gurobi parameters.
#' @param NumberSolutions \code{integer} number of solutions to return. Defaults to 1.
#' @param verbose \code{logical} should messages be printed?
#'
#' @details The Gurobi Optimizer is a commercial library for solving linear, quadratic, and mixed integer programming problems. More information on Gurobi Optimization, and online documentation can be found at http://www.gurobi.com. See the package vignette for a worked example.
#'
#' The gurobi function takes a pair of list variables, each consisting of multiple named components.
#' The first argument, model, contains the optimization model to be solved. Many of model's named components are optional. The following is an enumeration of all the named components of the model argument.
#'
#' model$A	The linear constraint matrix. This can be dense or sparse. Sparse matrices should be built using either sparseMatrix from the Matrix package, or simple_triplet_matrix from the slam package.
#' model$obj	The linear objective vector (the c vector in the problem statement above). You must specify one value for each column of A.
#' model$sense	The senses of the linear constraints. Allowed values are '=', '<=', or '>='. You must specify one value for each row of A.
#' model$rhs	The right-hand side vector for the linear constraints (the b vector in the problem statement above). You must specify one value for each row of A.
#' model$lb	Optional. The lower bound vector. When present, you must specify one value for each column of A. When absent, each variable has a lower bound of 0.
#' model$ub	Optional. The upper bound vector. When present, you must specify one value for each column of A. When absent, the variables have infinite upper bounds.
#' model$vtypes	Optional. The variable type vector. This vector is used to capture variable integrality constraints. Allowed values are 'C' (continuous), 'B' (binary), 'I' (integer), 'S' (semi-continuous), or 'N' (semi-integer). Binary variables must be either 0 or 1. Integer variables can take any integer value between the specified lower and upper bounds. Semi-continuous variables can take any value between the specified lower and upper bounds, or a value of zero. Semi-integer variables can take any integer value between the specified lower and upper bounds, or a value of zero. When present, you must specify one value for each column of A. When absent, each variable is treated as being continuous.
#' model$modelsense	Optional. The optimization sense. Allowed values are 'min' (minimize) or 'max' (maximize). When absent, the default optimization sense is minimization.
#' model$modelname	Optional. The name of the model. The name appears in the Gurobi log, and when writing a model to a file.
#'
#' @return A \code{list} object containing the optimal solution, with the following components:
#' result$status	The status of the optimization, returned as a string. The desired result is "OPTIMAL", which indicates that an optimal solution to the model was found. Other status are possible, for example if the model has no feasible solution or if you set a Gurobi parameter that leads to early solver termination. Status codes are documented in the Gurobi Reference Manual.
#' result$objval	The value of the objective function for the computed solution. Not populated if optimization terminated without finding a feasible solution.
#' result$x	Variable values for the best solution found. One entry per column of A. Not present if optimization terminated without finding a feasible solution.
#' result$slack	Constraint slacks. One entry per row of A.
#' result$pi	Dual multipliers for the constraints. One entry per row of A. Only returned for continuous models.
#' result$rc	Variable reduced costs. One entry per column of A. Only returned for continuous models.
#' result$vbasis	Variable basis status values for the computed optimal basis. You generally should not concern yourself with the contents of this array. If you wish to use an advanced start later, you would simply copy the vbasis and cbasis arrays into the corresponding components for the next model. This array contains one entry for each column of A.
#' result$cbasis	Constraint basis status values for the computed optimal basis. This array contains one entry for each row of A.
#' 
#' @examples
#' #
#' # minimize:   x +   y + 2 z
#' # subject to: x + 2 y + 3 z <= 4
#' #             x +   y       >= 1
#' #             x, y, z binary
#' model <- list()
#' model$A <- matrix(c(1, 2, 3, 1, 1, 0), nrow = 2, ncol=3, byrow=T)
#' model$obj <- c(1, 1, 2)
#' model$sense <- c("<=", ">=")
#' model$rhs <- c(4, 1)
#' model$vtype <- "B"
#' params <- list(Presolve=2, TimeLimit=100.0)
#' result <- gurobi(model, params)
#' print(result$objval)
#' print(result$x)
#' @export
gurobi <- function (model, params = NULL, NumberSolutions=1, verbose=FALSE) {
	## check input for validity
	# check that model has all elements
	expect_true('A' %in% names(model))
	expect_true('obj' %in% names(model))
	expect_true('sense' %in% names(model))
	expect_true('rhs' %in% names(model))
	expect_true('vtype' %in% names(model))
	expect_equal(length(model$obj), ncol(model$A))
	expect_true(all(!is.na(model$obj)))
	if (length(model$vtype)==1) model$vtype=rep(model$vtype[1], ncol(model$A))
	expect_equal(length(model$vtype), ncol(model$A))
	expect_true(all(!is.na(model$vtype)))
	expect_equal(length(model$rhs), length(model$sense))
	expect_true(all(model$sense %in% c('=', '<=', '>=', '<', '>')))
	expect_true(all(!is.na(model$sense)))
	if (is.null(model$sense)) model$sense='min'
	expect_equal(length(model$sense),nrow(model$A))
	expect_true(all(!is.na(model$sense)))
	expect_true(NumberSolutions>0)
	if (is.null(model$modelsense)) model$modelsense<-'min'
	expect_true(model$modelsense %in% c('min', 'max'))
	expect_equal(length(model$modelsense),1)
	if (is.null(model$modelname)) model$modelname<-'model'
	model$modelname<-as.character(model$modelname)
	if (is.null(model$lb)) model$lb<-numeric(0)
	if (is.null(model$ub)) model$ub<-numeric(0)
	if (is.null(params)) params <- list()
	expect_true(all(!is.na(model$lb)))
	expect_true(all(!is.na(model$ub)))
	## prepare model inputs
	# convert model$A to sparse matrix
	if (!inherits(model$A, 'dgTMatrix'))
		model$A <- as(model$A, 'dgTMatrix')
	model$A_rows <- model$A@i
	model$A_cols <- model$A@j
	model$A_vals <- model$A@x
	expect_true(all(!is.na(model$A_rows)))
	expect_true(all(!is.na(model$A_cols)))
	expect_true(all(!is.na(model$A_vals)))
	model$sense <- gsub('>=', '>', model$sense, fixed=TRUE)
	model$sense <- gsub('<=', '<', model$sense, fixed=TRUE)
	## run gurobi
	out <- solve_gurobi(
		model=model,
		param_names=names(params),
		param_vals=unlist(as.character(params), recursive=TRUE, use.names=FALSE),
		NumberSolutions,
		verbose
	)
	## post processing
	if (length(out$obj)!=NumberSolutions & out$status=='OPTIMAL')
		warning(paste0('only ',length(out$obj),' solutions found.'))
	## return result
	return(out)
}

