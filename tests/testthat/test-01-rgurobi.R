context('rgurobi')

test_that('gurobi (linear problem)', {
	# init
	model <- list()
	model$A <- matrix(c(1, 2, 3, 0, 1, 1, 0, 0), nrow = 2, ncol=4, byrow=T)
	model$obj <- c(1, 1, 2, 0.1)
	model$sense <- c("<=", ">=")
	model$rhs <- c(4, 1)
	model$vtype  <- "B"
	params <- list(Presolve=2, TimeLimit=100.0)
	result1 <- rgurobi::gurobi(model, params)
	result2 <- gurobi::gurobi(model, params)
	# tests
	expect_equal(result1$status, "OPTIMAL")
	expect_equal(nrow(result1$x), 1)
	expect_equal(length(result1$objval), 1)
	expect_equal(result1$objval, result2$objval)
	expect_equal(result1$x[1,], result2$x)
	if (file.exists('gurobi.log')) unlink('gurobi.log')
})

test_that('gurobi (integer problem)', {
	# load packages
	data(city1990)
	library(dplyr)
	library(tidyr)
	library(fields)
	library(sp)
	library(rworldxtra)
	library(ggplot2)

	## intialization
	n <- nrow(city1990)
	model <- list(modelsense='min')

	## preliminary processing
	# create distance matrix
	cities.dists <- rdist.earth(select(city1990, longitude, latitude))
	transport.costs <- cities.dists * 0.0025 *
		matrix(rep(city1990$first.demand, each=n), byrow=TRUE, ncol=n, nrow=n)

	## main processing
	# create variable dictionary
	tmp <- expand.grid(seq_len(n), seq_len(n))
	var.dict <- c(paste0('X_',seq_len(n)),
		paste0('Y_',tmp[[1]], '_', tmp[[2]]))
	var.dict <- structure(seq_along(var.dict), .Names=var.dict)

	# store model objective function (eqn 1a)
	model$obj <- c(city1990$fixed.cost)
	for (i in seq_len(n))
		for (j in seq_len(n))
			model$obj <- c(model$obj, transport.costs[i,j])

	# preallocate model constraints
	counter <- 0
	model$A_rows <- c()
	model$A_cols <- c()
	model$A_vals <- c()
	model$rhs <- c()
	model$sense <- c()

	# constraint (eqn 1b)
	for (i in seq_len(n)) {
		counter <- counter + 1
		model$A_rows <- c(model$A_rows, rep(counter, n))
		model$A_cols <- c(model$A_cols,
			unname(var.dict[paste0('Y_',i,'_',seq_len(n))]))
		model$A_vals <- c(model$A_vals, rep(1, n))
	}
	model$rhs <- c(model$rhs, rep(1, n))
	model$sense <- c(model$sense, rep('=', n))

	# constraint (eqn 1d)
	for (i in seq_len(n)) {
		model$A_rows <- c(model$A_rows, counter+seq_len(n))
		model$A_cols <- c(model$A_cols, rep(i, n))
		model$A_vals <- c(model$A_vals, rep(1, n))

		model$A_rows <- c(model$A_rows, counter+seq_len(n))
		model$A_cols <- c(model$A_cols,
			unname(var.dict[paste0('Y_',seq_len(n),'_',i)]))
		model$A_vals <- c(model$A_vals, rep(-1, n))

		model$rhs <- c(model$rhs, rep(0, n))
		model$sense <- c(model$sense, rep('>', n))
		
		counter <- counter + n
	}

	# construct model matrix
	model$A <- sparseMatrix(i=model$A_rows, j=model$A_cols, x=model$A_vals)

	# constraint (eqn 1e)
	model$vtype <- rep('B', length(var.dict))
	model$ub <- rep(1, length(var.dict))
	model$lb <- rep(0, length(var.dict))

	# solve model
	result1 <- rgurobi::gurobi(model, params=list(Presolve=2), NumberSolutions=2)
	result2 <- gurobi::gurobi(model, params=list(Presolve=2))
	
	## tests
	expect_equal(result1$objval[1], result2$objval)
	expect_equal(result1$x[1,], result2$x)
	if (file.exists('gurobi.log')) unlink('gurobi.log')
})

test_that('rapr problem', {
	# load model
	load('model.rda')

	# solve model
	result1 <- rgurobi::gurobi(model, params=list(Presolve=2))
	result2 <- gurobi::gurobi(model, params=list(Presolve=2))
	
	# tests
	expect_equal(result1$objval[1], result2$objval)
	expect_equal(result1$x[1,], result2$x)

})
