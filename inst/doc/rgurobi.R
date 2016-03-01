## ----global, include=FALSE-----------------------------------------------
# load rapr R package
library(rgurobi)

# set cache globally
knitr::opts_chunk$set(cache=TRUE)

# set seed for reproducibility
set.seed(500)

## ------------------------------------------------------------------------
# load data
data(city1990)

# show first 20 rows of data
head(city1990)

## ---- message=FALSE------------------------------------------------------
# load packages
library(dplyr)
library(tidyr)
library(fields)
library(sp)
library(rworldxtra)
library(ggplot2)

## ---- message=FALSE, fig.height=2.5, fig.width=4, fig.cap='Points denote the location of cities. The color of each point indicates the relative demand for the product in the city.'----
# load map data
data(countriesHigh)
countries.FPLY <- countriesHigh[countriesHigh$ADMIN == 
	'United States of America',]

# make plot
ggplot() +
	geom_polygon(data=countries.FPLY, aes(x=long, y=lat, group=group),
		fill='grey85', color='grey70') +
	geom_point(data=city1990, aes(x=longitude, y=latitude, color=first.demand),
		size=2, alpha=0.6) +
	theme_classic() +
	theme(axis.ticks=element_blank(), axis.text=element_blank(),
		plot.margin=unit(c(0,0,0,0),'cm'), axis.line=element_blank(),
		strip.background = element_rect(fill='grey20'),
		strip.text = element_text(color='white'),
		axis.title=element_blank(), legend.title=element_blank()) +
	coord_cartesian(
		xlim=range(city1990$longitude),
		ylim=range(city1990$latitude)) +
	scale_color_distiller(name='Demand', palette='YlGnBu')

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
# solve the problem
result <- gurobi(model, params=list(Presolve=0), NumberSolutions=2)

# print total cost of best solution
print(result$obj[1])

# extract selected facility locations
solution.locations <- result$x[,seq_len(n),drop=FALSE]

# print number of facilities in best solution
print(sum(solution.locations[1,]))

## ---- message=FALSE, fig.height=2.5, fig.width=5, fig.cap='Points denote cities. Cities that have been selected to host a facility are denoted in blue. Lines indicate transportation routes.'----
# prepare data
city1990$Solution <- c('Not selected','Selected')[solution.locations[1,]+1]
city1990$Frequency <- colMeans(solution.locations)
connections <- data.frame(
	connection=grep('^Y\\_.*$', names(var.dict), value=TRUE),
	status=result$x[1,grep('^Y\\_.*$', names(var.dict), value=FALSE)])
connections <- connections %>% 
	filter(status==1) %>%
	mutate(
		i=as.numeric(sapply(strsplit(as.character(connection), '\\_'), `[[`, 2)),
		j=as.numeric(sapply(strsplit(as.character(connection), '\\_'), `[[`, 3)),
		i.longitude=city1990$longitude[i],
		i.latitude=city1990$latitude[i],
		j.longitude=city1990$longitude[j],
		j.latitude=city1990$latitude[j]
	)
connections <- cbind(
		gather(connections, point, longitude, i.longitude, j.longitude),
		select(gather(connections, point, latitude, i.latitude, j.latitude),
			latitude)
	) %>% 
	select(connection,i,j,longitude,latitude) %>%
	arrange(connection)

# visualise best solution
ggplot() +
	geom_polygon(data=countries.FPLY, aes(x=long, y=lat, group=group),
		fill='grey85', color='grey70') +
	geom_line(data=connections, aes(x=longitude, y=latitude, group=connection),
		color='grey10') +
	geom_point(data=city1990, aes(x=longitude, y=latitude,
		color=Solution)) +
	theme_classic() +
	theme(axis.ticks=element_blank(), axis.text=element_blank(),
		plot.margin=unit(c(0,0,0,0),'cm'), axis.line=element_blank(),
		strip.background = element_rect(fill='grey20'),
		strip.text = element_text(color='white'),
		axis.title=element_blank()) +
	coord_cartesian(
		xlim=range(city1990$longitude),
		ylim=range(city1990$latitude))

## ---- message=FALSE, fig.height=2.5, fig.width=5, fig.cap='Points indicate cities. The size of each point indicates the number of times that the corresponding city has been selected as a place to build a facility. '----
# make plot
ggplot() +
	geom_polygon(data=countries.FPLY, aes(x=long, y=lat, group=group),
		fill='grey85', color='grey70') +
	geom_point(data=city1990, aes(x=longitude, y=latitude, size=Frequency)) +
	theme_classic() +
	theme(axis.ticks=element_blank(), axis.text=element_blank(),
		plot.margin=unit(c(0,0,0,0),'cm'), axis.line=element_blank(),
		strip.background = element_rect(fill='grey20'),
		strip.text = element_text(color='white'), axis.title=element_blank()) +
	coord_cartesian(
		xlim=range(city1990$longitude),
		ylim=range(city1990$latitude))

