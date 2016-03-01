rgurobi
============

[![DOI](https://zenodo.org/badge/18940/paleo13/rgurobi.svg)](https://zenodo.org/badge/latestdoi/18940/paleo13/rgurobi)

### Solve optimisation problems using the commercial [Gurobi software package](http://www.gurobi.com/). This package provides similar functionality to the [official gurobi R package](http://www.gurobi.com/products/modeling-languages/r). However, this package allows users to retrieve multiple solutions from the solution pool after solving a problem. Note that this package does not support quadratic expressions.

To install this package, the Gurobi software package first must be installed (see here for instructions: [Linux](www.gurobi.com/documentation/6.5/quickstart_linux.pdf), [Windows](www.gurobi.com/documentation/6.5/quickstart_windows.pdf), [Mac OSX](www.gurobi.com/documentation/6.5/quickstart_mac.pdf)). 

Additionally, the `GUROBI_HOME` environmental variable must be set to the Gurobi installation directory. **The package will fail to install if this variable is not set.**

To install this R package, use the following R code:

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('paleo13/rgurobi')
```

Once this package has been installed, you can read through the vignette for a tutorial on how to use it.

[View it here](https://github.com/paleo13/rgurobi/raw/master/inst/doc/rgurobi.pdf), or by running this R code:

```
# open vignette in web browser
vignette('rgurobi', package='rgurobi')
```

**If this R package helped you, please cite it.**

Hanson J.O. (2016). rgurobi: Solve optimisation problems using Gurobi. Version 1.0.0. doi: 10.5281/zenodo.35188.
