
.onUnload <- function(libpath) {
	library.dynam.unload('rgurobi', libpath)
}
