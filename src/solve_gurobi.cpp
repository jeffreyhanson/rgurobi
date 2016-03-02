#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;


#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>
#include <iostream>
#include "gurobi_c++.h"

// [[Rcpp::export]]
Rcpp::List solve_gurobi(Rcpp::List model, std::vector<std::string> param_names, std::vector<std::string> param_vals, std::size_t NumberSolutions, bool verbose) {
	//// Initialization 
	if (verbose) Rcout << std::endl << "Initialization" << std::endl;
	
	//// Preliminary processing
	if (verbose) Rcout << "Preliminary processing" << std::endl;
	// load data as vectors
	if (verbose) Rcout << "\tloading data" << std::endl;
	std::vector<int> A_rows=model["A_rows"];
	std::vector<int> A_cols=model["A_cols"];
	std::vector<double> A_vals=model["A_vals"];
	std::vector<double> obj=model["obj"];
	std::vector<double> rhs=model["rhs"];
	std::vector<double> ub=model["ub"];
	std::vector<double> lb=model["lb"];
	std::vector<std::string> vtype=model["vtype"];
	std::vector<std::string> sense=model["sense"];
	std::string modelsense=model["modelsense"];
	std::string modelname=model["modelname"];
	
	// cache helper variables
	if (verbose) Rcout << "\tstoring helper vars" << std::endl;
	int ncol = obj.size();
	int nrow = rhs.size();
	// prepare data for gurobi
	if (verbose) Rcout << "\tcreating native c objects" << std::endl;
	std::vector<char> vtype_char(ncol);
	for (std::size_t i=0; i<ncol; ++i) vtype_char[i]=vtype[i][0];
	std::vector<char> sense_char(nrow);
	for (std::size_t i=0; i<nrow; ++i) sense_char[i]=sense[i][0];
	std::vector<std::string> names(nrow);
	for (std::size_t i=0; i<nrow; ++i) names[i]="C"+std::to_string(i);
	
	/// initialize gurobi object
	if (verbose) Rcout << "\tinitializing grb object" << std::endl;
	GRBEnv env_grb = GRBEnv();
	GRBModel model_grb = GRBModel(env_grb);
	
	// set model name
	model_grb.set(GRB_StringAttr_ModelName, modelname);
	model_grb.update();
	// add parameters
	if (verbose) Rcout << "\tadding parameters" << std::endl;
	for (std::size_t i=0; i<param_names.size(); ++i)
		model_grb.getEnv().set(param_names[i], param_vals[i]);
	// create vars
	if (verbose) Rcout << "\tadding vars" << std::endl;
	GRBVar* vars_grb;
	if (lb.size()>0 & ub.size()>0) {
		vars_grb=model_grb.addVars(&lb[0], &ub[0], &obj[0], &vtype_char[0], NULL, ncol);
	} else if (lb.size()>0 & ub.size()==0) {
		vars_grb=model_grb.addVars(&lb[0], NULL, &obj[0], &vtype_char[0], NULL, ncol);
	} else if (lb.size()==0 & ub.size()>0) {
		vars_grb=model_grb.addVars(NULL, &ub[0], &obj[0], &vtype_char[0], NULL, ncol);
	} else if (lb.size()==0 & ub.size()==0) {
		vars_grb=model_grb.addVars(NULL, NULL, &obj[0], &vtype_char[0], NULL, ncol);
	} else {
		Rf_error("Error processing lb and ub data");
	}
	model_grb.update();
	// set model sense
	if (modelsense=="min") {
		model_grb.set(GRB_IntAttr_ModelSense, 1);
	} else {
		model_grb.set(GRB_IntAttr_ModelSense, -1);
	}
	model_grb.update();
	
	// add constraints
	if (verbose) Rcout << "\tadding constrs" << std::endl;
	GRBLinExpr *lhs_grb = new GRBLinExpr[nrow];
	for (std::size_t i=0; i<nrow; ++i)
		lhs_grb[i]=0;
	for (std::size_t i=0; i<A_rows.size(); ++i) {
		lhs_grb[A_rows[i]]+=A_vals[i]*vars_grb[A_cols[i]];
	}
	model_grb.addConstrs(lhs_grb, &sense_char[0], &rhs[0], &names[0], nrow);
	model_grb.update();
	
	//// Main processing
	if (verbose)  Rcout << "Main processing" << std::endl;
	// run gurobi
	model_grb.optimize();
	// check result validity
	int optimstatus=model_grb.get(GRB_IntAttr_Status);
	if (optimstatus==GRB_OPTIMAL) {
	} else if (optimstatus==GRB_INF_OR_UNBD) {
		return(Rcpp::List::create(Rcpp::Named("status") = Rcpp::wrap("INFEASIBLE_OR_UNBOUNDED")));
	} else if (optimstatus==GRB_INFEASIBLE) {
		return(Rcpp::List::create(Rcpp::Named("status") = Rcpp::wrap("INFEASIBLE")));
	} else if (optimstatus==GRB_UNBOUNDED) {
		return(Rcpp::List::create(Rcpp::Named("status") = Rcpp::wrap("UNBOUNDED")));
	} else {
		return(Rcpp::List::create(Rcpp::Named("status") = Rcpp::wrap("ERROR_"+std::to_string(optimstatus))));
	}

	//// Exports
	if (verbose) Rcout << "Exports" << std::endl;
	// extract solutions
	if (verbose) Rcout << "\textracting solutions" << std::endl;
	NumberSolutions = std::min(static_cast<std::size_t>(model_grb.get(GRB_IntAttr_SolCount)), NumberSolutions);
	Rcpp::NumericMatrix x(NumberSolutions, ncol);
	Rcpp::NumericVector objval(NumberSolutions);
	for (std::size_t k=0; k<NumberSolutions; ++k) {
		// init
		model_grb.getEnv().set(GRB_IntParam_SolutionNumber, k);
		objval[k]=0.0;
		// iterate over variables and store values
		for (std::size_t j=0; j<ncol; ++j) {
			objval[k]+=static_cast<double>(vars_grb[j].get(GRB_DoubleAttr_Obj) * vars_grb[j].get(GRB_DoubleAttr_Xn));
			x(k,j)=static_cast<double>(vars_grb[j].get(GRB_DoubleAttr_Xn));
		}
	}
	std::string modelstatus;
	switch(static_cast<int>(model_grb.get(GRB_IntAttr_Status))) {
		case 1:
			modelstatus="LOADED"; break;
		case 2:
			modelstatus="OPTIMAL"; break;
		case 3:
			modelstatus="INFEASIBLE"; break;
		case 4:
			modelstatus="INF_OR_UNBD"; break;
		case 5:
			modelstatus="UNBOUNDED"; break;
		case 6:
			modelstatus="CUTOFF"; break;
		case 7:
			modelstatus="ITERATION_LIMIT"; break;
		case 8:
			modelstatus="NODE_LIMIT"; break;
		case 9:
			modelstatus="TIME_LIMIT"; break;
		case 10:
			modelstatus="SOLUTION_LIMIT"; break;
		case 11:
			modelstatus="INTERRUPTED"; break;
		case 12:
			modelstatus="NUMERIC"; break;
		case 13:
			modelstatus="SUBOPTIMAL"; break;
		case 14:
			modelstatus="INPROGRESS"; break;
		default:
			modelstatus="UNKNOWN_ERROR"; break;
	}
	
	double runtime = static_cast<double>(model_grb.get(GRB_DoubleAttr_Runtime));
	double itercount = static_cast<double>(model_grb.get(GRB_DoubleAttr_IterCount));
	double baritercount = static_cast<double>(model_grb.get(GRB_IntAttr_BarIterCount));
	double nodecount = static_cast<double>(model_grb.get(GRB_DoubleAttr_NodeCount));
	
	// clean-up
	if (verbose) Rcout << "\tgarbage collection" << std::endl;
	delete[] lhs_grb;
	
	// return result list
	if (verbose) Rcout << "\treturning results" << std::endl;
	return(
		Rcpp::List::create(
			Rcpp::Named("status") = modelstatus,
			Rcpp::Named("runtime") = runtime,
			Rcpp::Named("itercount") = itercount,
			Rcpp::Named("baritercount") = baritercount,
			Rcpp::Named("nodecount") = nodecount,
			Rcpp::Named("objval") = Rcpp::wrap(objval),
			Rcpp::Named("x") = Rcpp::wrap(x)
		)
	);
}