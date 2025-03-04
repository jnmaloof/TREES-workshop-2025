//**********************************************************************************
//added dependence on Ca - March, 2007
//Multivariate normal proposal distribution, trace option removed - 2/12/07
//LINEAR D, lfscl, Q, T, and swc - 10/10/06
//parameter values turn constraints off & on - 2/12/07
//adding additional data col swp in kPa (negative)
//made more compact, uses canopy D for gs and above canopy D for pm and gHa - 271004
//(c)2007 - Sudeep Samanta
//Added carbon cycling (c)2007, 2008 - Scott Mackay
//Added plant water balance model (c) 2010, 2011 David Roberts & Scott Mackay
//Added canopy phenology (c) 2013, 2014 Phil Savoy & Scott Mackay
//**********************************************************************************
#ifndef _MAIN_H
#define _MAIN_H

#include "simulator2.h"
#include "time_keeper.h"
#include "data_store.h"
#include "state_store.h"
#include <Rcpp.h>

Rcpp::List rTREES(
    Rcpp::DataFrame env_driver_in,
    Rcpp::DataFrame base_parameters,
    Rcpp::List root_shoot_soil,
    Rcpp::LogicalVector which_out,
    bool verbosity,
    int softerror_max
);

#endif
