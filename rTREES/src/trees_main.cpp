//
//trees_main.cpp
//
//main(): First function called when TREES in run
//
//Allows TREES to run in either Bayesian statistical mode when one or more parameters is preceded by an 'm',
//    or otherwise as a single, deterministic simulation
//
//Bayesian mode implements simulations described in Samanta et al 2007, 2008 Water Resources Research
//    and Mackay et al 2012 Journal of Hydrology
//
//Plant hydraulics implementation described in Mackay et al 2015 Water Resources Research
//
//Canopy photosynthesis implementation described in Loranty et al 2010 WRR and JGR-BGC
//
//
//
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
//
//

#include "trees_main.h"
#include <Rcpp.h>
// #include <unistd.h>

//' This function prepares the input files and runs simulator. Only the first three parameters are mandatory.
//' @param env_driver data.frame() containing environmental data for each time step. See Driver_inputs() for explanations of required columns. Use ValidateDriver() to check for errors.
//' @param base_parameters data.frame() where the first column is parameter names and the second column contains values. See Possible_parameters() for explanations of required parameters.
//' @param root_shoot_soil list() where each item is a vector for each type of parameter in orig param_mod file
//' @param which_out vector() of length 3 of boolean values to restrict which outputs are returned. Format is in order sim,hyd,leaf
//' @param softerror_max integer for how sensitive TREES should be before failing. This mostly exists to allow infrequent hydraulic errors to only stop the simulation if they occur a certain number of times.
//' @param verbosity true displays extra verbosity.
//'
//' @return a list of leaf areas, simulation outputs, and hydraulic outputs
//'
//' @examples
//' demo_standard_run<<-rTREES(
//'      env_driver = BuildDriver(ndays = 10),
//'      base_parameters = ExampleParameters(),
//'      root_shoot_soil = ExampleRSS()
//'  )
//' demo_cleaned_run<<-Clean_rTREES_output(
//'     rTREES_output = demo_standard_run
//'     )
//' @export
// [[Rcpp::export]]
Rcpp::List rTREES(
    Rcpp::DataFrame env_driver,
    Rcpp::DataFrame base_parameters,
    Rcpp::List root_shoot_soil,
    Rcpp::LogicalVector which_out =Rcpp::LogicalVector::create(1,1,1),
    bool verbosity=false,
    int softerror_max=5
)
{


  //clone inputs because Rcpp defaults to passing by reference.
  //MCH does not find this to be a good memory safe option
  //This is also more function oriented
  Rcpp::DataFrame env_driver_c=Rcpp::clone(env_driver);
  Rcpp::DataFrame base_parameters_c=Rcpp::clone(base_parameters);
  Rcpp::List root_shoot_soil_c=Rcpp::clone(root_shoot_soil);
  Rcpp::LogicalVector which_out_c =Rcpp::clone(which_out);
  int softerror_max_c=softerror_max;
  //cloning these was silly because they are not real R objects so Rcpp::clone
  //wouldn't work. They aren't ever modified so it shouldn't matter.
  bool verbosity_c=verbosity;//;Rcpp::clone(verbosity);


  //rTREES added
  Rcpp::StringVector  fluxout_names;
  Rcpp::StringVector  hydrout_names;
  Rcpp::StringVector  lfareaout_names(504);
  int i;

  Rcpp::List fluxout, hydrout;
  Rcpp::List lfareaout(504);
  Data_Store indata;
  State_Store state;
  int nsteps;



  Rcpp::NumericVector current_params=Rcpp::as<Rcpp::NumericVector>(base_parameters_c[0]);
  current_params.names()=Rcpp::as<Rcpp::CharacterVector>(base_parameters_c[1]);

  //For hydraulic model
  struct trees_params treesParams;
  int smodules;
  int rmodules;

  std::vector<bool> to_out = {which_out_c[0] == true,which_out_c[1] == true,which_out_c[2] == true};// == true is a bit hacky but Rcpp::as wasn't working

  // if(
  //   Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[0]==5||
  //     Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[1]==5
  // ){
  //   /*test silent speed*/ if(verbosity_c) Rcpp::Rcout<< "fixing time \n";
  //   for(i=0;i<=env_driver_c.nrow();i++){
  //     Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[i]=Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[i]*6;
  //   }
  // }else if(
  //     Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[0]==50||
  //       Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[1]==50
  // ){
  //   /*test silent speed*/ if(verbosity_c) Rcpp::Rcout<< "fixing time \n";
  //   for(i=0;i<=env_driver_c.nrow();i++){
  //     Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[i]=Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[i]*6/10;
  //   }
  // }else
  if (
      (Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[0] == 0 || Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[0] == 30) &&
        (Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[1] == 0 || Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[1] == 30)
  ) {
    // Time is good, do nothing
    if (verbosity_c) Rcpp::Rcout << "time is good \n";
  } else {
    if (verbosity_c) Rcpp::Rcout << "fixing time \n";
    if (Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[0] > Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[1]) {
      for (i = 0; i < env_driver_c.nrow(); i += 2) {
        Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[i] = 30.0;
        if (i + 1 < env_driver_c.nrow()) { // Check if the next index is valid
          Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[i + 1] = 0.0;
        }
      }
    } else {
      for (i = 0; i < env_driver_c.nrow(); i += 2) {
        Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[i] = 0.0;
        if (i + 1 < env_driver_c.nrow()) { // Check if the next index is valid
          Rcpp::as<Rcpp::NumericVector>(env_driver_c["min"])[i + 1] = 30.0;
        }
      }
    }
  }
  //   if(
  //
  // ){
  //
  // }else{
  //   //time is bad
  //   Rcpp::Rcerr<<"ERROR the minutes for the driver should be base 60 and I can only correct for base 100 or 10. \n";
  // }


  nsteps = get_obs(env_driver_c, indata);
  /*test silent speed*/ if(verbosity_c){
    Rcpp::Rcout<< "Driver contains " << indata.no_of_vars() << " variables "<< nsteps << " data points \n"<< "Should be: "<<env_driver_c.ncol() <<" and "<<env_driver_c.nrow()<<"\n";
  }

  smodules =root_shoot_soil_c["smodules"];//getData(pmodules);
  treesParams.smodules = smodules;


  for(i=1; i<=smodules; i++)
  {
    treesParams.al[i] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["al"])[i-1];//getData(pmodules);
    treesParams.dslat[i] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["dslat"])[i-1];// getData(pmodules);
    treesParams.dsax[i] =  Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["dsax"])[i-1];//getData(pmodules);
  }

  rmodules =root_shoot_soil_c["rmodules"];// getData(pmodules);
  treesParams.rmodules = rmodules;


  for(i=1; i<=rmodules; i++)
  {
    treesParams.ar[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["ar"])[i-1];//getData(pmodules);
    treesParams.drlat[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["drlat"])[i-1];// getData(pmodules);
    treesParams.drax[i + smodules + 1] =  Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["drax"])[i-1];//getData(pmodules);
    //DSM June 2023 - soil texture for each layer
    treesParams.layer_GMP[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["layer_GMP"])[i-1];//getData(pmodules);
    treesParams.layer_GSD[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["layer_GSD"])[i-1];//getData(pmodules);
    treesParams.layer_BD[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["layer_BD"])[i-1];//getData(pmodules);
    treesParams.layer_porosity[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["layer_porosity"])[i-1];//getData(pmodules);
    treesParams.layer_sand_fraction[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["layer_sand_fraction"])[i-1];//getData(pmodules);
    treesParams.layer_clay_fraction[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["layer_clay_fraction"])[i-1];//getData(pmodules);
    treesParams.layer_residual_water_content[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["layer_residual_water_content"])[i-1];//getData(pmodules);
    treesParams.layer_initial_water_content[i + smodules + 1] = Rcpp::as<Rcpp::NumericVector>(root_shoot_soil_c["layer_initial_water_content"])[i-1];//getData(pmodules);
  }


  std::vector<double> blank_fill(nsteps,0.0);
  //Parameter File Validation - **no validation done for shoot and root module read in (add later)
  if(to_out[0]){
    // fluxout_names.push_back("ti" );
    fluxout_names.push_back("year" );
    fluxout_names.push_back("jday" );
    fluxout_names.push_back("hour" );
    fluxout_names.push_back("min" );

    fluxout_names.push_back("simET" );
    fluxout_names.push_back("WPlant_K" );
    fluxout_names.push_back("Soil_Psi" );
    fluxout_names.push_back("Leaf_Psi" );
    fluxout_names.push_back("Psi_Crit" );
    fluxout_names.push_back("Ecrit");
    fluxout_names.push_back("Ec");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("RhizFlux"+std::to_string(i)));

    }

    fluxout_names.push_back("Gs");
    fluxout_names.push_back("LAI");
    fluxout_names.push_back("SLA");
    fluxout_names.push_back("liveLAI");
    fluxout_names.push_back("Rmaint");
    fluxout_names.push_back("Rgrowth");
    fluxout_names.push_back("reproduction");
    fluxout_names.push_back("leafNSC");
    fluxout_names.push_back("stemNSC");
    fluxout_names.push_back("rootNSC");
    fluxout_names.push_back("chloroStarch");
    fluxout_names.push_back("chloroSugar");
    fluxout_names.push_back("waterStress");
    fluxout_names.push_back("litterH2O");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("theta"+std::to_string(i)));
    }
    fluxout_names.push_back("thetaRoot");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("soilPsi"+std::to_string(i)));
      //    fluxout << "soilP" << i << "\t";
    }

    fluxout_names.push_back("Can_Evap");
    fluxout_names.push_back("Snowpack");
    fluxout_names.push_back("SnowEdef");
    fluxout_names.push_back("Vcmax25");
    fluxout_names.push_back("Vcmax_sun");
    fluxout_names.push_back("Vcmax_shd");
    fluxout_names.push_back("Jmax25");
    fluxout_names.push_back("J_sun");
    fluxout_names.push_back("J_shd");
    fluxout_names.push_back("Asun");
    fluxout_names.push_back("Ashd");

    // Av_sun\t Av_shd \t Aj_sun\t Aj_shd\t phi2_sun\t phi2_shd\t betaA_sun\t betaA_shd
    fluxout_names.push_back("Av_sun");
    fluxout_names.push_back("Av_shd");
    fluxout_names.push_back("Aj_sun");
    fluxout_names.push_back("Aj_shd");
    fluxout_names.push_back("phi2_sun");
    fluxout_names.push_back("phi2_shd");
    fluxout_names.push_back("betaA_sun");
    fluxout_names.push_back("betaA_shd");

    fluxout_names.push_back("Lsun");
    fluxout_names.push_back("Lshd");
    fluxout_names.push_back("Tsun");
    fluxout_names.push_back("Tshd");
    fluxout_names.push_back("Dsun");
    fluxout_names.push_back("Dshd");
    fluxout_names.push_back("Ci_sun");
    fluxout_names.push_back("Ci_shd");
    fluxout_names.push_back("PARsun");
    fluxout_names.push_back("PARshd");
    fluxout_names.push_back("gs_sun");
    fluxout_names.push_back("gs_shd");
    fluxout_names.push_back("NEE");
    fluxout_names.push_back("NPP");
    fluxout_names.push_back("R_total");
    fluxout_names.push_back("R_ag");
    fluxout_names.push_back("R_bg");
    fluxout_names.push_back("Rd_sun");
    fluxout_names.push_back("Rd_shd");
    fluxout_names.push_back("Csapwood");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("FibRootC"+std::to_string(i)));
    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("FineRootC"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("TotRootC"+std::to_string(i)));

    }
    /*
     for (i = 0; i < treesParams.rmodules; i++)
     {
     fluxout << "CoarseRoot" << i << "\t";
     }
     */
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("FineRootCN"+std::to_string(i)));

    }
    fluxout_names.push_back("LeafCN");


    //Croot1\tCroot2\tCroot3\tCroot4\tCroot5\tCroot6\tCroot7\t";
    //		fluxout << "Croot8\tCroot9\tCroot10\tRhizCl\tRhizNl\t";
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("humusC"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("RhizCl"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("RhizNl"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("AAexudateC"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("SugarExudateC"+std::to_string(i)));

    }
    //fluxout << "RhizCl\tRhizNl\t";
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("MicrobC"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("MicrobN"+std::to_string(i)));

    }
    /*
     for (i = 0; i < treesParams.rmodules; i++)
     {
     fluxout << "Nleach" << i << "\t";
     }
     */
    fluxout_names.push_back("RhizosphereNitrateNitrogen");
    fluxout_names.push_back("RhizosphereAmmoniumNitrogen");
    fluxout_names.push_back("PlantN");
    fluxout_names.push_back("plantNstat");
    fluxout_names.push_back("RLA");
    fluxout_names.push_back("CanopyCover");


    for (i = 0; i < treesParams.rmodules; i++)
    {
      fluxout_names.push_back(("ar"+std::to_string(i)));

    }

    for(i=0;i<fluxout_names.length();i++)
    {
      fluxout.push_back(blank_fill);
    }

    fluxout.names()=fluxout_names;
}
  else{
    fluxout.push_back(NA_REAL);
    fluxout.names()="none";
  }


  if(to_out[1]){
    // hydrout_names.push_back("ti");
    hydrout_names.push_back("year" );
    hydrout_names.push_back("jday" );
    hydrout_names.push_back("hour" );
    hydrout_names.push_back("min" );

    hydrout_names.push_back("latStemK");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("latRootK"+std::to_string(i)));

    }
    hydrout_names.push_back("StemAxialYm");
    hydrout_names.push_back("StemLatYm");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("RootAxialYm"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("RootLatYm"+std::to_string(i)));

    }
    hydrout_names.push_back("StemAxialKm");
    hydrout_names.push_back("StemLatKm");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("RootAxialKm"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("RootLatKm"+std::to_string(i)));

    }
    hydrout_names.push_back("StemAxial_b");
    hydrout_names.push_back("StemLat_b");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("RootAxial_b"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("RootLat_b"+std::to_string(i)));

    }

    hydrout_names.push_back("StemAxial_c");
    hydrout_names.push_back("StemLat_c");

    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("RootAxial_c"+std::to_string(i)));

    }
    for (i = 0; i < treesParams.rmodules; i++)
    {
      hydrout_names.push_back(("RootLat_c"+std::to_string(i)));

    }

    for(i=0;i<hydrout_names.length();i++)
    {
      hydrout.push_back(blank_fill);
    }

    hydrout.names()=hydrout_names;
  }
  else{
    hydrout.push_back(NA_REAL);
    hydrout.names()="none";
  }

  if(to_out[2])//if using leaf module this shall be returned
  {
    // lfareaout_names.push_back("ti");
    lfareaout_names[0]="year";//.push_back("year" );
    lfareaout_names[1]="jday";//.push_back("jday" );
    lfareaout_names[2]="hour";//.push_back("hour" );
    lfareaout_names[3]="min";//.push_back("min" );
    for (i = 1; i <= 500; i++)
    {
      lfareaout_names[i+3]="Area_Leaf_"+std::to_string(i);//.push_back(("Area_Leaf_"+std::to_string(i)));
    }
    for(i=0;i<lfareaout_names.length();i++)
    {
      lfareaout[i]=blank_fill;//.push_back(blank_fill);
    }
    lfareaout.names()=lfareaout_names;
  }
  else{
    if(verbosity_c) Rcpp::Rcout<<"Leaf module was not utilized."<<"\n";
    lfareaout.push_back(NA_REAL);
    lfareaout.names()="none";
  }


    /*test silent speed*/ if(verbosity_c) Rcpp::Rcout<<"Starting simulator"<<"\n";

    // ///
    // // Convert the struct instance to a char pointer
    // char *ptr = reinterpret_cast<char*>(&treesParams);
    //
    // // Calculate the size of the struct
    // size_t structSize = sizeof(treesParams);
    // Rcpp::List result;
    //
    //
    // // Loop through the memory, treating it as bytes, and print them
    // for (size_t i = 0; i < structSize; ++i) {
    //   result.push_back(static_cast<int>(ptr[i]));
    // }
    // return(result);
    // ///
    /////////////// moved from simulator
    treesParams.altitude = current_params["altitude"];  //altitude (meters)
    treesParams.lat = current_params["latitude"];  //latitude (degrees)
    treesParams.longi = current_params["longitude"]; //longitude (degrees)
    treesParams.z_ref = current_params["z_ref"];  //ref height, m
    treesParams.lai = current_params["lai"];  //single sided lai

    treesParams.Al = treesParams.lai;
    treesParams.previous_lai = treesParams.lai;
    treesParams.lai_at_sat_kl = treesParams.Al;
    //treesParams.Al = max(1.0, treesParams.lai);

    treesParams.canopy_ht = current_params["canopy_height"]; //height of canopy, m
    treesParams.laiFullCanopyHeight = current_params["lai_at_full_canopy_height"]; //
    treesParams.live_lai = treesParams.laiFullCanopyHeight;

    treesParams.l_angle = current_params["l_angle"]; //leaf angle distribution
    treesParams.canopy_e = current_params["canopy"];  //canopy emissivity
    treesParams.fPAR_beam = current_params["fPAR_beam"];  //fraction of PAR in beam radiation
    treesParams.fPAR_diff = current_params["fPAR_diff"];  //fraction of PAR in diffuse radiation
    treesParams.alpha_PAR = current_params["alpha_PAR"];  //alpha for PAR & NIR typically 0.8 & 0.2
    treesParams.alpha_NIR = current_params["alpha_NIR"];
    treesParams.omega = current_params["omega"];  //parameter used to adjust ext. coefficient (0 to 1)
    treesParams.p_crown = current_params["p_crown"];  //parameter used in clumping factor calculation (1 to 3.34)
    //Aerodynamic parameters
    treesParams.d_factor = current_params["d_factor"];  //d = d_factor*canopy_ht
    treesParams.zm_factor = current_params["zm_factor"];  //zm = zm_factor*canopy_ht
    treesParams.zh_factor = current_params["zh_factor"];  //zh = zh_factor*zm
    treesParams.ps_model = static_cast<int>(current_params["ps_model"]);  //photosynthesis model to select (1,2, or 3)
    //Photosynthesis parameters
    treesParams.Rd_mult = current_params["Rd_mult"];  //Rd = Rd_mult * Vmax
    treesParams.Jmax_mult = current_params["Jmax_mult"];  //Jmax = Jmax_mult * Vmax
    treesParams.thetaJ = current_params["thetaJ"];  //J curvature parameter
    treesParams.phiJ_sun = current_params["phiJ_sun"];  //effective quantum yield of sunlit PSII system, e-/umol
    treesParams.phiJ_shd = current_params["phiJ_shd"];  //effective quantum yield of shaded PSII system, e-/umol
    treesParams.Nleaf = current_params["Nleaf"];  //leaf N concentration (kg/m2)
    treesParams.N_fixed_proportion = current_params["N_fixed_proportion"];  //leaf N concentration (kg/m2)
    treesParams.Nrubisco = current_params["Nrubisco"];  //leaf proportion of N in rubisco
    treesParams.Kc25 = current_params["Kc25"];  //(Pa) MM const carboxylase, 25 deg C
    treesParams.q10Kc = current_params["q10Kc"];  //(DIM) Q_10 for kc
    treesParams.Ko25 = current_params["Ko25"];  //(Pa) MM const oxygenase, 25 deg C
    treesParams.q10Ko = current_params["q10Ko"];  //(DIM) Q_10 for ko
    treesParams.act25 = current_params["act25"];  //(umol/mgRubisco/min) Rubisco activity
    treesParams.q10act = current_params["q10act"];  //(DIM) Q_10 for Rubisco activity
    //Added for C4 - DSM August 2019
    treesParams.Vcmax25 = current_params["Vcmax25"];  //maximum Rubisco activity at 25 C, umol m-2 s-1
    treesParams.Vpmax25 = current_params["Vpmax25"];  //maximum PEP carbolylase activity at 25 C, umol m-2 s-1
    treesParams.Jmax25 = current_params["Jmax25"];  //maximum electron transport rate at 25 C, umol m-2 s-1
    treesParams.gammaStar25 = current_params["gammaStar25"];  //compensation point, ubar
    treesParams.Kp25 = current_params["Kp25"];  //Michaelis constant of PEP carboxylase for CO2 at 25 C, ubar
    treesParams.Vpr = current_params["Vpr"];  //PEP regeneration rate, umol m-2 s-1
    treesParams.f = current_params["f"];  //correction for spectral quality of light
    treesParams.x = current_params["x"];  //partitioning factor of electron transport rate
    treesParams.absorptance = current_params["absorptance"];  //fraction of irradiance absorbed
    treesParams.E_Vcmax = current_params["E_Vcmax"];  //activation energy, maximum carboxylation rate, kJ mol-1
    treesParams.E_Vpmax = current_params["E_Vpmax"];  //activation energy, maximum PEP rate, kJ mol-1
    treesParams.E_Jmax = current_params["E_Jmax"];  //activation energy, electron transport, kJ mol-1
    treesParams.E_Kp = current_params["E_Kp"];  //activation energy, Michaelis reaction of PEP, kJ mol-1
    treesParams.E_Kc = current_params["E_kc"];  //activation energy, Michaelis reaction of carboxylation, kJ mol-1
    treesParams.E_Ko = current_params["E_ko"];  //activation energy, Michaelis reaction of oxygenation, kJ mol-1
    treesParams.E_Rd = current_params["E_Rd"];  //activation energy, Michaelis reaction of mitochondrial respiration, kJ mol-1
    treesParams.E_gammaStar = current_params["E_gammaStar"];  //activation energy, Michaelis reaction of compensation point, kJ mol-1
    treesParams.gm = current_params["gm"];  //mesophyll conductance to CO2, mol m-2 s-1
    treesParams.gbs = current_params["gbs"];  //conductance of the bundle sheath, mol m-2 s-1
    treesParams.alphaGmax = current_params["alphaGmax"];  //fraction of glycolate carbon diverted to glycine during photorespiration
    treesParams.alphaSmax = current_params["alphaSmax"];  //fraction of glycolate carbon diverted to serine during photorespiration
    treesParams.Nmax = current_params["Nmax"];  //maximum rate of de novo nitrogen supply to the chloroplast, umol N m-2 s-1

    //Plant hydraulic Parameters
    treesParams.Gsref0 = current_params["Gsref0"];  //maximum gs, molm-2s-1 (leaf hemisurface area)

    treesParams.m = current_params["m"]; //kPa-1 as D in kPa
    treesParams.isAmphistomatous = static_cast<bool>(current_params["isAmphistomatous"]); //stomata on two sides
    treesParams.Md = current_params["Md"];
    treesParams.midday_at_sat_kl = current_params["midday_at_sat_kl"];
    treesParams.e_at_saturated_kl= current_params["e_at_saturated_kl"];
    treesParams.rhizosphere_width= current_params["rhizosphere_width"];//mm Sperry et al., 1998 PC&E
    //treesParams.E_inc= current_params[""];
    treesParams.E_inc=0.0001;
    treesParams.soilshells= static_cast<int>(current_params["soilshells"]);
    // treesParams.GMP= current_params["GMP"];
    // treesParams.GSD= current_params["GSD"];
    // treesParams.BD= current_params["BD"];
    // treesParams.porosity = current_params["porosity"];  //
    // treesParams.silt_fraction= current_params["silt_fraction"];
    // treesParams.clay_fraction= current_params["clay_fraction"];
    // treesParams.residual= current_params["residual"];
    treesParams.frac_absorbing_length= current_params["frac_absorbing_length"];
    treesParams.Capacitance= current_params["Capacitance"];
    treesParams.axK_latKl_shoot_modules= current_params["axK_latKl_shoot_modules"];
    treesParams.axKr_latKr_root_modules= current_params["axKr_latKr_root_modules"];
    treesParams.per_total_R_in_root_system= current_params["per_total_R_in_root_system"];
    treesParams.pd_at_sat_kl= current_params["pd_at_sat_kl"];
    //calculate kl from hydraulic parameters, kl=e/(pd-md)
    treesParams.saturated_kl_for_whole_plant = treesParams.e_at_saturated_kl/(treesParams.pd_at_sat_kl-treesParams.midday_at_sat_kl);
    treesParams.ax_Shoot_b_value= current_params["ax_Shoot_b_value"];
    treesParams.ax_Shoot_c_value= current_params["ax_Shoot_c_value"];
    treesParams.lat_Shoot_b_value= current_params["lat_Shoot_b_value"];
    treesParams.lat_Shoot_c_value= current_params["lat_Shoot_c_value"];
    treesParams.ax_Root_b_value= current_params["ax_Root_b_value"];
    treesParams.ax_Root_c_value= current_params["ax_Root_c_value"];
    treesParams.lat_Root_b_value= current_params["lat_Root_b_value"];
    treesParams.lat_Root_c_value= current_params["lat_Root_c_value"];
    // treesParams.initial_conductivity_root= current_params["initial_conductivity_root"];
    // treesParams.decrement_root= current_params["decrement_root"];
    // treesParams.initial_conductivity_shoot= current_params["initial_conductivity_shoot"];
    // treesParams.decrement_shoot= current_params["decrement_shoot"];
    //Biogeochemical cycling parameters
    treesParams.theta_opt = current_params["theta_opt"];  //
    treesParams.optimal_soil_T = current_params["optimal_soil_T"];  //
    treesParams.growth_resp_proportion = current_params["growth_resp_proportion"];  //
    treesParams.resp_coef_root = current_params["resp_coef_root"];  //
    treesParams.resp_coef_stem = current_params["resp_coef_stem"];  //
    treesParams.resp_coef_leaf = current_params["resp_coef_leaf"];  //
    treesParams.resp_coefficient = current_params["resp_coefficient"];  //
    treesParams.EaSx = current_params["EaSx"];  //
    treesParams.kMsx = current_params["kMsx"];  //
    treesParams.xASx = current_params["xASx"];  //

    treesParams.kd = current_params["kd"];  //
    treesParams.kn = current_params["kn"];  //
    treesParams.kea = current_params["kea"];  //
    treesParams.kes = current_params["kes"];  //
    treesParams.kl = current_params["kl"];  //
    treesParams.kh = current_params["kh"];  //

    treesParams.fr_minCN = current_params["fr_minCN"];  //
    treesParams.fr_maxCN = current_params["fr_maxCN"];  //
    treesParams.leaf_minCN = current_params["leaf_minCN"];  //
    treesParams.leaf_maxCN = current_params["leaf_maxCN"];  //

    treesParams.Cbelowground = current_params["Cbelowground"];  //
    treesParams.Clitter_frac = current_params["Clitter_frac"];  //
    treesParams.Croot_frac = current_params["Croot_frac"];  //
    treesParams.Clitter = treesParams.Clitter_frac * treesParams.Cbelowground;
    treesParams.Croot = treesParams.Croot_frac * treesParams.Cbelowground;
    treesParams.Cstem = current_params["Cstem"];  //
    treesParams.Csapwood = current_params["Csapwood"];  //
    treesParams.Croot_coarse_frac = current_params["Croot_coarse_frac"];//
    treesParams.Croot_coarse = treesParams.Croot_coarse_frac * treesParams.Cbelowground;
    treesParams.Csoil = (1.0-treesParams.Clitter_frac - treesParams.Croot_frac - treesParams.Croot_coarse_frac) * treesParams.Cbelowground;
    treesParams.interception_per_leafArea = current_params["interception_per_leafArea"];//
    treesParams.litter_capacity = current_params["litter_capacity"];//
    treesParams.litter_capacity_init = treesParams.litter_capacity;
    // treesParams.theta_deep0 = current_params["theta_deep0"];//
    // treesParams.theta_mid0 = current_params["theta_mid0"];//
    // treesParams.theta_shallow0 = current_params["theta_shallow0"];//
    treesParams.litter_store0 = current_params["litter_store"];//
    treesParams.SLA = current_params["SLA"];  //
    treesParams.SLA_instant = treesParams.SLA;
    treesParams.SRL1 = current_params["SRL1"];  //
    treesParams.minRootDiam = current_params["minRootDiam"];  //
    treesParams.maxRootDiam = current_params["maxRootDiam"];  //
    treesParams.rootDiamMultiplier = pow(treesParams.maxRootDiam/treesParams.minRootDiam,1.0/9.0);
    treesParams.minRootLifespan = current_params["minRootLifespan"];  //
    treesParams.LWP_spring_minimum = current_params["LWP_spring_minimum"];  //
    treesParams.LWP_stomatal_closure = current_params["LWP_stomatal_closure"];  //
    treesParams.is_bryophyte = static_cast<int>(current_params["is_bryophyte"]);  //
    treesParams.capRiseScalar = current_params["capRiseScalar"];  //

   ///
   treesParams.precipReduction= current_params["precipReduction"];
    //How to to multiple precipitation by for experimental drought designs
    if (treesParams.precipReduction < 0.0)
    {
      treesParams.precipReduction = 0.0;
    }
    //Threshold for Ec convergence
    treesParams.drainScalar= current_params["drainScalar"];
    treesParams.leafNSCscalar = current_params["leafNSCscalar"];
    //What are these for?

    treesParams.usePhenology= static_cast<bool>(current_params["usePhenology"]);
    treesParams.leafLifeSpan= current_params["leafLife"];
    //Maximum number of iterations to achieve convergence of DELTA
    treesParams.max_iterations = static_cast<int>(current_params["max_iteration"]);
    //Conductance to use for Darcy's Law, 1=WholePlant,2=AxialComponents, 3=Shoot,4=AxialRoot, 5=LaterialRoot
    treesParams.microbiomeScalar = current_params["microbiomeScalar"];
    // MCH 07082020
    treesParams.microbialrainrate = current_params["microbialrainrate"]; //
    //MCH 23092020
    treesParams.raininAmmonium = current_params["raininAmmonium"]; //
    treesParams.raininNitrate = current_params["raininNitrate"]; //
    treesParams.raininMineralN = current_params["raininMineralN"]; //
    treesParams.raininLabileC = current_params["raininLabileC"]; //
    //Parameters for snowpack accumulation and melt
    treesParams.snowpack_water_equivalent = current_params["snowpack_water_equivalent"]; //
    treesParams.snowpack_E_deficit_max = current_params["snowpack_E_deficit_max"]; //
    treesParams.melt_Rcoef = current_params["melt_Rcoef"]; //
    //Run with full hydraulics or not (1 = yes, 0 = no)
    treesParams.useHydraulics = static_cast<bool>(current_params["useHydraulics"]); //
    treesParams.useInputStress = static_cast<bool>(current_params["useInputStress"]); //
    treesParams.useInputWaterTable = static_cast<bool>(current_params["useInputWaterTable"]); //
    treesParams.dayToStopMaizeRefilling = current_params["dayToStopMaizeRefilling"]; //
    treesParams.allowLeafRefilling = true;
    //Leaf growth module parameters
    treesParams.useLeafModule = static_cast<bool>(current_params["useLeafModule"]); //
    treesParams.leafAreaMax = current_params["leafAreaMax"]; //
    treesParams.initialLeafSize = current_params["initialLeafSize"]; //
    treesParams.leafArea_Rate = current_params["leafArea_Rate"]; //
    treesParams.dur_LeafExpansion= current_params["dur_LeafExpansion"]; //
    treesParams.SLA_max = current_params["SLA_max"]; //
    treesParams.SLA_min = current_params["SLA_min"]; //
    treesParams.leaf_insertAngle = current_params["leaf_insertAngle"]; //
    treesParams.leaf_len_to_width = current_params["leaf_len_to_width"]; //
    treesParams.proportion_CD = current_params["proportion_CD"]; //
    treesParams.phyllochron = current_params["phyllochron"]; //
    treesParams.floweringTime = current_params["floweringTime"]; //
    treesParams.Tbase = current_params["Tbase"]; //
    treesParams.therm_plant = current_params["therm_plant"]; //
    treesParams.projectedArea_init = current_params["projectedArea_init"]; //
    treesParams.pot_size = current_params["pot_size"]; //
    treesParams.root_to_shoot = current_params["root_to_shoot"]; //
    treesParams.leaf_to_stem = current_params["leaf_to_stem"]; //
    treesParams.useLeafGamma = static_cast<bool>(current_params["useLeafGamma"]); //
    treesParams.Kalpha = current_params["Kalpha"]; //
    treesParams.Kbeta = current_params["Kbeta"]; //
    treesParams.Nalpha = current_params["Nalpha"]; //
    treesParams.Nbeta = current_params["Nbeta"]; //
    treesParams.ralpha = current_params["ralpha"]; //
    treesParams.rbeta = current_params["rbeta"]; //

    //DK - August 2023
    //PhiPS2 (effective quantum yield of PSII) parameters
    treesParams.usePhiPS2 = static_cast<bool>(current_params["usePhiPS2"]); //(bool) current_params[i].get_value(); i++; //
    treesParams.Q_ref = current_params["Q_ref"]; //current_params[i].get_value(); i++; //
    treesParams.beta0_refQ = current_params["beta0_refQ"]; //current_params[i].get_value(); i++; //
    treesParams.beta1_refQ = current_params["beta1_refQ"]; //current_params[i].get_value(); i++; //
    treesParams.beta2_refQ = current_params["beta2_refQ"]; //current_params[i].get_value(); i++; //
    treesParams.beta3_refQ = current_params["beta3_refQ"]; //current_params[i].get_value(); i++; //
    treesParams.alpha_PS2 = current_params["alpha_PS2"]; //current_params[i].get_value(); i++; //
    treesParams.kappa_PS2 = current_params["kappa_PS2"]; //current_params[i].get_value(); i++; //


    treesParams.delta = treesParams.m*treesParams.Gsref0; // sensitivity of stomata to VPD, mol m-2 s-1
    //////////////////////////////
    treesParams.VERBOSE=verbosity_c;
    treesParams.softerror=0;
    treesParams.maxsofterror=softerror_max_c;

    // try {
    //   // Code that might throw an exception
    //   // throw SomeExceptionType("Error message");
    // I need to build in some sort of throw manually
    // }
    // catch( ExceptionName e1 )  {
    //   // catch block catches the exception that is thrown from try block
    // }


  simulator(
    indata,
    state,
    // current_params,
    treesParams,
    fluxout,
    hydrout,
    lfareaout,
    to_out
  );



      /*test silent speed*/ if(verbosity_c) Rcpp::Rcout << "Simulation completed" <<" \n";


  return(
    Rcpp::List::create(
      Rcpp::Named("sim_out")=fluxout,Rcpp::Named("hyd_out")=hydrout,Rcpp::Named("lfa_out")=lfareaout
  )
  );//,
      // Rcpp::Named("sim_out_names")=fluxout_names,Rcpp::Named("hyd_out_names")=hydrout_names,Rcpp::Named("lfa_out_names")=lfareaout_names)
  // );
}

