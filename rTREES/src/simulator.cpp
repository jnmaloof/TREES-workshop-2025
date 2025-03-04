//
//simulator.cpp
//
//simulator(): Implements the front end to the TREES simulations, including the time loop
//
//There are three implentations of simulator(). The first implementation is called by the Bayesian
//    code in trees_main.cpp. The second implementation is called by trees_main.cpp when just a single
//    deterministic simulation is desired. The third implementation is called by getPIL.cpp, a
//    post-processing program used with the Bayesian analysis, and not part of the TREES executable
//
//Author: D. Scott Mackay, sometime in 2007
//
//
//This function handles:
//    - initialization of state variables, arrays, etc
//    - time loop
//    - post-simulation bookkeeping
//This function has a polymorphic behavior with:
//    - first simulator() called from Bayesian MCMC loop in trees_main
//    - second simulator() called once from trees_main
//    - third simulator() called from get_PIL for post-MCMC analysis
//
//

// #include <iostream>
// #include <stdlib.h>
// #include <stdio.h>
// #include <cmath>
// #include "constants.h"
// #include "simulator2.h"
// #include <unistd.h>
#include "trees_main.h"



//---------------------------------------------------------------------------------------------------
//This is called for single simulations with fixed parameters
// Note: The full water balance model can be used here if data availability or problem justifies it
//---------------------------------------------------------------------------------------------------
void simulator(
    Data_Store& in_data,
    State_Store& state,
    // Rcpp::NumericVector& current_params,
    trees_params& treesParams,
    Rcpp::List& fluxout,
    Rcpp::List& hydrout,
    Rcpp::List& lfareaout,
    std::vector<bool>& to_out
)
{
  int lastsofterror=0;//make zero after testing
  int errorfree_steps=0;
  int n_tsteps = in_data.no_of_steps();  //# of time steps in input data
  int i, j, k;
  int l;
  Time ti;
  double u_ref, t_ref, D_ref, precip, Qpar, t_canopy, D_canopy, CO2_atm, Ecobs;
  double p_atm, t_surface, t_soil, t_root, Zw, NEEobs;
  double xylem_capacity_multiplier, Al_init;
  std::vector<double> thetaSoil(ULAT), tempSoil(ULAT);
  int year, yday, hour, min;
  struct sim_out simOut;
  std::vector<double> GSI_vector(22);
  //These variables interface with the HydraulicModel model
  std::vector<double> EcState(n_tsteps+1,0.0);///THIS WAS THE CULPRIT FOR rTREES CRASH AT LAST TS. Fixed with +1..
  //Should investigate what was going on in OG code.
  std::vector<double> EcStateInner(50, 0.0);
  std::vector<double> LeafPsiInner(50, 0.0);
  std::vector<double> KpInner(50, 0.0);
  std::vector<int> InnerStep(50, 0.0);
  int ModelStatus = 0;
  double Ecrit = 0.0;
  double Thresh = 0.0;
  int ts;

  //Most of these aren't being used... except psimin_in
  // double p_in[MD][MD], ks_in[MD][MD], newb_in[MD][MD], newc_in[MD][MD],
  double psimin_in[MD][MD];

  for (i = 1; i <= 21; ++i)
  {
    GSI_vector[i] = 0.0;
  }

  //declare and assign parameters
  i = 0;
  //this is moved to trees_main.cpp
  double m =  treesParams.m;
  Al_init = treesParams.Al;

  //Calculate Brooks-Corey ks (cm/hr), bubbling pressure (cm), pore-size distribution index, and residual water content
  // double ks, bubbling_pressure, pore_size_index, residual, fieldCapacity;
  // double pClay; // = 100.0*treesParams.clay_fraction;
  // double pSand; // = 100.0*(1.0 - treesParams.silt_fraction - treesParams.clay_fraction);
  //soil_texture_parameters(treesParams.porosity, pClay, pSand, ks, bubbling_pressure, pore_size_index, residual);
  //std::cout << "Calculated residual = " << residual << endl;
  //treesParams.ks = ks;
  //treesParams.bubbling_pressure = bubbling_pressure;
  //treesParams.pore_size_index = pore_size_index;
  //treesParams.residual = residual;
  //residual = treesParams.residual;

  if (treesParams.useHydraulics == true)
  {
    /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << ">>> TREES, v. 3.2.3, deterministic mode with plant water balance <<< \n\n";

  }
  else
  {
    /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << ">>> TREES, v. 3.2.3, deterministic mode without plant water balance <<< \n\n";

  }
  /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout <<" \n";

  //Calculate field capacity
  //DSM June 2023 - now done in simulation_functions to handle multiple soil layer texture
  //fieldCapacity = field_capacity(bubbling_pressure, pore_size_index, residual, treesParams.porosity);
  //treesParams.field_capacity = fieldCapacity;
  if (treesParams.is_bryophyte == 1)
  {
    /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << "Running with Bryophytes \n";

  }

  //Initialize state variables here
  Var_Dictionary vd;
  std::string deep_name = "deep";
  vd.insert(deep_name);
  std::string mid_name = "mid";
  vd.insert(mid_name);
  std::string shallow_name = "shallow";
  vd.insert(shallow_name);
  std::string litter_name = "litter";
  vd.insert(litter_name);
  std::string canopy_name = "canopy";
  vd.insert(canopy_name);
  std::string lwp_name = "lwp";
  std::string kl_name = "kl";
  std::string ecrit_name = "ecrit";
  std::string psicrit_name = "psicrit";
  std::string snowpack_name = "snowpack";
  std::string snowEdef_name = "snowEdef";
  std::string xylemCap_name = "xylem";

  //Initialize state variables for carbon pools
  std::string photosynthate_name = "photosynthate";
  vd.insert(photosynthate_name);
  std::string nsc_name = "nsc";
  vd.insert(nsc_name);
  std::string root_name = "root";
  vd.insert(root_name);
  vd.insert(lwp_name);
  vd.insert(kl_name);
  vd.insert(ecrit_name);
  vd.insert(psicrit_name);
  vd.insert(snowpack_name);
  vd.insert(snowEdef_name);

  vd.insert(xylemCap_name);

  std::string regen_name = "regen";
  vd.insert(regen_name);

  std::string currentTargetLai_name = "currenttargetlai";
  vd.insert(currentTargetLai_name);
  std::string forecastTargetLai_name = "forecasttargetlai";
  vd.insert(forecastTargetLai_name);




  state.allocate(vd);   //allocate memory



  // 	state.set_val_at(treesParams.theta_deep0, DEEPSW);
  // 	state.set_val_at(treesParams.theta_mid0, MIDSW);
  //         state.set_val_at(treesParams.theta_shallow0, SHALLOWSW);
  state.set_val_at(treesParams.litter_store0, LITTER);
  state.set_val_at(0.0, CANOPY);
  state.set_val_at(0.0, PSN);
  state.set_val_at(treesParams.lai/treesParams.SLA*10000.0*((0.14/treesParams.leafLifeSpan)+0.10)+treesParams.Croot*0.10 + treesParams.Croot_coarse*0.10 + 0.04*treesParams.Csapwood, NSC);
  state.set_val_at(treesParams.Cbelowground*treesParams.Croot_frac, ROOTC);
  state.set_val_at(treesParams.snowpack_water_equivalent, SNOWPACK);
  state.set_val_at(treesParams.snowpack_E_deficit_max, SNOWEDEF);
  state.set_val_at(treesParams.saturated_kl_for_whole_plant, KL);

  state.set_val_at(0.0, REGEN);

  //FLAG laiFullCanopyHeight only seems relevant to corn and the couple seasonal lines in simulation_functions
  //CURRENTTARGETLAI is used to determine lai_pot in simulation and process _functions
  //and doesnt seem relevant to leaf modules
  //will try just switching this to new code for FP
  if(treesParams.useLeafModule==1){
    state.set_val_at(treesParams.lai, CURRENTTARGETLAI);
  }
  else{
    state.set_val_at(treesParams.laiFullCanopyHeight, CURRENTTARGETLAI);
  }

  state.set_val_at(0.0, FORECASTTARGETLAI);


  //Initialize soil theta in each layer (up to ULAT layers)
  //DSM June 2023 - for multiple soil layer texture
  for (j = 0; j < treesParams.rmodules; j++)
  {
    thetaSoil[j] = treesParams.layer_initial_water_content[j+3];
  }


  //define a BiogeochemicalCycles object
  BiogeochemicalCycles bgc(treesParams);
  //Define the canopy closure based on crown diameter
  treesParams.canopy_cover = bgc.computeCanopyCover(treesParams);

  double rootArea = bgc.computeRootArea(treesParams);
  bgc.computeLateralRootWeibulls(treesParams);

  /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << "rootDiamMultiplier = " << treesParams.rootDiamMultiplier <<" \n";
  /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << "rootArea = " << rootArea <<" \n";

  treesParams.Ar_Al = rootArea / treesParams.lai / treesParams.canopy_cover;
  treesParams.aral_at_sat_kl = treesParams.Ar_Al;
  treesParams.Ar_Al_init = treesParams.Ar_Al;

  //define a HydraulicModel object
  HydraulicModel *hydraulicModel = new HydraulicModel();


  //define parameters for setup()
  bool reset;
  HydraulicModel::outputStruct ecrit_k_psi, k_p_e;
  std::vector<double> ecritOut(MD), pc(MD), klpred(MD), ppredOut(MD), epredOut(MD), nodeFail(MD);
  std::vector<char> nodeTyp(MD);
  std::vector<std::vector<std::vector<double> > > psi(NREC, std::vector<std::vector<double> >(MD, std::vector<double>(ULAT)));
  std::vector<std::vector<double> > rflux(NREC, std::vector<double>(NMAX));
  std::vector<double> evap(NREC);

  double soilpsiavg[NREC], soilpsiavg_in[NREC], nts[NREC], kroot[MD], axr[MD], latr[MD];
  double kshoot[MD], dslat[MD], dsax[MD], drlat[MD],drax[MD], ll[MD];
  double ksat[MD][MD], bsat[MD][MD], ccsat[MD][MD], newb[MD][MD], newc[MD][MD];
  double ip[MD][ULAT], b[MD][ULAT], b1[MD][ULAT], n[MD][ULAT], n1[MD][ULAT];
  double r[MD][ULAT], vu[MD][ULAT], vl[MD][ULAT], cf[MD][ULAT];
  std::vector<std::vector<double>> pe(MD,std::vector<double>(ULAT)); //rTREES watch
  double ws_[MD][ULAT];
  double ks2[MD][ULAT], p[MD][ULAT], dpp[MD][ULAT], jl[MD][ULAT], wnu[MD][ULAT], wu[MD][ULAT];
  double wnl[MD][ULAT], wl[MD][ULAT], cpu[MD][ULAT], cpl[MD][ULAT], cpp[MD][ULAT], ku[MD][ULAT];
  double kl[MD][ULAT], f[MD][ULAT], cc[MD][2], psinode[NMAX][NMAX], psimin[NMAX][NMAX];
  double jmatrix[NMAX][NMAX], jmatrix2[NMAX][NMAX];
  double percent[NMAX], rssoil[NMAX], rs[NMAX], dp[NMAX], ff[NMAX];
  int col[NMAX], row[NMAX],  indxx[NMAX];
  double subtract[NMAX], pressure[NKINC], plc[NKINC], plcweib[NKINC];
  double rsquare[NKINC], soilpsi[ULAT], al[MD], ar[MD], saturatedKs[MD][4];
  double ptarg, e=0.0, rr, rzw, rd, einc;
  int SoilRhizElements;
  double dt, gmd, gsd, bkd, fs, fc, Fabs, cpplant, saxlat, raxlat, rfract;
  double kla, aral, latot, ratot;
  int shootElements = treesParams.smodules, ktotal;
  double soilpsimin=0.0;
  double soilpsimin_in;
  int ShootModules, rootElements = treesParams.rmodules, RootModules, totmodules;
  double axShoot_b, axShoot_c, latShoot_b, latShoot_c, axRoot_b, axRoot_c, latRoot_b, latRoot_c;
  double plco, pdecrement, sumy1, sumy2, sumprod, cincrement, soilvol=0.0;
  double rlateral, rLat_base, slateral, sLat_base, raxial, pleafave;
  double mortality_threshold, fertilization0, fertilization, RLnew, time_to_drop;
  int tnode;
  double fac = 0.0, modelledKL;
  int HydraulicModelFailCond=0;
  double totalRootArea;

  //call setup() to initialize HydraulicModel objects
  if (treesParams.useHydraulics == true)
  {
    ratot = rootArea;
    latot =  bgc.getLeafBiomassCarbon()*treesParams.SLA/10000.0;
    // latot=treesParams.lai*treesParams.canopy_cover;
    /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout <<"latot = ";
    /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout <<latot;
    /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout <<"\n";


    reset = true;
    //FIRST call to setup()
    /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << ">>> Invoking hydraulic setup() <<< \n\n";

    hydraulicModel->setup(
        reset,
        ecrit_k_psi,
        k_p_e,
        ecritOut,
        pc,
        klpred,
        ppredOut,
        epredOut,
        nodeFail,
        nodeTyp,
        psi,
        rflux,
        soilpsiavg,
        nts,
        evap,
        kroot,
        axr,
        latr,
        kshoot,
        dslat,
        dsax,
        drlat,
        drax,
        ll,
        ksat,
        bsat,
        ccsat,
        newb,
        newc,
        ip,
        b,
        b1,
        n,
        n1,
        r,
        vu,
        vl,
        cf,
        pe,
        ws_,
        ks2,
        p,
        dpp,
        jl,
        wnu,
        wu,
        wnl,
        wl,
        cpu,
        cpl,
        cpp,
        ku,
        kl,
        f,
        cc,
        psinode,
        psimin,
        jmatrix,
        jmatrix2,
        percent,
        rssoil,
        rs,
        dp,
        ff,
        col,
        row,
        indxx,
        subtract,
        pressure,
        plc,
        plcweib,
        rsquare,
        soilpsi,
        al,
        ar,
        saturatedKs,
        ptarg,
        e,
        rr,
        rzw,
        rd,
        einc,
        SoilRhizElements,
        dt,
        gmd,
        gsd,
        bkd,
        fs,
        fc,
        Fabs,
        cpplant,
        saxlat,
        raxlat,
        rfract,
        kla,
        aral,
        latot,
        ratot,
        shootElements,
        ktotal,
        soilpsimin,
        ShootModules,
        rootElements,
        RootModules,
        totmodules,
        axShoot_b,
        axShoot_c,
        latShoot_b,
        latShoot_c,
        axRoot_b,
        axRoot_c,
        latRoot_b,
        latRoot_c,
        plco,
        pdecrement,
        sumy1,
        sumy2,
        sumprod,
        cincrement,
        soilvol,
        rlateral,
        rLat_base,
        slateral,
        sLat_base,
        raxial,
        pleafave,
        tnode,
        fac,
        modelledKL,
        HydraulicModelFailCond,
        treesParams
    );



    ratot = bgc.computeRootArea(treesParams);
    bgc.updateLateralRootWeibulls(bsat, ccsat, treesParams);

    for (int mm=1; mm<=treesParams.smodules; mm++)
    {
      al[mm] = treesParams.al[mm] * latot;
    }
    for (int mm=1+treesParams.smodules+1; mm <=(treesParams.rmodules+treesParams.smodules+1); mm++)
    {
      ar[mm] = treesParams.ar[mm] * ratot;
      ll[mm] = (ar[mm] / (2.0 * M_PI * rr));
    }
  }

  state.set_val_at(treesParams.pd_at_sat_kl, LPSI);
  state.set_val_at(treesParams.saturated_kl_for_whole_plant, KL);
  state.set_val_at(treesParams.e_at_saturated_kl, ECRIT);

  int mm;
  //time loop
  simOut.Soil_Psi = 0.0;

  //FLAG
  treesParams.realized_lai =0.1;


  for (i = 0; i < n_tsteps; i++)
  {
    // /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << "Running: "<<i<<"/"<<n_tsteps<<"="<< (static_cast<double>(i))/(static_cast<double>(n_tsteps))*100.0 << "%\r" << std::flush;

    if(in_data.check_step(i) == 0) //valid step w/ valid data
    {

      //++++++++++++++++
      //Switch parameters from old stand (at death) to regenerating stand with dead needles reducing radiation
      // - modification for AGU 2013 DSM
      //
      // Step 1. Redefine parameters
      // Step 2. Re-call setup() for hydraulicModel
      // Step 3. Set a flag, treesParams.regen = true
      // Step 4. In simulation functions need a modified canopy radiation model

      treesParams.xylemScalar = 1.0;
      time_to_drop = 0.0;
      if (state.get_val_at(REGEN) == 0.0 && treesParams.xylemScalar < 0.06)
      {
        state.set_val_at(0.0, CANOPY);
        //record the status of the old canopy dead leaf area index
        treesParams.dead_lai = treesParams.Al;
        treesParams.dead_lai_drop_rate = treesParams.dead_lai / 365.25 / 48.0;
        time_to_drop = treesParams.dead_lai/treesParams.dead_lai_drop_rate;

        treesParams.Gsref0 *= 1.0;
        treesParams.delta = m*treesParams.Gsref0;
        treesParams.e_at_saturated_kl *= 1.0;

        //reset appropriate parameters
        treesParams.canopy_ht = 1.0;
        treesParams.Croot_frac *= 1/treesParams.lai_at_sat_kl;
        treesParams.Croot *= 1.0/treesParams.lai_at_sat_kl;
        treesParams.Cstem *= 0.05;
        treesParams.Csapwood = 550.0/treesParams.lai_at_sat_kl;
        treesParams.lai = treesParams.live_lai = treesParams.Al = treesParams.lai_at_sat_kl = 1.0;
        treesParams.Al = 1.01;

        state.set_val_at(treesParams.lai/treesParams.SLA*10000*0.10+treesParams.Cbelowground*treesParams.Croot_frac*0.10 + 0.04*treesParams.Csapwood, NSC);
        RLnew = treesParams.Ar_Al;
        treesParams.Ar_Al = treesParams.Ar_Al_init = treesParams.aral_at_sat_kl = RLnew;
        treesParams.xylemScalar = 1.0;

        for (mm=1; mm<=treesParams.smodules; mm++)
        {
          /*
           treesParams.al[mm] = 1.0;
           treesParams.dslat[mm] = 0.4;
           */
          treesParams.dsax[mm] = 1.0;
        }

        e = fac = soilvol = soilpsimin = 0.0;

        //Reset K's to saturation
        for (mm = 1; mm <= totmodules+1; mm++)
        {
          if ( mm != ShootModules+1)  // skip over "hidden" module
          {
            ks2[mm][1] = ksat[mm][1];
            ks2[mm][0] = ksat[mm][0];
          }
        }

        //re-call setup() to initialize HydraulicModel  objects for new canopy
        if (treesParams.useHydraulics == true)
        {
          reset = true;
          /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << ">>> Reinvoke hydraulic setup() <<< \n\n";
          //second
          hydraulicModel->setup(
              reset,
              ecrit_k_psi,
              k_p_e,
              ecritOut,
              pc,
              klpred,
              ppredOut,
              epredOut,
              nodeFail,
              nodeTyp,
              psi,
              rflux,
              soilpsiavg,
              nts,
              evap,
              kroot,
              axr,
              latr,
              kshoot,
              dslat,
              dsax,
              drlat,
              drax,
              ll,
              ksat,
              bsat,
              ccsat,
              newb,
              newc,
              ip,
              b,
              b1,
              n,
              n1,
              r,
              vu,
              vl,
              cf,
              pe,
              ws_,
              ks2,
              p,
              dpp,
              jl,
              wnu,
              wu,
              wnl,
              wl,
              cpu,
              cpl,
              cpp,
              ku,
              kl,
              f,
              cc,
              psinode,
              psimin,
              jmatrix,
              jmatrix2,
              percent,
              rssoil,
              rs,
              dp,
              ff,
              col,
              row,
              indxx,
              subtract,
              pressure,
              plc,
              plcweib,
              rsquare,
              soilpsi,
              al,
              ar,
              saturatedKs,
              ptarg,
              e,
              rr,
              rzw,
              rd,
              einc,
              SoilRhizElements,
              dt,
              gmd,
              gsd,
              bkd,
              fs,
              fc,
              Fabs,
              cpplant,
              saxlat,
              raxlat,
              rfract,
              kla,
              aral,
              latot,
              ratot,
              shootElements,
              ktotal,
              soilpsimin,
              ShootModules,
              rootElements,
              RootModules,
              totmodules,
              axShoot_b,
              axShoot_c,
              latShoot_b,
              latShoot_c,
              axRoot_b,
              axRoot_c,
              latRoot_b,
              latRoot_c,
              plco,
              pdecrement,
              sumy1,
              sumy2,
              sumprod,
              cincrement,
              soilvol,
              rlateral,
              rLat_base,
              slateral,
              sLat_base,
              raxial,
              pleafave,
              tnode,
              fac,
              modelledKL,
              HydraulicModelFailCond,
              treesParams
          );
        }

        treesParams.forceRefilling = true;
        state.set_val_at(1.0, REGEN);
      } //end REGEN reset
      else if (state.get_val_at(REGEN) == 1.0)
      {
        treesParams.forceRefilling = false;
        treesParams.xylemScalar = 1.0;
      }
      else
      {
        treesParams.forceRefilling = false;
      }

      if (state.get_val_at(REGEN) == 1.0)
      {
        if (time_to_drop <= 1.0)
        {
          treesParams.Nleaf *= (1.0+fertilization);
          fertilization = 0.0;
          treesParams.dead_lai -= treesParams.dead_lai_drop_rate;
          if (treesParams.litter_capacity_init < 0.02)
            treesParams.litter_capacity_init += 8.56E-7;
          if (treesParams.dead_lai < 0.0)
            treesParams.dead_lai = 0.0;
          if (treesParams.dead_lai < 0.01)
          {
            treesParams.Nleaf /= (1.0+fertilization0);
            fertilization0 = 0.0;
          }
        }
        if (time_to_drop > 0)
        {
          time_to_drop -= 0.000057078; //years 30min-1
        }
      }

      //Update root area to leaf area ratio from rootCarbon stores
      //DSM - July 2015

      rootArea = bgc.computeRootArea(treesParams);
      latot = bgc.getLeafBiomassCarbon()*treesParams.SLA/10000.0;
      //treesParams.Ar_Al = rootArea / treesParams.lai / treesParams.canopy_cover;
      treesParams.Ar_Al = rootArea / latot / treesParams.canopy_cover;
      treesParams.aral_at_sat_kl = treesParams.Ar_Al;

      ts = i+1; //HydraulicModel initialization check, ts=0 initializes Gs
      //get input data
      ti = in_data.get_time(i);
      j=0;


      u_ref = in_data.get_val_for("u_ref",i);//in_data.get_val_at(j,i); j++;
      t_ref = in_data.get_val_for("t_ref",i);//get_val_at(j,i); j++;
      D_ref = in_data.get_val_for("D_ref",i);//get_val_at(j,i); j++;
      precip = in_data.get_val_for("precip",i);//get_val_at(j,i); j++;
      precip *= 0.001; //convert mm to m
      precip *= treesParams.precipReduction;

      Qpar = in_data.get_val_for("Qpar",i);//get_val_at(j,i); j++;
      t_canopy = in_data.get_val_for("t_canopy",i);//get_val_at(j,i); j++;
      D_canopy = in_data.get_val_for("D_canopy",i);//get_val_at(j,i); j++;
      p_atm = in_data.get_val_for("p_atm",i);//get_val_at(j,i); j++;
      CO2_atm = in_data.get_val_for("CO2_atm",i);//get_val_at(j,i); j++;
      t_surface = in_data.get_val_for("t_surface",i);//get_val_at(j,i); j++;
      t_soil = in_data.get_val_for("t_soil",i);//get_val_at(j,i); j++;
      t_root = in_data.get_val_for("t_root",i);//get_val_at(j,i); j++;
      Zw = in_data.get_val_for("Zw",i);//get_val_at(j,i); j++;
      xylem_capacity_multiplier = in_data.get_val_for("xylem_capacity_multiplier",i);//get_val_at(j,i); j++;


      if (state.get_val_at(REGEN) == 0.0)
      {
        treesParams.xylemScalar = xylem_capacity_multiplier;
      }
      if (treesParams.useInputStress == true)
      {
        treesParams.stressScalar = in_data.get_val_for("stressScalar",i);//.get_val_at(j,i); j++;
      }
      //ignore = in_data.get_val_at(j,i); j++;
      // NEEobs = in_data.get_val_for("NEEobs",i);//.get_val_at(j,i); j++;
      // Ecobs = in_data.get_val_for("Ecobs",i);//get_val_at(j,i);

      //Initialize soil temp in each layer (up to ULAT layers)
      for (j = 0; j < treesParams.rmodules; j++)
      {
        if (j == 0)
        {
          tempSoil[j] = 0.5*(t_surface+t_soil);
        }
        else if (j == 1)
        {
          tempSoil[j] = t_soil;
        }
        else
        {
          tempSoil[j] = t_root;
        }
      }

      ti.get_time(year, yday, hour, min);

      //This next set of code is used to recall the plant water balance model setup
      //  allowing for adjustments in root volume, and xylem refilling
      treesParams.updatedHydraulics = false;
      treesParams.forceRefilling = false;
      if (treesParams.useHydraulics == true)
      {
        //
        //Concept: root pressure is sufficient to allow refilling when soil water potential is higher than -0.3 MPa
        //         added for maize, refilling occurs at 11:30 pm
        //Citation: Gleason et al 2017 Frontiers in Plant Science
        //	   Mackay, September 23 2019
        //Stages: Early vegetative growth: to day 157; late vegetative growth to 200;
        //  	  early reproductive to day 225
        //
        //	Refilling under positive pressure may be an artifact (per Sean Gleason) - DSM May 2023
        //

        //if (treesParams.xylemScalar == 0.99 || (treesParams.useLeafModule == false && treesParams.usePhenology == false && i % 48 == 47 && soilpsiavg[1] > -0.3 && simOut.Soil_Psi > -0.033))

        if (treesParams.xylemScalar == 0.99)
        {
          treesParams.forceRefilling = true;
          treesParams.updatedHydraulics = true;
          if (treesParams.useLeafModule == false && treesParams.usePhenology == false)
          {
            if (treesParams.lai < treesParams.laiFullCanopyHeight)
            {
              treesParams.allowLeafRefilling = true;
            }
            else
            {
              treesParams.allowLeafRefilling = false;
            }
          }
          if (yday < 100 || treesParams.usePhenology == false)
          {
            //at refilling reset the potential lai
            treesParams.lai_at_sat_kl = treesParams.live_lai;
          }
        }
        if ((i > 1 && i % 48 == 0)) //every day
        {

          latot = bgc.getLeafBiomassCarbon()*treesParams.SLA/10000.0;

          ratot = bgc.computeRootArea(treesParams);
          bgc.updateLateralRootWeibulls(bsat, ccsat, treesParams);
          for (mm=1; mm<=treesParams.smodules; mm++)
          {
            al[mm] = treesParams.al[mm] * latot;
          }
          for (mm=1+treesParams.smodules+1; mm <=(treesParams.rmodules+treesParams.smodules+1); mm++)
          {
            ar[mm] = treesParams.ar[mm] * ratot;
            ll[mm] = (ar[mm] / (2.0 * M_PI * rr));
          }
          //recompute the maximum saturated hydraulic conductance as the root area and leaf area changes
          //for perennials call this once every 10 days; for annuals call every other day
          if (
              (latot > 0.1)&& (
                  (treesParams.usePhenology == true && treesParams.forceRefilling == true) ||
                    (treesParams.usePhenology == true && treesParams.leafLifeSpan >= 1.0 && i % 480 == 0) ||
                    (treesParams.usePhenology == false)
              )
          )
          {
            for (mm=1; mm <=(treesParams.rmodules+treesParams.smodules+1); mm++)
            {
              psimin_in[mm][0] = psimin[mm][0];
              psimin_in[mm][1] = psimin[mm][1];
            }
            soilpsiavg_in[1] = soilpsiavg[1];
            soilpsimin_in = soilpsimin;
            reset = false;

            //third
            /*test silent speed*/ if(treesParams.VERBOSE) Rcpp::Rcout << ">>> Reinvoke hydraulic setup() <<< \n\n";

            hydraulicModel -> setup(
                reset,
                ecrit_k_psi,
                k_p_e,
                ecritOut,
                pc,
                klpred,
                ppredOut,
                epredOut,
                nodeFail,
                nodeTyp,
                psi,
                rflux,
                soilpsiavg,
                nts,
                evap,
                kroot,
                axr,
                latr,
                kshoot,
                dslat,
                dsax,
                drlat,
                drax,
                ll,
                ksat,
                bsat,
                ccsat,
                newb,
                newc,
                ip,
                b,
                b1,
                n,
                n1,
                r,
                vu,
                vl,
                cf,
                pe,
                ws_,
                ks2,
                p,
                dpp,
                jl,
                wnu,
                wu,
                wnl,
                wl,
                cpu,
                cpl,
                cpp,
                ku,
                kl,
                f,
                cc,
                psinode,
                psimin,
                jmatrix,
                jmatrix2,
                percent,
                rssoil,
                rs,
                dp,
                ff,
                col,
                row,
                indxx,
                subtract,
                pressure,
                plc,
                plcweib,
                rsquare,
                soilpsi,
                al,
                ar,
                saturatedKs,
                ptarg,
                e,
                rr,
                rzw,
                rd,
                einc,
                SoilRhizElements,
                dt,
                gmd,
                gsd,
                bkd,
                fs,
                fc,
                Fabs,
                cpplant,
                saxlat,
                raxlat,
                rfract,
                kla,
                aral,
                latot,
                ratot,
                shootElements,
                ktotal,
                soilpsimin,
                ShootModules,
                rootElements,
                RootModules,
                totmodules,
                axShoot_b,
                axShoot_c,
                latShoot_b,
                latShoot_c,
                axRoot_b,
                axRoot_c,
                latRoot_b,
                latRoot_c,
                plco,
                pdecrement,
                sumy1,
                sumy2,
                sumprod,
                cincrement,
                soilvol,
                rlateral,
                rLat_base,
                slateral,
                sLat_base,
                raxial,
                pleafave,
                tnode,
                fac,
                modelledKL,
                HydraulicModelFailCond,
                treesParams
            );

            // This is a good place to handle failures before moving on if not caught in setup()


            soilpsiavg[1] = soilpsiavg_in[1];
            soilpsimin = soilpsimin_in;
            treesParams.updatedHydraulics = true;

            for (mm=1; mm <=(treesParams.rmodules+treesParams.smodules+1); mm++)
            {
              psimin[mm][0] = psimin_in[mm][0];
              psimin[mm][1] = psimin_in[mm][1];
            }

            ratot = bgc.computeRootArea(treesParams);
            bgc.updateLateralRootWeibulls(bsat, ccsat, treesParams);
            for (mm=1; mm<=treesParams.smodules; mm++)
            {
              al[mm] = treesParams.al[mm] * latot;
            }
            for (mm=1+treesParams.smodules+1; mm <=(treesParams.rmodules+treesParams.smodules+1); mm++)
            {
              ar[mm] = treesParams.ar[mm] * ratot;
              ll[mm] = (ar[mm] / (2.0 * M_PI * rr));
            }
          }
        }
      }

      bool silent = false;

      //call simulations functions for current time step
      simOut = simulation_functions(
        silent,
        ts,
        Ecrit,
        Thresh,
        state,
        thetaSoil,
        tempSoil,
        ti,
        u_ref,
        t_ref,
        D_ref,
        precip,
        Qpar,
        t_canopy,
        D_canopy,
        p_atm,
        CO2_atm,
        t_surface,
        t_soil,
        t_root,
        Zw,
        treesParams,
        EcState,
        EcStateInner,
        GSI_vector,
        EcState[ts - 1],// This may be related to odd rTREES failure. see EcState above
               ModelStatus,
               LeafPsiInner,
               KpInner,
               InnerStep,
               bgc,
               hydraulicModel,
               ecrit_k_psi,
               k_p_e,
               ecritOut,
               pc,
               klpred,
               ppredOut,
               epredOut,
               nodeFail,
               nodeTyp,
               psi,
               rflux,
               soilpsiavg,
               nts,
               evap,
               kroot,
               axr,
               latr,
               kshoot,
               dslat,
               dsax,
               drlat,
               drax,
               ll,
               ksat,
               bsat,
               ccsat,
               newb,
               newc,
               ip,
               b,
               b1,
               n,
               n1,
               r,
               vu,
               vl,
               cf,
               pe,
               ws_,
               ks2,
               p,
               dpp,
               jl,
               wnu,
               wu,
               wnl,
               wl,
               cpu,
               cpl,
               cpp,
               ku,
               kl,
               f,
               cc,
               psinode,
               psimin,
               jmatrix,
               jmatrix2,
               percent,
               rssoil,
               rs,
               dp,
               ff,
               col,
               row,
               indxx,
               subtract,
               pressure,
               plc,
               plcweib,
               rsquare,
               soilpsi,
               al,
               ar,
               saturatedKs,
               ptarg,
               e,
               rr,
               rzw,
               rd,
               einc,
               SoilRhizElements,
               dt,
               gmd,
               gsd,
               bkd,
               fs,
               fc,
               Fabs,
               cpplant,
               saxlat,
               raxlat,
               rfract,
               kla,
               aral,
               latot,
               ratot,
               shootElements,
               ktotal,
               soilpsimin,
               ShootModules,
               rootElements,
               RootModules,
               totmodules,
               axShoot_b,
               axShoot_c,
               latShoot_b,
               latShoot_c,
               axRoot_b,
               axRoot_c,
               latRoot_b,
               latRoot_c,
               plco,
               pdecrement,
               sumy1,
               sumy2,
               sumprod,
               cincrement,
               soilvol,
               rlateral,
               rLat_base,
               slateral,
               sLat_base,
               raxial,
               pleafave,
               tnode,
               fac,
               modelledKL,
               HydraulicModelFailCond
      );

      state.set_val_at(xylem_capacity_multiplier, XYLEM);
      if((to_out[0])){
        Rcpp::as<Rcpp::NumericVector>(fluxout["year"])[ts-1]=(year);
        Rcpp::as<Rcpp::NumericVector>(fluxout["jday"])[ts-1]=(yday);
        Rcpp::as<Rcpp::NumericVector>(fluxout["hour"])[ts-1]=(hour);
        Rcpp::as<Rcpp::NumericVector>(fluxout["min"])[ts-1]=(min);

        Rcpp::as<Rcpp::NumericVector>(fluxout["simET"])[ts-1]=(simOut.Et);
        Rcpp::as<Rcpp::NumericVector>(fluxout["WPlant_K"])[ts-1]=(simOut.WPlant_K);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Soil_Psi"])[ts-1]=(simOut.Soil_Psi);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Leaf_Psi"])[ts-1]=(simOut.Leaf_Psi);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Psi_Crit"])[ts-1]=(simOut.Psi_Crit);

        Rcpp::as<Rcpp::NumericVector>(fluxout["Ecrit"])[ts-1]=(simOut.Ecrit);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Ec"])[ts-1]=(simOut.Ec);
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["RhizFlux"+std::to_string(j)])[ts-1]=(simOut.rhizFlux[j]);
        }
        Rcpp::as<Rcpp::NumericVector>(fluxout["Gs"])[ts-1]=(simOut.Gs);
        Rcpp::as<Rcpp::NumericVector>(fluxout["LAI"])[ts-1]=(treesParams.realized_lai);


        Rcpp::as<Rcpp::NumericVector>(fluxout["SLA"])[ts-1]=(treesParams.SLA);
        Rcpp::as<Rcpp::NumericVector>(fluxout["liveLAI"])[ts-1]=(treesParams.live_lai);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Rmaint"])[ts-1]=(simOut.rmaint);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Rgrowth"])[ts-1]=(simOut.rgrowth);
        Rcpp::as<Rcpp::NumericVector>(fluxout["reproduction"])[ts-1]=(bgc.getFruitCarbon());
        Rcpp::as<Rcpp::NumericVector>(fluxout["leafNSC"])[ts-1]=(bgc.getLeafNSC());
        Rcpp::as<Rcpp::NumericVector>(fluxout["stemNSC"])[ts-1]=(bgc.getStemNSC());
        Rcpp::as<Rcpp::NumericVector>(fluxout["rootNSC"])[ts-1]=(bgc.getRootNSC());
        Rcpp::as<Rcpp::NumericVector>(fluxout["chloroStarch"])[ts-1]=(bgc.getChloroplastStarch());
        Rcpp::as<Rcpp::NumericVector>(fluxout["chloroSugar"])[ts-1]=(bgc.getChloroplastSugar());
        Rcpp::as<Rcpp::NumericVector>(fluxout["waterStress"])[ts-1]=(simOut.waterStress);
        Rcpp::as<Rcpp::NumericVector>(fluxout["litterH2O"])[ts-1]=(state.get_val_at(LITTER));
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["theta"+std::to_string(j)])[ts-1]=(thetaSoil[j]);
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["soilPsi"+std::to_string(j)])[ts-1]=(simOut.soilPsi[j]);
        }
        Rcpp::as<Rcpp::NumericVector>(fluxout["thetaRoot"])[ts-1]=(simOut.thetaRoot);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Can_Evap"])[ts-1]=(simOut.canopy_evaporation);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Snowpack"])[ts-1]=(simOut.snowpack);
        Rcpp::as<Rcpp::NumericVector>(fluxout["SnowEdef"])[ts-1]=(simOut.snowpack_E_deficit);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Vcmax25"])[ts-1]=(simOut.Vcmax25);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Vcmax_sun"])[ts-1]=(simOut.Vcmax_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Vcmax_shd"])[ts-1]=(simOut.Vcmax_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Jmax25"])[ts-1]=(simOut.Jmax25);
        Rcpp::as<Rcpp::NumericVector>(fluxout["J_sun"])[ts-1]=(simOut.J_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["J_shd"])[ts-1]=(simOut.J_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Asun"])[ts-1]=(simOut.A_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Ashd"])[ts-1]=(simOut.A_shd);
        //
        // << simOut.Av_sun << '\t' << simOut.Av_shd << '\t'
        //<< simOut.Aj_sun << '\t' << simOut.Aj_shd << '\t'
        //<< simOut.phi2_sun << '\t' << simOut.phi2_shd
        //<< '\t' << simOut.betaA_sun << '\t' << simOut.betaA_shd

        Rcpp::as<Rcpp::NumericVector>(fluxout["Av_sun"])[ts-1]=(simOut.Av_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Av_shd"])[ts-1]=(simOut.Av_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Aj_sun"])[ts-1]=(simOut.Aj_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Aj_shd"])[ts-1]=(simOut.Aj_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["phi2_sun"])[ts-1]=(simOut.phi2_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["phi2_shd"])[ts-1]=(simOut.phi2_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["betaA_sun"])[ts-1]=(simOut.betaA_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["betaA_shd"])[ts-1]=(simOut.betaA_shd);



        //
        Rcpp::as<Rcpp::NumericVector>(fluxout["Lsun"])[ts-1]=(simOut.L_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Lshd"])[ts-1]=(simOut.L_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Tsun"])[ts-1]=(simOut.T_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Tshd"])[ts-1]=(simOut.T_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Dsun"])[ts-1]=(simOut.D0_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Dshd"])[ts-1]=(simOut.D0_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Ci_sun"])[ts-1]=(simOut.Ci_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Ci_shd"])[ts-1]=(simOut.Ci_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["PARsun"])[ts-1]=(simOut.PAR_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["PARshd"])[ts-1]=(simOut.PAR_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["gs_sun"])[ts-1]=(simOut.gs_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["gs_shd"])[ts-1]=(simOut.gs_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["NEE"])[ts-1]=(simOut.NEE);
        Rcpp::as<Rcpp::NumericVector>(fluxout["NPP"])[ts-1]=(simOut.NPP);
        Rcpp::as<Rcpp::NumericVector>(fluxout["R_total"])[ts-1]=(simOut.R_total);
        Rcpp::as<Rcpp::NumericVector>(fluxout["R_ag"])[ts-1]=(simOut.R_ag);
        Rcpp::as<Rcpp::NumericVector>(fluxout["R_bg"])[ts-1]=(simOut.R_bg);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Rd_sun"])[ts-1]=(simOut.Rd_sun);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Rd_shd"])[ts-1]=(simOut.Rd_shd);
        Rcpp::as<Rcpp::NumericVector>(fluxout["Csapwood"])[ts-1]=(bgc.getLiveStemCarbon());
        double rc_sum;
        for (j = 0; j < treesParams.rmodules; j++)
        {
          k = 0;
          Rcpp::as<Rcpp::NumericVector>(fluxout["FibRootC"+std::to_string(j)])[ts-1]=(bgc.getRootCarbon(j,k));
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          k = 1;
          Rcpp::as<Rcpp::NumericVector>(fluxout["FineRootC"+std::to_string(j)])[ts-1]=(bgc.getRootCarbon(j,k));
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          rc_sum = 0.0;
          for (k = 0; k < 10; k++)
          {
            rc_sum += bgc.getRootCarbon(j,k);
          }
          Rcpp::as<Rcpp::NumericVector>(fluxout["TotRootC"+std::to_string(j)])[ts-1]=(rc_sum);
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          rc_sum = bgc.getFineRootCarbon(j)/bgc.getFineRootBiomassN(j);
          Rcpp::as<Rcpp::NumericVector>(fluxout["FineRootCN"+std::to_string(j)])[ts-1]=(rc_sum);
        }

        Rcpp::as<Rcpp::NumericVector>(fluxout["LeafCN"])[ts-1]=(bgc.getLeafBiomassCarbon()/bgc.getLeafBiomassN() );

        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["humusC"+std::to_string(j)])[ts-1]=(bgc.getHumus(j));
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["RhizCl"+std::to_string(j)])[ts-1]=(bgc.getRhizosphereCl(j));
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["RhizNl"+std::to_string(j)])[ts-1]=(bgc.getRhizosphereNl(j));
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["AAexudateC"+std::to_string(j)])[ts-1]=(bgc.getAminoAcidExudateC(j));
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["SugarExudateC"+std::to_string(j)])[ts-1]=(bgc.getSugarExudateC(j));
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["MicrobC"+std::to_string(j)])[ts-1]=(bgc.getRhizosphereLiveMicrobialCarbon(j));
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["MicrobN"+std::to_string(j)])[ts-1]=(bgc.getRhizosphereMicrobialNitrogen(j));
        }
        Rcpp::as<Rcpp::NumericVector>(fluxout["RhizosphereNitrateNitrogen"])[ts-1]=(bgc.getRhizosphereNitrateNitrogen());
        Rcpp::as<Rcpp::NumericVector>(fluxout["RhizosphereAmmoniumNitrogen"])[ts-1]=(bgc.getRhizosphereAmmoniumNitrogen());
        Rcpp::as<Rcpp::NumericVector>(fluxout["PlantN"])[ts-1]=(bgc.getPlantN());
        Rcpp::as<Rcpp::NumericVector>(fluxout["plantNstat"])[ts-1]=(bgc.plantNstatus);
        Rcpp::as<Rcpp::NumericVector>(fluxout["RLA"])[ts-1]=(treesParams.Ar_Al);
        Rcpp::as<Rcpp::NumericVector>(fluxout["CanopyCover"])[ts-1]=(treesParams.canopy_cover);
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(fluxout["ar"+std::to_string(j)])[ts-1]=(treesParams.ar[j+3]);
        }

      }







      if(to_out[1]){
        // Rcpp::as<Rcpp::NumericVector>(hydrout["ti"])[ts-1]=(ti);
        Rcpp::as<Rcpp::NumericVector>(hydrout["year"])[ts-1]=(year);
        Rcpp::as<Rcpp::NumericVector>(hydrout["jday"])[ts-1]=(yday);
        Rcpp::as<Rcpp::NumericVector>(hydrout["hour"])[ts-1]=(hour);
        Rcpp::as<Rcpp::NumericVector>(hydrout["min"])[ts-1]=(min);

        Rcpp::as<Rcpp::NumericVector>(hydrout["latStemK"])[ts-1]=(k_p_e.latK[1]);


        // hydrout << ti << '\t' << k_p_e.latK[1] << '\t';
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["latRootK"+std::to_string(j)])[ts-1]=(k_p_e.latK[2+j]);
          // hydrout << k_p_e.latK[2+j] << '\t';
        }
        Rcpp::as<Rcpp::NumericVector>(hydrout["StemAxialYm"])[ts-1]=(psimin[1][0]);
        Rcpp::as<Rcpp::NumericVector>(hydrout["StemLatYm"])[ts-1]=(psimin[1][1]);

        // hydrout << psimin[1][0] << "\t" << psimin[1][1] << "\t";
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["RootAxialYm"+std::to_string(j)])[ts-1]=(psimin[j+3][0]);
          // hydrout << psimin[j+3][0] << "\t";
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["RootLatYm"+std::to_string(j)])[ts-1]=(psimin[j+3][1]);
          // hydrout << psimin[j+3][1] << "\t";
        }
        Rcpp::as<Rcpp::NumericVector>(hydrout["StemAxialKm"])[ts-1]=(ks2[1][0]);
        Rcpp::as<Rcpp::NumericVector>(hydrout["StemLatKm"])[ts-1]=(ks2[1][1]);

        // hydrout << ks2[1][0] << "\t" << ks2[1][1] << "\t";
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["RootAxialKm"+std::to_string(j)])[ts-1]=( ks2[j+3][0]);
          // hydrout << ks2[j+3][0] << "\t";
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["RootLatKm"+std::to_string(j)])[ts-1]=( ks2[j+3][1]);
          // hydrout << ks2[j+3][1] << "\t";
        }

        Rcpp::as<Rcpp::NumericVector>(hydrout["StemAxial_b"])[ts-1]=(b[1][0]);
        Rcpp::as<Rcpp::NumericVector>(hydrout["StemLat_b"])[ts-1]=(b[1][1]);

        // hydrout << b[1][0] << "\t" << b[1][1] << "\t";

        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["RootAxial_b"+std::to_string(j)])[ts-1]=( b[j+3][0]);
          // hydrout << b[j+3][0] << "\t";
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["RootLat_b"+std::to_string(j)])[ts-1]=(b[j+3][1]);
          // hydrout << b[j+3][1] << "\t";
        }
        Rcpp::as<Rcpp::NumericVector>(hydrout["StemAxial_c"])[ts-1]=(cc[1][0]);
        Rcpp::as<Rcpp::NumericVector>(hydrout["StemLat_c"])[ts-1]=(cc[1][1]);

        // hydrout << cc[1][0] << "\t" << cc[1][1] << "\t";
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["RootAxial_c"+std::to_string(j)])[ts-1]=(cc[j+3][0]);
          // hydrout << cc[j+3][0] << "\t";
        }
        for (j = 0; j < treesParams.rmodules; j++)
        {
          Rcpp::as<Rcpp::NumericVector>(hydrout["RootLat_c"+std::to_string(j)])[ts-1]=(cc[j+3][1]);
          // hydrout << cc[j+3][1] << "\t";
        }
        // hydrout <<" \n";
        //clear vector
      } //end if
    }
    //get leaf area data for this ts
    if(to_out[2])//if using leaf module this shall be returned
    {
      Rcpp::as<Rcpp::NumericVector>(lfareaout["year"])[ts-1]=(year);
      Rcpp::as<Rcpp::NumericVector>(lfareaout["jday"])[ts-1]=(yday);
      Rcpp::as<Rcpp::NumericVector>(lfareaout["hour"])[ts-1]=(hour);
      Rcpp::as<Rcpp::NumericVector>(lfareaout["min"])[ts-1]=(min);
      for(j=0; j<(bgc.Lf_idx) ; ++j )
      {
        Rcpp::as<Rcpp::NumericVector>(lfareaout["Area_Leaf_"+std::to_string(j+1)])[ts-1]=(bgc.getSingleLeafArea(j));
        // lfareaout << bgc.getSingleLeafArea(j) << "\t" ;
      }
      // lfareaout <<" \n";
    }

    if(treesParams.softerror>treesParams.maxsofterror){
      if(treesParams.VERBOSE) Rcpp::Rcout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! \n ERROR:: Too many soft errors have accumulated. Aborting simulation. \n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
      i=n_tsteps;
    }
    // Every 48 time steps (equivalent to a days worth) reduce softerror counter by 1.
    // This should allow intermittent softerrors while exiting the simulation on persistent errors.
    // The errorfree_steps counter intentionally resets after 48 errorfree_steps to focus on 'recent'
    // good/bad steps.
    if(treesParams.softerror==lastsofterror){
      errorfree_steps+=1;
      if(errorfree_steps==48){
        if(treesParams.softerror>0){
          treesParams.softerror-=1;
        }
        errorfree_steps=0;
      }
    }
    lastsofterror=treesParams.softerror;
  } //end time loop
  EcStateInner.clear();
  LeafPsiInner.clear();
  KpInner.clear();
  InnerStep.clear();
  GSI_vector.clear();
  EcState.clear();
  if (treesParams.useHydraulics == true)
  {
    // free3DArray(psi, NREC, MD);
    // free2DArray(rflux, NREC);
  }
  delete hydraulicModel;
} //end simulator




