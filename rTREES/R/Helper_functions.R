#system.file("FP_ex","FP_parameter.p",package="rTREES") is how to ref data shipped with pkgs


##This chunk makes sure the rcpp and rtrees functions work correctly

#' @exportPattern "^[[:alpha:]]+"
#' @importFrom Rcpp evalCpp
#' @useDynLib  rTREES, .registration = TRUE
NULL

#' @importFrom magrittr `%>%`
#' @export
magrittr::`%>%`

#' Read in original style TREES input files
#' @param driver path to tab deliminated driver file (typically ends in .txt)
#' @param parameters path to parameter file (typically ends in .p)
#' @param root_shoot_soil path to root/shoot module file (typically named root_shoot_soil previously called param_mod)
#' @examples
#' demo_inputs_from_file<-Load_TREES_files(
#'   driver=system.file("examples","demo_driver.txt",package="rTREES"),#"path/to/driver.txt",
#'   parameters = system.file("examples","demo_parameter.p",package="rTREES"),#"path/to/parameters.p",
#'  root_shoot_soil = system.file("examples","demo_root_shoot_soil",package="rTREES")#"path/to/rss"
#' )
#' @export
Load_TREES_files <- function(
    driver = NA,
    parameters = NA,
    root_shoot_soil = NA
){
  list_out <- list()
  if (!is.na(driver)){
    dr_in <- data.table::fread(driver, data.table = FALSE)
    names_dr_in <- names(dr_in)
    col_1 <- dr_in[, 1]
    col_2 <- dr_in[, 2]
    names_dr_in <- c("year", "jday", "hour", "min", names_dr_in[3:length(names_dr_in)])

    if (as.numeric(dr_in[3, 1]) > 1000){
      time_in <- dr_in %>%
        dplyr::mutate(year = as.numeric(
          substr(
            col_1,
            start = 1,
            stop = 4
            )
          ),
          jday = as.numeric(
            substr(
              col_1,
              start = 5,
              stop = 7
              )
            )
          )
      dr_out <- cbind(
        data.frame(
          year = time_in$year,
          jday = time_in$jday,
          hour = floor(dr_in[, 2]),
          min = (dr_in[, 2] - floor(dr_in[, 2])) * 10
        ),
        dr_in %>%
          dplyr::select(3:ncol(dr_in))
      )
    } else{
      dr_out <- cbind(
        data.frame(
          year = rep(2018, nrow(dr_in)),
          jday = col_1,
          hour = floor(col_2),
          min = (col_2 - floor(col_2)) * 10
        ),
        dr_in %>%
          dplyr::select(3:ncol(dr_in))
      )
    }

    all_driver <- Driver_inputs()
    names(dr_out) <- all_driver$ParameterName

    ValidateDriver(dr_out)
    list_out <- append(list_out, list(driver = dr_out))
  }
  all_params <- ExampleParameters()
  if (!is.na(parameters)) {
    p_in <- ValidateParameters(parameters)
    p_out <- data.frame(
      Value = p_in$Value,
      Parameter_Name = all_params$Parameter_Name,
      p_file_notes = p_in$Notes
    )
    list_out <- append(list_out, list(parameters = p_out))
  }

  if (!is.na(root_shoot_soil)) {
    pm_out <- CleanRootShootParams(root_shoot_soil = root_shoot_soil)
    list_out <- append(list_out, list(root_shoot_soil = pm_out))

  }

  return(list_out)
}

#' Write out original style TREES files for sharing changes
#' @import readr
#' @importFrom tidyr unite
#' @import dplyr
#' @param destination Path to store files to.
#' @param prefix Character string to prepend all files with.
#' @param driver Valid driver data.frame.
#' @param parameter Valid parameter data.frame.
#' @param rss valid root_shoot_soil list.
#' @return Returns a vector of the locations where the files were written.
#' @examples
#' #'
#' #Write_TREES_files(
#'   #destination="inst",#"path/to/destination",#use whole path if outside working dir
#'   #prefix="demo",
#'   #driver=BuildDriver(),
#'   #parameter=ExampleParameters(),
#'   #rss=ExampleRSS()
#' #)
#' @export
Write_TREES_files<-function(
    destination=getwd(),#include whole path if outside working dir
    prefix="demo",
    driver=NA,
    parameter=NA,
    rss=NA
){
  if(substr(destination,nchar(destination),nchar(destination))=="/"){

  }else{
    destination<-paste0(destination,"/")
  }
  driver_dest=NA
  p_dest=NA
  rss_dest=NA
  if(!any(is.na(driver))){
    driver_dest<-paste0(destination,prefix,"_driver.txt")
    driver<-driver %>%
      dplyr::mutate(
        Time=hour+ifelse(min>0,0.5,0.0),jday=paste0(year,jday)
      ) %>%
      dplyr::select(
        jday,Time,u_ref,t_ref,
        D_ref,precip,Qpar,t_canopy,
        D_canopy,	p_atm,CO2_atm,
        t_surface,t_soil,t_root,
        Zw,xylem_capacity_multiplier,
        NEEobs,	Ecobs
      )

    readr::write_tsv(x = driver,file = driver_dest,col_names = TRUE)
  }

  if(!any(is.na(parameter))){
    p_dest<-paste0(destination,prefix,"_parameter.p")
    parameter<-parameter%>%
      tidyr::unite(combined,
                   dplyr::everything(), sep = " ") %>%
      dplyr::pull(combined)

    readr::write_lines(x = parameter,file=p_dest)
  }

  if(!any(is.na(rss))){
    rss_dest<-paste0(destination,prefix,"_root_shoot_soil")
    rss_out<-c(paste("#_of_shoot_modules",rss$smodules))
    for(i in 1:rss$smodules){
      rss_out<-append(
        rss_out,
        c(
          paste("leaf_area_fraction",rss$al[i]),
          paste("length_lateral",rss$dslat[i]),
          paste("length_axial",rss$dsax[i])

        )
      )
    }
    rss_out<-append(
      rss_out,c(paste("#_of_root_modules",rss$rmodules))
    )
    for(i in 1:(rss$rmodules)){
      rss_out<-append(
        rss_out,
        c(
          paste("leaf_area_fraction",rss$ar[i]),
          paste("length_lateral",rss$drlat[i]),
          paste("length_axial",rss$drax[i]),
          paste("layer_GMP",rss$layer_GMP[i]),
          paste("layer_GSD",rss$layer_GSD[i]),
          paste("layer_BD",rss$layer_BD[i]),
          paste("layer_porosity",rss$layer_porosity[i]),
          paste("layer_sand_fraction",rss$layer_sand_fraction[i]),
          paste("layer_clay_fraction",rss$layer_clay_fraction[i]),
          paste("layer_residual_water_content",rss$layer_residual_water_content[i]),
          paste("layer_initial_water_content",rss$layer_initial_water_content[i])

        )
      )
    }

    readr::write_lines(x = rss_out,file = rss_dest)
  }

  return(c(driver_dest,p_dest,rss_dest))
}

#' Clean parameter values
#' @param dirty_vector The vector of character strings to be cleaned
#' @return Splits the parameter values, in a character vector from reading a file, from the rest of the contents(notes) and returns them as a dataframe.
#' @examples
#' demo_raw_p<-ReformRawP(
#' dirty_vector =readLines(
#'   con = system.file("examples","demo_parameter.p",package="rTREES")
#'   )
#' )
#' @export
ReformRawP<-function(
    dirty_vector=c("2013/tY")
    ){

  out_param<-c(rep(0.0,length(dirty_vector)))
  for(i in 1:length(dirty_vector)){
    out_param[i]<-as.double(
      regmatches(
        dirty_vector[i],
        gregexpr(
          pattern = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",
          text =  dirty_vector[i]
        )
      )[[1]][1]
    )

  }
  out_notes<-c(rep(0.0,length(dirty_vector)))
  for(i in 1:length(dirty_vector)){
    out_notes[i]<-as.character(
      regmatches(
        dirty_vector[i],
        gregexpr(
          pattern = "[a-zA-Z].*",
          text =  dirty_vector[i]
        )
      )[[1]][1]
    )
  }
  out_comb<-data.frame(Value=out_param,Notes=out_notes)
  return(out_comb)
}



#' Parameter Names
#' @return Data.frame of names, descriptions, and typical ranges for TREES parameters.
#' @export
ExampleParameters <- function() {
  #Needs rewrite to row wise for easier maint.
  params <- rbind(
    c(2180,"altitude","m@Larm ",0,30000),
    c(-40.447,"latitude"," ",-90,90),
    c(-105.238,"longitude"," ",-180,180),
    c(0.45,"z_ref","m ",0,1000),
    c(0.3,"lai","single sided  ",0.05,10),
    c(0.3,"canopy_height","m ",0,100),
    c(1,"lai_at_full_canopy_height"," ",0.05,10),
    c(1,"l_angle","spherical may sample ",0,Inf),
    c(0.97,"canopy","emissivity ",0.8,1),
    c(0.5,"fPAR_beam","fraction of solar radiation that is PAR    ",0.5,0.5),
    c(0.5,"fPAR_diff","fraction of solar radiation that is PAR    ",0.5,0.5),
    c(0.8,"alpha_PAR"," ",0.7,1),
    c(0.2,"alpha_NIR"," ",0.2,0.2),
    c(0.5,"omega"," ",0.4,1),
    c(2,"p_crown"," ",1,3.34),
    c(0.5,"d_factor","C&Nfig5.5 ",0,1),
    c(0.1,"zm_factor","C&Nfig5.5 ",0.08,1.2),
    c(0.2,"zh_factor",7.19,0.2,0.2),
    c(1,"ps_model","photosynthesis model to use (1 = original C3 2 = experimental C3 3 = C4)  ",1,3),
    c(0.02,"Rd_mult","Rd=Rd_mult*Vcmax",0.001,0.05),
    c(2.7,"Jmax_mult","ratio of Jmax to Vcmax",1.5,3),
    c(0.8,"thetaJ","curvature parameter  ",0.7,0.9),
    c(0.4,"phiJ_sun","quantum yield e-/photon ",0,0.5),
    c(0.4,"phiJ_shd","quantum yield e-/photon ",0,0.5),
    c(0.001815,"Nleaf"," ",0.0001,0.003),
    c(0,"N_fixed_proportion"," ",0,1),
    c(0.2,"Nrubisco","proportion ",0.1,0.2),
    c(38.67764,"Kc25","(Pa) MM const carboxylase 25 deg C  ",-Inf,Inf),
    c(2.1,"q10Kc","(DIM) Q_10 for kc (default 2.1) ",-Inf,Inf),
    c(26123.26,"Ko25","(Pa) MM const oxygenase 25 deg C",-Inf,Inf),
    c(1.2,"q10Ko","(DIM) Q_10 for ko",-Inf,Inf),
    c(3.6,"act25","(umol/mgRubisco/min) Rubisco activity",-Inf,Inf),
    c(2.4,"q10act","(DIM) Q_10 for Rubisco activity (default was 2.4)",-Inf,Inf),
    c(48.85,"Vcmax25","maximum Rubisco activity at 25 C umol m-2 s-1",10,200),
    c(460,"Vpmax25","maximum PEP carbolylase activity at 25 C umol m-2 ",10,500),
    c(175,"Jmax25","maximum electron transport rate at 25 C umol m-2 s-1 ",15,600),
    c(38.6,"gammaStar25","compensation point at 25 C umol",-Inf,Inf),
    c(80,"Kp25","Michaelis constant of PEP carboxylase for CO2 at 25 C ubar ",-Inf,Inf),
    c(80,"Vpr","PEP regeneration rate umol m-2 s-1",-Inf,Inf),
    c(0,"f","correction for spectral quality of light ",-Inf,Inf),
    c(0.4,"x","partitioning factor of electron transport rate ",0,1),
    c(0.92,"absorptance","fraction of irradiance absorbed   ",0.85,0.92),
    c(67.294,"E_Vcmax","activation energy maximum carboxylation rate kJ mol-1",-Inf,Inf),
    c(70.373,"E_Vpmax","activation energy maximum PEP rate kJ mol-1",-Inf,Inf),
    c(77.9,"E_Jmax","activation energy electron transport kJ mol-1",-Inf,Inf),
    c(36.3,"E_Kp","activation energy Michaelis reaction of PEP kJ mol-1",-Inf,Inf),
    c(59.36,"E_kc","activation energy Michaelis reaction of carboxylation kJ mol-1",-Inf,Inf),
    c(35.94,"E_ko","activation energy Michaelis reaction of oxygenation kJ mol-1",-Inf,Inf),
    c(66.3,"E_Rd","activation energy Michaelis reaction of mitochondrial respiration kJ mol-1",-Inf,Inf),
    c(23.4,"E_gammaStar","activation energy Michaelis reaction of compensation point kJ mol-1",-Inf,Inf),
    c(1.78,"gm","mesophyll conductance to CO2 mol m-2 s-1",0.5,3),
    c(0.003,"gbs","conductance of the bundle sheath mol m-2 s-1   ",0.003,0.003),
    c(0.09,"alphaGmax","fraction of glycolate carbon diverted to glycine during photorespiration",-Inf,Inf),
    c(0.38,"alphaSmax","fraction of glycolate carbon diverted to serine during photorespiration",-Inf,Inf),
    c(0,"Nmax","maximum rate of de novo nitrogen supply to the chloroplast umol N m-2 s-1",-Inf,Inf),
    c(0.4,"Gsref0","reference canopy stomatal conductance (mol m-2 s-1)    ",0.01,1),
    c(0.54,"m","(proportion of Gsref0) ",0.45,0.59),
    c(0,"isAmphistomatous","(1 or 0) has stomata on both sides of leaf ",0,1),
    c(-0.1,"Md","used for diagnosing hydraulic model - if pressure goes higher than this value you get an error",-1,-0.01),
    c(-0.9,"midday_at_sat_kl"," ",-3,-0.1),
    c(4,"e_at_saturated_kl"," ",0.1,10),
    c(4,"rhizosphere_width"," ",3,5),
    c(4,"soilshells"," ",2,6),
    c(1,"frac_absorbing_length","keep this at 1 unless you have a good reason to change it    ",0,1),
    c(0.01,"Capacitance"," ",0.01,0.1),
    c(1,"axK_latKl_shoot_modules","keep this at 1 unless you have a good reason to change it    ",1,1),
    c(1,"axKr_latKr_root_modules","keep this at 1 unless you have a good reason to change it    ",1,1),
    c(50,"per_total_R_in_root_system","keep this at 50 unless you have a good reason to change it    ",25,75),
    c(-0.25,"pd_at_sat_kl"," ",-1.6,-0.1),
    c(1.57,"ax_Shoot_b_value"," ",1.2,6),
    c(2.38,"ax_Shoot_c_value"," ",1,12),
    c(1.57,"lat_Shoot_b_value"," ",1.2,6),
    c(2.38,"lat_Shoot_c_value"," ",1,12),
    c(1.57,"ax_Root_b_value"," ",1.2,6),
    c(2.38,"ax_Root_c_value"," ",1,12),
    c(1.57,"lat_Root_b_value"," ",1.2,6),
    c(2.38,"lat_Root_c_value"," ",1,12),
    c(3,"initial_conductivity_root","used to set saturated K's  ",1,100),
    c(0.01,"decrement_root","default 0.001  ",-Inf,Inf),
    c(6,"initial_conductivity_shoot","used to set saturated K's  ",1,100),
    c(0.02,"decrement_shoot"," ",-Inf,Inf),
    c(0.22,"theta_opt"," ",0.2,0.3),
    c(30,"optimal_soil_T"," ",25,35),
    c(1,"growth_resp_proportion"," ",1,1),
    c(0.0011,"resp_coef_root","kg kg-1 day-1 deg   ",-Inf,Inf),
    c(0.0002,"resp_coef_stem","kg kg-1 day-1 deg   ",-Inf,Inf),
    c(0.0004,"resp_coef_leaf","kg kg-1 day-1 deg   ",-Inf,Inf),
    c(0.05,"resp_coefficient","(Q10) degC-1",-Inf,Inf),
    c(72.26,"EaSx","Kjmol-1",-Inf,Inf),
    c(0.000000995,"kMsx","gCcm-3soil",-Inf,Inf),
    c(5.38e-10,"xASx"," ",-Inf,Inf),
    c(0.0085,"kd","d-1 ",0.001,0.01),
    c(0.6,"kn","m3 d-1 gC-1 ",0.4,0.8),
    c(0,"kea","m3 d-1 gC-1 (for exudates)",-Inf,Inf),
    c(0,"kes","m3 d-1 gC-1 (for exudates)",-Inf,Inf),
    c(0.000065,"kl","m3 d-1 gC-1 ",-Inf,Inf),
    c(0.0000025,"kh","m3 d-1 gC-1 ",-Inf,Inf),
    c(12,"fr_minCN","minimum fine root C:N ratio  ",10,200),
    c(22,"fr_maxCN","maximum fine root C:N ratio  ",10,200),
    c(12,"leaf_minCN","minimum leaf C:N ratio   ",10,200),
    c(22,"leaf_maxCN","maximum leaf C:N ratio   ",10,200),
    c(79200,"Cbelowground","kg ha-1  ",-Inf,Inf),
    c(0.000027,"Clitter_frac"," ",-Inf,Inf),
    c(0.00024,"Croot_frac","dim ",-Inf,Inf),
    c(1,"Cstem","kg ha-1  ",-Inf,Inf),
    c(1,"Csapwood","kg ha-1  ",-Inf,Inf),
    c(0.00024,"Croot_coarse_frac","dim ",-Inf,Inf),
    c(0.0000001,"interception_per_leafArea","m m2 m-2 ",0,0.00025),
    c(0.0001,"litter_capacity","m ",-Inf,Inf),
    c(0.0001,"litter_store","m ",-Inf,Inf),
    c(400,"SLA","m2 kgC-1 leaf ",3,400),
    c(434.3,"SRL1","m gC-1 specific root length at root diameter of 250 um ",15,1500),
    c(0.000125,"minRootDiam","m diameter of finest root  ",0.0001,0.001),
    c(0.003,"maxRootDiam","m diameter of thickest root  ",-Inf,Inf),
    c(0.2,"minRootLifespan","years lifespan of finest root at lowest C:N ratio  ",0.01,2),
    c(0.5,"LWP_spring_minimum","-MPa ",-Inf,Inf),
    c(2.35,"LWP_stomatal_closure","-MPa",-Inf,Inf),
    c(0,"is_bryophyte","(1 is yes 0 is no) ",-Inf,Inf),
    c(0.1,"capRiseScalar","(0 to 1) ",-Inf,Inf),
    c(1,"precipReduction"," ",-Inf,Inf),
    c(0,"drainScalar","(0 to 1) proportion of drainage absorbed by water table ",-Inf,Inf),
    c(0.2,"leafNSCscalar","(proportion of leaf structural carbon)  ",-Inf,Inf),
    c(0,"usePhenology"," ",-Inf,Inf),
    c(99999999999,"leafLife","Span ",-Inf,Inf),
    c(10,"max_iteration","(the_max_number_of_iterations_to_achieve_convergence_Delta<THR ",-Inf,Inf),
    c(100,"microbiomeScalar","unitless multiplier for the initial nutrient status of microbiome",-Inf,Inf),
    c(0,"microbialrainrate"," ",-Inf,Inf),
    c(0,"raininAmmonium"," ",-Inf,Inf),
    c(0,"raininNitrate"," ",-Inf,Inf),
    c(0,"raininMineralN"," ",-Inf,Inf),
    c(0,"raininLabileC"," ",-Inf,Inf),
    c(0,"snowpack_water_equivalent","m ",-Inf,Inf),
    c(0,"snowpack_E_deficit_max","deg C  ",-Inf,Inf),
    c(0.0015,"melt_Rcoef","m degC-1 30-min-1 ",-Inf,Inf),
    c(1,"useHydraulics","set to 1 if you want the full hydraulic model ",-Inf,Inf),
    c(0,"useInputStress","little used function allowing for use of previously computed water stress as input    ",-Inf,Inf),
    c(1,"useInputWaterTable"," ",-Inf,Inf),
    c(213,"dayToStopMaizeRefilling","used when both usePhenology and useLeafModule are set to zero (false)",-Inf,Inf),
    c(1,"useLeafModule","use Brassica rapa vegetative grtimeowth sub-model",-Inf,Inf),
    c(15,"leafAreaMax"," K  cm2",-Inf,Inf),
    c(0.11,"initialLeafSize","A_pot_in cm2",-Inf,Inf),
    c(0.000689,"leafArea_Rate","r ",-Inf,Inf),
    c(9708.083,"dur_LeafExpansion"," ",-Inf,Inf),
    c(400,"SLA_max","SLA_max m2 kgC-1",-Inf,Inf),
    c(60,"SLA_min","SLA_min m2 kgC-1",-Inf,Inf),
    c(60,"leaf_insertAngle"," leaf insertion angle   ",-Inf,Inf),
    c(2.25,"leaf_len_to_width"," leaf length to width ratio ",-Inf,Inf),
    c(0.95,"proportion_CD","a ",-Inf,Inf),
    c(2000000000,"phyllochron","phyllochron",-Inf,Inf),
    c(5000000000,"floweringTime","TTF ",-Inf,Inf),
    c(0.96,"Tbase","Tb ",-Inf,Inf),
    c(5000,"therm_plant"," ",-Inf,Inf),
    c(5,"projectedArea_init"," projected shoot area at initiation cm2",-Inf,Inf),
    c(77,"pot_size","max projected area cm2   ",-Inf,Inf),
    c(0.148,"root_to_shoot"," ",-Inf,Inf),
    c(1.56,"leaf_to_stem"," ",-Inf,Inf),
    c(0,"useLeafGamma"," ",-Inf,Inf),
    c(90.221837,"Kalpha"," ",-Inf,Inf),
    c(7.575909,"Kbeta"," ",-Inf,Inf),
    c(100.937764,"Nalpha"," ",-Inf,Inf),
    c(956.938476,"Nbeta"," ",-Inf,Inf),
    c(82.363411,"ralpha"," ",-Inf,Inf),
    c(119482.227606,"rbeta"," ",-Inf,Inf),
    c(0,"usePhiPS2","A boolean to turn on water-modulated PSII photochemistry",-Inf,Inf),
    c(800,"Q_ref","Reference light used for ChlF measurement",-Inf,Inf),
    c(0.05,"beta0_refQ","regression coefficienct in 'PhiPS2 ~ Leaf water potential' under a reference light",-Inf,Inf),
    c(0.76,"beta1_refQ","regression coefficienct in 'PhiPS2 ~ Leaf water potential' under a reference light",-Inf,Inf),
    c(6.22,"beta2_refQ","regression coefficienct in 'PhiPS2 ~ Leaf water potential' under a reference light",-Inf,Inf),
    c(8.03,"beta3_refQ","regression coefficienct in 'PhiPS2 ~ Leaf water potential' under a reference light",-Inf,Inf),
    c(0.82,"alpha_PS2","maximum light-adapted effective quantum yield of Photosystem II",-Inf,Inf),
    c(0.04,"kappa_PS2","non-zero minimum effective quantum yield of Photosystem II",-Inf,Inf),
    c(10,"sd_err_Ec"," ",-Inf,Inf),
    c(2.12692,"sd_err_NEE"," ",-Inf,Inf),
    c(0,"sd_err_Ec_weight"," ",-Inf,Inf)
  ) %>%
    as.data.frame() %>%
    dplyr::rename_with(~c("Value","Parameter_Name","Notes","Lower_range","Upper_range")) %>%
    dplyr::mutate(
      Value=as.double(Value),
      Lower_range=as.double(Lower_range),
      Upper_range=as.double(Upper_range)
      )
  return(params)
}


#' Change parameters
#' @import tibble
#' @param base_parameters Data.frame of the initial parameter values in first column or or path to text file of parameters values (one on each line in correct order).
#' @param NewValues Data.frame with columns named for the parameters and one row containg the new values for those parameters. Verify correct names by checking ExampleParameters() or param_search()
#' This only works with one row of values at a time.
#' @param VERBOSE Set verbosity to TRUE or FALSE
#'
#' @return Data frame of the base parameters with all modifications from NewValues and validated against default parameter set.
#' @examples
#' demo_new_parameters<-ChangeParameter(
#'    base_parameters=ExampleParameters(),
#'    NewValues=data.frame(
#'      "initial_conductivity_root"=25,
#'      "Gsref0"=0.1
#'    ),
#'    VERBOSE=TRUE
#' )
#' #'
#' @export
ChangeParameter<-function(
    base_parameters,#or data_frame with
    NewValues=data.frame("SLA"=0),
    VERBOSE=TRUE
){

  NewValues =NewValues[1,,drop=FALSE]#This should only be one row only read first row
  Parameters<-names(NewValues)
  all_params<-rTREES::ExampleParameters()
  has_error<-0
  if(is.data.frame(base_parameters)){#&nrow(base_parameters)==174){
    ParamFile<-base_parameters[,1]#This will only select the first column.
  }else if(is.character(base_parameters)){
    # ParamFile<-readLines(con = base_parameters)#fread(base_parameters,data.table = FALSE)#This reads base file
    ParamFile<-ValidateParameters(base_parameters)#ReformRawP(ParamFile)

  }else{
    stop("Invalid 'base_parameters' supplied. \n Please supply either: \n A full file name '(with path)' or \n a data frame with the first column as the values. \n Both must have all values for parameters listed in 'ExampleParameters()' ")
  }



  if(all(Parameters%in%all_params$Parameter_Name)){
    if(VERBOSE){
          message(paste("Setting values for","'", paste(Parameters,sep=" & ",collapse = " & "),"'",collapse=""))

    }
    # print(Parameters)
    for(h in 1:length(Parameters)){
      #This is not perfect and is why the base file must be in the same order as the example. This is to preserve maximum compatibility.
      ParamFile[all_params$Parameter_Name%in%Parameters[h]]<-as.double(NewValues[1,h,drop=FALSE])
      # line<-all_params[all_params$Parameter_Name%in%Parameters[h],1]
      #
      #  ParamFile[line]<-as.double(NewValues[1,h,drop=FALSE])
     }
  }else{
    has_error<-has_error+1
    poss_match<-data.frame("Your_Input"=Parameters)
    warning("Invalid parameter name supplied \n",immediate. = TRUE)
    for(m in 1:length(Parameters)){
      poss_match[m,"Valid_Options :"]<-paste(
        all_params[agrepl(Parameters[m],x = all_params$Parameter_Name,max.distance = 1.0125),2],
        collapse=" -OR- \n")%>%
        substr(start=1,stop=100) %>% {
          if(nchar(.)==100){paste(.,"...Too many matches")}else{.}

        }
    }
    if(length(poss_match>0)){

      stop(paste("Did you maybe mean one of these::", print(poss_match %>% tibble::tibble())))

    }

  }

  ParamFile<-data.frame(Value=unlist(ParamFile),Parameter_Name=all_params$Parameter_Name)

    return(ParamFile)


}


#' Load, prepare, and validate root/shoot module parameters.
#' @param root_shoot_soil A path to a root_shoot_soil file or a data.frame() first column is parameter names and the second column contains values.
#' @return list() containing root/shoot module parameters properly formatted for rTREES
#' @examples
#' demo_rss_from_file<-CleanRootShootParams(
#'  root_shoot_soil = system.file("examples","demo_root_shoot_soil",package="rTREES")
#' )
#' @export
CleanRootShootParams<-function(
    root_shoot_soil
    ){

  if(is.character(root_shoot_soil)){
    pm_in<-data.table::fread(root_shoot_soil,data.table = FALSE)
    nshoot<-as.numeric(pm_in[1,2])
    nroot<-as.numeric(pm_in[2+nshoot*3,2])
    if((nshoot*3+nroot*11+2)!=nrow(pm_in)){
      warning("WARNING! Number of root or shoot modules in file does not match provided number of parameters",immediate. = TRUE)
    }
    pm_out<-list(
      smodules= (nshoot),
      rmodules= (nroot),

      al=as.numeric(pm_in[seq(from=2,to=1+3*nshoot,by=3),2]),
      dslat=as.numeric(pm_in[seq(from=3,to=2+3*nshoot,by=3),2]),
      dsax=as.numeric(pm_in[seq(from=4,to=3+3*nshoot,by=3),2]),

      ar=as.numeric(pm_in[seq(from=3+nshoot*3,to=2+nshoot*3+11*nroot,by=11),2]),
      drlat=as.numeric(pm_in[seq(from=4+nshoot*3,to=3+nshoot*3+11*nroot,by=11),2]),
      drax=as.numeric(pm_in[seq(from=5+nshoot*3,to=4+nshoot*3+11*nroot,by=11),2]),
      layer_GMP=as.numeric(pm_in[seq(from=6+nshoot*3,to=5+nshoot*3+11*nroot,by=11),2]),
      layer_GSD=as.numeric(pm_in[seq(from=7+nshoot*3,to=6+nshoot*3+11*nroot,by=11),2]),
      layer_BD=as.numeric(pm_in[seq(from=8+nshoot*3,to=7+nshoot*3+11*nroot,by=11),2]),
      layer_porosity=as.numeric(pm_in[seq(from=9+nshoot*3,to=8+nshoot*3+11*nroot,by=11),2]),
      layer_sand_fraction=as.numeric(pm_in[seq(from=10+nshoot*3,to=9+nshoot*3+11*nroot,by=11),2]),
      layer_clay_fraction=as.numeric(pm_in[seq(from=11+nshoot*3,to=10+nshoot*3+11*nroot,by=11),2]),
      layer_residual_water_content=as.numeric(pm_in[seq(from=12+nshoot*3,to=11+nshoot*3+11*nroot,by=11),2]),
      layer_initial_water_content=as.numeric(pm_in[seq(from=13+nshoot*3,to=12+nshoot*3+11*nroot,by=11),2])
    )
  }else if(is.data.frame(root_shoot_soil)){
    pm_in<-root_shoot_soil
    nshoot<-as.numeric(pm_in[1,2])
    nroot<-as.numeric(pm_in[2+nshoot*3,2])
    if((nshoot*3+nroot*11+2)!=nrow(pm_in)){
      stop("ERROR! Number of root or shoot modules in file does not match provided number of modules.")
    }
    pm_out<-list(
      smodules= (nshoot),
      rmodules= (nroot),

      al=as.numeric(pm_in[seq(from=2,to=1+3*nshoot,by=3),2]),
      dslat=as.numeric(pm_in[seq(from=3,to=2+3*nshoot,by=3),2]),
      dsax=as.numeric(pm_in[seq(from=4,to=3+3*nshoot,by=3),2]),

      ar=as.numeric(pm_in[seq(from=3+nshoot*3,to=2+nshoot*3+11*nroot,by=11),2]),
      drlat=as.numeric(pm_in[seq(from=4+nshoot*3,to=3+nshoot*3+11*nroot,by=11),2]),
      drax=as.numeric(pm_in[seq(from=5+nshoot*3,to=4+nshoot*3+11*nroot,by=11),2]),
      layer_GMP=as.numeric(pm_in[seq(from=6+nshoot*3,to=5+nshoot*3+11*nroot,by=11),2]),
      layer_GSD=as.numeric(pm_in[seq(from=7+nshoot*3,to=6+nshoot*3+11*nroot,by=11),2]),
      layer_BD=as.numeric(pm_in[seq(from=8+nshoot*3,to=7+nshoot*3+11*nroot,by=11),2]),
      layer_porosity=as.numeric(pm_in[seq(from=9+nshoot*3,to=8+nshoot*3+11*nroot,by=11),2]),
      layer_sand_fraction=as.numeric(pm_in[seq(from=10+nshoot*3,to=9+nshoot*3+11*nroot,by=11),2]),
      layer_clay_fraction=as.numeric(pm_in[seq(from=11+nshoot*3,to=10+nshoot*3+11*nroot,by=11),2]),
      layer_residual_water_content=as.numeric(pm_in[seq(from=12+nshoot*3,to=11+nshoot*3+11*nroot,by=11),2]),
      layer_initial_water_content=as.numeric(pm_in[seq(from=13+nshoot*3,to=12+nshoot*3+11*nroot,by=11),2])
    )
  }else if(inherits(root_shoot_soil, "list") ){
    pm_out<-root_shoot_soil
  }else{
    stop("ERROR: root_shoot_soil supplied is not a path, list, or data frame.")

  }
  message("Root-Shoot-Soil layer parameters loaded.")

  return(pm_out)
}



#' Driver_inputs
#' @return Data frame of exact parameter names and their descriptions for drivers.
#' @export
Driver_inputs<-function(){
  drive_in<-data.frame(
    ParameterName=c(
      "year",
      "jday",
      "hour",
      "min",
      "u_ref" ,
      "t_ref",
      "D_ref",
      "precip"  ,
      "Qpar",
      "t_canopy",
      "D_canopy",
      "p_atm",
      "CO2_atm",
      "t_surface",
      "t_soil" ,
      "t_root",
      "Zw",
      "xylem_capacity_multiplier",
      "NEEobs",
      "Ecobs"
      # "Date",
      # "Time",
      # "u_ref",
      # "t_ref",
      # "d_ref",
      # "precip",
      # "q_par",
      # "t_canopy",
      # "d_canopy",
      # "p_atm",
      # "CO2_atm",
      # "T0",
      # "Tsurf",
      # "Troot",
      # "Zw",
      # "lai",
      # "A",
      # "Ec"
    ),
    Units=c(
      "Year",
      "YEARDAY",
      "hour",
      "min",
      "m s-1",
      "deg C",
      "kPa",
      "mm",
      "umol m-2 s-1",
      "deg C",
      "kPa",
      "kPa",
      "ppm",
      "deg C",
      "deg C",
      "deg C",
      "m",
      "unitless",
      "umol m-2 s-1",
      "mm 30min-1"
    ),
    Description=c(
      "Year of time step in format YYYY",
      "Julian or year day of timestep",
      "hour (base 24)",
      "min (fraction of hour) (should be only 0 or 5 for on the hour and half hour)",
      # "Using year as well as day the code will expect leap-years to have 366 days",
      # "Starting at 0 (midnight), incrementing by 0.5, ending with 23.5 (11:30 pm) for each day",
      "Wind velocity at the reference height (assumed to be above the canopy)",
      "Temperature at the reference height",
      "Vapor pressure deficit of the atmosphere at the reference height",
      "Precipitation above the canopy or irrigation made directly to the soil surface",
      "Incoming photosynthetically active radiation",
      "Temperature within the canopy; use the same value as t_ref if you don't have this observation",
      "Vapor pressure deficit within the canopy; use d_ref if you don't have this observation",
      "Atmospheric pressure observed at your location; can be computed from elevation",
      "Concentration of carbon dioxide in the atmosphere",
      "Temperature at the ground surface",
      "Temperature of the top soil layer",
      "Average temperature of the root zone",
      "Depth of the water table; can be used to provide a water source from below",
      "Has been used for multiple purposes; currently, a 1.0 does nothing, 0.99 forces xylem refilling",
      "Legacy column for use with the Bayesian MCMC algorithm",
      "Legacy column for use with the Bayesian MCMC algorithm"

    ),
    Notes=c(
      "ex 2000",
      "ex 254",
      "ex 14",
      "ex 5",
      "This cannot have zeroes; model will force value > 0.0",
      "This is a critical value to have",
      "This is a critical value to have",
      "This is a critical value to have",
      "This is a critical value to have",
      "",
      "",
      "This is a critical value to have",
      "This is a critical value to have",
      "",
      "",
      "",
      "This can be enabled or disabled in simulation_functions.cpp",
      "",
      "Use -999 if you have no data",
      "Use -999 if you have no data"

    )

  )
  return(drive_in)
}



#' Driver parameter visualizer.
#' @importFrom tidyr pivot_longer
#' @param driver Data.frame or file path for driver. Must have column names matching Driver_inputs()
#' @param dailys TRUE or FALSE aggregate data to daily values?
#' @return ggplot object facet wrapped to view each of the driver inputs.
#' @examples
#' Visual_driver(driver = BuildDriver())
#' Visual_driver(driver = BuildDriver(),dailys = TRUE)
#'
#' @export
Visual_driver<-function(driver,
                        dailys=FALSE
){
  if(is.data.frame(driver)){
    in_drv<-as.data.frame(driver)
  }else{
    in_drv<-data.table::fread(driver,data.table = FALSE)
  }

  if(!dailys){
    in_drv<-in_drv %>%
      dplyr::mutate(min=ifelse(min==0,0,30)) %>%
      dplyr::mutate(time_step=as.POSIXct(x = paste(year,jday,hour,min,sep = "-"),format="%Y-%j-%H-%M")) %>%
      dplyr::select(-year,-jday,-hour,-min) %>%
      tidyr::pivot_longer(cols=-time_step,names_to = "Vars",values_to = "Vals")
    plt_drv<-ggplot2::ggplot(in_drv,ggplot2::aes(x=time_step,y=Vals))+
      ggplot2::geom_point()+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1))+
      ggplot2::facet_wrap(~Vars,scales = "free")
  }else{
    in_drv<-in_drv %>%
      dplyr::mutate(min=ifelse(min==0,0,30)) %>%
      dplyr::mutate(time_step=as.POSIXct(x = paste(1979,1,hour,min,sep = "-"),format="%Y-%j-%H-%M")) %>%
      dplyr::select(-year,-jday,-hour,-min) %>%
      tidyr::pivot_longer(cols=-time_step,names_to = "Vars",values_to = "Vals")
    plt_drv<-ggplot2::ggplot(in_drv,ggplot2::aes(x=time_step,y=Vals))+
      ggplot2::geom_point()+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1))+
      ggplot2::facet_wrap(~Vars,scales = "free")
  }
  return(plt_drv)
}



#' Validate Driver
#'
#' @param Driver Data frame or path of driver to be validated.
#' @return The number of issues detected. Details are printed to console.
#' @examples
#' bdr<-BuildDriver()
#' bdr[matrix(
#'   sample(
#'     x = c(TRUE,FALSE),
#'     size = nrow(bdr)*ncol(bdr),
#'     prob = c(0.01,0.99),
#'     replace = TRUE
#'   ),
#'   nrow = nrow(bdr),
#'   ncol = ncol(bdr)
#' )]<- NA
#' ValidateDriver(Driver = bdr)
#'
#' @export
ValidateDriver<-function(
    Driver="driver.txt"# Path/file validates format and completeness of specified driver
){

  issues<-0
  if(!is.data.frame(Driver)){
    driver_in<-data.table::fread(Driver,data.table = FALSE)

  }else{
    driver_in<-as.data.frame(Driver)
    Driver<-"Driver"
  }
  for(i in 1:ncol(driver_in)){
    if(any(is.na(driver_in[, i]))){

      warning(paste(names(driver_in)[i], "is missing values.", sep = " "),immediate. = TRUE)
      issues<-issues+1
    } else if(!is.numeric(driver_in[, i])) {
      warning(paste(
        names(driver_in)[i],
        "is not numeric. May have characters or irregular values",
        sep = " "
      ),immediate. = TRUE)
      test_col = as.numeric(driver_in[, i])
      bad_index = which(is.na(test_col), arr.ind = TRUE)
      warning(paste(
        "Check these rows in",
        names(driver_in)[i],
        " :",
        paste(bad_index, collapse = ",")
      ),immediate. = TRUE)
      issues<-issues+1
    } else{
      # message(paste(
      #   Driver,
      #   " column ",
      #   names(driver_in)[i],
      #   " appears complete and numeric."
      # ),immediate. = TRUE)
    }

  }

  base_drvr<-Driver_inputs()
  missing_cols<-vector()
  for(drpn in base_drvr$ParameterName){
    if(drpn %in% names(driver_in)){
      #continue
    }else{
      missing_cols<-append(missing_cols,drpn)
      issues<-issues+1
    }
  }
  if(length(missing_cols)>0){
    warning(
      paste(
        Driver,
        "column names do no not match. \n Missing:  \n",paste(missing_cols,collapse = "\n"),"  \n",
        "Check Driver_inputs() for proper naming convention"
      ),immediate. = TRUE
    )
  }
  message(paste("Driver validation complete,",issues,"possible issues identified."))
  return(issues)

}

#' Attempt Driver Repair
#' @param na_df Data frame of driver containing NA values. Must contain all columns properly named. See BuildDriver() or Driver_inputs()
#' @return Data frame of driver with NA values filled or imputed as well as possible.
#' @examples
#' bdr<-BuildDriver()
#' bdr[matrix(
#'   sample(
#'     x = c(TRUE,FALSE),
#'     size = nrow(bdr)*ncol(bdr),
#'     prob = c(0.01,0.99),
#'     replace = TRUE
#'   ),
#'   nrow = nrow(bdr),
#'   ncol = ncol(bdr)
#' )]<- NA # Add random NA to simulate a driver with missing values.
#' demo_repaired_driver<-attempt_driver_repair(bdr)
#' Visual_driver(demo_repaired_driver)
#' @export
attempt_driver_repair<-function(na_df){
  #force characters to numeric or NA
  na_df<-na_df %>%
    dplyr::mutate(across(everything(),as.double))

  na_df_dt<-na_df %>% dplyr::select(c(year,jday,hour,min))
  #fix datetime columns
  if(any(is.na(na_df_dt))){
    first_good_y_idx<-which(!is.na(na_df_dt$year))[1]
    first_good_j_idx<-which(!is.na(na_df_dt$jday))[1]
    first_good_h_idx<-which(!is.na(na_df_dt$hour))[1]
    first_non0_min<-which(na_df_dt$min>0)[1]
    if(all(!is.na(na_df_dt[1,]))){
      temp_dr<-BuildDriver(ndays = 365+2*ceiling(nrow(na_df_dt)/48)+1,start_jday =1,year = na_df_dt[1,"year"]-1 ) %>%
        dplyr::select(c(year,jday,hour,min))

      temp_match<-temp_dr%>%
        dplyr::mutate(idx=dplyr::row_number())%>%
        dplyr::filter(
          year==na_df_dt[1,"year"],
          jday==na_df_dt[1,"jday"],
          hour==na_df_dt[1,"hour"],
          min==na_df_dt[1,"min"]
          )
      which_match<-temp_match[1,"idx"]
      na_df_dt<-temp_dr[which_match:nrow(temp_dr),1:4][1:nrow(na_df_dt),]

    }else{
      if(!is.na(first_non0_min)){
        if(first_non0_min%%2){
          na_df_dt$min<-c(0,30)
        }else{
          na_df_dt$min<-c(30,0)
        }
      }else{
        na_df_dt$min<-c(0,30)
      }

      mx_yrs<-ceiling(nrow(na_df_dt)/48/366)
      temp_dr<-BuildDriver(
        ndays = mx_yrs*2*366,
        start_jday =1,
        year = na_df_dt[first_good_y_idx,"year"]-mx_yrs
      ) %>%
        dplyr::select(c(year,jday,hour,min))

      full_line<-which(
        !(
          is.na(na_df_dt$year)&&
            is.na(na_df_dt$jday)&&
            is.na(na_df_dt$hour)&&
            is.na(na_df_dt$min)
        )
      )[1]

      if(full_line>0){
        temp_match<-temp_dr%>%
          dplyr::mutate(idx=dplyr::row_number())%>%
          dplyr::filter(
            year==na_df_dt[full_line,"year"],
            jday==na_df_dt[full_line,"jday"],
            hour==na_df_dt[full_line,"hour"],
            min==na_df_dt[full_line,"min"]
          )
        match_line<-temp_match[1,"idx"]
        na_df_dt<-temp_dr[(match_line-full_line+1):nrow(na_df_dt)]

      }else{

      }
    }
  }

  na_df<-na_df %>% dplyr::select(-c(year,jday,hour,min))

  #fill in for constant columns
  for(i in 1:ncol(na_df)){
    if(all(na.omit(na_df[,i])==na.omit(na_df[,i])[1])){
      na_df[,i]<-na.omit(na_df[,i])[1]
    }
  }
  #fill in for duplicated columns (temperature)
  for(i in 1:ncol(na_df)){
    for(j in (1:ncol(na_df))){
      if(!(i==j)){
        joint_idx<-((!is.na(na_df[,i]))*(!is.na(na_df[,j])))
        if(
          all(na_df[,i][joint_idx]==na_df[,j][joint_idx])&&
          any(na_df[,i][joint_idx]>0)&&any(na_df[,i][joint_idx]>0)
          ){
          na_df[,i][!is.na(na_df[,j])]<-na_df[,j][!is.na(na_df[,j])]
          na_df[,j][!is.na(na_df[,i])]<-na_df[,i][!is.na(na_df[,i])]
        }
      }
    }
  }

  #This is just because precip includes both natural and artificial inputs and therefore cannot be reliably predicted or imputed.
  if(any(is.na(na_df[,"precip"]))){
    na_df[is.na(na_df[,"precip"]),"precip"]<-0.0
  }

  na_rows<-c()
  na_cols<-c()
  for(i in 1:nrow(na_df)){
    if(any(is.na(na_df[i,]))){
      na_rows<-append(na_rows,i)
    }
  }
  for(i in 1:ncol(na_df)){
    if(any(is.na(na_df[,i]))){
      na_cols<-append(na_cols,i)
    }
  }

  ### This is where the actual imputation will occur ###
  # More sophisticated imputations are in the works, but for now we are just using a linear approximation
  for(i in na_cols){
    known_idx<-!is.na(na_df[,i])
    x_val<-seq_along(na_df[,i])
    na_df[,i]<-approx(x_val[known_idx],na_df[known_idx,i],xout=x_val)$y
  }
  rep_df<-cbind(na_df_dt,na_df) %>%
    dplyr::mutate(across(everything(),as.double))
  return(rep_df)
}

### Tools for repairing parameter files. Not for general application. ###

#' Tokenizer
#' @param text_string Character string or vector to be broken into tokens
#' @param token_length Optional size if token chunks
#' @return Vector of tokens.
#' @examples
#' tiny_tokenizer(
#'   text_string = "Some example text",
#'   token_length = 3
#' )
#' @export
tiny_tokenizer<-function(
    text_string,
    token_length=3
    ){
  if(!is.character(text_string)){
    if(!is.character(text_string[1])){
      stop(paste("Incorrect inputs to Tokenizer",paste(text_string,collapse = "/n")))
    }
  }
  alpha_num<-gsub("[^a-zA-Z0-9]", " ", tolower(text_string))
  if(is.na(alpha_num)){
    stop("NA passed to Tokenizer")
  }
  alpha_num_single_spaced <-gsub("\\s{2,}", " ", alpha_num)
  alpha_num_single_spaced_split<-strsplit(x = alpha_num_single_spaced,split = " ")
  subset_tiny_tokens<-vector()
  for(i in 1:length(alpha_num_single_spaced_split)){
    subset_split<-alpha_num_single_spaced_split[[i]]
    for(wrd in subset_split){
      if(nchar(wrd)<(token_length+1)){
        subset_tiny_tokens<-append(x = subset_tiny_tokens,values = wrd)
      }else{
        sub_wrds<-vector()
        pattern_split <- paste0('(?<=.{',token_length,'})')
        sub_wrds<-unlist(strsplit(wrd, pattern_split, perl=TRUE))
        if(length(sub_wrds)>2){
          sub_wrds[1]<-paste0(sub_wrds[1],"##")
          sub_wrds[2:(length(sub_wrds)-1)]<-paste0("##",sub_wrds[2:(length(sub_wrds)-1)],"##")
          sub_wrds[length(sub_wrds)]<-paste0("##",sub_wrds[length(sub_wrds)])
        }else{
          sub_wrds[1]<-paste0(sub_wrds[1],"##")
          sub_wrds[length(sub_wrds)]<-paste0("##",sub_wrds[length(sub_wrds)])
        }

        subset_tiny_tokens<-append(x = subset_tiny_tokens,values = sub_wrds)
      }
    }
  }
  return(subset_tiny_tokens)
}

#' Cosine Similarity ranks the similarity of text.
#' @param text1 first string or vector of strings to compare
#' @param text2 second
#' @param token_length Optional size if token chunks
#' @param relative_corpus_table Optional table of all possible values for weighing uniqueness of tokens
#' @return cosine similarity of text1 and text2
#' @examples
#' cosine_sim(
#'  text1="the root carbon",
#'  text2="root nitrogen",
#'  token_length=6,
#'  relative_corpus_table=table(tiny_tokenizer("A collection of text to represent the expected
#'   distribution of tokens before evaluating
#'   the text of interest to avoid over weighing
#'    common tokens such as articles."))
#' )
#' @export
cosine_sim<-function(
    text1,
    text2,
    token_length=3,
    relative_corpus_table=NA
){
  if(any(is.na(c(text1,text2)))){
    message(text1)
    message(text2)
    stop("NAs found in inputs")
  }
  tkns1<-tiny_tokenizer(text1,token_length = token_length)
  tkns2<-tiny_tokenizer(text2,token_length = token_length)
  jtkns<-unique(c(tkns1,tkns2))
  if(any(is.na(relative_corpus_table))){
    numer_t<-0
    denom_a<-0
    denom_b<-0
    for(i in 1:length(jtkns)){
      A_i<-(sum(jtkns[i]==tkns1) )
      B_i<-(sum(jtkns[i]==tkns2) )
      numer_t<-numer_t+(A_i*B_i)
      denom_a<-denom_a+(A_i)^2
      denom_b<-denom_b+(B_i)^2
    }
    cos_sim<-numer_t/((denom_a^(0.5))*(denom_b^(0.5)))
  }else{
    numer_t<-0
    denom_a<-0
    denom_b<-0
    freq_table<-relative_corpus_table
    for(t1 in 1:length(tkns1)){
      if(is.na(freq_table[tkns1[t1]])){freq_table[tkns1[t1]]<-0}
    }
    for(t2 in 1:length(tkns2)){
      if(is.na(freq_table[tkns2[t2]])){freq_table[tkns2[t2]]<-0}
    }
    tb1<-table(tkns1)
    tb2<-table(tkns2)
    freq_table[names(tb1)]<-freq_table[names(tb1)]+tb1
    freq_table[names(tb2)]<-freq_table[names(tb2)]+tb2

    freq_jtkns<-as.vector(freq_table[jtkns])
    for(i in 1:length(jtkns)){
      rel_f<-freq_jtkns[i]
      cur_jtkn<-jtkns[i]
      A_i<-(sum(cur_jtkn==tkns1)/rel_f)
      B_i<-(sum(cur_jtkn==tkns2)/rel_f)
      numer_t<-numer_t+(A_i*B_i)
      denom_a<-denom_a+(A_i)^2
      denom_b<-denom_b+(B_i)^2
    }
    cos_sim<-numer_t/((denom_a^(0.5))*(denom_b^(0.5)))
  }

  return(cos_sim)
}

#' Repair Parameters
#' @importFrom tidyr unite
#' @import dplyr
#' @import progressr
#' @param params_in Data frame for broken TREES parameters.
#' @return list containing attempted repairs and error count.
#' @examples
#' demo_repair_p<-attempt_p_repair(
#'  params_in = ExampleParameters()[
#'  sample(1:174,170,replace = FALSE),
#'  ]#example of missing rows in an incorrect order
#' )
#' @export
attempt_p_repair <- function(
    # real_params,
    params_in
) {
  real_params<-ExampleParameters()
  message("Attempting input parameter repairs.")
  new_vp<-real_params %>%
    dplyr::mutate(score=00,id_match=0)
  if("Notes"%in%names(new_vp)){
    new_vp<-new_vp %>%
      dplyr::mutate(combo=paste(Parameter_Name,Notes))
  }else{
    new_vp<-new_vp %>%
      dplyr::mutate(combo=Parameter_Name)
  }
  if(ncol(params_in)>1){
    params_in<-params_in %>%
      tidyr::unite("combo",!Value)
  }else{
      stop("No parameter names or notes included.")
    }

  all_possible<-paste(params_in$combo,new_vp$combo,collapse = " ")
  tkn_length<-3
  all_possible_table<-table(tiny_tokenizer(all_possible,token_length = tkn_length))
  nr_nvp<-nrow(new_vp)
  new_vp_out<-new_vp %>%
    dplyr::mutate(
      id_match=0,
      score=0
    )

  p<-function(...){""}
  progressbar=!isTRUE(getOption("rstudio.notebook.executing")) #I might make this an arg
  if(progressbar){
    tryCatch(
      {
        progressr::handlers(global = TRUE)
        p <- progressr::progressor(steps =as.integer(nr_nvp*nrow(params_in)))
      }
    )
  }
  for(comp in 1:nr_nvp){
    scores<-vector()
    for(sc in 1:nrow(params_in)){
      p()
      # message(paste(params_in[sc,"combo"],
      #               new_vp[comp,"combo"]))
      scores<-append(
        scores,
        abs(
          1-cosine_sim(
            params_in[sc ,"combo"],
            new_vp[comp,"combo"],
            tkn_length,
            relative_corpus_table =all_possible_table
          )
        )
      )
    }
    new_vp_out[comp,"id_match"]<-which.min(scores)
    new_vp_out[comp,"score"]<-min(scores)
  }
  new_vp<-new_vp_out %>%
    dplyr::mutate(
      dups=(duplicated(id_match)|duplicated(id_match,fromLast = TRUE))
      ) %>%
    dplyr::mutate(weak=ifelse(score>0.8,TRUE,FALSE))
  dups_names<-new_vp[new_vp$dups,"Parameter_Name"]
  weak_names<-new_vp[new_vp$weak,"Parameter_Name"]
  warning(paste("The following parameters had duplicate assignments or weak matches. They will be set to NA and need manual repairs:\n","\n>Duplicates:\n",paste("\t",dups_names,collapse = "\n"),"\n\n>Weak matches:\n",paste("\t",weak_names,collapse="\n")),immediate. = TRUE)
  new_vp[,"Value"]<-NA
  for(fn in 1:nrow(new_vp)){
    if(new_vp[fn,"dups"]|new_vp[fn,"weak"]){
      new_vp[fn,"Value"]<-NA
    }else{
      new_vp[fn,"Value"]<-params_in[new_vp[fn,"id_match"],1]
    }
  }
  return(list(new_vp,issues=sum(new_vp$dups)+sum(new_vp$weak)))
}

#' Validate Parameters and attempt repairs
#' @param parameters Data.frame or file path for TREES parameters in need of validation.
#' @return data frame of repaired parameters.
#' @examples
#' demo_p<-ExampleParameters()
#' demo_good_p_valid<-ValidateParameters(demo_p)
#' demo_p_bad<-demo_p
#' demo_p_bad[matrix(
#'   sample(x = c(TRUE,FALSE),
#'          size = nrow(demo_p_bad)*ncol(demo_p_bad),
#'          prob = c(0.01,0.99),
#'          replace = TRUE
#'   ),
#'   nrow = nrow(demo_p_bad),
#'   ncol = ncol(demo_p_bad)
#' )]<-NA
#' demo_p_bad<-demo_p_bad[sample(1:174,170,replace = FALSE),]
#' demo_bad_p_valid<-ValidateParameters(demo_p_bad)
#' @export
ValidateParameters<-function(
    parameters="param.p"
){
  issues<-0

  if(is.character(parameters)){
    parameters_in<-ReformRawP(
      dirty_vector = readLines(
        con = parameters))

  }else if(is.data.frame(parameters)){
    if("Value" %notin% names(parameters)){
      warning("No column with name 'Value' found. Attempting to use column 1 as Value",immediate. = TRUE)
    }
    if(ncol(parameters)>1){
      parameters_in<-parameters[,1:2]
      names(parameters_in)<-c("Value","Notes")
    }else{
      warning("Only one column provided, will not be able to repair if anything is missing",immediate. = TRUE)
      parameters_in<-cbind(parameters,NA)
      names(parameters_in)<-c("Value","Notes")
      issues<-issues+1
    }
    issues<-issues+1
  }
  all_params<-rTREES::ExampleParameters()

  if(nrow(parameters_in)==nrow(all_params)){
    parameters_in<-cbind(
      Value=parameters_in$Value,
      all_params[,2:5],
      original_notes=parameters_in$Notes
    )
  }else{
    warning("WARNING: the supplied parameter file is missing rows. It is likely from an older version of TREES. Attempting to repair using cosine similarity. Missing parameters will be set to NA",immediate. = TRUE)
    repair_res<-attempt_p_repair(params_in=parameters_in )
    issues<-issues+repair_res[[2]]
    parameters_in=repair_res[[1]]
  }
  final_warns<-c("")
  parameters_in[,1]<-as.numeric(parameters_in[,1])
  for(i in 1:nrow(parameters_in)){

    if( is.na(parameters_in[i, 1]) ){

      final_warns<-c(final_warns,paste("\t",parameters_in[i,2], "is missing a value", sep = " "))
      issues<-issues+1

    } else if(!is.finite(parameters_in[i, 1])) {
      final_warns<-c(final_warns,paste("\t",
        parameters_in[i,2],
        "is not numeric or finite.",
        sep = " "
      ))
      issues<-issues+1

    }else if(
      parameters_in[i, 1]<parameters_in[i, "Lower_range"]|
      parameters_in[i, 1]>parameters_in[i, "Upper_range"]
      ){
      final_warns<-c(final_warns,paste("\t",
        parameters_in[i,"Parameter_Name"],
        "is outside the expected range.",
        sep = " "
      ))
      issues<-issues+1
    }
  }
  if(length(final_warns>1)){
    warning(paste(final_warns,sep="  \n",collapse = "  \n"),immediate. = TRUE)
  }
  message(paste("Parameter validation complete,",issues,"possible issues identified."))
  return(parameters_in)
}

#' Validate root_shoot_soil
#' @param root_shoot_soil List for TREES root, shoot, and soil texture by layer parameters in need of validation.
#' @return None, prints out findings.
#' ValidateRootShootSoil(
#' root_shoot_soil = ExampleRSS()
#' )
#' @export
ValidateRootShootSoil<-function(
    root_shoot_soil
){

  #canopy_cover should be within 25% of 1
  #uses lat root of first root layer and comparing to lat branch to determine canopy cover for scalar between indv plant and 1ha footprint

  issues<-0
  if(inherits(root_shoot_soil,"list")){
    rss_in<-CleanRootShootParams(root_shoot_soil = root_shoot_soil)
  }else if(is.character(root_shoot_soil)){
    rss_in<-CleanRootShootParams(root_shoot_soil = root_shoot_soil)
  }else{
    stop("root_shoot_soil must be formatted as a list or be a string")
  }

  if(any((rss_in$layer_sand_fraction+rss_in$layer_clay_fraction)>1.0)){
    issues<-issues+1
  }

  if(
    (
      length(rss_in$ar)+
      length(rss_in$drlat)+
      length(rss_in$drax)+
      length(rss_in$layer_GMP)+
      length(rss_in$layer_GSD)+
      length(rss_in$layer_BD)+
      length(rss_in$layer_porosity)+
      length(rss_in$layer_sand_fraction)+
      length(rss_in$layer_clay_fraction)+
      length(rss_in$layer_residual_water_content)+
      length(rss_in$layer_initial_water_content)
    )>
    11*rss_in$rmodules
    ){
    issues<-issues+abs(
      length(rss_in$ar)+
      length(rss_in$drlat)+
      length(rss_in$drax)+
      length(rss_in$layer_GMP)+
      length(rss_in$layer_GSD)+
      length(rss_in$layer_BD)+
      length(rss_in$layer_porosity)+
      length(rss_in$layer_sand_fraction)+
      length(rss_in$layer_clay_fraction)+
      length(rss_in$layer_residual_water_content)+
      length(rss_in$layer_initial_water_content)-
      11*rss_in$rmodules
      )
    message("Error: root_shoot_soil soil layer input incorrect dimensions.")

  }
#
  if(
    length(rss_in$al)+
    length(rss_in$dslat)+
    length(rss_in$dsax)>
    3*rss_in$smodules
  ){
    issues<-issues+abs(
      length(rss_in$al)+
        length(rss_in$dslat)+
        length(rss_in$dsax)-
        3*rss_in$smodules
    )
  }
  for(i in 1:length(rss_in)){

    for(j in 1:length(rss_in[[i]])){
      if(
        !is.numeric(rss_in[[i]][j])||
        !is.finite(rss_in[[i]][j]) ||
        rss_in[[i]][j]>30 ||
        rss_in[[i]][j]<0
        ){
        issues<-issues+1
      }
    }
  }
  #stem length x lateral stem length = sum root axial lengths x first root layer lateral length
  t1<-rss_in$dslat*rss_in$dsax
  t2<-sum(rss_in$drax)*rss_in$drlat[1]

  #If the stem axial length times the lateral length is more than 25% different than the sum of the root axial lengths times the first root layers lateral length, then add an issue.
  if(abs(t1-t2)>0.25*t1){
    #t1>=(t2*1.25)|ts1>=(ts*0.75) I don't recall what ts was
    message("Warning! Inputs typically conform to: stem_length * lateral_stem_length ~= sum(root_axial_lengths) * first_root_layer_lateral_length")
    issues<-issues+1
  }
  message(paste("Validation complete,",issues,"possible issues identified."))
  return(issues)
}

#' Example Root, Shoot, and Soil layer/module parameters
#' @param file If TRUE then a character string that can be saved to a text file will be produced.
#' @return List of valid Root, Shoot, and Soil layer/module parameters
#' @export
ExampleRSS<-function(
    file=FALSE
    ){
  ex_rss <- list(
    smodules = 1,
    rmodules = 4,
    al = 1,
    dslat = 0.3556,
    dsax = 0.2,
    ar = c(0.2, 0.3, 0.3, 0.2),
    drlat = c(0.8, 0.7, 0.6, 0.5),
    drax = c(0.02, 0.02, 0.0289, 0.02),
    layer_GMP = c(0.2, 0.2, 0.2, 0.2),
    layer_GSD = c(10, 10, 10, 10),
    layer_BD = c(1.5, 1.5, 1.5, 1.5),
    layer_porosity = c(0.3, 0.3, 0.3, 0.3),
    layer_sand_fraction = c(0.65, 0.65, 0.65, 0.65),
    layer_clay_fraction = c(0.1, 0.1, 0.1, 0.1),
    layer_residual_water_content = c(0.12, 0.12, 0.12, 0.12),
    layer_initial_water_content = c(0.27, 0.27, 0.28, 0.29)
  )
  if(file){
    ex_rss<-"#_of_shoot_modules 1
leaf_area_fraction 1
length_lateral    0.48
length_axial      0.2
#_of_root_modules  4
leaf_area_fraction 0.2
length_lateral     0.8
length_axial       0.02
geometric_mean_particle_diameter 0.2
geometric_standard_deviation_of_particle_size 10.0
soil_bulk_density 1.5
porosity 0.3
sand_fraction 0.65
clay_fraction 0.1
residual_water_content 0.12
initial_water_content 0.27
leaf_area_fraction 0.3
length_lateral     0.7
length_axial       0.02
geometric_mean_particle_diameter 0.2
geometric_standard_deviation_of_particle_size 10.0
soil_bulk_density 1.5
porosity 0.3
sand_fraction 0.65
clay_fraction 0.1
residual_water_content 0.12
initial_water_content 0.27
leaf_area_fraction 0.3
length_lateral     0.6
length_axial       0.0289
geometric_mean_particle_diameter 0.2
geometric_standard_deviation_of_particle_size 10.0
soil_bulk_density 1.5
porosity 0.3
sand_fraction 0.65
clay_fraction 0.1
residual_water_content 0.12
initial_water_content 0.28
leaf_area_fraction 0.2
length_lateral     0.5
length_axial       0.02
geometric_mean_particle_diameter 0.2
geometric_standard_deviation_of_particle_size 10.0
soil_bulk_density 1.5
porosity 0.3
sand_fraction 0.65
clay_fraction 0.1
residual_water_content 0.12
initial_water_content 0.29"
  }

  return(ex_rss)
}


#' Build Driver
#' @param  lat Latitude
#' @param  long Longitude
#' @param  alt Altitude
#' @param  ndays Number of days to run the driver
#' @param  year Year
#' @param  start_jday =1,
#' @param  dewpoint = 12,
#' @param  co2_atm =410.5,
#' @param  wind_speed =1.01,
#' @param  precipdays = c(20, 30, 50, 80, 90, 110, 140, 150, 200, 210, 290)
#' @param  precip_amount =6.25 how much precip occurs in a precip day
#' @param temp_scalar Scales the tempertature vectors
#' @return Data frame of a valid driver
#' @examples
#' demo_driver<-BuildDriver(
#'   lat = 35.818,
#'   long = -106.305,
#'   alt = 2150.0,
#'   ndays = 50,
#'   year = 2023,
#'   start_jday=1,
#'   dewpoint = 12,
#'   co2_atm=410.5,
#'   wind_speed=1.01,
#'   precipdays = c(20, 30, 50, 80, 90, 110, 140, 150, 200, 210, 290),
#'   precip_amount = 6.25,
#'   temp_scalar=1
#' )
#' @export
BuildDriver<-function(
    lat = 35.818,
    long = -106.305,
    alt = 2150.0,
    ndays = 365,
    year = 2023,
    start_jday=1,
    dewpoint = 12,
    co2_atm=410.5,
    wind_speed=1.01,
    precipdays = c(5, 30, 50, 80, 90, 110, 140, 150, 200, 210, 290),
    precip_amount = 6.25,
    temp_scalar=1
){
  dy<-start_jday
  nsteps <- 48
  maxDewpoint <- 20
  dewpointScalar <- 0.5


  calc_zenith <- function(jday, time, lat, long)
  {
    corr <- long_corr(lat, long)
    temp <- deg2rad(279.575 + (0.9856*jday))
    corr <- corr + (-104.7*sin(temp) +596.2*sin(2.0*temp) +4.3*sin(3.0*temp) -12.7*sin(4.0*temp)
                    -429.3*cos(temp) -2.0*cos(2.0*temp) +19.3*cos(3*temp))/3600.0
    solnoon <- 12.0 - corr
    alat <- deg2rad(lat)
    temp <- deg2rad(278.97 + 0.9856*jday + 1.9165*sin(deg2rad(356.6+0.9856*jday)))
    declin <- asin(0.39785*sin(temp))

    ztemp <- sin(alat)*sin(declin) + cos(alat)*cos(declin)*cos(deg2rad(15.0*(time - solnoon)))
    zenith <- acos(ztemp)
    return (zenith)

  }

  #
  #adjust for the standard meridian for the local time zone
  #
  long_corr <- function(lat, long)
  {
    alat <- abs(lat)
    along <- abs(long)
    cen_meridian <- 15.0*floor((along + 7.5)/15.0)
    correction <- (cen_meridian - along) / 15.0
    return(correction)
  }


  deg2rad <- function(deg)
  {
    PI <- 3.14159265358979323846
    rad <- deg * PI / 180.0
    return(rad)
  }

  #Qo is Wm-2
  calc_Qo <- function(jday, time, lat, long)
  {
    Ssc <- 1370 * 2100 / 2740 #Wm-2
    zenith <- calc_zenith(jday, time, lat, long)
    MPI2 <- 1.57079632679489661923
    elev <- MPI2 - zenith
    Qo <- Ssc * sin(elev) * (1.0 + 0.033*cos(deg2rad(360.0*jday/365.0)))
    return(Qo)
  }

  calc_ppfd <- function(jday, time, lat, long)
  {
    Qo <- calc_Qo(jday, time, lat, long)
    ppfd <- Qo / 2.12766 / 0.235
    return(ppfd)
  }
  is_leap_year <- function(year) {
    return(year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0))
  }
  ppfdVector <- rep(0,ndays*nsteps)
  yearVector<-rep(year,ndays*nsteps)
  dayVector <- rep(0,ndays*nsteps)
  timeVector <- rep(0,ndays*nsteps)
  precipVector <- rep(0,ndays*nsteps)
  tempVector <- rep(0,ndays*nsteps)
  VPDVector <- rep(0,ndays*nsteps)

  driver_names<-c(      "year",
                        "jday",
                        "hour",
                        "min",
                        "u_ref" ,
                        "t_ref",
                        "D_ref",
                        "precip"  ,
                        "Qpar",
                        "t_canopy",
                        "D_canopy",
                        "p_atm",
                        "CO2_atm",
                        "t_surface",
                        "t_soil" ,
                        "t_root",
                        "Zw",
                        "xylem_capacity_multiplier",
                        "NEEobs",
                        "Ecobs")
  driver_df <- data.frame(matrix(data=0,nrow=ndays*nsteps,ncol=length(driver_names)))
  names(driver_df)<-driver_names

  hours<-rep(floor(seq(from=0,to=23.5,by=0.5)),ndays)
  mins<-rep(rep(c(0,30),24),ndays)

  for (i in 1:length(ppfdVector))
  {
    hr <- hours[i]+mins[i]/60
    ppfd <- calc_ppfd(dy, hr, lat, long)
    if (ppfd < 0) ppfd <- 0
    if (dy %in% precipdays && (hr == 5 || hr == 6))
    {
      precipVector[i] = precip_amount
    }
    ppfdVector[i] <- ppfd
    dayVector[i] <- dy
    yearVector[i]<-year
    timeVector[i] <- hr
    ppfd12 <- calc_ppfd(dy, 12, lat, long)

    dewpoint <- maxDewpoint*(ppfd12/2200)
    tempVector[i] <- sin(0.5*pi*ppfd/2200)*25*(1-dewpointScalar*dewpoint/maxDewpoint)*(ppfd12/2200)+dewpoint
    avp <- 0.611*exp(17.3*dewpoint/(dewpoint+238.3))
    svp <- 0.611*exp(17.3*tempVector[i]/(tempVector[i]+238.3))
    VPDVector[i] <- max(0,svp-avp)
    if (i %% nsteps == 0){
      dy <- dy + 1
      if(dy>365 ){
        if(!is_leap_year(year)|dy==367){
          year<-year+1
          dy<-1
        }
      }else if(dy>367|dy<1){
        stop(
          "Unauthorized attempt to alter the orbital period of the planet detected!"
        )
      }
    }
  }
  tempVector<-tempVector*temp_scalar
  driver_df[ ,"year"] <- yearVector
  driver_df[ ,"jday"]<-dayVector
  driver_df[ ,"hour"] <-hours
  driver_df[ ,"min"] <- mins
  driver_df[ ,"u_ref"] <- wind_speed
  driver_df[ ,"t_ref"] <- tempVector
  driver_df[ ,"D_ref"] <- VPDVector
  driver_df[ ,"precip"] <- precipVector
  driver_df[ ,"Qpar"] <- ppfdVector
  driver_df[ , "t_canopy"] <- tempVector#driver_df[ ,4]
  driver_df[ ,"D_canopy"] <- driver_df[ ,5]
  driver_df[ ,"p_atm"] <- 101.3*exp(-(alt/8200.0))
  driver_df[ , "CO2_atm"] <- co2_atm
  driver_df[ , "t_surface"] <- tempVector#driver_df[ ,4]
  driver_df[ ,"t_soil"] <- tempVector#driver_df[ ,4]
  driver_df[ , "t_root"] <-tempVector# driver_df[ ,4]
  driver_df[ , "Zw"] <- 7.5
  driver_df[ ,"xylem_capacity_multiplier"] <- 1.0
  driver_df[ ,"NEEobs"] <- -999
  driver_df[ ,"Ecobs"] <- -999
  return(driver_df)
}


#' Unit expression to help with plotting
#' @param variable_name The name of the variable you want the expression for. Currently only supports sim_out variables.
#' @return object of class expression for labels in ggplot axis.
#' @export
Var_unit<-function(
    variable_name
){
  Unit_vector<-c(
    simET=as.expression(bquote("Evapotranspiration mm "~s^-1)),
    WPlant_K=as.expression(bquote("Plant hydraulic conductance mmol"~m^-2~s^-1~MPa^-1)),
    Soil_Psi=as.expression(bquote(Psi[Soil]~"(MPa)" )),
    Leaf_Psi=as.expression(bquote(Psi[Leaf]~"(MPa)" )),
    Psi_Crit=as.expression(bquote(Psi[Crit]~"(MPa)" )),
    Ecrit=as.expression(bquote("Critical transpiration mmol"~m^-2~s^-1)),
    Ec=as.expression(bquote("Transpiration mmol"~m^-2~s^-1)),
    RhizFlux0=as.expression(bquote("Rhizosphere flux 1 mmol"~m^-2~s^-1)),
    RhizFlux1=as.expression(bquote("Rhizosphere flux 2 mmol"~m^-2~s^-1)),
    RhizFlux2=as.expression(bquote("Rhizosphere flux 3 mmol"~m^-2~s^-1)),
    RhizFlux3=as.expression(bquote("Rhizosphere flux 4 mmol"~m^-2~s^-1)),
    Gs=as.expression(bquote("Stomatal conductance mol"~m^-2~s^-1)),
    LAI=as.expression(bquote("LAI ("~m^2 ~m^-2~")" )),
    SLA=as.expression(bquote("SLA ("~m^2 ~kgC^-1~")" )),
    liveLAI=as.expression(bquote("Live LAI ("~m^2 ~m^-2~")" )),
    Rmaint=as.expression(bquote("Maintenance respiration ("~kgC~ha^-1~")" )),
    Rgrowth=as.expression(bquote("Growth respiration ("~kgC~ha^-1~")" )),
    reproduction=as.expression(bquote("Reproduction (gC "~m^-2 ~")" )),
    leafNSC=as.expression(bquote("Leaf Non-structural carbon ("~kgC~ha^-1~")" )),
    stemNSC=as.expression(bquote("Stem Non-structural carbon ("~kgC~ha^-1~")" )),
    rootNSC=as.expression(bquote("Root Non-structural carbon ("~kgC~ha^-1~")" )),
    chloroStarch=as.expression(bquote("Chloro starch ("~kgC~ha^-1~")" )),
    chloroSugar=as.expression(bquote("Chloro sugar ("~kgC~ha^-1~")" )),
    waterStress=as.expression(bquote("Water Stress")),
    litterH2O=as.expression(bquote("Litter water ("~m^3 ~m^-3~")")),
    theta0=as.expression(bquote("Layer 0 soil water ("~m^3 ~m^-3~")")),
    theta1=as.expression(bquote("Layer 1 soil water ("~m^3 ~m^-3~")")),
    theta2=as.expression(bquote("Layer 2 soil water ("~m^3 ~m^-3~")")),
    theta3=as.expression(bquote("Layer 3 soil water ("~m^3 ~m^-3~")")),
    thetaRoot=as.expression(bquote("Average root water ("~m^3 ~m^-3~")")),
    Can_Evap=as.expression(bquote("Free evap from wet canopy"~mm~s^-1)),
    Snowpack="Snow pack m",
    SnowEdef=as.expression(bquote("Snow energy deficit"~"°"~C^-1)),
    Vcmax25=as.expression(bquote("Maximum carboxylation at 25 °C (μmol"~m^-2~s^-1~")")),
    Vcmax_sun=as.expression(bquote("Leaf level Vcmax(sun) (μmol"~m^-2~s^-1~")")),
    Vcmax_shd=as.expression(bquote("Leaf level Vcmax(shade) (μmol"~m^-2~s^-1~")")),
    Jmax25=as.expression(bquote("Maximum J at 25 °C (μmol"~m^-2~s^-1~")")),
    J_sun=as.expression(bquote("Leaf level J(sun) (μmol"~m^-2~s^-1~")")),
    J_shd=as.expression(bquote("Leaf level J(sun) (μmol"~m^-2~s^-1~")")),
    Asun=as.expression(bquote("Leaf level photosynthesis(sun) (μmol"~m^-2~s^-1~")")),
    Ashd=as.expression(bquote("Leaf level photosynthesis(shade) (μmol"~m^-2~s^-1~")")),
    Lsun=as.expression(bquote("Leaf level area(sun) ("~m^2 ~m^-2~")")),
    Lshd=as.expression(bquote("Leaf level area(shade) ("~m^2 ~m^-2~")")),
    Tsun=as.expression(bquote("Leaf level temperature (°C)")),
    Tshd=as.expression(bquote("Leaf level temperature (°C)")),
    Dsun=as.expression(bquote("Leaf(sun) VPD (kPa)")),
    Dshd=as.expression(bquote("Leaf(sun) VPD (kPa)")),
    Ci_sun=as.expression(bquote("Leaf level intercellular"~CO^2~"(sun) (ppm)")),
    Ci_shd=as.expression(bquote("Leaf level intercellular"~CO^2~"(sun) (ppm)")),
    PARsun=as.expression(bquote("Leaf level absorbed PAR(sun) (μmol"~m^-2~s^-1~")")),
    PARshd=as.expression(bquote("Leaf level absorbed PAR(shade) (μmol"~m^-2~s^-1~")")),
    gs_sun=as.expression(bquote("Leaf level stomatal conductance(sun) (mol"~m^-2~s^-1~")")),
    gs_shd=as.expression(bquote("Leaf level stomatal conductance(sun) (mol"~m^-2~s^-1~")")),
    NEE=as.expression(bquote("Net ecosystem exchange (μmol"~m^-2~s^-1~")")),
    NPP=as.expression(bquote("Net primary production (μmol"~m^-2~s^-1~")")),
    R_total=as.expression(bquote("Total respiration (μmol"~m^-2~s^-1~")")),
    R_ag=as.expression(bquote("Above ground respiration (μmol"~m^-2~s^-1~")")),
    R_bg=as.expression(bquote("Below ground respiration (μmol"~m^-2~s^-1~")")),
    Rd_sun=as.expression(bquote("Dark respiration(sun) (μmol"~m^-2~s^-1~")")),
    Rd_shd=as.expression(bquote("Dark respiration(shade) (μmol"~m^-2~s^-1~")")),
    Csapwood=as.expression(bquote("Stem carbon ("~kgC~ha^-1~")")),
    FibRootC0=as.expression(bquote("Root order 1 carbon content layer 0("~kgC~ha^-1~")")),
    FibRootC1=as.expression(bquote("Root order 1 carbon content layer 1("~kgC~ha^-1~")")),
    FibRootC2=as.expression(bquote("Root order 1 carbon content layer 2("~kgC~ha^-1~")")),
    FibRootC3=as.expression(bquote("Root order 1 carbon content layer 3("~kgC~ha^-1~")")),
    FineRootC0=as.expression(bquote("Root order 2 carbon content layer 0("~kgC~ha^-1~")")),
    FineRootC1=as.expression(bquote("Root order 2 carbon content layer 1("~kgC~ha^-1~")")),
    FineRootC2=as.expression(bquote("Root order 2 carbon content layer 2("~kgC~ha^-1~")")),
    FineRootC3=as.expression(bquote("Root order 2 carbon content layer 3("~kgC~ha^-1~")")),
    TotRootC0=as.expression(bquote("Total root carbon content layer 0("~kgC~ha^-1~")")),
    TotRootC1=as.expression(bquote("Total root carbon content layer 1("~kgC~ha^-1~")")),
    TotRootC2=as.expression(bquote("Total root carbon content layer 2("~kgC~ha^-1~")")),
    TotRootC3=as.expression(bquote("Total root carbon content layer 3("~kgC~ha^-1~")")),
    FineRootCN0=as.expression(bquote("Fine root CN layer 0("~kgC~kgN^-1~")")),
    FineRootCN1=as.expression(bquote("Fine root CN layer 1("~kgC~kgN^-1~")")),
    FineRootCN2=as.expression(bquote("Fine root CN layer 2("~kgC~kgN^-1~")")),
    FineRootCN3=as.expression(bquote("Fine root CN layer 3("~kgC~kgN^-1~")")),
    LeafCN=as.expression(bquote("Leaf CN ("~kgC~kgN^-1~")")),
    humusC0=as.expression(bquote("Humus carbon content layer 0("~kgC~ha^-1~")")),
    humusC1=as.expression(bquote("Humus carbon content layer 1("~kgC~ha^-1~")")),
    humusC2=as.expression(bquote("Humus carbon content layer 2("~kgC~ha^-1~")")),
    humusC3=as.expression(bquote("Humus carbon content layer 3("~kgC~ha^-1~")")),
    RhizCl0=as.expression(bquote("Labile organic carbon content layer 0("~kgC~ha^-1~")")),
    RhizCl1=as.expression(bquote("Labile organic carbon content layer 1("~kgC~ha^-1~")")),
    RhizCl2=as.expression(bquote("Labile organic carbon content layer 2("~kgC~ha^-1~")")),
    RhizCl3=as.expression(bquote("Labile organic carbon content layer 3("~kgC~ha^-1~")")),
    RhizNl0=as.expression(bquote("Labile organic nitrogen content layer 0("~kgN~ha^-1~")")),
    RhizNl1=as.expression(bquote("Labile organic nitrogen content layer 1("~kgN~ha^-1~")")),
    RhizNl2=as.expression(bquote("Labile organic nitrogen content layer 2("~kgN~ha^-1~")")),
    RhizNl3=as.expression(bquote("Labile organic nitrogen content layer 3("~kgN~ha^-1~")")),
    AAexudateC0=as.expression(bquote("Amino acid content layer 0("~kgC~ha^-1~")")),
    AAexudateC1=as.expression(bquote("Amino acid content layer 1("~kgC~ha^-1~")")),
    AAexudateC2=as.expression(bquote("Amino acid content layer 2("~kgC~ha^-1~")")),
    AAexudateC3=as.expression(bquote("Amino acid content layer 3("~kgC~ha^-1~")")),
    SugarExudateC0=as.expression(bquote("Sugar exudate content layer 0("~kgC~ha^-1~")")),
    SugarExudateC1=as.expression(bquote("Sugar exudate content layer 1("~kgC~ha^-1~")")),
    SugarExudateC2=as.expression(bquote("Sugar exudate content layer 2("~kgC~ha^-1~")")),
    SugarExudateC3=as.expression(bquote("Sugar exudate content layer 3("~kgC~ha^-1~")")),
    MicrobC0=as.expression(bquote("Live microbial carbon layer 0("~kgC~ha^-1~")")),
    MicrobC1=as.expression(bquote("Live microbial carbon layer 1("~kgC~ha^-1~")")),
    MicrobC2=as.expression(bquote("Live microbial carbon layer 2("~kgC~ha^-1~")")),
    MicrobC3=as.expression(bquote("Live microbial carbon layer 3("~kgC~ha^-1~")")),
    MicrobN0=as.expression(bquote("Live microbial nitrogen layer 0("~kgN~ha^-1~")")),
    MicrobN1=as.expression(bquote("Live microbial nitrogen layer 1("~kgN~ha^-1~")")),
    MicrobN2=as.expression(bquote("Live microbial nitrogen layer 2("~kgN~ha^-1~")")),
    MicrobN3=as.expression(bquote("Live microbial nitrogen layer 3("~kgN~ha^-1~")")),
    "RhizN-"=as.expression(bquote("Rhizosphere nitrate content("~kgN~ha^-1~")")),
    "RhizN+"=as.expression(bquote("Rhizosphere ammonium content("~kgN~ha^-1~")")),
    PlantN=as.expression(bquote("Total plant nitrogen("~kgN~ha^-1~")")),
    PlantNstat=as.expression(bquote("Plant N status")),
    RLA=as.expression(bquote("Root to leaf area ("~m^2[root]~m^-2[leaf]~")")),
    ar0=as.expression(bquote("Fraction of root area in layer 0 ("~m^2[rootlayer]~m^-2[totalroot])),
    ar1=as.expression(bquote("Fraction of root area in layer 1 ("~m^2[rootlayer]~m^-2[totalroot])),
    ar2=as.expression(bquote("Fraction of root area in layer 2 ("~m^2[rootlayer]~m^-2[totalroot])),
    ar3=as.expression(bquote("Fraction of root area in layer 3 ("~m^2[rootlayer]~m^-2[totalroot]))
  )

  which_unit<-Unit_vector[variable_name]

  return(which_unit)
}


#' Clean rTREES output
#' @param rTREES_output raw rTREES list output from a single simulation.
#' @return A list of 3 data frames corresponding to the original TREES outputs of sim,hyd, and leaf.
#' @examples
#' demo_standard_run<-rTREES(
#'   env_driver = BuildDriver(ndays = 10),
#'   base_parameters = ExampleParameters(),
#'   root_shoot_soil = ExampleRSS()
#' )
#' demo_cleaned_results<-Clean_rTREES_output(
#'   rTREES_output=demo_standard_run
#'
#' )
#' @export
Clean_rTREES_output<-function(
    rTREES_output=list()#,
    # driver=data.frame()
){
  if(length(rTREES_output$sim_out$year)>2){

    if(!is.null(rTREES_output$sim_out)&&"none"%notin%names(rTREES_output$sim_out)){
      sim_df<-data.frame(
        matrix(
          ncol=length(
            names(rTREES_output$sim_out)
          ),
          nrow=length(rTREES_output$sim_out$year)
        )
      )

      for(i in 1:length(names(rTREES_output$sim_out))){
        sim_df[,i]<-rTREES_output$sim_out[i] %>% unlist()

      }
      names(sim_df)<-names(rTREES_output$sim_out)
    }else{
      sim_df<-data.frame("none"=NA)
    }

    if(!is.null(rTREES_output$hyd_out)&&"none"%notin%names(rTREES_output$hyd_out)){
      hyd_df<-data.frame(
        matrix(
          ncol=length(
            names(rTREES_output$hyd_out)
          ),
          nrow=length(rTREES_output$sim_out$year)
        )
      )

      for(i in 1:length(names(rTREES_output$hyd_out))){

        hyd_df[,i]<-rTREES_output$hyd_out[i] %>% unlist()


      }
      names(hyd_df)<-names(rTREES_output$hyd_out)
    }else{
      hyd_df<-data.frame("none"=NA)
    }


    if(!is.null(rTREES_output$lfa_out)&&"none"%notin%names(rTREES_output$lfa_out)){
      lf_df<-data.frame(
        matrix(
          ncol=length(
            names(rTREES_output$lfa_out)
          ),
          nrow=length(rTREES_output$sim_out$year)
        )
      )
      for(i in 1:length(names(rTREES_output$lfa_out))){

        lf_df[,i]<-rTREES_output$lfa_out[i] %>% unlist()
      }
      names(lf_df)<-names(rTREES_output$lfa_out)
    }else{
      lf_df<-data.frame("none"=NA)
    }

    return(list(sim=sim_df,hyd=hyd_df,leaf=lf_df))

  }else{
    message("hmmm something went wrong here...")
    return(list(sim="error in cleaning",hyd="error in cleaning",leaf="error in cleaning"))
  }
}


#' Clean and make long the leaf data from a parallel run
#' @param par_res The output of a parallel rTREES run.
#' @return A data.frame() of leaf data in long format.
#' @export
long_df_leaf_parallel<-function(
    par_res
    ){
  n_dr<-length(par_res)
  dr_names<-names(par_res)
  n_samp<-length(par_res[[1]])
  samp_names<-names(par_res[[1]])
  n_leaf<-ncol(par_res[[1]][[1]][["leaf"]])-4
  # leaf_areas<-list()
  # for(i in 1:n_leaf){
  #   leaf_areas[[i]]<-vector()
  # }

  long_df<-data.frame(
    UTC_date_time=vector(),
    driver=vector(),
    parameter=vector(),
    leaf_number=vector(),
    leaf_area=vector()
  )

  for(dr in 1:n_dr){
    for(sp in 1:n_samp){
      for(lf in 1:n_leaf){
        long_df<-rbind(
          long_df,
          data.frame(
            UTC_date_time=as.POSIXct(
              x=paste(
                par_res[[dr]][[sp]][["leaf"]][,"year"],
                par_res[[dr]][[sp]][["leaf"]][,"jday"],
                par_res[[dr]][[sp]][["leaf"]][,"hour"],
                min(par_res[[dr]][[sp]][["leaf"]][,"min"]*100,1)*c(0,30),
                "+0000",
                sep="-"
              ),
              format="%Y-%j-%H-%M-%z",
              tz = "UTC"
            ),
            driver=dr_names[dr],
            parameter=samp_names[sp],
            leaf_number=lf,
            leaf_area=par_res[[dr]][[sp]][["leaf"]][[lf+4]]
          )
        )
      }
    }

  }

  return(long_df)
}

#' Clean and make long the sim data from a parallel run
#' @param par_res The output of a parallel rTREES run.
#' @return A data.frame() of leaf data in long format.
#' @export
long_df_sim_parallel<-function(
    par_res
    ){
  n_dr<-length(par_res)
  dr_names<-names(par_res)
  n_samp<-length(par_res[[1]])
  samp_names<-names(par_res[[1]])
  n_sims<-ncol(par_res[[1]][[1]][["sim"]])-4
  sim_names<-names(par_res[[1]][[1]][["sim"]])[5:(4+n_sims)]

  long_df<-data.frame(
    UTC_date_time=vector(),
    driver=vector(),
    parameter=vector(),
    sim_name=vector(),
    sim_val=vector()
  )

  for(dr in 1:n_dr){
    for(sp in 1:n_samp){
      for(ns in 1:n_sims){
        curr<-par_res[[dr]][[sp]][["sim"]]
        long_df<-rbind(
          long_df,
          data.frame(
            UTC_date_time=ez_dt(
              year=curr$year,
              jday = curr$jday,
              hour = curr$hour,
              min = curr$min
            ),
            driver=dr_names[dr],
            parameter=samp_names[sp],
            sim_name=sim_names[ns],
            sim_val=par_res[[dr]][[sp]][["sim"]][[ns+4]]
          )
        )
      }
    }

  }

  return(long_df)
}

#' Clean and make long the hyd data from a parallel run
#' @param par_res The output of a parallel rTREES run.
#' @return A data.frame() of leaf data in long format.
#' @export
long_df_hyd_parallel<-function(
    par_res
){
  n_dr<-length(par_res)
  dr_names<-names(par_res)
  n_samp<-length(par_res[[1]])
  samp_names<-names(par_res[[1]])
  n_hyds<-ncol(par_res[[1]][[1]][["hyd"]])-4
  hyd_names<-names(par_res[[1]][[1]][["hyd"]])[5:(4+n_hyds)]

  long_df<-data.frame(
    UTC_date_time=vector(),
    driver=vector(),
    parameter=vector(),
    hyd_name=vector(),
    hyd_val=vector()
  )

  for(dr in 1:n_dr){
    for(sp in 1:n_samp){
      for(ns in 1:n_hyds){
        curr<-par_res[[dr]][[sp]][["hyd"]]
        long_df<-rbind(
          long_df,
          data.frame(
            UTC_date_time=ez_dt(
              year=curr$year,
              jday = curr$jday,
              hour = curr$hour,
              min = curr$min
            ),
            driver=dr_names[dr],
            parameter=samp_names[sp],
            hyd_name=hyd_names[ns],
            hyd_val=par_res[[dr]][[sp]][["hyd"]][[ns+4]]
          )
        )
      }
    }

  }

  return(long_df)
}


#' Possible variables (Need to add explainations to these)
#' @param n_layers The number of soil layers
#' @return A vector of all possible rTREES variable outputs
#' @export
Possible_Outputs<-function(
    n_layers=4
    ){
  all_names_pos<-c(
 "year","jday","hour","min","simET","WPlant_K",
 "Soil_Psi","Leaf_Psi","Psi_Crit","Ecrit","Ec",
 paste0("RhizFlux",1:n_layers-1) ,
 #"RhizFlux0","RhizFlux1","RhizFlux2","RhizFlux3","RhizFlux4",
 "Gs","LAI","SLA","liveLAI","Rmaint",
 "Rgrowth","reproduction","leafNSC","stemNSC",
 "rootNSC","chloroStarch","chloroSugar","waterStress",
 "litterH2O",
 paste0("theta",1:n_layers-1),
 #"theta0","theta1","theta2","theta3","theta4",
 "thetaRoot","Can_Evap","Snowpack",
 "SnowEdef","Vcmax25","Vcmax_sun","Vcmax_shd","Jmax25",
 "J_sun","J_shd","Asun","Ashd",

 "Av_sun","Av_shd","Aj_sun","Aj_shd","phi2_sun","phi2_shd","betaA_sun","betaA_shd",

 "Lsun","Lshd","Tsun",
 "Tshd","Dsun","Dshd","Ci_sun","Ci_shd","PARsun",
 "PARshd","gs_sun","gs_shd","NEE","NPP","R_total","R_ag",
 "R_bg","Rd_sun","Rd_shd","Csapwood",

 paste0("soilPsi",1:n_layers-1),

 paste0("FibRootC",1:n_layers-1),
 #"FibRootC0", "FibRootC1","FibRootC2","FibRootC3","FibRootC4",
 paste0("FineRootC",1:n_layers-1),
 #"FineRootC0","FineRootC1","FineRootC2","FineRootC3","FineRootC4",
 paste0("TotRootC",1:n_layers-1),
 #"TotRootC0","TotRootC1", "TotRootC2","TotRootC3","TotRootC4",
 paste0("FineRootCN",1:n_layers-1),
 #"FineRootCN0", "FineRootCN1","FineRootCN2","FineRootCN3","FineRootCN4",
 "LeafCN",
 paste0("humusC",1:n_layers-1),
 #"humusC0","humusC1","humusC2","humusC3","humusC4",
 paste0("RhizCl",1:n_layers-1),
 #"RhizCl0","RhizCl1","RhizCl2","RhizCl3","RhizCl4",
 paste0("RhizNl",1:n_layers-1),
 #"RhizNl0", "RhizNl1","RhizNl2","RhizNl3","RhizNl4",
 paste0("AAexudateC",1:n_layers-1),
 #"AAexudateC0", "AAexudateC1","AAexudateC2","AAexudateC3","AAexudateC4",
 paste0("SugarExudateC",1:n_layers-1),
 #"SugarExudateC0","SugarExudateC1","SugarExudateC2","SugarExudateC3","SugarExudateC4",
 paste0("MicrobC",1:n_layers-1),
 #"MicrobC0","MicrobC1","MicrobC2","MicrobC3","MicrobC4",
 paste0("MicrobN",1:n_layers-1),
 #"MicrobN0","MicrobN1","MicrobN2","MicrobN3","MicrobN4",
 "RhizosphereNitrateNitrogen","RhizosphereAmmoniumNitrogen",
 "PlantN","plantNstat","RLA","CanopyCover",
 paste0("ar",1:n_layers-1),
 #"ar0","ar1","ar2","ar3","ar4",
 "latStemK",
 paste0("latRootK",1:n_layers-1),
 #"latRootK0","latRootK1","latRootK2","latRootK3","latRootK4",
 "StemAxialYm","StemLatYm",
 paste0("RootAxialYm",1:n_layers-1),
 #"RootAxialYm0","RootAxialYm1","RootAxialYm2","RootAxialYm3","RootAxialYm4",
 paste0("RootLatYm",1:n_layers-1),
 #"RootLatYm0","RootLatYm1","RootLatYm2","RootLatYm3","RootLatYm4",
 "StemAxialKm","StemLatKm",
 paste0("RootAxialKm",1:n_layers-1),
 #"RootAxialKm0","RootAxialKm1","RootAxialKm2","RootAxialKm3","RootAxialKm4",
 paste0("RootLatKm",1:n_layers-1),
 #"RootLatKm0","RootLatKm1","RootLatKm2","RootLatKm3","RootLatKm4",
 "StemAxial_b","StemLat_b",
 paste0("RootAxial_b",1:n_layers-1),
 #"RootAxial_b0","RootAxial_b1","RootAxial_b2","RootAxial_b3","RootAxial_b4",
 paste0("RootLat_b",1:n_layers-1),
 #"RootLat_b0","RootLat_b1","RootLat_b2","RootLat_b3","RootLat_b4",
 "StemAxial_c","StemLat_c",
 paste0("RootAxial_c",1:n_layers-1),
 #"RootAxial_c0","RootAxial_c1","RootAxial_c2","RootAxial_c3", "RootAxial_c4",
 paste0("RootLat_c",1:n_layers-1)
 #"RootLat_c0","RootLat_c1","RootLat_c2", "RootLat_c3","RootLat_c4"
 )
  return(all_names_pos)
}


#' For getting lognormal parameters from a desired mean and sd
#' @param m Mean of desired distribution
#' @param s Standard deviation of desired distribution
#' @return A named vector of doubles for the meanlog and sdlog.
#' @export
norm_to_lnorm<-function(
    m,
    s
){
  meanlog <- log(m^2 / sqrt(s^2 + m^2))
  sdlog <- sqrt(log(1 + (s^2 / m^2)))
  return(c(meanlog=meanlog,sdlog=sdlog))
}

#' For getting mean and sd from lognormal parameters
#' @param lm meanlog or location parameter
#' @param ls Standard deviation of desired distribution
#' @return A named vector of doubles for the mean and sd.
#' @export
lnorm_to_norm<-function(
    lm,
    ls
){
  mean<-exp(lm + (ls^2)/2)
  sd<- sqrt((exp(ls^2) - 1) * exp(2*lm + ls^2))
  return(c(mean=mean,sd=sd))
}

#' Simplifies cleaning up TREES datetime output.
#' @param year year
#' @param jday day of year (julian day)
#' @param hour hour (24 hour time)
#' @param min minutes (all non 0 values are assumed 30 min)
#' @param tz timezone offset in the format of two digit hour and two digit min preceded by + or -
#' @return datetime POSIXct object
#' @export
ez_dt<-function(
    year,
    jday,
    hour,
    min,
    tz="+0000"
){
  return(
    as.POSIXct(
      x = paste(
        year,
        jday,
        hour,
        min(min*100,1)*30,#handles all min rTREES formats (typically 5, 0.5, and 30)
        "+0000",
        sep = "-"
        ),
      format="%Y-%j-%H-%M-%z",
      tz = "UTC"
      )
    )
}

#' Get the index of a parameter
#' Used mostly internally to rTREES.
#' @param parameter_name valid parameter name as is found in ExampleParameters()
#' @return returns the index of the input parameter name.
#' @export
param_idx<-function(
    parameter_name=""
){
  lookup<-which(
    rTREES::ExampleParameters()$Parameter_Name==parameter_name
  )
  if(length(lookup)==0){
    idx<-NA
  }else{
    idx<-lookup
  }
  return(idx)
}


#' Search for the exact name of a parameter when you are unsure
#' Can't recall the excat name of a parameter? Happens to me all the time. Here is a little function I tossed together to **sometimes** help with that. The logic is the same as that in the attempt_p_repair function, so don't expect perfection.
#' @param query key words or guesses of the parameters name
#' @param n_return how many results would you liked returned? Defaults to top 5
#' @return The contents of the ExampleParameter() row, including the parameter name, that best matches the query.
#' @export
param_search<-function(
    query="",
    n_return=5
){
  params_in<-ExampleParameters() %>%
    tidyr::unite("combo",!Value)
  all_possible<-paste(params_in$combo,collapse = " ")
  tkn_length<-3
  all_possible_table<-table(
    rTREES::tiny_tokenizer(all_possible,token_length = tkn_length)
  )
  scores<-vector()
  for(i in 1:nrow(params_in)){
    scores<- append(
      scores,
      abs(
        1-rTREES::cosine_sim(
          params_in[i ,"combo"],
          query,
          tkn_length,
          relative_corpus_table =all_possible_table
        )
      )
    )
  }
  params_in<-ExampleParameters() %>%
    dplyr::mutate(score=scores) %>%
    dplyr::arrange((score))

  return(params_in[1:n_return,"Parameter_Name"])
}


#' Custom operator to more cleanly perform !\%in\%.
#' @param a The left side vector of elements to look for in the right side vector.
#' @param b The right side vector of elements to look through for matches to the left side vector.
#' @return TRUE or FALSE a is not in b
#' @examples
#' c("bananas")%notin%c("apple","cat","music")
#' c("apple")%notin%c("apple","cat","music")
#' @export
`%notin%` <- function(
  a,
  b
){
  if(!is.vector(a)){
    tryCatch({
      a<-as.vector(a)
    }, error = function(e){
      stop(paste("The left side object is not coercible to a vector.\n",e))
    }
    )
  }
  if(!is.vector(b)){
    tryCatch({
      b<-as.vector(b)
    }, error = function(e){
      stop(paste("The right side object is not coercible to a vector.\n",e))
    }
    )
  }
  return(!(a %in% b))
}


#' Get the max depth of a list.
#' Adopted from https://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r#comment18364266_13433689
#' @param this the list of interest
#' @return Maximum depth of list as an integer.
#' @examples
#' depth(ExampleRSS())
#' @export
depth <- function(
    this
){
  ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
}

#' Run parallel jobs
#' This is a generic function that will run any function over all provided argument combinations in parallel using callr.
#' @import progressr
#' @import callr
#' @param list_args_list Arguments for each iteration of the function to run.
#' @param all_pkgs Packages required for function to run
#' @param run_func Function that will run arguments. Note, do not include () and try to include function package. Ex package::function
#' @param n_cores How many parallel cores to use
#' @param timeout_scalar If not zero and a calibration function is provided this value is multiplied by the estimated maximum time calculated by the calibration_fun to determine when a run should timeout.
#' @param calibration_fun A function that will determine the maximum time a single run should take. If only a number is provided it will be treated as the maximum time in seconds.
#' @param progressbar If TRUE a progressbar will be shown in the console. This fails in snippets.
#'
#' @return list of results where each element of the list corresponds to a row of parameters in new_parameters
#' @export
spooky_parallel<-function(
    list_args_list=list(
      job_1=list(
        drivers=list(dr1=rTREES::BuildDriver(ndays = 10,start_jday = 100)),
        base_parameters=ExampleParameters(),
        new_parameters=data.frame(),
        rss=ExampleRSS(),
        verbosity=FALSE,
        softerror_max=5,
      )
    ),
    all_pkgs=c("rTREES"),
    run_func=rTREES::rTREES,#switch to wrapper
    n_cores=rTREES::number_of_cores()-2,
    timeout_scalar=0,
    calibration_fun=NA,#function(...){return(1)}
    progressbar=TRUE
){
  n_cores<-min(n_cores,length(list_args_list),rTREES::number_of_cores())
  if(!is.function(calibration_fun)&&!is.na(calibration_fun)&&is.numeric(calibration_fun)&&calibration_fun>1){
    calibration_fun<-function(...)substitute(mx,list(mx=calibration_fun))
  }
  if(timeout_scalar>0&&n_cores>1&&is.function(calibration_fun)){
    max_time<-calibration_fun(
      list_args_list=list_args_list,
      all_pkgs=all_pkgs,
      run_func=run_func,
      n_cores=n_cores
    )*timeout_scalar
  }else if(timeout_scalar==0||n_cores==1){
    max_time<-Inf
  }else{
    stop("incorrect timeout_scalar or n_cores")
  }


  #base job object
  job_base<-list(
    job_id=NA,
    job_status="uninitialized",#uninitialized,running,finshed
    job_session=NA,#callr::r_session$new(),#This will be the callr::rsesson
    job_packages=NA,#c("rTREES"),#
    job_function=NA,#rTREES
    job_fun_args=NA,#list(
    # env_driver =all_drvr[[1]],
    # base_parameters = param_set[[1]],
    # root_shoot_soil = rss_set[[1]],
    # verbosity = verbosity,
    # softerror_max = softerror_max
    # ),#named list of args for rTREES
    job_start_time=NA,
    job_results=NA,
    job_stdout=NA,
    job_stderr=NA,
    job_error=NA
  )

  #init job list
  job_ls<-list()
  #build job list
  job_idx<-1
  for(ar in 1:length(list_args_list)){
    temp_new_job<-job_base
    temp_new_job$job_id<-list_args_list[[ar]]$job_id
    temp_new_job$job_packages<-all_pkgs
    temp_new_job$job_function<-run_func
    temp_new_job$job_fun_args<-list_args_list[[ar]]$args
    job_ls[[ar]]<-temp_new_job
  }

  n_jobs<-length(list_args_list)
  unfinished_jobs<-TRUE
  curr_idx<-0
  finished_count<-0
  running<-0
  first_run<-TRUE
  reloop<-FALSE
  if(isTRUE(getOption("rstudio.notebook.executing"))&progressbar){
    progressbar<-FALSE
    message("Disable progressbar when executing rstudio chunks.")
  }
  p<-function(...){""}
  if(progressbar){
    tryCatch(
      {
        progressr::handlers(global = TRUE)
        p <- progressr::progressor(steps =as.integer(n_jobs))
      }
    )
  }
  while(unfinished_jobs){
    if(!first_run&&curr_idx>=n_jobs){
      curr_idx<-1
      if(running==0){
        if(reloop){
          unfinished_jobs<-FALSE
          warning("Jobs failed to init")
        }else{
          reloop<-TRUE
        }
      }
    }else{
      curr_idx<-curr_idx+1
      reloop<-FALSE
    }
    first_run<-FALSE
    finished_count<-0
    running<-0
    for(i in 1:n_jobs){
      if(
        job_ls[[i]]$job_status=="running"&&
        job_ls[[i]]$job_session$get_state()=="busy"
      ){
        running<-running+1
      }else if(job_ls[[i]]$job_status=="finished"||
               job_ls[[i]]$job_status=="timeout"){
        finished_count<-finished_count+1
      }
    }

    # print(paste("finished: ",finished_count," and running: ",running))

    if(finished_count>=n_jobs){
      unfinished_jobs<-FALSE
    }

    if(running<n_cores&&unfinished_jobs){
      if(
        job_ls[[curr_idx]]$job_status=="uninitialized"
      ){#start a new job
        job_ls[[curr_idx]]$job_session<-callr::r_session$new()
        job_ls[[curr_idx]]$job_session$call(
          function(
    job_fun_args,
    job_packages,
    fun_in
          ){
            library(job_packages,character.only =TRUE);
            fun_in(
              job_fun_args
            )
          },
    args =list(
      job_fun_args=job_ls[[curr_idx]]$job_fun_args,
      job_packages=job_ls[[curr_idx]]$job_packages,
      fun_in=job_ls[[curr_idx]]$job_function
    )
        )
        job_ls[[curr_idx]]$job_start_time<-Sys.time()
        job_ls[[curr_idx]]$job_status<-"running"
        # print("new job")
      }
    }
    if(#check if done
      job_ls[[curr_idx]]$job_status=="running"&&
      job_ls[[curr_idx]]$job_session$poll_process(100)=="ready"
    ){
      tempres<-job_ls[[curr_idx]]$job_session$read()
      job_ls[[curr_idx]]$job_results<-tempres$result
      job_ls[[curr_idx]]$job_stdout<-tempres$stdout
      job_ls[[curr_idx]]$job_stderr<-tempres$stderr
      job_ls[[curr_idx]]$job_error<-tempres$error
      job_ls[[curr_idx]]$job_status<-"finished"
      job_ls[[curr_idx]]$job_session$close()
      p()
      # print("fin")
    }
    if(#timeout check
      job_ls[[curr_idx]]$job_status=="running"&&
      job_ls[[curr_idx]]$job_session$poll_process(100)!="ready"&&
      difftime(Sys.time(),job_ls[[curr_idx]]$job_start_time,units = "secs")>max_time
    ){
      job_ls[[curr_idx]]$job_session$close()
      job_ls[[curr_idx]]$job_status<-"timeout"
      p()
      # print("to")
    }
  }

  return(job_ls)
}

#' Prepare inputs for spooky_parallel
#' @param base_parameters Base set of input parameters
#' @param new_parameters New parameters to test
#' @param drivers list of drivers to run
#' @param rss Root shoot and soil parameters (list of list)
#' @param vars_to_return Which variables are you interested in?
#' @param interval_to_return Time interval to be returned. 0.5 is half hourly, 1 is hourly, 12 is twice daily, 24 is daily, et
#' @param aggregate_time_steps Should the time steps not included be averaged to the interval returned?
#' @param verbosity Include standard output?
#' @param softerror_max How many soft errors should be tolerated? It is normal for long runs, especially over seasons, to have a few soft errors without it causing any issues.
#' @return args list for spooky_parallel
#' @export
prep_rTREES_parallel<-function(
    base_parameters=parameters,
    new_parameters,
    drivers=NA,
    rss=NA,
    vars_to_return="all",
    interval_to_return=12,
    aggregate_time_steps=FALSE,
    verbosity=FALSE,
    softerror_max=5
){
  #driver check
  #parameter check
  #rss check
  which_out<-c(FALSE,FALSE,FALSE)
  all_possible_vars<-rTREES::Possible_Outputs(n_layers=rss$rmodules)
  if(any(tolower(vars_to_return)%in%"all")){
    vars_to_return<-all_possible_vars
    which_out[1:3]<-TRUE
  }else{
    if(any(tolower(vars_to_return)%in%tolower(all_possible_vars[5:which(all_possible_vars==paste0("ar",rss$rmodules-1))]))){
      which_out[1]<-TRUE
    }else{
      which_out[1]<-FALSE
    }
    if(any(tolower(vars_to_return)%in%tolower(all_possible_vars[(1+which(all_possible_vars==paste0("ar",rss$rmodules-1))):length(all_possible_vars)]))){
      which_out[2]<-TRUE
    }else{
      which_out[2]<-FALSE
    }
    if(any(tolower(vars_to_return)%in%c("leaf","lf","lfa"))){
      which_out[3]<-TRUE
    }else{
      which_out[3]<-FALSE
    }
  }

  if(depth(drivers)==1){
    drivers<-list(drivers)
  }
  ndrvrs<-length(drivers)
  if(is.null(names(drivers))){
    name_vec<-c()
    for(i in 1:ndrvrs){
      name_vec[i]<-paste0("driver-",i)
    }
    names(drivers)<-name_vec
  }
  #build parameter list
  # base_parameter_s<-ExampleParameters()
  # new_parameters<-data.frame(
  #   microbiomeScalar=runif(500,5,50),
  #   leafArea_Rate=runif(500,0.0001,0.001))
  param_set<-lapply(
    1:nrow(new_parameters),
    function(ro) {
      # list(
      parameters = rTREES::ChangeParameter(
        base_parameters,
        new_parameters[ro,,drop=FALSE],
        VERBOSE = FALSE
      )
      # )
    }
  )
  nprms<-length(param_set)
  all_set_names<-vector()
  for(j in 1:nrow(new_parameters)){
    set_name<-c()
    for(n in 1:ncol(new_parameters)){
      set_name<-paste(set_name,paste(
        names(new_parameters)[n],
        gsub("e\\+", "*10^", format(new_parameters[j,n],scientific=TRUE,digits=4)
        ),sep="@"
      ),sep="&"
      )
    }
    all_set_names[j]<-set_name
  }
  names(param_set)<-all_set_names
  #build rss list
  # rss_set<-list(
  #   ExampleRSS()
  # )
  if(depth(rss)==1){
    rss_set<-list(rss)
  }else{
    rss_set<-rss
  }
  nrss<-length(rss_set)
  # args_idx<-1
  named_list_of_args<-list()
  for(dr in 1:ndrvrs){
    for(rss in 1:nrss){#will add this name in the future so it will be driver::rss::param
      for(pr in 1:nprms){
        named_list_of_args[[paste(names(drivers)[dr],names(param_set)[pr],sep="::")]]<-list(
          job_id=list(driver=names(drivers)[dr],parameters=names(param_set)[pr]),
          args=list(
            env_driver =drivers[[dr]],
            base_parameters = param_set[[pr]],
            root_shoot_soil = rss_set[[rss]],
            which_out = which_out,
            verbosity = verbosity,
            softerror_max = softerror_max,
            vars_to_return=vars_to_return,
            interval_to_return=interval_to_return,
            aggregate_time_steps=aggregate_time_steps
          )
        )
        # args_idx<-args_idx+1
      }
    }
  }

  return(named_list_of_args)#also includes element for job_id
}

#' rTREES wrapper for spooky_parallel. This exists because spooky_parallel only takes and passes one arg.
#' @param args_in named list of rTREES inputs
#' @return cleaned and aggregated results of a single element from the args list.
#' @export
rTREES_spooky_wrapper<-function(
    args_in=list(
      base_parameters=parameters,
      env_driver=NA,
      root_shoot_soil=NA,
      vars_to_return="all",
      interval_to_return=12,
      aggregate_time_steps=FALSE,
      verbosity=FALSE,
      softerror_max=5
    )

){
  rtrees_results<-rTREES::rTREES(
    env_driver = args_in$env_driver, #drivers[[dr]],
    base_parameters =args_in$base_parameters,
    root_shoot_soil = args_in$root_shoot_soil,
    which_out=args_in$which_out,
    verbosity=args_in$verbosity,
    softerror_max = args_in$softerror_max
  )%>%
    rTREES::Clean_rTREES_output() %>%
    rTREES::aggregate_and_filter(
      root_shoot_soil=args_in$root_shoot_soil,
      interval_to_return=args_in$interval_to_return,
      vars_to_return=args_in$vars_to_return,
      aggregate_time_steps=args_in$aggregate_time_steps
    )
  return(rtrees_results)
}

#' Aggregates and filters outputs of a single rTREES run
#' @param rtrees_results Results from rTREES::Clean_rTREES_output
#' @param root_shoot_soil see rTREES_parallel_run
#' @param interval_to_return see rTREES_parallel_run
#' @param vars_to_return see rTREES_parallel_run
#' @param aggregate_time_steps see rTREES_parallel_run
#' @return Aggregated and filters rTREES results
#' @export
aggregate_and_filter<-function(
    rtrees_results,
    root_shoot_soil,
    interval_to_return=48,
    vars_to_return,
    aggregate_time_steps=FALSE
){
  rtrees_sim<-NA
  rtrees_hyd<-NA
  rtrees_leaf<-NA
  interval_to_return<-interval_to_return*2
  all_possible_vars<-rTREES::Possible_Outputs(n_layers=root_shoot_soil$rmodules)
  which_out<-c(FALSE,FALSE,FALSE)
  if(any(tolower(vars_to_return)%in%"all")){
    vars_to_return<-all_possible_vars
    which_out[1:3]<-TRUE
  }else{
    if(any(tolower(vars_to_return)%in%tolower(all_possible_vars[5:which(all_possible_vars==paste0("ar",root_shoot_soil$rmodules-1))]))){
      which_out[1]<-TRUE
    }else{
      which_out[1]<-FALSE
    }
    if(any(tolower(vars_to_return)%in%tolower(all_possible_vars[(1+which(all_possible_vars==paste0("ar",root_shoot_soil$rmodules-1))):length(all_possible_vars)]))){
      which_out[2]<-TRUE
    }else{
      which_out[2]<-FALSE
    }
    if(any(tolower(vars_to_return)%in%c("leaf","lf","lfa"))){
      which_out[3]<-TRUE
    }else{
      which_out[3]<-FALSE
    }
  }

  vars_to_return<-c("year","jday","hour","min",vars_to_return)

  if(aggregate_time_steps){

    if(which_out[1]&&inherits(rtrees_results[["sim"]],"data.frame")&&"none"%notin%names(rtrees_results[["sim"]])){
      rtrees_sim<-rtrees_results[["sim"]][,] %>%
        dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
        dplyr::summarize(dplyr::across(dplyr::any_of(vars_to_return),~mean(.x,na.rm=TRUE))) %>%
        as.data.frame()
    }
    if(which_out[2]&&inherits(rtrees_results[["hyd"]],"data.frame")&&"none"%notin%names(rtrees_results[["hyd"]])){
      rtrees_hyd<-rtrees_results[["hyd"]][,] %>%
        dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
        dplyr::summarize(dplyr::across(dplyr::any_of(vars_to_return),~mean(.x,na.rm=TRUE))) %>%
        as.data.frame()
    }
    if(which_out[3]&&inherits(rtrees_results[["leaf"]],"data.frame")&&"none"%notin%names(rtrees_results[["leaf"]])){
      rtrees_leaf<-rtrees_results[["leaf"]][,colSums(rtrees_results[["leaf"]]) != 0] %>%
        dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
        dplyr::summarize(dplyr::across(dplyr::everything(),~mean(.x,na.rm=TRUE))) %>%
        as.data.frame()
    }
  }else{
    if(which_out[1]&&inherits(rtrees_results[["sim"]],"data.frame")&&"none"%notin%names(rtrees_results[["sim"]])){
      rtrees_sim<-rtrees_results[["sim"]] %>%
        dplyr::select(dplyr::any_of(vars_to_return)) %>%
        dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
    }
    if(which_out[2]&&inherits(rtrees_results[["hyd"]],"data.frame")&&"none"%notin%names(rtrees_results[["hyd"]])){
      rtrees_hyd<-rtrees_results[["hyd"]] %>%
        dplyr::select(dplyr::any_of(vars_to_return)) %>%
        dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
    }
    if(which_out[3]&&inherits(rtrees_results[["leaf"]],"data.frame")&&"none"%notin%names(rtrees_results[["leaf"]])){
      rtrees_leaf<-rtrees_results[["leaf"]][,colSums(rtrees_results[["leaf"]]) != 0]%>%
        dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
    }

  }
  return(list(
    sim=rtrees_sim,
    hyd=rtrees_hyd,
    leaf=rtrees_leaf
  )
  )
}

#' Timeout calibration function for spooky_parallel when using rTREES
#' @param list_args_list Outputs from prep_rTREES_parallel()
#' @param all_pkgs Required packages
#' @param run_func Function to run (rTREES_wrapper)
#' @param n_cores Number of cores to use
#' @return time estimate for longest run out of args list
#' @export
rTREES_calib_function<-function(
    list_args_list,
    all_pkgs,
    run_func,
    n_cores
){
  message("\n\n\n*********************************************************\nBeginning calibration, ignore console until complete.\n*********************************************************\n")
  #calibrate
  lngst_dr<-list_args_list[[1]]$args$env_driver
  for(ld in 1:length(list_args_list)){
    if(nrow(list_args_list[[ld]]$args$env_driver)>nrow(lngst_dr)){
      lngst_dr<-list_args_list[[ld]]$args$env_driver
    }
  }
  if(nrow(lngst_dr)<6){
    tdriver<-rTREES::BuildDriver(ndays = 20,start_jday = 200)
  }else{
    tdriver<-lngst_dr
  }
  calib_max<-nrow(tdriver)
  calib_steps<-c(
    calib_max,
    floor(calib_max*0.9),
    # floor(calib_max*0.8),#This is a very straight line, don't need ten.
    floor(calib_max*0.7),
    # floor(calib_max*0.6),
    floor(calib_max*0.5),
    # floor(calib_max*0.4),
    floor(calib_max*0.3),
    # floor(calib_max*0.2),
    floor(calib_max*0.1)
  )
  tdiff<-vector()
  for(i in 1:length(calib_steps)){
    test_args<-list()
    for(j in 1:min(n_cores,length(list_args_list))){
      test_args[[j]]<-list_args_list[[j]]
      test_args[[j]]$args$env_driver<-test_args[[j]]$args$env_driver[1:calib_steps[i],]
    }
    t0<-Sys.time()

    spooky_parallel(
      list_args_list=test_args,
      all_pkgs=all_pkgs,
      run_func=run_func,
      timeout_scalar=0,
      n_cores = n_cores,
      calibration_fun = NA
    )
    tdiff[i]<-difftime(Sys.time(),t0,units = "secs")
  }
  coefs<-lm(tdiff~calib_steps)$coefficients
  max_time<-(coefs[1]+nrow(lngst_dr)*coefs[2])
  message("\n*********************************************************\nCalibration complete\n*********************************************************\n\n\n")
  return(max_time)
}


#' Clean up final outputs from spooky_parallel
#' @param job_ls Output of spooky_parallel when using rTREES
#' @param debug When TRUE retain information from callr run. This includes, errors, exit conditions, and more. If rTREES verbosity is also TRUE, that will be stored in the job_stdout element. Use cat() when trying to view job_stdout.
#' @return If debug is FALSE, returns the typical rTREES parallel results. If debug is TRUE, returns a list with typical rTREES parallel results and the full job outputs from callr.
#' @export
rTREES_par_finish<-function(
    job_ls,
    debug=FALSE
){
  # #
  final_results<-list()
  for(aj in 1:length(job_ls)){
    final_results[[job_ls[[aj]]$job_id$driver]][[job_ls[[aj]]$job_id$parameters]]<-job_ls[[aj]]$job_results
  }

  for(i in 1:length(job_ls)){
    job_ls[[i]]$job_session$close()
  }
  message("\n*********************************************************\nSimulations complete\n*********************************************************\n\n\n")
  if(debug==TRUE){
    return(
      list(
        rTREES=final_results,
        debug=job_ls
      )
    )
  }else{
    return(final_results)
  }
}

#' If any of the elements of spooky_parallel failed to complete (only visible if debug was set to TRUE), then you can use this function to retry running only the ones that failed. You can use this to also set longer run times (actual maximum run time, not a scalar) or turn on verbosity for viewing job_stdout.
#' @param results_debug Secong list element output from rTREES_parallel_run() or rTREES_par_finsh() when debug is TRUE OR output from rTREES::spooky_parallel.
#' @param n_cores Number of cores to use
#' @param max_time Actual maximum run time
#' @param verbosity Use verbosity (only applies if verbosity is an argument of the function being run)
#' @return Debug results with new results of runs that didn't complete before. NOTE this does not guarantee the run will work. On a second failure from timeout, try longer max_time. If it is any other failure, investigate stdout or other debug information.
#' @export
retry_spooky<-function(
    results_debug,
    n_cores,
    max_time=0,
    verbosity=FALSE
){

  retry_args<-list()
  retry_func<-results_debug[[1]]$job_function
  retry_pkgs<-results_debug[[1]]$job_packages
  retry_idx<-1
  orig_idx<-vector()
  for(i in 1:length(results_debug)){
    if(results_debug[[i]]$job_status!="finished"){
      retry_args[[retry_idx]]<-list()
      retry_args[[retry_idx]]$job_id<-results_debug[[i]]$job_id
      retry_args[[retry_idx]]$args<-results_debug[[i]]$job_fun_args
      retry_args[[retry_idx]]$args$verbosity<-verbosity
      retry_idx<-retry_idx+1
      orig_idx<-append(orig_idx,i)
    }
  }
  if(max_time>0){
    calib_func<-function(...)substitute(mx,list(mx=max_time))
    timeout_scalar<-1
  }else{
    calib_func<-NA
    timeout_scalar<-0
  }
  retry_results<-spooky_parallel(
    list_args_list=retry_args,
    all_pkgs=retry_pkgs,
    run_func=retry_func,
    n_cores=n_cores,
    timeout_scalar=timeout_scalar,
    calibration_fun = calib_func
  )
  for(fin in 1:length(retry_results)){
    results_debug[[orig_idx[fin]]]<-retry_results[[fin]]
  }
  return(results_debug)
}


#' rTREES parallel run
#' Takes a data frame with each column being a parameter (run ExampleParameters() to see proper names)
#' and each row is a simulation to run with those parameters and runs rTREES with those parameters. These
#' parameter sets are run in parallel and a list of run results are returned in the same order as the
#' data frame.
#' @param base_parameters Initial parameters. Using Load_TREES_files() is recommended
#' @param drivers Environmental driver. Using Load_TREES_files() is recommended
#' @param root_shoot_soil Initial root_shoot_soil. Using Load_TREES_files() is recommended
#' @param new_parameters parameters to be run. rTREES will be run over each row.
#' @param ncores The number of cores you would like jobs to be split among. It is recommended that you keep a couple cores free for your system to retain functionality while running simulations.
#' @param vars_to_return rTREES outputs you want to have returned. Using 'all' is allowed but not recommend. See Possible_Outputs() for options.
#' @param interval_to_return Time interval to be returned. 0.5 is half hourly, 1 is hourly, 12 is twice daily, 24 is daily, etc..
#' @param aggregate_time_steps If TRUE, the time steps will be averaged around the interval_to_return steps.
#' @param verbosity Allow TREES to generate output to job_stdout.
#' @param softerror_max How many failures to converge is TREES allowed within 24 hours.
#' @param timeout_scalar Multiplyer for calibrated timeout. e.x. 2 would set the timeout to two times the calibrated estimate.
#' @param debug When TRUE retain information from callr run. This includes, errors, exit conditions, and more. If rTREES verbosity is also TRUE, that will be stored in the job_stdout element. Use cat() when trying to view job_stdout.
#'
#' @return list of results where each element of the list corresponds to a row of parameters in new_parameters
#'
#' @examples
#' demo_parallel_run<-rTREES_parallel_run(
#'   base_parameters=ExampleParameters(),
#'   drivers=BuildDriver(ndays = 10),
#'   root_shoot_soil=ExampleRSS(),
#'   new_parameters= data.frame(
#'     leafArea_Rate=1:10,
#'     microbiomeScalar=1:10
#'   ),
#'   vars_to_return="all",
#'   ncores=rTREES::number_of_cores()-2,
#'   interval_to_return=12,
#'   aggregate_time_steps=FALSE,
#'   verbosity=FALSE,
#'   softerror_max=5,
#'   timeout_scalar=5,
#'   debug=FALSE
#' )
#' @export
rTREES_parallel_run<-function(
    base_parameters=parameters,
    drivers=driver,
    root_shoot_soil=root_shoot_soil,
    new_parameters= data.frame(
      leafAreaMax=1:10,
      leafArea_Rate=1:10,
      microbiomeScalar=1:10
    ),
    vars_to_return="all",
    ncores=rTREES::number_of_cores()-2,
    interval_to_return=12,
    aggregate_time_steps=FALSE,
    verbosity=FALSE,
    softerror_max=5,
    timeout_scalar=5,
    debug=FALSE
){

  rTREES::prep_rTREES_parallel(
    base_parameters=base_parameters,
    new_parameters=new_parameters,
    drivers=drivers,
    rss=root_shoot_soil,
    vars_to_return=vars_to_return,
    interval_to_return=interval_to_return,
    aggregate_time_steps=aggregate_time_steps,
    verbosity=verbosity,
    softerror_max=softerror_max
  ) %>%
    rTREES::spooky_parallel(
      all_pkgs="rTREES",
      run_func=rTREES::rTREES_spooky_wrapper,
      n_cores=ncores,
      timeout_scalar=timeout_scalar,
      calibration_fun = rTREES::rTREES_calib_function
    ) %>%
    rTREES::rTREES_par_finish(
      debug=debug
    )
}





####*** GRAVE YARD ***###
#
# # This function is for calibrating the time out during long iterations.
# #  import tictoc
# #  param query key words or guesses of the parameters name
# #  param n_return how many results would you liked returned? Defaults to top 5
# #
# #  return The contents of the ExampleParameter() row, including the parameter name, that best matches the query.
# #  export
# #
# #  examples
# # timeout_calibration(
# # ExampleDriver(),
# # ExampleParameters(),
# # ExampleRSS()
# # )
# timeout_calibration<-function(driver,parameters,rss){
#   message("\n\n\n*********************************************************\nBeginning calibration, ignore console until complete.\n*********************************************************\n")
#   ncores<-future::availableCores()-2
#   all_toc<-vector()
#   calib_max<-nrow(driver)
#   calib_steps<-c(
#     calib_max,
#     floor(calib_max*0.9),
#     # floor(calib_max*0.8),#This is a very straight line, don't need ten.
#     floor(calib_max*0.7),
#     # floor(calib_max*0.6),
#     floor(calib_max*0.5),
#     # floor(calib_max*0.4),
#     floor(calib_max*0.3),
#     # floor(calib_max*0.2),
#     floor(calib_max*0.1)
#   )
#   if(any(calib_steps<3)){
#     calib_steps[which(calib_steps<3)]<-rep(3,sum(which(calib_steps<3)))
#   }
#   for(i in 1:length(calib_steps)){
#     tic()
#     temp_dr<-driver[1:calib_steps[i],]
#     dump<-rTREES_parallel_run(
#       base_parameters = parameters,
#       drivers=temp_dr,
#       root_shoot_soil = rss,
#       new_parameters = data.frame(
#         altitude=rnorm(ncores,parameters[1,1],parameters[1,1]*0.25)
#       ),
#       vars_to_return = "all",
#       ncores=ncores,
#       interval_to_return = 0.5,
#       aggregate_time_steps = FALSE,
#       verbosity =FALSE,
#       softerror_max = 5,
#       timeout_scalar=0
#     )
#     temptoc=toc()
#     all_toc[i]=temptoc$toc-temptoc$tic
#   }
#   coefs<-lm(all_toc~calib_steps)
#   message("\n*********************************************************\nCalibration complete\n*********************************************************\n\n\n")
#   return(as.vector(coefs$coefficients))
# }




# rTREES_parallel_run<-function(
    #     base_parameters=parameters,
#     drivers=driver,
#     root_shoot_soil=root_shoot_soil,
#     new_parameters= data.frame(
#       leafAreaMax=1:10,
#       leafArea_Rate=1:10,
#       microbiomeScalar=1:10
#     ),
#     vars_to_return="all",
#     ncores=future::availableCores()-2,
#     interval_to_return=12,
#     aggregate_time_steps=FALSE,
#     # save_step_interval=0,
#     verbosity=FALSE,
#     softerror_max=5,
#     timeout_scalar=5
# ){
#
#   progressr::handlers(global = TRUE)
#
#   interval_to_return<-interval_to_return*2
#   if(ncores>future::availableCores()){
#     warning("Available cores exceeded. Dropping to maximum detected cores.",immediate. = TRUE)
#     ncores<-future::availableCores()
#   }
#   # ncores<-parallel::detectCores()-2
#   # doParallel::registerDoParallel(cores=ncores)
#   future::plan(future::multisession,workers=ncores)
#
#   all_possible_vars<-Possible_Outputs(n_layers=root_shoot_soil$rmodules)
#
#   ###> determine if some outputs should be ignored
#   which_out<-c(FALSE,FALSE,FALSE)
#   if(any(vars_to_return%in%"all")){
#     vars_to_return<-all_possible_vars
#     which_out[1:3]<-TRUE
#   }else{
#     if(any(vars_to_return%in%all_possible_vars[5:which(all_possible_vars==paste0("ar",root_shoot_soil$rmodules-1))])){
#       which_out[1]<-TRUE
#     }
#     else{
#       which_out[1]<-FALSE
#     }
#     if(any(vars_to_return%in%all_possible_vars[(1+which(all_possible_vars==paste0("ar",root_shoot_soil$rmodules-1))):length(all_possible_vars)])){
#       which_out[2]<-TRUE
#     }else{
#       which_out[2]<-FALSE
#     }
#   }
#
#   ###<
#   if(inherits(drivers,"data.frame")){
#     drivers<-list(dr_1=drivers)
#   }
#   drvr_chk<-0
#   for(drls in drivers){
#
#     drvr_chk<-drvr_chk+ValidateDriver(drls,verbosity)
#   }
#   if(drvr_chk>0){
#     warning("Bad drivers detected. Use ValidateDriver() to test drivers.")
#     stop()
#   }
#   # Verify inputs #
#   if(
#     is.data.frame(base_parameters)&
#     is.list(drivers)&
#     is.list(root_shoot_soil)&
#     is.data.frame(new_parameters)&
#     is.vector(vars_to_return)&
#     (is.numeric(ncores)|is.integer(ncores))&
#     (is.numeric(interval_to_return)|is.integer(interval_to_return))&
#     is.logical(aggregate_time_steps)&
#     # (is.numeric(save_step_interval)|is.integer(save_step_interval))&
#     is.logical(verbosity)
#   ){
#     #continue
#   }else{
#     bad_inputs<-c(
#       is.data.frame(base_parameters),
#       is.list(drivers),
#       is.list(root_shoot_soil),
#       is.data.frame(new_parameters),
#       is.vector(vars_to_return),
#       (is.numeric(ncores)|is.integer(ncores)),
#       (is.numeric(interval_to_return)|is.integer(interval_to_return)),
#       is.logical(aggregate_time_steps),
#       (is.numeric(save_step_interval)|is.integer(save_step_interval)),
#       is.logical(verbosity)
#     )
#     message(bad_inputs)
#     stop()
#   }
#
#   vars_to_return<-c("year","jday","hour","min",vars_to_return)
#   all_paramter_sets<-lapply(
#     1:nrow(new_parameters),
#     function(ro) {
#       list(
#         set_id = ro,
#         parameters = rTREES::ChangeParameter(
#           base_parameters,
#           new_parameters[ro,,drop=FALSE],
#           VERBOSE = FALSE
#           )
#         )
#       }
#     )
#
#   if(
#     any(sapply(all_paramter_sets, function(x) 1==x[["parameters"]][["Value"]][param_idx("useLeafModule")]))
#   ){
#     which_out[3]<-TRUE
#   }else{
#     which_out[3]<-FALSE
#   }
#
#   #determine longest driver and calibrate off that:
#   lngst_dr<-drivers[[1]]
#   for(ld in drivers){
#     if(nrow(ld)>nrow(lngst_dr)){
#       lngst_dr<-ld
#     }
#   }
#   time_limit<-0
#   if(timeout_scalar==0){
#     time_limit<-0
#   }else{
#     timeout_coef<-timeout_calibration(
#       lngst_dr,
#       base_parameters,
#       root_shoot_soil
#     )
#     time_limit<-(timeout_coef[1]+nrow(lngst_dr)*timeout_coef[2])*timeout_scalar
#   }
#
#   if(verbosity){
#     message("Begin parallel run.")
#   }
#
#   # alldp<-tidyr::expand_grid(drs=1:length(drivers),prs=1:length(all_paramter_sets)) %>%
#   #   dplyr::mutate(dp=paste(drs,prs,sep = "@"))
#
#   p <- progressr::progressor(steps = as.integer(length(drivers)*length(all_paramter_sets)) )
#
#   run_res<-foreach::foreach(
#     dr=drivers,#1:length(drivers),
#     .options.future = list(
#       seed = TRUE,
#       globals = structure(TRUE, add = c("verbosity","root_shoot_soil","vars_to_return","aggregate_time_steps","which_out","interval_to_return","time_limit"))
#       )
#     )%:%
#     foreach::foreach(
#       ru=all_paramter_sets#1:length(all_paramter_sets)
#       )%dofuture%{
#       rtrees_sim<-NA
#       rtrees_hyd<-NA
#       rtrees_leaf<-NA
#       if(time_limit>0){
#         rtrees_results<-NA
#         tryCatch({
#           # Set a time limit
#           # setTimeLimit(elapsed = time_limit, transient = TRUE)
#           setTimeLimit(time_limit, transient = TRUE)
#           # Perform the task (this is where your actual computation will go)
#           rtrees_results<-rTREES::rTREES(
#             env_driver = dr, #drivers[[dr]],
#             base_parameters =ru["parameters"],
#             root_shoot_soil = root_shoot_soil,
#             which_out=which_out,
#             verbosity=verbosity
#           )%>%
#             Clean_rTREES_output(driver=dr)
#
#         }, error = function(e) {
#           # Handle timeout error (or any other error)
#           message(paste("ERROR or TIMED OUT! If timed out, see hyd for driver and leaf for parameters.", e$message))
#         }, finally = {
#           # Reset the time limit after each iteration
#         })
#       }else{
#         rtrees_results<-rTREES::rTREES(
#           env_driver = dr, #drivers[[dr]],
#           base_parameters =ru["parameters"],
#           root_shoot_soil = root_shoot_soil,
#           which_out=which_out,
#           verbosity=verbosity
#         )%>%
#           Clean_rTREES_output(driver=dr)
#       }
#
#       if(time_limit>0&&length(rtrees_results)==1&&is.na(rtrees_results)){
#         rtrees_sim<-"TIMED OUT! See hyd for driver and leaf for parameters."
#         rtrees_hyd<-dr
#         rtrees_leaf<-ru
#         which_out<-c(FALSE,FALSE,FALSE)
#       }
#
#       p("running...")
#
#       if(aggregate_time_steps){
#         if(which_out[1]&&inherits(rtrees_results[["sim"]],"data.frame")){
#           rtrees_sim<-rtrees_results[["sim"]][,] %>%
#             dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#             dplyr::summarize(dplyr::across(dplyr::any_of(vars_to_return),~mean(.x,na.rm=TRUE))) %>%
#             as.data.frame()
#         }
#         if(which_out[2]&&inherits(rtrees_results[["hyd"]],"data.frame")){
#           rtrees_hyd<-rtrees_results[["hyd"]][,] %>%
#             dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#             dplyr::summarize(dplyr::across(dplyr::any_of(vars_to_return),~mean(.x,na.rm=TRUE))) %>%
#             as.data.frame()
#         }
#         if(which_out[3]&&inherits(rtrees_results[["leaf"]],"data.frame")){
#           rtrees_leaf<-rtrees_results[["leaf"]][,colSums(rtrees_results[["leaf"]]) != 0] %>%
#             dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#             dplyr::summarize(dplyr::across(dplyr::everything(),~mean(.x,na.rm=TRUE))) %>%
#             as.data.frame()
#         }
#
#       }else{
#         if(which_out[1]&&inherits(rtrees_results[["sim"]],"data.frame")&&!"none"%in%names(rtrees_results[["sim"]])){
#           rtrees_sim<-rtrees_results[["sim"]] %>%
#             dplyr::select(dplyr::any_of(vars_to_return)) %>%
#             dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#         }
#         if(which_out[2]&&inherits(rtrees_results[["hyd"]],"data.frame")&&!"none"%in%names(rtrees_results[["hyd"]])){
#           rtrees_hyd<-rtrees_results[["hyd"]] %>%
#             dplyr::select(dplyr::any_of(vars_to_return)) %>%
#             dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#         }
#         if(which_out[3]&&inherits(rtrees_results[["leaf"]],"data.frame")&&!"none"%in%names(rtrees_results[["leaf"]])){
#           rtrees_leaf<-rtrees_results[["leaf"]][,colSums(rtrees_results[["leaf"]]) != 0]%>%
#             dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#         }
#
#       }
#       return(
#         list(
#           sim=rtrees_sim,
#           hyd=rtrees_hyd,
#           leaf=rtrees_leaf
#         )
#       )
#     }
#
#   if(is.null(names(drivers))){
#     name_vec<-c()
#     for(i in 1:length(drivers)){
#       name_vec[i]<-paste0("driver-",i)
#     }
#     names(drivers)<-name_vec
#   }
#
#   names(run_res)<-names(drivers)
#
#   for(i in 1:length(drivers)){
#     dr_set_names<-c()
#     for(j in 1:nrow(new_parameters)){
#       set_name<-c()
#       for(n in 1:ncol(new_parameters)){
#         set_name<-c(
#           set_name,
#           names(new_parameters)[n],
#           gsub("e\\+", "*10^", format(new_parameters[j,n],scientific=TRUE,digits=4)
#           )
#         )
#       }
#       set_name<-paste(set_name,collapse="@")
#       dr_set_names[j]<-set_name
#     }
#     names(run_res[[i]])<-dr_set_names
#
#   }
#   return(run_res)
#
# }
# rTREES_parallel_run_old<-function(
    #     base_parameters=parameters,
#     drivers=driver,
#     root_shoot_soil=root_shoot_soil,
#     new_parameters= data.frame(
#       leafAreaMax=1:10,
#       leafArea_Rate=1:10,
#       microbiomeScalar=1:10
#     ),
#     vars_to_return="all",
#     ncores=parallel::detectCores()-2,
#     interval_to_return=12,
#     aggregate_time_steps=FALSE,
#     save_step_interval=0,
#     verbosity=FALSE,
#     softerror_max=5
# ){
#   interval_to_return<-interval_to_return*2
#   if(ncores>parallel::detectCores()){
#     warning("Available cores exceeded. Dropping to maximum detected cores.",immediate. = TRUE)
#     ncores<-parallel::detectCores()
#   }
#   # ncores<-parallel::detectCores()-2
#   doParallel::registerDoParallel(cores=ncores)
#   all_possible_vars<-Possible_Outputs(n_layers=root_shoot_soil$rmodules)
#
#   ###> determine if some outputs should be ignored
#   which_out<-c(FALSE,FALSE,FALSE)
#   if(any(vars_to_return%in%"all")){
#     vars_to_return<-all_possible_vars
#     which_out[1:3]<-TRUE
#   }else{
#     if(any(vars_to_return%in%all_possible_vars[5:which(all_possible_vars==paste0("ar",root_shoot_soil$rmodules-1))])){
#         which_out[1]<-TRUE
#       }
#       else{
#         which_out[1]<-FALSE
#       }
#       if(any(vars_to_return%in%all_possible_vars[(1+which(all_possible_vars==paste0("ar",root_shoot_soil$rmodules-1))):length(all_possible_vars)])){
#         which_out[2]<-TRUE
#       }else{
#         which_out[2]<-FALSE
#       }
#   }
#
#   ###<
#   if(inherits(drivers,"data.frame")){
#     drivers<-list(dr_1=drivers)
#   }
#   drvr_chk<-0
#   for(drls in drivers){
#
#     drvr_chk<-drvr_chk+ValidateDriver(drls)
#   }
#   if(drvr_chk>0){
#     message("Bad drivers detected. Use ValidateDriver() to test drivers.")
#     stop()
#   }
#   # Verify inputs #
#   if(
#     is.data.frame(base_parameters)&
#     is.list(drivers)&
#     is.list(root_shoot_soil)&
#     is.data.frame(new_parameters)&
#     is.vector(vars_to_return)&
#     (is.numeric(ncores)|is.integer(ncores))&
#     (is.numeric(interval_to_return)|is.integer(interval_to_return))&
#     is.logical(aggregate_time_steps)&
#     (is.numeric(save_step_interval)|is.integer(save_step_interval))&
#     is.logical(verbosity)
#   ){
#     #continue
#   }else{
#     bad_inputs<-c(
#       is.data.frame(base_parameters),
#         is.list(drivers),
#         is.list(root_shoot_soil),
#         is.data.frame(new_parameters),
#         is.vector(vars_to_return),
#         (is.numeric(ncores)|is.integer(ncores)),
#         (is.numeric(interval_to_return)|is.integer(interval_to_return)),
#         is.logical(aggregate_time_steps),
#         (is.numeric(save_step_interval)|is.integer(save_step_interval)),
#         is.logical(verbosity)
#     )
#     message(bad_inputs)
#     stop()
#   }
#
#   vars_to_return<-c("year","jday","hour","min",vars_to_return)
#   all_paramter_sets<-lapply(1:nrow(new_parameters), function(ro) {
#     list(set_id = ro, parameters = rTREES::ChangeParameter(base_parameters, new_parameters[ro,,drop=FALSE],VERBOSE = FALSE))
#   })
#
#   if(
#     any(sapply(all_paramter_sets, function(x) 1==x[["parameters"]][["Value"]][param_idx("useLeafModule")]))
#     ){
#     which_out[3]<-TRUE
#   }else{
#     which_out[3]<-FALSE
#   }
#
#   message("Begin parallel run.")
#
#   ##### This chunk should allow saving intermediate steps
#   #currently it breaks it up by the longer of driver and parameters into steps set with save_step_interval. It will then save the results to a uniquely named object in your working directory and continue on.
#
#   #broken at the max thing
#   if(FALSE){#save_step_interval>0){
#     full_drivers<-drivers
#     full_p<-all_paramter_sets
#     full_run_res<-list()
#     if(length(drivers)>length(all_paramter_sets)){
#       by_dr<-TRUE
#     }else{
#       by_dr<-FALSE
#     }
#     stepping<-TRUE
#     on_step_idx=1
#     while(stepping){
#       if(by_dr){
#         drivers<-full_drivers[on_step_idx:max(on_step_idx+save_step_interval,length(drivers))]
#         on_step_idx<-max(on_step_idx+save_step_interval,length(drivers))
#         if(on_step_idx==max(drivers)){
#           stepping<-FALSE
#         }
#
#       }else{
#         all_paramter_sets<-full_p[on_step_idx:max(on_step_idx+save_step_interval,length(full_p))]
#         on_step_idx<-max(on_step_idx+save_step_interval,length(full_p))
#         if(on_step_idx==max(all_paramter_sets)){
#           stepping<-FALSE
#         }
#       }
#       run_res<-foreach::foreach(dr=1:length(drivers))%:%
#         foreach::foreach(ru=1:length(all_paramter_sets))%dopar%{
#           rtrees_sim<-NA
#           rtrees_hyd<-NA
#           rtrees_leaf<-NA
#           rtrees_results<-rTREES::rTREES(
#             env_driver =  drivers[[dr]],
#             base_parameters = all_paramter_sets[[ru]]["parameters"],
#             root_shoot_soil = root_shoot_soil,
#             which_out = which_out,
#             verbosity=verbosity
#           ) %>% Clean_rTREES_output(driver=drivers[[dr]])
#
#           if(aggregate_time_steps){
#             if(which_out[1]){
#               rtrees_sim<-rtrees_results[["sim"]][,] %>%
#               dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#               dplyr::summarize(dplyr::across(dplyr::any_of(vars_to_return),~mean(.x,na.rm=TRUE))) %>%
#               as.data.frame()
#             }
#             if(which_out[2]){
#               rtrees_hyd<-rtrees_results[["hyd"]][,] %>%
#                 dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#               dplyr::summarize(dplyr::across(dplyr::any_of(vars_to_return),~mean(.x,na.rm=TRUE))) %>%
#               as.data.frame()
#             }
#             # useLeafModule
#             if(which_out[3]){
#               rtrees_leaf<-rtrees_results[["leaf"]][,colSums(rtrees_results[["leaf"]]) != 0] %>%
#                 dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#                 dplyr::summarize(dplyr::across(dplyr::everything(),~mean(.x,na.rm=TRUE))) %>%
#               as.data.frame()
#             }
#
#           }else{
#             if(which_out[1]){
#               rtrees_sim<-rtrees_results[["sim"]] %>%
#                 dplyr::select(dplyr::any_of(vars_to_return)) %>%
#                 dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#             }
#             if(which_out[2]){
#               rtrees_hyd<-rtrees_results[["hyd"]] %>%
#                 dplyr::select(dplyr::any_of(vars_to_return)) %>%
#                 dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#             }
#             # useLeafModule
#             if(which_out[3]){
#               rtrees_leaf<-rtrees_results[["leaf"]][,colSums(rtrees_results[["leaf"]]) != 0]%>%
#                 dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#             }
#
#
#           }
#           return(
#             list(
#               # driver_id=dr,
#               # set_id=all_parameter_sets[[ru]]["set_id"],
#               sim=rtrees_sim,
#               hyd=rtrees_hyd,
#               leaf=rtrees_leaf
#             )
#           )
#         }
#
#
#       if(is.null(names(drivers))){
#         name_vec<-c()
#         for(i in 1:length(drivers)){
#           name_vec[i]<-paste0("driver-",i)
#         }
#         names(drivers)<-name_vec
#       }
#
#       names(run_res)<-names(drivers)
#
#
#       for(i in 1:length(drivers)){
#         dr_set_names<-c()
#         for(j in 1:nrow(new_parameters)){
#           set_name<-c()
#           for(n in 1:ncol(new_parameters)){
#             set_name<-c(
#               set_name,
#               names(new_parameters)[n],
#               gsub("e\\+", "*10^", format(new_parameters[j,n],scientific=TRUE,digits=4)
#               )
#             )
#           }
#           set_name<-paste(set_name,collapse="@")
#           dr_set_names[j]<-set_name
#         }
#         names(run_res[[i]])<-dr_set_names
#       }
#
#       full_run_res<-append(full_run_res,run_res)
#       save(full_run_res,file="latest_step.Rdata")
#       message("finished a step and saved current results.")
#
#     }
#     run_res<-full_run_res
#   }else{
#     #This is the standard run
#     # message(paste0("using ",length(drivers)," and ",length(all_paramter_sets)))
#   run_res<-foreach::foreach(dr=1:length(drivers))%:%
#     foreach::foreach(ru=1:length(all_paramter_sets))%dopar%{
#       rtrees_sim<-NA
#       rtrees_hyd<-NA
#       rtrees_leaf<-NA
#
#       rtrees_results<-rTREES::rTREES(
#         env_driver =  drivers[[dr]],
#         base_parameters = all_paramter_sets[[ru]]["parameters"],
#         root_shoot_soil = root_shoot_soil,
#         which_out=which_out,
#         verbosity=verbosity
#       ) %>% Clean_rTREES_output(driver=drivers[[dr]])
#
#       if(aggregate_time_steps){
#         if(which_out[1]){
#           rtrees_sim<-rtrees_results[["sim"]][,] %>%
#             dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#             dplyr::summarize(dplyr::across(dplyr::any_of(vars_to_return),~mean(.x,na.rm=TRUE))) %>%
#             as.data.frame()
#         }
#         if(which_out[2]){
#           rtrees_hyd<-rtrees_results[["hyd"]][,] %>%
#             dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#             dplyr::summarize(dplyr::across(dplyr::any_of(vars_to_return),~mean(.x,na.rm=TRUE))) %>%
#             as.data.frame()
#         }
#         if(which_out[3]){
#           rtrees_leaf<-rtrees_results[["leaf"]][,colSums(rtrees_results[["leaf"]]) != 0] %>%
#             dplyr::group_by(group = (dplyr::row_number() - 1) %/% interval_to_return) %>%
#             dplyr::summarize(dplyr::across(dplyr::everything(),~mean(.x,na.rm=TRUE))) %>%
#             as.data.frame()
#         }
#
#       }else{
#         if(which_out[1]){
#           rtrees_sim<-rtrees_results[["sim"]] %>%
#             dplyr::select(dplyr::any_of(vars_to_return)) %>%
#             dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#         }
#         if(which_out[2]){
#
#           rtrees_hyd<-rtrees_results[["hyd"]] %>%
#             dplyr::select(dplyr::any_of(vars_to_return)) %>%
#             dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#         }
#         if(which_out[3]){
#
#           rtrees_leaf<-rtrees_results[["leaf"]][,colSums(rtrees_results[["leaf"]]) != 0]%>%
#             dplyr::filter((dplyr::row_number()-1)%%interval_to_return==0)
#         }
#
#       }
#       return(
#         list(
#           sim=rtrees_sim,
#           hyd=rtrees_hyd,
#           leaf=rtrees_leaf
#         )
#       )
#     }
#
#   if(is.null(names(drivers))){
#     name_vec<-c()
#     for(i in 1:length(drivers)){
#       name_vec[i]<-paste0("driver-",i)
#     }
#     names(drivers)<-name_vec
#   }
#
#   names(run_res)<-names(drivers)
#
#   for(i in 1:length(drivers)){
#     dr_set_names<-c()
#     for(j in 1:nrow(new_parameters)){
#       set_name<-c()
#       for(n in 1:ncol(new_parameters)){
#         set_name<-c(
#           set_name,
#           names(new_parameters)[n],
#           gsub("e\\+", "*10^", format(new_parameters[j,n],scientific=TRUE,digits=4)
#                )
#           )
#       }
#       set_name<-paste(set_name,collapse="@")
#       dr_set_names[j]<-set_name
#     }
#     names(run_res[[i]])<-dr_set_names
#   }
# }
#   return(run_res)
#
# }



#   Change Root Shoot parameters
#
#  param root_shoot_soil Data frame structured ... or list of vectors for each element in each layer
#
#   return Currently not finished but should be a list with the type seen in the comments of source
#  export
# ChangeRootShootParams<-function(
    #     root_shoot_soil=NULL
# ){
#
#   if(is.data.frame(root_shoot_soil)&nrow(root_shoot_soil)>7){
#
#   }else if(is.character(root_shoot_soil)){
#     root_shoot_soil<-data.table::fread(root_shoot_soil,data.table = FALSE)
#   }else{
#     # stop("Invalid 'root_shoot_soil' supplied. Please supply either: A full file name '(with path)' or a data frame. Both are required to have all inputs required by TREES")
#
#   }
#
#   # n_shoot_modules<-root_shoot_soil[1,2]#"#_of_shoot_modules"
#   # n_root_modules<-root_shoot_soil[n_shoot_modules*3+2,2]#"#_of_root_modules"
#   #
#   #
#   Root_Shoot_Soil<-root_shoot_soil
#   # for(i in 1:n_shoot_modules){
#   #   Root_Shoot_Soil[i+1,2]<-root_shoot_soil[i+1,2]
#   #   Root_Shoot_Soil[i+2,2]<-root_shoot_soil[i+2,2]
#   #   Root_Shoot_Soil[i+3,3]<-root_shoot_soil[i+3,3]
#   #
#   #   # Root_Shoot_Soil<-rbind(Root_Shoot_Soil,
#   #   #                  paste("leaf_area_fraction ",root_shoot_soil[i,"la_frac"]),
#   #   #                  paste("length_lateral ",root_shoot_soil[i,"length_lateral"]),
#   #   #                  paste("lenght_axial ",root_shoot_soil[i,"lenght_axial"])
#   #   #                  )
#   # }
#   #
#   #
#   # Root_Shoot_Soil<-rbind(Root_Shoot_Soil, paste("#_of_root_modules ",n_root_modules))
#   # for(j in 1:n_root_modules){
#   #   row_shift<-j*n_shoot_modules*3+3
#   #
#   #   Root_Shoot_Soil[row_shift,2]<-root_shoot_soil[row_shift,2]
#   #   Root_Shoot_Soil[row_shift+1,2]<-root_shoot_soil[row_shift+1,2]
#   #   Root_Shoot_Soil[row_shift+2,2]<-root_shoot_soil[row_shift+2,2]
#   #
#   #   # Root_Shoot_Soil<-rbind(Root_Shoot_Soil,
#   #   #                  paste("leaf_area_fraction ",root_shoot_soil[row_shift,"la_frac"]),
#   #   #                  paste("length_lateral ",root_shoot_soil[row_shift,"length_lateral"]),
#   #   #                  paste("lenght_axial ",root_shoot_soil[row_shift,"lenght_axial"])
#   #   # )
#   # }
#
#
#     return(Root_Shoot_Soil)
#
#
# }
#
#
# # Prepare Plot for SIM and HYD results. !!!-Deprecated in favor of long_df_sim_parallel and long_df_hyd_parallel-!!!
# # importFrom tidyr pivot_longer
# # param simulation_results Raw output from rTREES or rTREES_parallel_run
# # param which_sim A vector of character strings for each name of the sim of hyd results you would like plotted.
# # param compare_to A wide format dataframe with column names matching which_sim or a long format dataframe with a column named Vars which corresponds to which_sim. Must also contain a column called datetime formatted as POSIXct. Run ?as.POSIXct() for information on formatting.
# # #param plot_type currently accepts "point", "line", "smooth", "boxplot", and "violin". Boxplot and violin use geom_point when there is only one value at each datetime point.
# #
# # @return Combines, filters, and transforms to long format results and optional external data.
# # @export
# preplotSIMHYD<-function(
    #     simulation_results=list(),
#     which_sim="all",
#     soil_layers=4,
#     compare_to=NA#,
#     # plot_type="point"
#     ){
#
#
#   if("all"%in%which_sim){
#     which_sim<-Possible_Outputs(soil_layers)
#   }
#
#   if(all(which_sim%notin%Possible_Outputs(soil_layers))){
#     stop("Incorrectly specified variable in which_sim")
#   }
#   sim_long<-data.frame()
#   if(all(names(simulation_results)%in%c("sim","hyd","leaf"))){
#     simulation_results<-list(driver0=list(simulation=simulation_results))
#     # sim_long<-simulation_results %>%
#     #   dplyr::mutate(datetime=as.POSIXct(x = paste(year,jday,hour,min(min*100,1)*30,sep = "-"),format="%Y-%j-%H-%M")) %>%
#     #   dplyr::select(-year,-jday,-hour,-min) %>%
#     #   tidyr::pivot_longer(cols=-datetime,names_to = "Vars",values_to = "Vals") %>%
#     #   dplyr::filter(Vars%in%which_sim)%>%
#     #   dplyr::mutate(source="simulation")
#   }#else if(inherits(simulation_results[[1]],what = "list")){
#
#   for(i in 1:length(simulation_results)){
#     # ic(i)
#     for(j in 1:length(simulation_results[[i]])){
#       # ic(j)
#       sim_long<-rbind(
#         simulation_results[[i]][[j]]$sim %>%
#         dplyr::mutate(datetime=as.POSIXct(x = paste(year,jday,hour,min(min*100,1)*30,sep = "-"),format="%Y-%j-%H-%M")) %>%
#         dplyr::select(-year,-jday,-hour,-min) %>%
#         tidyr::pivot_longer(cols=-datetime,names_to = "Vars",values_to = "Vals") %>%
#         dplyr::filter(Vars%in%which_sim),
#         simulation_results[[i]][[j]]$hyd %>%
#           dplyr::mutate(datetime=as.POSIXct(x = paste(year,jday,hour,min(min*100,1)*30,sep = "-"),format="%Y-%j-%H-%M")) %>%
#           dplyr::select(-year,-jday,-hour,-min) %>%
#           tidyr::pivot_longer(cols=-datetime,names_to = "Vars",values_to = "Vals") %>%
#           dplyr::filter(Vars%in%which_sim)
#       )%>%
#         dplyr::mutate(source=paste(names(simulation_results)[i],names(simulation_results[[i]])[j],sep = ":\n")) %>%
#         rbind(sim_long)
#       # ic(lobstr::tree(sim_long))
#     }
#     # }
#     #I need to check if this is the raw output from rTREES and run it through
#     #clean function or see if it is from rTREES_parallel and clean it up here.
#     #if it is from parallel, then all I need to do is make simulation=paste(simulation,newparams)
#     #and everything should work.
#   }
#
#
#
#   if(is.list(compare_to)){
#     if(inherits(compare_to,what = "data.frame")){
#       compare_to<-list(compare0=compare_to)
#     }
#     if(is.null(names(compare_to))){
#       name_vec<-c()
#       for(i in 1:length(compare_to)){
#         name_vec[i]<-paste0("compare-",i)
#       }
#       names(compare_to)<-name_vec
#     }
#     all_compare<-data.frame()
#     for(i in 1:length(compare_to)){
#       if("Vars"%in%names(compare_to[[i]])){
#         match_compare<-compare_to[[i]] %>%
#           dplyr::filter(Vars%in%which_sim) %>%
#           dplyr::select(datetime,Vars,Vals) %>%
#           dplyr::mutate(source=names(compare_to)[i])
#         }else if(any(which_sim%in%names(compare_to[[i]]))){
#           match_compare<-compare_to[[i]] %>%
#             dplyr::select(datetime,any_of(which_sim)) %>%
#             tidyr::pivot_longer(cols=-datetime,names_to = "Vars",values_to = "Vals") %>%
#             dplyr::mutate(source=names(compare_to)[i])
#           }else{
#             stop("No matching variables in compare_to")
#           }
#       all_compare<-rbind(all_compare,match_compare)
#       }
#
#     sim_long<-rbind(sim_long,all_compare)
#   }
#
#
#   # plot_out<-ggplot2::ggplot(data = sim_long,aes(x=datetime,y=Vals,color=source))
# #   if(plot_type=="point"){
# #     plot_out<-plot_out+
# #       geom_point()
# #   }else if(plot_type=="line"){
# #     plot_out<-plot_out+
# #       geom_line()
# #   }else if(plot_type=="smooth"){
# #     plot_out<-plot_out+
# #       geom_smooth()
# #   }else if(plot_type=="boxplot"){
# #
# # #FIX plots to take in new data. If the (unique(sim_data$source>1 it is multitudes))
# #     plot_out<-ggplot2::ggplot(mapping=aes(x=datetime,y=Vals,group=datetime,color=source))+
# #       geom_boxplot(data=match_compare)+
# #       geom_point(data=sim_long_b)
# #
# #   }else if(plot_type=="violin"){
# #
# #     sim_long_b<-sim_long %>%
# #       dplyr::filter(source=="simulation")
# #     plot_out<-ggplot2::ggplot(mapping=aes(x=datetime,y=Vals,group=datetime,color=source))+
# #       geom_violin(data=match_compare)+
# #       geom_point(data=sim_long_b)
# #
# #   }else{
# #     stop("incorrect plot type specified")
# #   }
# #
# #   plot_out<-plot_out+
# #     ggplot2::facet_wrap(~Vars,scales = "free")
#
#   return(sim_long)
# }
#
#
