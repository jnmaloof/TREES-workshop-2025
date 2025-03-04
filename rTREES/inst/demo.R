library(rTREES)
#exists(quote(Gsref0))


# Driver_inputs
## This is just for explaining the Driver inputs.
View(Driver_inputs())


# BuildDriver
demo_driver<-BuildDriver(
  lat = 35.818,
  long = -106.305,
  alt = 2150.0,
  ndays = 50,
  year = 2023,
  start_jday=1,
  dewpoint = 12,
  co2_atm=410.5,
  wind_speed=1.01,
  precipdays = c(20, 30, 50, 80, 90, 110, 140, 150, 200, 210, 290),
  precip_amount = 6.25,
  temp_scalar=1
)

# ExampleParameters
demo_parameters<-ExampleParameters()

# ExampleRSS
demo_RSS<-ExampleRSS()




# param_search
## Use this function when you are unsure of the exact name of a parameter.
param_search(
  query = "soil",
  n_return = 10
)

# param_idx
## Use this function to find the index of a parameter
param_idx(
  parameter_name = "initial_conductivity_root"
)
demo_parameters[78,]


# ChangeParameter
## This isn't meant to be called directly. rTREES_parallel_run uses this function internally to create parameter sets for each run.
demo_new_parameters<-ChangeParameter(
  base_parameters=demo_parameters,
  NewValues=data.frame(
    "initial_conductivity_root"=25,
    "Gsref0"=0.1
  ),
  VERBOSE=TRUE
)

# ChangeRootShootParams
## Not yet implemented nor necessary at the moment since lists are simple enough to work with directly and it would be rare to only change a small part of the RSS inputs.


# Load_TREES_files
demo_inputs_from_file<-Load_TREES_files(
  driver="inst/mal_driver.txt",
  parameters = "inst/mal_parameter.p",
  root_shoot_soil = "inst/mal_root_shoot_soil"
)

## I used demo_new_parameters to make the mal_parameter.p by saving it and then manually deleting parts of the file. These names are the parameters that will need manual corrections. The following functions demonstrate how rTREES attempts to read and repair the file.

# ValidateParameters
## This is what Load_TREES_files calls internally to load and clean parameters
demo_input_p_from_file<-ValidateParameters("inst/mal_parameter.p")

# ReformRawP
## This is what ValidateParameters calls internally to reformat the raw text of the parameter file into a form easier to work with.
demo_raw_p<-ReformRawP(
  dirty_vector =readLines(
  con = "inst/mal_parameter.p")
  )

# attempt_p_repair
## This is how ValidateParameters repairs bad inputs
demo_rep_p_from_file<-attempt_p_repair(
  params_in = demo_new_parameters[sample(1:174,170,replace = F),]#example of missing rows in an incorrect order
)
demo_rep_p_from_file[[1]]$Parameter_Name[!(demo_new_parameters$Value==demo_inputs_from_file$parameters$Value)]
demo_rep_p_from_file[[1]]$Parameter_Name[which(demo_rep_p_from_file[[1]]$Value==999999)]#999999 is the value assigned to a broken or missing value.
demo_rep_p_from_file[[1]]$Parameter_Name[!(demo_rep_p_from_file[[1]]$Value==demo_new_parameters$Value)]
demo_rep_p_from_file[[1]]$Parameter_Name[!(demo_rep_p_from_file[[1]]$Value==demo_inputs_from_file$parameters$Value)]



# CleanRootShootParams
demo_rss_from_file<-CleanRootShootParams(
  root_shoot_soil = system.file("examples","demo_root_shoot_soil",package="rTREES")
  )
ValidateRootShootSoil(
  root_shoot_soil = system.file("examples","demo_root_shoot_soil",package="rTREES")
)

# ValidateRootShootSoil
ValidateRootShootSoil(
  root_shoot_soil = demo_rss_from_file
  )


# ValidateDriver
ValidateDriver(Driver = "inst/mal_driver.txt",verbosity = T)

# ImputeDriver
demo_bad_driver<-demo_inputs_from_file$driver
demo_fixed_driver<-attempt_driver_repair(demo_bad_driver)


# Visual_driver
Visual_driver(driver = demo_fixed_driver)
Visual_driver(driver = demo_driver,dailys = F)

bdr<-BuildDriver()
bdr[matrix(sample(x = c(T,F),size = nrow(bdr)*ncol(bdr),prob = c(0.01,0.99),replace = T), nrow = nrow(bdr),ncol = ncol(bdr))]<- NA ## using matrix indexing
View(bdr)


bdp<-ExampleParameters()

# %>%
## This is just to ease import of the pipe


# %notin%
c("bananas")%notin%c("apple","cat","music")









# tiny_tokenizer
tiny_tokenizer(
  text_string = "Some example text",
  token_length = 3
)
# cosine_sim
cosine_sim(
  text1="the root carbon",
  text2="root nitrogen",
  token_length=6,
  relative_corpus_table=table(tiny_tokenizer("A collection of text to represent the expected distribution of tokens before evaluating the text of interest to avoid overweighing common tokens such as articles."))
)

# depth
demo_list<-list(
  layer_1=list(
    layer_2=list(
      layer_3=list(
        layer_4=42
      )
    )
  ),
  no_change=list(
    still_no_change=list(
      almost_change=list(
        same_depth=list(
          now_new_depth=43
        )
      )
    )
  )
)
depth(
  this=demo_list
)


# ez_dt
ez_dt(
  year=2025,
  jday=10,
  hour=14,
  min=30,
  tz="+0000"
)

# lnorm_to_norm
lnorm_to_norm(
  lm=2,
  ls=2
)

# norm_to_lnorm
norm_to_lnorm(
  m=2,
  s=2
)


# Write_TREES_files
# Write_TREES_files(
#   destination=getwd(),#include whole path if outside working dir
#   prefix="demo",
#   driver=NA,
#   parameter=NA,
#   rss=NA
# )

# Write_TREES_files(
#   destination="inst/",#include whole path if outside working dir
#   prefix="mal",
#   driver=demo_driver,
#   parameter=demo_new_parameters,
#   rss=demo_RSS
# )

# Write_TREES_files(
#   destination="inst/examples/",#include whole path if outside working dir
#   prefix="demo",
#   driver=BuildDriver(ndays=20),
#   parameter=ExampleParameters(),
#   rss=ExampleRSS()
# )

# number_of_cores
## This is mostly helpful for automatically picking the maximum number of cores available for parallel runs. WARNING it is highly recommended to keep at least two cores free to prevent your system from locking up.
number_of_cores()

# Possible_Outputs
## Because rTREES_parallel_run gives the option of filtering what is returned to reduce memory burden, this function was created to help list what outputs are available. n_layers refers to the number of soil layers from the Root_Shoot_Soil input.
demo_possible_outputs<-Possible_Outputs(n_layers = 4)


# rTREES
demo_standard_run<-rTREES(
  env_driver = demo_driver,
  base_parameters = demo_new_parameters,
  root_shoot_soil = demo_RSS
  )
# Clean_rTREES_output

# aggregate_and_filter
aggregate_and_filter()


# rTREES_parallel_run
demo_parallel_run<-rTREES_parallel_run(
  base_parameters=demo_parameters,
  drivers=demo_driver,
  root_shoot_soil=demo_RSS,
  new_parameters= data.frame(
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
  debug=F
)




# prep_rTREES_parallel
# rTREES_calib_function
# rTREES_par_finish
# rTREES_spooky_wrapper








# long_df_hyd_parallel
# long_df_leaf_parallel
# long_df_sim_parallel


# Var_unit



# spooky_parallel
# retry_spooky
