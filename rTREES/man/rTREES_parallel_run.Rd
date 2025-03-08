% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helper_functions.R
\name{rTREES_parallel_run}
\alias{rTREES_parallel_run}
\title{rTREES parallel run
Takes a data frame with each column being a parameter (run ExampleParameters() to see proper names)
and each row is a simulation to run with those parameters and runs rTREES with those parameters. These
parameter sets are run in parallel and a list of run results are returned in the same order as the
data frame.}
\usage{
rTREES_parallel_run(
  base_parameters = parameters,
  drivers = driver,
  root_shoot_soil = root_shoot_soil,
  new_parameters = data.frame(leafAreaMax = 1:10, leafArea_Rate = 1:10, microbiomeScalar
    = 1:10),
  vars_to_return = "all",
  ncores = rTREES::number_of_cores() - 2,
  interval_to_return = 12,
  aggregate_time_steps = FALSE,
  verbosity = FALSE,
  softerror_max = 5,
  timeout_scalar = 5,
  debug = FALSE
)
}
\arguments{
\item{base_parameters}{Initial parameters. Using Load_TREES_files() is recommended}

\item{drivers}{Environmental driver. Using Load_TREES_files() is recommended}

\item{root_shoot_soil}{Initial root_shoot_soil. Using Load_TREES_files() is recommended}

\item{new_parameters}{parameters to be run. rTREES will be run over each row.}

\item{vars_to_return}{rTREES outputs you want to have returned. Using 'all' is allowed but not recommend. See Possible_Outputs() for options.}

\item{ncores}{The number of cores you would like jobs to be split among. It is recommended that you keep a couple cores free for your system to retain functionality while running simulations.}

\item{interval_to_return}{Time interval to be returned. 0.5 is half hourly, 1 is hourly, 12 is twice daily, 24 is daily, etc..}

\item{aggregate_time_steps}{If TRUE, the time steps will be averaged around the interval_to_return steps.}

\item{verbosity}{Allow TREES to generate output to job_stdout.}

\item{softerror_max}{How many failures to converge is TREES allowed within 24 hours.}

\item{timeout_scalar}{Multiplyer for calibrated timeout. e.x. 2 would set the timeout to two times the calibrated estimate.}

\item{debug}{When TRUE retain information from callr run. This includes, errors, exit conditions, and more. If rTREES verbosity is also TRUE, that will be stored in the job_stdout element. Use cat() when trying to view job_stdout.}
}
\value{
list of results where each element of the list corresponds to a row of parameters in new_parameters
}
\description{
rTREES parallel run
Takes a data frame with each column being a parameter (run ExampleParameters() to see proper names)
and each row is a simulation to run with those parameters and runs rTREES with those parameters. These
parameter sets are run in parallel and a list of run results are returned in the same order as the
data frame.
}
\examples{
demo_parallel_run<-rTREES_parallel_run(
  base_parameters=ExampleParameters(),
  drivers=BuildDriver(ndays = 10),
  root_shoot_soil=ExampleRSS(),
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
  debug=FALSE
)
}
