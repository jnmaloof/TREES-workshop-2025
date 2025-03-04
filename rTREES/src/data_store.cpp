//***********************************
//implementation of Data_Store object
//******************** SS 2002 ******

#include "data_store.h"


Data_Store::Data_Store()
{
  std::vector<std::vector<double> > values;
}

Data_Store::Data_Store(const Data_Store& dl)
{
//copy the dictionary
	int flag;
	flag = dictionary.copy_dictionary(dl.dictionary);
	assert(flag != 0);

	flag = time_series.copy_array(dl.time_series);
	assert(flag != 0);
//copy values
// Rcpp::Rcout<<"ds.23 flag"<<flag<<"\n";
	allocate();
	for (int i = 0; i < dictionary.n_vars(); i++)
	{
		for (int j = 0; j < time_series.n_steps(); j++)
		{
			values[i][j] = dl.values[i][j];
		}
	}
}

Data_Store::~Data_Store()
{
	clear_values();
}

void Data_Store::allocate()
{
//to be allocated for so many vars
	int nv = dictionary.n_vars();
	int tsteps = time_series.n_steps();
	assert(nv != 0 && tsteps != 0);
//release what is already there
	clear_values();
//now allocate
	// values = new double*[nv];

	values= std::vector<std::vector<double> >(nv, std::vector<double>(tsteps));

	assert(values.size() != 0 );
	// for (int i = 0; i < nv; i++)
	// {
	// 	values[i] = new double[tsteps];
	// 	assert(values[i] != NULL);
	// }
}

void Data_Store::clear_values()
{
        if (values.size() != 0)
        {
          values.clear();
                // int n = dictionary.n_vars();
                // for (int i = 0; i < n; i++)
                // {
                //         // if (values[i].size() != 0)
                //         // {
                //         //         values.emp;
                //         //         // values[i] = NULL;
                //         // }
                // }
                // delete[] values;
                // values = NULL;

        }
}


int Data_Store::get_dictionary(const Var_Dictionary &vd)
{
	return dictionary.copy_dictionary(vd);
}

int Data_Store::set_steps(int mstep)
{
	return time_series.allocate(mstep);
}

void Data_Store::allocate(const Var_Dictionary &vd, int mstep)
{
//make the dictionary
	assert(get_dictionary(vd) != 0);
  get_dictionary(vd);
  // Rcpp::Rcout<<"check dictionary "<<dictionary.n_vars()<<"\n";

//allocate the correct steps in time series
	assert(set_steps(mstep) != 0);
	set_steps(mstep);
	// Rcpp::Rcout<<"check time_series "<<time_series.n_steps()<<"\n";
//allocate the data array
	allocate();
}

int Data_Store::set_time(const Time& t, int st)
{
	return time_series.set_time(t,st);
}

int Data_Store::put_val_at(double val, int ind, int st)
{
	int flag = 0;
  // Rcpp::Rcout<<"ds.106 put_val_at called \n"<< "dict.nvars "<< dictionary.n_vars()<<", ts.nsteps="<<time_series.n_steps()<<"\n";
//make sure it is a valid index
	if (ind < dictionary.n_vars() && st < time_series.n_steps() && ind >= 0 && st >= 0)
	{
	  // Rcpp::Rcout<<"ds.109 dict.nvars= "<< dictionary.n_vars()<<", ts.nsteps="<<time_series.n_steps()<<"\n";
		values[ind][st] = val;
		flag =1;
	}
	return flag;
}

int Data_Store::put_val_for(double val, const std::string &vname, int st)
{
	int reqind = dictionary.index(vname);
	return put_val_at(val, reqind, st);
}

int Data_Store::get_index_for(const std::string &vname)
{
  int ind_for = dictionary.index(vname);
  return ind_for;
}
std::string Data_Store::get_name_for(const int ind)
{
  std::string name_for;
  dictionary.var_at(name_for,ind);
  return name_for;
}

int Data_Store::insert_col(std::string &vname, const Data_Store& orig)
{
        int flag = 0;
        int tocopy, in_here, in_orig;
//the lower number of steps is copied, hopefully both same
        tocopy = small_int(orig.time_series.n_steps(), time_series.n_steps());

//get the required indices
        in_here = dictionary.index(vname);
        in_orig = orig.dictionary.index(vname);
        if (in_here >= 0 && in_orig >= 0)
        {
                for (int i = 0; i < tocopy; i++)
		{
                        values[in_here][i] = orig.values[in_orig][i];
		}
                flag = 1;
        }
        return flag;
}

int Data_Store::check_step(int the_step) const
{
	int validity = 0;
//max the_step < no_of_steps
	if (the_step >= no_of_steps())
	{
		validity = -1;
	}
	else
	{
		for (int i = 0; i < no_of_vars(); i++)
		{
			if (get_val_at(i, the_step) == INVALID_DATA)
			{
				validity++;
			}
		}
	}
	return validity;
}

Time Data_Store::get_time(int step) const
{
	Time cur_time = time_series.get_time(step);
	return cur_time;
}

double Data_Store::get_val_at(int ind, int st) const
{
	double value = INVALID_DATA;
//make sure it is a valid index
	if (ind < dictionary.n_vars() && st < time_series.n_steps() && ind >= 0 && st >= 0)
	{
		value = values[ind][st];
	}
	return value;
}

double Data_Store::get_val_for(const std::string &vname, int st) const
{
	int reqind = dictionary.index(vname);
	return get_val_at(reqind, st);
}

// int Data_Store::carve(const Data_Store& orig, int start, int end)
// {
// 	int flag = 1;
// 	std::string vn;
//
// //copy the relevant part of the dictionary
// 	flag = flag*dictionary.copy_dictionary(orig.dictionary);
// //copy the time series
// 	if (flag != 0)
// 	{
// 		flag = flag*time_series.copy_array(orig.time_series);
// 	}
// 	if (flag != 0)
// 	{
// 		allocate();
// //copy varaiable values
// 		for (int i = 0; i < dictionary.n_vars(); i++)
// 		{
// //get the variable name
// 			dictionary.var_at(vn, i);
// //copy the values
// 			flag = flag*insert_col(vn, orig);
// 		}
// 	}
// 	return flag;
// }//end carve

int Data_Store::no_of_steps() const
{
	return time_series.n_steps();
}

int Data_Store::no_of_vars() const
{
	return dictionary.n_vars();
}

// std::ostream& print_step(std::ostream& out, const Data_Store& dl, int step)
// {
// 	if (step < dl.time_series.n_steps())
// 	{
// 		print_step(out, dl.time_series, step);
// 		for (int i = 0; i < dl.dictionary.n_vars(); i++)
// 		{
// 			out << '\t' << dl.values[i][step];
// 		}
// 	}
// 	return out;
// }

int get_obs(Rcpp::DataFrame &obs_file, Data_Store& in_data)
{
  // ifstream fin;//rTREES will break
  Var_Dictionary vd;
  Time time;
  int n_steps;    //# of time steps in file
  double temp_val;        //temporary
  String temp_string;
  //find n_steps
  n_steps = obs_file.nrow(); //datalines(obs_file);
  if (n_steps <= 0)
  {
    Rcpp::Rcerr<<"ds.244: no steps! \n";
    return 0;       //no data or file problem
  }
  else
  {
    // open_infile(obs_file, fin);
    //// get the first 2 strings out (not vars)
    //                 for (int i = 0; i < 2; i++)
    // 		{
    //                         fin >> temp_string;
    // 		}
    // Rcpp::Rcout<<"ds.254 n_steps "<<n_steps<< " \n";
    //get the dictionary
    read_dictionary(obs_file, vd);
    // Rcpp::Rcout<<"ds.258 vd nvars:"<<vd.n_vars()<<"\n";
    //allocate memeory
    in_data.allocate(vd, n_steps);
    //read data
    for (int i = 0; i < n_steps; i++)
    {
      // Rcpp::Rcout<<"ds.262 n_vars "<<vd.n_vars()<< " \n";

      //refer time_keeper.h
      std::vector<double> in_time{
        Rcpp::as<Rcpp::NumericVector>(obs_file["year"])[i],
        Rcpp::as<Rcpp::NumericVector>(obs_file["jday"])[i],
        Rcpp::as<Rcpp::NumericVector>(obs_file["hour"])[i],
        Rcpp::as<Rcpp::NumericVector>(obs_file["min"])[i]
      };


      read_time(in_time, time);
      // Rcpp::Rcout<<"Test time "<<time.get_year()<<","<<time.get_day()<<","<<"\n";
      std::string temp_name;
      in_data.set_time(time, i);
      for (int j = 0; j < vd.n_vars(); j++)
      {


        temp_val=  Rcpp::as<std::vector<double>>(obs_file[j])[i];
        // Rcpp::Rcout<<"ds.286 check val set at "<< j<<" step "<<i<< " = "<< temp_val<<"\n";

        in_data.put_val_at(temp_val,j,i);
        if(in_data.get_val_at(j,i)!=temp_val){
          Rcpp::Rcerr<<"Error! value not not correctly set! \n";
        }

        // temp_name = Rcpp::as<std::vector<std::string>>(obs_file.names())[j];

        // Rcpp::Rcout<<"try by name: "<<temp_name<<","<<in_data.get_index_for(temp_name)<<"\n";
        // if(in_data.get_name_for(j)!=temp_name){
        //   Rcpp::Rcerr<<"Error! names are broken \n";
        // }
        //
        // if(in_data.get_val_for(temp_name,i)!=temp_val){
        //   Rcpp::Rcerr<<"Error! name didn't work either! \n";
        //
        // }
      }

      // fin.ignore(MAX_LINE, '\n');
    }
    // fin.close();
    return n_steps;
  }//end else
}//end get_obs

/*
 * used to retrieve shoot and root module parameters for the sperry model
 */
// double getData(ifstream &in)
// {
//     	double data;
//
//     	while( in.peek() < 45 || in.peek() > 57 )
// 	{
//         	in.ignore(256,' ');
//     	}
//
//     	in >> data;
//     	return data;
// }


