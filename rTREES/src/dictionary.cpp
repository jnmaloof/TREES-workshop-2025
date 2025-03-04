//*****************************************************
//implementation of dictionary.h
//***************** SS 2002 ***************************
#include "dictionary.h"


Var_Dictionary::Var_Dictionary(int can)
:/*capacity(can), current(0), */list()
{
	// assert(capacity > 0);
	Rcpp::StringVector list;//Var_name[capacity];
	// list.resize(capacity);

	// assert(list != NULL);
}

Var_Dictionary::Var_Dictionary(const Var_Dictionary& dorig)
{
	assert(copy_dictionary(dorig) != 0);
  copy_dictionary(dorig);
}

Var_Dictionary::~Var_Dictionary()
{
	reset();
}

void Var_Dictionary::reset()
{
// 	if (list != NULL)
// 	{
//
// 		// delete[] list;
//         	// list = NULL;
// 	}
int i = 0;
for( i=0;i<list.length();i++){
  list.erase(i);

}
        // capacity = 0;
        // current = 0;
}

// int Var_Dictionary::space_left()
// {
// 	if (current < capacity)
// 	{
// 		return 1;
// 	}
// 	else
// 	{
// 		return 0;
// 	}
// }

// int Var_Dictionary::create_space(int extra)
// {
// 	int flag = 1; //check
// 	int inc_space = extra + capacity;
//
// 	std::string nlist = " "; //temporary storage
// 	nlist.resize(current);
// 	if (nlist.length() == 0)
// 	{
// 		flag = 0;
// 	}
// 	else
// 	{
// 	//copy the existing ones
// 		// for (int i = 0; i < current; i++)
// 		// {
// 		// 	strcpy(nlist[i], list[i]);
// 		// }
// 	//allocate extra memory
// 		delete[] list;
// 		list = new Var_name[inc_space];
// 		if (list == NULL)
// 		{
//                 	flag = 0; //failed
// 		}
// 		else
// 		{
// 			capacity = inc_space;
// 			//copy back
// 			for (int i = 0; i < current; i++)
// 			{
// 				strcpy(list[i], nlist[i]);
// 			}
// 			delete[] nlist;
// 			nlist = NULL;
// 			flag = 1;
// 		}
// 	}
// 	return flag;
// }


int Var_Dictionary::insert(const std::string &vname)
{
	int flag = 1; //check
	// if (!space_left() )
	// {
	// 	flag = create_space(); //flag becomes 0 if space not created
	// }
	if (flag)
	{
	  int i =0;
	  // for(i=0;i<vname.length();i++){
	    list.push_back(vname);
	  // }
		// strcpy(list[current], vname);
		// current++;
	}
	return flag;
}

int Var_Dictionary::index(const std::string &vname) const
{
	int i = 0;

  while(vname != Rcpp::as<std::string>(list[i])){
    i++;
    // Rcpp::Rcout<<"Getting index for "<<vname<<","<<i<<"\n";

  }

	return i;
}

int Var_Dictionary::var_at(std::string& var, int at) const
{
	int flag = 0;
	if (at < list.length())
	{
		var=Rcpp::as<std::string>(list[at]) ;
		flag = 1;
	}
	return flag;
}

int Var_Dictionary::n_vars() const
{
	return list.length();
}

// int Var_Dictionary::append_dictionary(const Var_Dictionary& dorig, int start, int end)
// {
//         int temp, flag=0;
//         Var_name vname;
// 	//setting up for start and end
// 	if (start > end) //in correct order
// 	{
// 		temp = start;
// 		start = end;
// 		end = temp;
// 	}
// 	//proper indices
// 	// end = small_int(end, dorig.current-1);
// 	// start = big_int(0, start);
// 	//get the variables
//         for (int i = start; i <= end; i++)
//         {
//                 flag = dorig.var_at(vname, i);
//                 flag = insert(vname);
//         }
//         return flag;
// }

int Var_Dictionary::append_dictionary(const Var_Dictionary& dorig)
{
  int flag=0;
  int i=0;
  int init_size=list.length();
  for(i=0;i<dorig.n_vars();i++){
    std::string var;
    dorig.var_at(var,i);
    list.push_back(var);
  }
  if(init_size+dorig.n_vars()==list.length()){
    flag=1;
  }
	return flag;
}

int Var_Dictionary::copy_dictionary(const Var_Dictionary& dorig)
{
	//current = 0; //puts in starting at the first variable
  list=Rcpp::StringVector();


	return append_dictionary(dorig);
}

// int Var_Dictionary::copy_dictionary(const Var_Dictionary& dorig, int start, int end)
// {
// 	current = 0; //puts in starting at the first variable
// 	return append_dictionary(dorig, start, end);
// }

//rTREES removed
// std::ostream& print_dictionary(std::ostream& out, const Var_Dictionary& vd)
// {
// 	for (int i = 0; i < vd.current; i++)
// 	{
// 		out << vd.list[i] << '\t';
// 	}
// 	return out;
// }
//
/*std::istream&*/void read_dictionary(Rcpp::DataFrame& obs_file, Var_Dictionary& vd)
{
        // char next_char;
        // in >> next_char;
        // while (next_char != '\n') //not end of line
        // {
                // if (next_char == ' ' || next_char == '\t')
		// {
                        // next_char = in.get();
		// }
                // else
                // {
                        // in.unget(); //put it back
                        // in >> vname;
                        Rcpp::CharacterVector all_names=obs_file.names();
  // Rcpp::Rcout<<"dict.224 names length:"<<all_names.length()<<"\n";

                        //put it in dictionary
                        std::string vartest;
                        int i=0;
                        for(i=0;i<all_names.length();i++ ){
                          vd.insert(Rcpp::as<std::string>(all_names[i]));
                          vd.var_at(vartest,i);
                          // Rcpp::Rcout<<"dict.230 vd.insert check:"<<vartest<<"\n";

                        }


                        // next_char = in.get();
                // }
        // }
        // return in;
}
//end rTREES removed
