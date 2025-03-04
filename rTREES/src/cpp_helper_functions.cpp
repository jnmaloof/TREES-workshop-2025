#include <Rcpp.h>
#include <thread>

//' This is a very simple function for portabl core detection.
//' @return The number of cores detected by your system
//' @export
// [[Rcpp::export]]
int number_of_cores() {
  return(std::thread::hardware_concurrency());
}


