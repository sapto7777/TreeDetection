#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;



// [[Rcpp::export]]
bool match_cpp(IntegerVector table, IntegerVector valueList) {
  
  bool result = false;
  IntegerVector::iterator iter = table.begin(), jter = valueList.begin();
  
  while((jter != valueList.end()) & (result == false)){
      iter = std::find(table.begin(), table.end(), *jter);
      if(iter != table.end()){
          result = true;
      }
      jter++;
  }
  
  return result;
}

