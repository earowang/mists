#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

template <int RTYPE>
List na_rle_impl(Vector<RTYPE> x) {
  std::vector<int> lengths;
  std::vector<bool> values;
  LogicalVector x_lgl = is_na(x);

  // Initialise first value
  int i = 0;
  bool prev = x_lgl[0];
  values.push_back(prev);
  lengths.push_back(1);

  for (LogicalVector::iterator it = x_lgl.begin() + 1; it != x_lgl.end(); ++it) {
    if (prev == *it) {
      // Same as previous so increment lengths
      lengths[i]++;
    } else {
      // Different & TRUE for NA, so add to values, and add 1 to lengths
      values.push_back(*it);
      lengths.push_back(1);

      i++;
      prev = *it;
    }
  }

  return List::create(_["lengths"] = lengths, _["values"] = values);
}

// Run length ecoding for missing values
// ref: https://github.com/hadley/adv-r/blob/master/extras/cpp/rle.cpp
// [[Rcpp::export]]
SEXP na_rle_cpp(SEXP x) {
  RCPP_RETURN_VECTOR(na_rle_impl, x);
}
