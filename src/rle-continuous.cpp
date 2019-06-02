#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// Run length ecoding for continuous values
// [[Rcpp::export]]
IntegerVector continuous_rle_impl(NumericVector x, double c) {
  NumericVector vec_diff = diff(x);
  LogicalVector vec_lgl(x.size());
  for (int i = 0; i < vec_diff.size(); i++) {
    if (vec_diff[i] == c) {
      vec_lgl[i + 1] = vec_lgl[i];
    } else {
      vec_lgl[i + 1] = !vec_lgl[i];
    }
  }

  IntegerVector lengths;
  int i = 0;
  bool prev = vec_lgl[0];
  lengths.push_back(1);
  for (LogicalVector::iterator it = vec_lgl.begin() + 1; it != vec_lgl.end(); ++it) {
    if (prev == *it) {
      lengths[i]++;
    } else {
      lengths.push_back(1);

      i++;
      prev = *it;
    }
  }

  return lengths;
}
