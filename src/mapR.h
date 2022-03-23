#ifndef _MAPRHEADER_
#define _MAPRHEADER_

// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/container/flat_map.hpp>
#include <boost/unordered_map.hpp>

typedef boost::container::flat_map<std::string, SEXP> omapR;
typedef boost::unordered::unordered_map<std::string, SEXP> umapR;

#endif