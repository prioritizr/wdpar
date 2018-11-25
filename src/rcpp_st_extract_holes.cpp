#include "package.h"

// [[Rcpp::export]]
Rcpp::List rcpp_st_extract_holes(Rcpp::List x) {
  // initialization
  const std::size_t n = x.size();
  SEXP curr_raw_element;
  Rcpp::List curr_new_element;
  Rcpp::List curr_list;
  Rcpp::List curr_list2;
  Rcpp::NumericMatrix curr_matrix;
  std::string curr_class;
  std::vector<SEXP> out(n);
  std::vector<SEXP> curr_vector;
  // pre-processing
  Rcpp::CharacterVector geometrycollection_classes = CharacterVector::create(
    "XY", "GEOMETRYCOLLECTION", "sfg");
  Rcpp::CharacterVector polygon_classes = CharacterVector::create(
    "XY", "POLYGON", "sfg");
  Rcpp::CharacterVector multipolygon_classes = CharacterVector::create(
    "XY", "MULTIPOLYGON", "sfg");
  // main processing
  for (std::size_t i = 0; i < n; ++i) {
    //// initialization
    R_CheckUserInterrupt();
    curr_raw_element = x[i];
    if (!Rf_inherits(curr_raw_element, "XY"))
      throw std::range_error("sfc object contains non-sfg element");
    curr_list = Rcpp::as<Rcpp::List>(x[i]);
    //// extract holes
    if (Rf_inherits(curr_raw_element, "POLYGON")) {
      if (curr_list.size() == 1) {
        curr_new_element = Rcpp::List::create();
        curr_new_element.attr("class") = geometrycollection_classes;
      } else if (curr_list.size() == 2) {
        curr_new_element = Rcpp::List::create(curr_list[1]);
        curr_new_element.attr("class") = polygon_classes;
      } else {
        curr_vector.reserve(curr_list.size() - 1);
        for (std::size_t j = 1; j < curr_list.size(); ++j)
          curr_vector.push_back(Rcpp::List::create(curr_list[j]));
        curr_new_element = Rcpp::wrap(curr_vector);
        curr_new_element.attr("class") = multipolygon_classes;
        curr_vector.clear();
        curr_vector.shrink_to_fit();
      }
    } else if (Rf_inherits(curr_raw_element, "MULTIPOLYGON")) {
      curr_list = Rcpp::as<Rcpp::List>(x[i]);
      curr_vector.reserve(curr_list.size() - 1);
      for (std::size_t p = 0; p < curr_list.size(); ++p) {
        curr_list2 = Rcpp::as<Rcpp::List>(curr_list[p]);
        if (curr_list2.size() > 1) {
          for (std::size_t j = 1; j < curr_list2.size(); ++j) {
            curr_vector.push_back(Rcpp::List::create(curr_list2[j]));
          }
        }
      }
      //// assign class to the list
      if (curr_vector.size() == 0) {
        // if contains no geometries, then convert to empty geometrycollection
        curr_new_element = Rcpp::List::create();
        curr_new_element.attr("class") = geometrycollection_classes;
      } else if (curr_new_element.size() == 1) {
        // if contains has one geometry, then convert to polygon
        curr_new_element = curr_vector[0];
        curr_new_element.attr("class") = polygon_classes;
      } else {
        // if contains has multiple geometries, then convert to multipolygon
        curr_new_element = Rcpp::wrap(curr_vector);
        curr_new_element.attr("class") = multipolygon_classes;
      }
      curr_vector.clear();
      curr_vector.shrink_to_fit();
    } else {
      //// if not polygon or multipolygon than keep the object as is
      curr_new_element = R_NilValue;
    }
    //// store hole data
    out[i] = curr_new_element;
  }
  // exports
  return Rcpp::wrap(out);
}
