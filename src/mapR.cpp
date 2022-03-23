// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/container/flat_map.hpp>
#include <boost/unordered_map.hpp>

typedef boost::container::flat_map<std::string, std::vector<double>> mapR;
typedef boost::unordered::unordered_map<std::string, SEXP> umapR;

// Rcpp::XPtr<mapR> mapPointer(std::vector<std::string> keys, Rcpp::List values)
// {
//   mapR map;
//   for(size_t i = 0; i < keys.size(); i++) {
//     std::vector<double> v = values[i];
//     map.emplace(keys[i], v);
//   }
//   Rcpp::XPtr<mapR> ptr(new mapR(map), true);
//   // ptr.attr("class") = "mapR";
//   return ptr;
// }

mapR mapNew(std::vector<std::string> keys, Rcpp::List values) {
  mapR map;
  for(size_t i = 0; i < keys.size(); i++) {
    std::vector<double> v = values[i];
    map.emplace(keys[i], v);
  }
  return map;
  // Rcpp::XPtr<mapR> ptr(new mapR(map), true);
  // // ptr.attr("class") = "mapR";
  // return ptr;
}

umapR umapNew(Rcpp::StringVector keys, Rcpp::List values) {
  umapR umap;
  for(size_t i = 0; i < keys.size(); i++) {
    SEXP v = values[i];
    umap.emplace(keys[i], v);
  }
  return umap;
}

class MAPR {
  std::vector<std::string> keys;
  Rcpp::List values;

 public:
  MAPR(std::vector<std::string> keys_, Rcpp::List values_)
      : keys(keys_), values(values_), ptr(new mapR(mapNew(keys_, values_))) {}

  Rcpp::XPtr<mapR> ptr;

  // Rcpp::XPtr<mapR> toMAPR() { return mapPointer(keys, values); }

  // private:
  //  std::vector<std::string> keys;
  //  Rcpp::List values;
};

class uMAPR {
  // std::vector<std::string> keys;
  // Rcpp::List values;
  umapR umap;

 public:
  uMAPR(Rcpp::StringVector keys_, Rcpp::List values_)
      : umap(umapNew(keys_, values_)), ptr(Rcpp::XPtr<umapR>(new umapR(umap))) {}

  Rcpp::XPtr<umapR> ptr;

  unsigned size() { return umap.size(); }

  SEXP at(std::string key) {
    umapR::iterator it = umap.find(key);
    if(it != umap.end()) {
      return it->second;
    } else {
      Rcpp::stop("Key not found.");
    }
  }

  bool has_key(std::string key) { return umap.find(key) != umap.end(); }

  Rcpp::StringVector keys() {
    Rcpp::StringVector out(0);
    for(umapR::iterator it = umap.begin(); it != umap.end(); it++) {
      out.push_back(it->first);
    }
    return out;
  }

  Rcpp::List values() {
    const unsigned s = umap.size();
    Rcpp::List out(s);
    unsigned i = 0;
    for(umapR::iterator it = umap.begin(); it != umap.end(); it++) {
      out(i) = it->second;
      i++;
    }
    return out;
  }

  void insert(std::string key, SEXP value) {
    umap.emplace(key, value);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }

  void assign(std::string key, SEXP value) {
    umap.insert_or_assign(key, value);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }

  void erase(std::string key) {
    umap.erase(key);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }

  void merase(Rcpp::StringVector keys) {
    for(Rcpp::String key : keys) {
      umap.erase(key);
    }
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }

  void merge(Rcpp::XPtr<umapR> umap2) {
    umap.merge(*umap2);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }
};

RCPP_MODULE(maprModule) {
  using namespace Rcpp;
  class_<MAPR>("MAPR")
      .constructor<std::vector<std::string>, Rcpp::List>()
      .field_readonly("ptr", &MAPR::ptr);
  // .method("mapPointer", &MAPR::toMAPR);
}

RCPP_MODULE(umaprModule) {
  using namespace Rcpp;
  class_<uMAPR>("uMAPR")
      .constructor<Rcpp::StringVector, Rcpp::List>()
      .field_readonly("ptr", &uMAPR::ptr)
      .method("size", &uMAPR::size)
      .method("at", &uMAPR::at)
      .method("has_key", &uMAPR::has_key)
      .method("insert", &uMAPR::insert)
      .method("assign", &uMAPR::assign)
      .method("erase", &uMAPR::erase)
      .method("merase", &uMAPR::merase)
      .method("merge", &uMAPR::merge)
      .method("keys", &uMAPR::keys)
      .method("values", &uMAPR::values);
}

class MAPRPTR {
  Rcpp::XPtr<mapR> mapPTR;
  mapR map;

 public:
  MAPRPTR(Rcpp::XPtr<mapR> mapPTR_) : mapPTR(mapPTR_), map(*mapPTR_) {}

  unsigned size() {
    // mapR map = *mapPTR;
    return map.size();
  }

  std::vector<double> at(std::string key) {
    // mapR map = *mapPTR;
    mapR::iterator it = map.find(key);
    if(it != map.end()) {
      return it->second;
    } else {
      Rcpp::stop("Key not found.");
    }
  }

  unsigned index(std::string key) {
    // mapR map = *mapPTR;
    mapR::iterator it = map.find(key);
    if(it != map.end()) {
      return map.index_of(it) + 1;
    } else {
      return 0;
    }
  }

  bool has_key(std::string key) {
    // mapR map = *mapPTR;
    return map.contains(key);
  }

  Rcpp::List nth(const unsigned i) {
    // mapR map = *mapPTR;
    const unsigned s = map.size();
    if(i >= s) {
      Rcpp::stop("Index too large.");
    }
    mapR::iterator it = map.nth(i);
    std::string key = it->first;
    std::vector<double> value = it->second;
    return Rcpp::List::create(Rcpp::Named("key") = key,
                              Rcpp::Named("value") = value);
  }

  std::vector<std::string> keys() {
    // mapR map = *mapPTR;
    std::vector<std::string> out(0);
    for(mapR::iterator it = map.begin(); it != map.end(); it++) {
      out.push_back(it->first);
    }
    return out;
  }

  Rcpp::List values() {
    // mapR map = *mapPTR;
    const unsigned s = map.size();
    Rcpp::List out(s);
    unsigned i = 0;
    for(mapR::iterator it = map.begin(); it != map.end(); it++) {
      out(i) = it->second;
      i++;
    }
    return out;
  }

  void insert(std::string key, std::vector<double> value) {
    // mapR map = *mapPTR;
    map.emplace(key, value);
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map));
  }

  void assign(std::string key, std::vector<double> value) {
    // mapR map = *mapPTR;
    map.insert_or_assign(key, value);
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map));
  }

  void erase(std::string key) {
    // mapR map = *mapPTR;
    map.erase(key);
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map));
  }

  void merase(std::vector<std::string> keys) {
    // mapR map = *mapPTR;
    for(std::string key : keys) {
      map.erase(key);
    }
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map));
  }

  void merge(Rcpp::XPtr<mapR> map2) {
    // mapR map1 = *mapPTR;
    // mapR map2 = *map2;
    map.merge(*map2);
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map));
  }

  // protected:
  //  Rcpp::XPtr<mapR> mapPTR;
  //  mapR map;
};

RCPP_MODULE(maprptrModule) {
  using namespace Rcpp;
  class_<MAPRPTR>("MAPRPTR")
      .constructor<Rcpp::XPtr<mapR>>()
      .method("size", &MAPRPTR::size)
      .method("at", &MAPRPTR::at)
      .method("index", &MAPRPTR::index)
      .method("has_key", &MAPRPTR::has_key)
      .method("nth", &MAPRPTR::nth)
      .method("insert", &MAPRPTR::insert)
      .method("assign", &MAPRPTR::assign)
      .method("erase", &MAPRPTR::erase)
      .method("merase", &MAPRPTR::merase)
      .method("merge", &MAPRPTR::merge)
      .method("keys", &MAPRPTR::keys)
      .method("values", &MAPRPTR::values);
}
