// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/container/flat_map.hpp>

typedef boost::container::flat_map<std::string, std::vector<double>> mapR;

Rcpp::XPtr<mapR> mapPointer(std::vector<std::string> keys, Rcpp::List values) {
  mapR map;
  for(size_t i=0; i<keys.size(); i++){
    std::vector<double> v = values[i];
    map.emplace(keys[i], v);
  }
  Rcpp::XPtr<mapR> ptr(new mapR(map), true);
  ptr.attr("class") = "mapR";
  return ptr;
}

class MAPR {
public:
  MAPR(std::vector<std::string> keys_, Rcpp::List values_) : keys(keys_), values(values_) {}
  
  Rcpp::XPtr<mapR> toMAPR() {
    return mapPointer(keys, values);
  }
  
private:
  std::vector<std::string> keys; Rcpp::List values;
};

RCPP_MODULE(maprModule) {
  using namespace Rcpp;
  class_<MAPR>("MAPR")
    .constructor<std::vector<std::string>, Rcpp::List>()
    .method("mapPointer", &MAPR::toMAPR);
}

class MAPRPTR {
public:
  MAPRPTR(Rcpp::XPtr<mapR> mapPTR_) : mapPTR(mapPTR_) {}
  
  unsigned size(){
    mapR map = *mapPTR;
    return map.size();
  }
  
  std::vector<double> at(std::string key) {
    mapR map = *mapPTR;
    mapR::iterator it = map.find(key);
    if(it != map.end()){
      return it->second;
    }else{
      Rcpp::stop("Key not found.");
    }
  }

  bool has_key(std::string key) {
    mapR map = *mapPTR;
    return map.contains(key);
  }
  
  std::vector<std::string> keys(){
    mapR map = *mapPTR;
    std::vector<std::string> out(0);
    for(mapR::iterator it = map.begin();it != map.end(); it++){
      out.push_back(it->first);
    }
    return out;
  }
  
  Rcpp::List values(){
    mapR map = *mapPTR;
    const unsigned s = map.size();
    Rcpp::List out(s);
    unsigned i = 0;
    for(mapR::iterator it = map.begin(); it != map.end(); it++){
      out(i) = it->second;
      i++;
    }
    return out;
  }
  
  void insert(std::string key, std::vector<double> value){
    mapR map = *mapPTR;
    map.emplace(key, value);
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map));
  }

  void erase(std::string key){
    mapR map = *mapPTR;
    map.erase(key);
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map));
  }
  
  void merge(Rcpp::XPtr<mapR> map) {
    mapR map1 = *mapPTR;
    mapR map2 = *map;
    map1.merge(map2);
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map1));
  }
  
protected:
  Rcpp::XPtr<mapR> mapPTR;
};

RCPP_MODULE(maprptrModule) {
  using namespace Rcpp;
  class_<MAPRPTR>("MAPRPTR")
    .constructor<Rcpp::XPtr<mapR>>()
    .method("size", &MAPRPTR::size)
    .method("at", &MAPRPTR::at)
    .method("has_key", &MAPRPTR::has_key)
    .method("insert", &MAPRPTR::insert)
    .method("erase", &MAPRPTR::erase)
    .method("merge", &MAPRPTR::merge)
    .method("keys", &MAPRPTR::keys)
    .method("values", &MAPRPTR::values);
}