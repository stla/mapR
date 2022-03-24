umapR umapNew(Rcpp::StringVector, Rcpp::List);

class uMAPR {
  // std::vector<std::string> keys;
  // Rcpp::List values;
  umapR umap;

 public:
  uMAPR(Rcpp::StringVector keys_, Rcpp::List values_)
      : umap(umapNew(keys_, values_)),
        ptr(Rcpp::XPtr<umapR>(&umap)) {}
  uMAPR(Rcpp::XPtr<umapR> ptr_)
      : umap(*(ptr_.get())), ptr(Rcpp::XPtr<umapR>(&umap)) {}

  Rcpp::XPtr<umapR> ptr;

  unsigned size() { return umap.size(); }

  Rcpp::RObject at(std::string key) {
    umapR::iterator it = umap.find(key);
    if(it != umap.end()) {
      return it->second;
    } else {
      Rcpp::stop("Key not found.");
    }
  }

  bool has_key(std::string key) { return umap.find(key) != umap.end(); }

  // Rcpp::StringVector keys() {
  //   Rcpp::StringVector out(0);
  //   for(umapR::iterator it = umap.begin(); it != umap.end(); it++) {
  //     out.push_back(it->first);
  //   }
  //   return out;
  // }

  Rcpp::StringVector keys() {
    unsigned s = umap.size();
    Rcpp::StringVector out(s);
    unsigned i = 0;
    for(umapR::iterator it = umap.begin(); it != umap.end(); it++) {
      out[i] = it->first;
      i++;
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
  }

  Rcpp::XPtr<umapR> extract(Rcpp::StringVector keys) {
    umapR submap;
    for(Rcpp::String key : keys) {
      umapR::iterator it = umap.find(key);
      if(it != umap.end()) {
        submap.emplace(key, it->second);
      }
    }
    return Rcpp::XPtr<umapR>(new umapR(submap));
  }

  void extract_inplace(Rcpp::StringVector keys) {
    umapR submap;
    for(Rcpp::String key : keys) {
      umapR::iterator it = umap.find(key);
      if(it != umap.end()) {
        submap.emplace(key, it->second);
      }
    }
    umap = submap;
  }
  
  Rcpp::XPtr<umapR> extract_by_erasing(Rcpp::StringVector keys){
    umapR* submapptr = new umapR(umap);
    umapR submap = *submapptr;
    for(umapR::iterator it = submap.begin(); it != submap.end(); it++) {
      if(std::find(keys.begin(), keys.end(), it->first) == keys.end()){
        submap.erase(it->first);
      }
    }
    // Rcpp::StringVector allkeys = this.keys2();
    // for(Rcpp::String key : allkeys) {
    //   if(std::find(keys.begin(), keys.end(), key) == keys.end()){
    //     submap.erase(key);
    //   }
    // }
    return Rcpp::XPtr<umapR>(new umapR(submap)); 
  }

  void extract_by_erasing_inplace(Rcpp::StringVector keys){
    for(umapR::iterator it = umap.begin(); it != umap.end(); it++) {
      if(std::find(keys.begin(), keys.end(), it->first) == keys.end()){
        umap.erase(it->first);
      }
    }
  }
  
  void merge(Rcpp::XPtr<umapR> umap2ptr) {
    umapR umap2 = *(umap2ptr.get());
    umap.merge(umap2);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }
};