umapR umapNew(Rcpp::StringVector, Rcpp::List);

class uMAPR {
  // std::vector<std::string> keys;
  // Rcpp::List values;
  // umapR umap;

 public:
   umapR umap;
   Rcpp::XPtr<umapR> ptr;
   uMAPR(Rcpp::StringVector keys_, Rcpp::List values_)
      : umap(umapNew(keys_, values_)),
        ptr(Rcpp::XPtr<umapR>(&umap, true)) {}
  uMAPR(Rcpp::XPtr<umapR> ptr_)
      : umap(*(ptr_.get())), ptr(Rcpp::XPtr<umapR>(&umap, true)) {}
  ~uMAPR() { 
    Rcpp::Rcout << "2) uMAPR deconstructor has been called\n";
    //ptr.release();
    //umap.clear();
    //ptr.release();
    // Rcpp::Rcout << umap.size() << "\n";
    // Rcpp::Rcout << (umap.find("a") != umap.end()) << "\n";
    // delete ptr.get(); 
    // Rcpp::Rcout << "uMAPR deconstructor success\n";
  }
  
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
    const unsigned s = umap.size();
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

  Rcpp::List toList() {
    const unsigned s = umap.size();
    Rcpp::StringVector Keys(s);
    Rcpp::List Values(s);
    unsigned i = 0;
    for(umapR::iterator it = umap.begin(); it != umap.end(); it++) {
      Keys(i) = it -> first;
      Values(i) = it->second;
      i++;
    }
    Rcpp::List out;
    out["keys"] = Keys;
    out["values"] = Values;
    return out;
  }
  
  void insert(std::string key, Rcpp::RObject value) {
    std::pair<umapR::iterator, bool> x = umap.emplace(key, value);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }

  void assign(std::string key, Rcpp::RObject value) {
    std::pair<umapR::iterator, bool> x = umap.insert_or_assign(key, value);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }

  void erase(std::string key) {
    unsigned x = umap.erase(key);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }

  void merase(Rcpp::StringVector keys) {
    for(Rcpp::String key : keys) {
      unsigned x = umap.erase(key);
    }
  }

  Rcpp::XPtr<umapR> extract(Rcpp::StringVector keys) {
    umapR submap;
    // umapR* submapptr = &submap;
    for(Rcpp::String key : keys) {
      umapR::iterator it = umap.find(key);
      if(it != umap.end()) {
        std::pair<umapR::iterator, bool> x = submap.emplace(key, it->second);
      }
    }
    // std::unique_ptr<umapR> submapptr(new umapR(submap));
    umapR* submapptr(new umapR(submap)); //&submap;
    Rcpp::XPtr<umapR> out = Rcpp::XPtr<umapR>(submapptr, true);
//    delete submapptr;
    return out;//new umapR(submap), true);
  }

  void extract_inplace(Rcpp::StringVector keys) {
    umapR submap;
    for(Rcpp::String key : keys) {
      umapR::iterator it = umap.find(key);
      if(it != umap.end()) {
        std::pair<umapR::iterator, bool> x = submap.emplace(key, it->second);
      }
    }
    umap = submap;
  }
  
  Rcpp::XPtr<umapR> extract_by_erasing(Rcpp::StringVector keys){
    umapR* submapptr = new umapR(umap);
    umapR submap = *submapptr;
    for(umapR::iterator it = submap.begin(); it != submap.end(); it++) {
      if(std::find(keys.begin(), keys.end(), it->first) == keys.end()){
        unsigned x = submap.erase(it->first);
      }
    }
    // Rcpp::StringVector allkeys = this.keys2();
    // for(Rcpp::String key : allkeys) {
    //   if(std::find(keys.begin(), keys.end(), key) == keys.end()){
    //     submap.erase(key);
    //   }
    // }
    delete submapptr;
    return Rcpp::XPtr<umapR>(&submap, true);//(new umapR(submap), true); 
  }

  void extract_by_erasing_inplace(Rcpp::StringVector keys){
    for(umapR::iterator it = umap.begin(); it != umap.end(); it++) {
      if(std::find(keys.begin(), keys.end(), it->first) == keys.end()){
        unsigned x = umap.erase(it->first);
      }
    }
  }
  
  void merge(Rcpp::XPtr<umapR> umap2ptr) {
    umapR umap2 = *(umap2ptr.get());
    umap.merge(umap2);
    // ptr = Rcpp::XPtr<umapR>(new umapR(umap, true));
  }
};