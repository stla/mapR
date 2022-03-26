omapR omapNew(Rcpp::StringVector, Rcpp::List);

class oMAPR {
 public:
  omapR omap;
  Rcpp::XPtr<omapR> ptr;
  oMAPR(Rcpp::StringVector keys_, Rcpp::List values_)
      : omap(omapNew(keys_, values_)), ptr(Rcpp::XPtr<omapR>(&omap, false)) {}
  oMAPR(Rcpp::XPtr<omapR> ptr_)
      : omap(*(ptr_.get())), ptr(Rcpp::XPtr<omapR>(&omap, false)) {}
  ~oMAPR() {
    // Rcpp::Rcout << "2) oMAPR deconstructor has been called\n";
    // if(ptr.get()){
    //   Rcpp::Rcout << "ptr.get\n";
    //   //ptr.release();
    // }
    // omap.clear();
    // ptr.release();
    // Rcpp::Rcout << omap.size() << "\n";
    // delete ptr.get();
    // Rcpp::Rcout << "oMAPR deconstructor success\n";
  }

  unsigned size() { return omap.size(); }

  Rcpp::List at(std::string key) {
    omapR::iterator it = omap.find(key);
    if(it != omap.end()) {
      return Just(it->second);
    } else {
      return Nothing();
    }
  }

  unsigned index(std::string key) {
    omapR::iterator it = omap.find(key);
    if(it != omap.end()) {
      return omap.index_of(it) + 1;
    } else {
      return 0;
    }
  }

  bool has_key(std::string key) { return omap.contains(key); }

  Rcpp::List nth(const unsigned i) {
    const unsigned s = omap.size();
    if(i >= s) {
      Rcpp::stop("Index too large.");
    }
    omapR::iterator it = omap.nth(i);
    std::string key = it->first;
    Rcpp::RObject value = it->second;
    return Rcpp::List::create(Rcpp::Named("key") = key,
                              Rcpp::Named("value") = value);
  }

  Rcpp::StringVector keys() {
    unsigned s = omap.size();
    Rcpp::StringVector out(s);
    unsigned i = 0;
    for(omapR::iterator it = omap.begin(); it != omap.end(); it++) {
      out[i] = it->first;
      i++;
    }
    return out;
  }

  Rcpp::List values() {
    const unsigned s = omap.size();
    Rcpp::List out(s);
    unsigned i = 0;
    for(omapR::iterator it = omap.begin(); it != omap.end(); it++) {
      out(i) = it->second;
      i++;
    }
    return out;
  }

  Rcpp::List toList() {
    const unsigned s = omap.size();
    Rcpp::StringVector Keys(s);
    Rcpp::List Values(s);
    unsigned i = 0;
    for(omapR::iterator it = omap.begin(); it != omap.end(); it++) {
      Keys(i) = it->first;
      Values(i) = it->second;
      i++;
    }
    Rcpp::List out;
    out["keys"] = Keys;
    out["values"] = Values;
    return out;
  }

  bool insert(std::string key, Rcpp::RObject value) {
    std::pair<omapR::iterator, bool> x = omap.emplace(key, value);
    return x.second;
  }

  bool assign(std::string key, Rcpp::RObject value) {
    std::pair<omapR::iterator, bool> x = omap.insert_or_assign(key, value);
    return x.second;
  }

  void erase(std::string key) {
    omap.erase(key);
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void merase(Rcpp::StringVector keys) {
    for(Rcpp::String key : keys) {
      omap.erase(key);
    }
  }

  Rcpp::XPtr<omapR> extract(Rcpp::StringVector keys) {
    omapR submap;
    // omapR* submapptr = &submap;
    for(Rcpp::String key : keys) {
      omapR::iterator it = omap.find(key);
      if(it != omap.end()) {
        submap.emplace(key, it->second);
      }
    }
    omapR* submapptr(new omapR(submap));  //&submap;
    Rcpp::XPtr<omapR> out = Rcpp::XPtr<omapR>(submapptr, false);
    //    delete submapptr;
    return out;  // new omapR(submap), true);
  }

  void extract_inplace(Rcpp::StringVector keys) {
    omapR submap;
    for(Rcpp::String key : keys) {
      omapR::iterator it = omap.find(key);
      if(it != omap.end()) {
        submap.emplace(key, it->second);
      }
    }
    omap = submap;
  }

  Rcpp::XPtr<omapR> extract_by_erasing(Rcpp::StringVector keys) {
    omapR* submapptr = new omapR(omap);
    omapR submap = *submapptr;
    for(omapR::iterator it = submap.begin(); it != submap.end(); it++) {
      if(std::find(keys.begin(), keys.end(), it->first) == keys.end()) {
        submap.erase(it->first);
      }
    }
    Rcpp::XPtr<omapR> out = Rcpp::XPtr<omapR>(new omapR(submap), false);
    delete submapptr;
    return out;
  }

  void extract_by_erasing_inplace(Rcpp::StringVector keys) {
    for(omapR::iterator it = omap.begin(); it != omap.end(); it++) {
      if(std::find(keys.begin(), keys.end(), it->first) == keys.end()) {
        omap.erase(it->first);
      }
    }
  }

  void merge(Rcpp::XPtr<omapR> omap2ptr) {
    // omapR omap1 = *omapPTR;
    omapR omap2 = *(omap2ptr.get());
    omap.merge(omap2);
    //   omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }
};