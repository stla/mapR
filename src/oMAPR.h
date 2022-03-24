omapR omapNew(Rcpp::StringVector, Rcpp::List);

class oMAPR {
  omapR omap;

 public:
  oMAPR(Rcpp::StringVector keys_, Rcpp::List values_)
      : omap(omapNew(keys_, values_)),
        ptr(Rcpp::XPtr<omapR>(&omap)) {} //new omapR(omap)
  oMAPR(Rcpp::XPtr<omapR> ptr_)
     : omap(*(ptr_.get())), ptr(Rcpp::XPtr<omapR>(&omap)) {}
   
  Rcpp::XPtr<omapR> ptr;

  unsigned size() { return omap.size(); }

  Rcpp::RObject at(std::string key) {
    omapR::iterator it = omap.find(key);
    if(it != omap.end()) {
      return it->second;
    } else {
      Rcpp::stop("Key not found.");
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

  void insert(std::string key, Rcpp::RObject value) {
    omap.emplace(key, value);
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void assign(std::string key, Rcpp::RObject value) {
    omap.insert_or_assign(key, value);
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void erase(std::string key) {
    omap.erase(key);
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void merase(Rcpp::StringVector keys) {
    for(Rcpp::String key : keys) {
      omap.erase(key);
    }
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void merge(Rcpp::XPtr<omapR> omap2ptr) {
    // omapR omap1 = *omapPTR;
    omapR omap2 = *(omap2ptr.get());
    omap.merge(omap2);
    //   omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }
};