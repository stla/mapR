omapR omapNew(std::vector<std::string>, Rcpp::List);

class oMAPR {
  omapR omap;

 public:
  oMAPR(std::vector<std::string> keys_, Rcpp::List values_)
      : omap(omapNew(keys_, values_)),
        ptr(Rcpp::XPtr<omapR>(new omapR(omap))) {}

  Rcpp::XPtr<omapR> ptr;

  unsigned size() { return omap.size(); }

  std::vector<double> at(std::string key) {
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
    std::vector<double> value = it->second;
    return Rcpp::List::create(Rcpp::Named("key") = key,
                              Rcpp::Named("value") = value);
  }

  std::vector<std::string> keys() {
    std::vector<std::string> out(0);
    for(omapR::iterator it = omap.begin(); it != omap.end(); it++) {
      out.push_back(it->first);
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

  void insert(std::string key, std::vector<double> value) {
    omap.emplace(key, value);
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void assign(std::string key, std::vector<double> value) {
    omap.insert_or_assign(key, value);
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void erase(std::string key) {
    omap.erase(key);
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void merase(std::vector<std::string> keys) {
    for(std::string key : keys) {
      omap.erase(key);
    }
    //     omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }

  void merge(Rcpp::XPtr<omapR> omap2) {
    // omapR omap1 = *omapPTR;
    // omapR omap2 = *omap2;
    omap.merge(*omap2);
    //   omapPTR = Rcpp::XPtr<omapR>(new omapR(omap));
  }
};