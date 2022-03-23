umapR umapNew(Rcpp::StringVector, Rcpp::List);

class uMAPR {
  // std::vector<std::string> keys;
  // Rcpp::List values;
  umapR umap;

 public:
  uMAPR(Rcpp::StringVector keys_, Rcpp::List values_)
      : umap(umapNew(keys_, values_)),
        ptr(Rcpp::XPtr<umapR>(new umapR(umap))) {}

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