#include <Rcpp.h>


// template <class T1, class T2>
// class Side {
//   union  {
//   T1 left;
//   T2 right;
//
//   //Side() : right{} {}
//
//
// };
// public:
//   ~Side() {}
//   //Side* operator&() { return &side; };
//
// };

template <class T1, class T2>
class Either {
  union Side {
    T1 left;
    T2 right;
    Side(void){};
    Side(const Side&);
    // La classe est référencée en indiquant
    // son type entre < et > ("Stack<T>").
    // Ici, ce n'est pas une nécessité
    // cependant.
    ~Side(void){};
    Side& operator=(const Side&);
  };
  
  template <class T1_, class T2_>
  friend Either<T1_, T2_> Left_(T1_ x);
  template <class T1_, class T2_>
  friend Either<T1_, T2_> Right_(T2_ x);
  
  
public:
  bool is_left;
  Side side;
  // Either<T1, T2> either;
  
  Either(void){};
  Either(const Either<T1, T2>&);
  // La classe est référencée en indiquant
  // son type entre < et > ("Stack<T>").
  // Ici, ce n'est pas une nécessité
  // cependant.
  ~Either(void){};
  Either<T1, T2>& operator=(const Either<T1, T2>&);
  
  explicit Either<T1, T2>(T2 right_) : is_left(false) {
    new(&side.right) T2;
    // side.left = T1();
    side.right = right_;
  }
  Either<T1, T2>(T1 left_, bool bs) : is_left(true) {
    new(&side.left) T1;
    // side.left = T1();
    side.left = left_;
  }
  // Either<T1, T2>(Rcpp::XPtr<Either<T1, T2>> ptr_, bool a, bool b)
  //   : either(*(ptr_.get())), is_left(either.is_left) {
  //   if(is_left){
  //     new(&side.left) T1;
  //     side.left = (either.side).left;
  //   }else{
  //     new(&side.right) T2;
  //     side.right = either.side.right;
  //   }
  // }
  
  //~Either<T1, T2>() {}
  // const Either<T1, T2> operator&() { return &this; };
  
  bool matchLeft(T1& x) {
    if(is_left)
      x = side.left;
    return is_left;
  }
  bool matchRight(T2& x) {
    if(!is_left)
      x = side.right;
    return !is_left;
  }
  
  T1 fromLeft(T1 dflt) {
    if(is_left) {
      return side.left;
    } else {
      return dflt;
    }
  }
  
  T2 fromRight(T2 dflt) {
    if(is_left) {
      return dflt;
    } else {
      return side.right;
    }
  }
  
  bool isLeft() { return is_left; }
  
  bool isRight() { return !is_left; }
  
  
  
  // Either& operator=(Either& other);
};

typedef Either<std::string, Rcpp::RObject> ErrorOrObject;
typedef class Either<std::string, Rcpp::RObject>* ErrorOrObjectPtr;
typedef class Either<std::string, Rcpp::RObject>& ErrorOrObjectRef;
// int main(int argc, char* argv[])
// {
//   Either<int, float> x = Left<int, float>(5);
//   int c;
//   if (x.matchLeft(c))
//     printf("the number is %i", c);
//   return 0;
// }