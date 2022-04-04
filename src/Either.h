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
  union  {
  T1 left;
  T2 right;
};
  bool is_left;
  
  template <class T1_, class T2_>
  friend Either<T1_, T2_> Left(T1_ x);
  template <class T1_, class T2_>
  friend Either<T1_, T2_> Right(T2_ x);

 public:
  explicit Either<T1, T2>(const T2 &right_) : is_left(false), right(right_) {
    //new(&side.right) T2;
    // side.left = T1();
    //side.right = right_;
  }
  explicit Either<T1, T2>(const T1 &left_, bool bs) : is_left(true), left(left_) {}
   
  ~Either<T1, T2>() {}
  Either<T1, T2>* operator&() { return &this; };

  bool matchLeft(T1& x) {
    if(is_left)
      x = left;
    return is_left;
  }
  bool matchRight(T2& x) {
    if(!is_left)
      x = right;
    return !is_left;
  }

  T1 fromLeft(T1 dflt) {
    if(is_left) {
      return left;
    } else {
      return dflt;
    }
  }

  T2 fromRight(T2 dflt) {
    if(is_left) {
      return dflt;
    } else {
      return right;
    }
  }

  bool isLeft() { return is_left; }

  bool isRight() { return !is_left; }
   
   //Either& operator=(Either& other);
};

typedef class Either<std::string, Rcpp::RObject> ErrorOrObject;
// int main(int argc, char* argv[])
// {
//   Either<int, float> x = Left<int, float>(5);
//   int c;
//   if (x.matchLeft(c))
//     printf("the number is %i", c);
//   return 0;
// }