template <class T1, class T2> 
class Either
{
  union Side
  {
    T1 left;
    T2 right; 
    
    Side() : right{} { }
  
    ~Side(){}
  };
  template<class T1_, class T2_> friend Either<T1_, T2_> Left(T1_ x);
  template<class T1_, class T2_> friend Either<T1_, T2_> Right(T2_ x);
public:
  bool is_left;
  union Side side;
  Either<T1, T2>(T2 right_): is_left(false), side() {
    new (&side.right) T2;
    //side.left = T1();
    side.right = right_;
  }
  ~Either(){}
  
  bool matchLeft(T1& x)
  {
    if (is_left)
      x = side.left;
    return is_left;    
  }
  bool matchRight(T2& x)
  {
    if (!is_left)
      x = side.right;
    return !is_left;    
  }
  
  T1 fromLeft(T1 dflt){
    if(is_left){
      return side.left;
    }else {
      return dflt;
    }
  }

  T2 fromRight(T2 dflt){
    if(is_left){
      return dflt;
    }else {
      return side.right;
    }
  }

  bool isLeft(){
    return is_left;
  }

  bool isRight(){
    return !is_left;
  }
  
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