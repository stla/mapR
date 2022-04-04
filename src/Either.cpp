#include "mapR.h"
#include "Either.h"

template <class T1, class T2> 
Either<T1, T2> Left_(T1 x)
{
  Either<T1, T2> e(x, true);
  //e.is_left = true;
  //e.left = x;
  return e;
}
template <class T1, class T2> 
Either<T1, T2> Right_(T2 x)
{
  Either<T1, T2> e(x);
  //e.is_left = false;
  //e.right = x;
  return e;
}



// // [[Rcpp::export]]
// Either<std::string, double> myright(double obj){
//   double x = obj;
//   Either<std::string, double> e(Right<std::string, double>(x));
//   return e;
// }

// // [[Rcpp::export]]
// ErrorOrObject test(const Rcpp::RObject x){
//   ErrorOrObject y(x);
//   return y;
// }

// template <class T1, class T2> 
// T1 fromLeft(T1 dflt, Either<T1, T2> e){
//   if(e.is_left){
//     return e.left;
//   }else {
//     return dflt;
//   }
// }
// 
// template <class T1, class T2> 
// T2 fromRight(T2 dflt, Either<T1, T2> e){
//   if(e.is_left){
//     return e.left;
//   }else {
//     return dflt;
//   }
// }

RCPP_MODULE(class_ErrorOrObject) {
  using namespace Rcpp;
  
  class_<ErrorOrObject>("ErrorOrObject")

    .constructor<Rcpp::RObject>()
    .constructor<std::string, bool>()

    .method("isLeft", &ErrorOrObject::isLeft)
    .method("isRight", &ErrorOrObject::isRight)
    .method("fromLeft", &ErrorOrObject::fromLeft)
    .method("fromRight", &ErrorOrObject::fromRight);
}

class EitherFunc {
public:
  Rcpp::XPtr<ErrorOrObject> Left(std::string x){
    return Rcpp::XPtr<ErrorOrObject>(new ErrorOrObject(Left_<std::string, Rcpp::RObject>(x)));
  }
  Rcpp::XPtr<ErrorOrObject> Right(Rcpp::RObject x){
    return Rcpp::XPtr<ErrorOrObject>(new ErrorOrObject(Right_<std::string, Rcpp::RObject>(x)));
  }
};

RCPP_MODULE(class_EitherFunc) {
  Rcpp::class_<EitherFunc>( "EitherFunc" )
  .constructor()
  .method("Left", &EitherFunc::Left)
  .method("Right", &EitherFunc::Right)
  ;
}
