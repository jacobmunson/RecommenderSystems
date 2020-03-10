#include <Rcpp.h>
#include <iostream>     // std::cout
#include <algorithm>    // std::unique, std::distance
#include <vector>       // std::vector
#include <cmath>
#include <list>
#include <utility>
#include <queue>
#include <numeric>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double rcpp_sum(NumericVector v){
  double sum = 0;
  for(int i = 0; i < v.length(); ++i){
    sum += v[i];
  }
  return(sum);
}

// [[Rcpp::export]]
double rcpp_multiply(NumericVector v){
  double sum = 1;
  for(int i = 0; i < v.length(); ++i){
    sum = sum*v[i];
  }
  return(sum);
}


// https://stackoverflow.com/questions/36993935/find-the-largest-n-unique-values-and-their-frequencies-in-r-and-rcpp
class histogram {
private:
  struct paired {
    typedef std::pair<double, unsigned int> pair_t;
    
    pair_t pair;
    unsigned int is_set;
    
    paired() 
      : pair(pair_t()),
        is_set(0)
    {}
    
    paired(double x)
      : pair(std::make_pair(x, 1)),
        is_set(1)
    {}
    
    bool operator==(const paired& other) const {
      return pair.first == other.pair.first;
    }
    
    bool operator==(double other) const {
      return is_set && (pair.first == other);
    }
    
    bool operator>(double other) const {
      return is_set && (pair.first > other);
    }
    
    bool operator<(double other) const {
      return is_set && (pair.first < other);
    }
    
    paired& operator++() {
      ++pair.second;
      return *this;
    }
    
    paired operator++(int) {
      paired tmp(*this);
      ++(*this);
      return tmp;
    }
  };
  
  struct greater {
    bool operator()(const paired& lhs, const paired& rhs) const {
      if (!lhs.is_set) return false;
      if (!rhs.is_set) return true;
      return lhs.pair.first > rhs.pair.first;
    }
  };  
  
  typedef std::priority_queue<
    paired,
    std::vector<paired>,
    greater
  > queue_t;
  
  unsigned int sz;
  queue_t queue;
  
  void insert(double x) {
    if (queue.empty()) {
      queue.push(paired(x));
      return;
    }
    
    if (queue.top() > x && queue.size() >= sz) return;
    
    queue_t qtmp;
    bool matched = false;
    
    while (queue.size()) {
      paired elem = queue.top();
      if (elem == x) {
        qtmp.push(++elem);
        matched = true;
      } else {
        qtmp.push(elem);
      }
      queue.pop();
    }
    
    if (!matched) {
      if (qtmp.size() >= sz) qtmp.pop();
      qtmp.push(paired(x));
    }
    
    std::swap(queue, qtmp);
  }
  
public:
  histogram(unsigned int sz_) 
    : sz(sz_), 
      queue(queue_t())
  {}
  
  template <typename InputIt>
  void insert(InputIt first, InputIt last) {
    for ( ; first != last; ++first) {
      insert(*first);
    }
  }
  
  Rcpp::List get() const {
    Rcpp::NumericVector values(sz);
    Rcpp::IntegerVector freq(sz);
    R_xlen_t i = 0;
    
    queue_t tmp(queue);
    while (tmp.size()) {
      values[i] = tmp.top().pair.first;
      freq[i] = tmp.top().pair.second;
      ++i;
      tmp.pop();
    }
    
    return Rcpp::List::create(
      Rcpp::Named("value") = values,
      Rcpp::Named("frequency") = freq);
  }
};


// [[Rcpp::export]]
Rcpp::List top_n(Rcpp::NumericVector x, int n = 5) {
  histogram h(n);
  h.insert(x.begin(), x.end());
  return h.get();
} 



// this works - https://www.geeksforgeeks.org/program-find-correlation-coefficient/
// [[Rcpp::export]]
float correlationCoefficient(NumericVector X, NumericVector Y) 
{ 
  
  int n = X.length();
  float sum_X = 0, sum_Y = 0, sum_XY = 0; 
  float squareSum_X = 0, squareSum_Y = 0; 
  
  for (int i = 0; i < n; i++){ 
    // sum of elements of array X. 
    sum_X = sum_X + X[i]; 
    
    // sum of elements of array Y. 
    sum_Y = sum_Y + Y[i]; 
    
    // sum of X[i] * Y[i]. 
    sum_XY = sum_XY + X[i] * Y[i]; 
    
    // sum of square of array elements. 
    squareSum_X = squareSum_X + X[i] * X[i]; 
    squareSum_Y = squareSum_Y + Y[i] * Y[i]; 
  } 
  
  // use formula for calculating correlation coefficient. 
  float corr = (float)(n * sum_XY - sum_X * sum_Y) / sqrt((n * squareSum_X - sum_X * sum_X) * (n * squareSum_Y - sum_Y * sum_Y)); 
             
  return corr; 
} 


// [[Rcpp::export]]
float cosine_vector_similarity(NumericVector X, NumericVector Y) 
{ 
  
  int n = X.length();
  float sum_X = 0, sum_Y = 0, sum_XY = 0; 
  float squareSum_X = 0, squareSum_Y = 0; 
  
  for (int i = 0; i < n; i++){ 
    // sum of elements of array X. 
    sum_X = sum_X + X[i]; 
    
    // sum of elements of array Y. 
    sum_Y = sum_Y + Y[i]; 
    
    // sum of X[i] * Y[i]. 
    sum_XY = sum_XY + X[i] * Y[i]; 
    
    // sum of square of array elements. 
    squareSum_X = squareSum_X + X[i] * X[i]; 
    squareSum_Y = squareSum_Y + Y[i] * Y[i]; 
  } 
  
  // use formula for calculating correlation coefficient. 
  float cos = (float)(sum_XY)/(sqrt(squareSum_X)*sqrt(squareSum_Y)); 
  
  return cos; 
} 









