
#include <Rcpp.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "pmurhash.h"

#ifdef __cplusplus
}
#endif

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector hbmPRNG_digest(RawVector Txt) {
  char *txt = (char*) RAW(Txt);
#if defined(R_VERSION) && R_VERSION >= R_Version(3,0,0)
  R_xlen_t nChar = XLENGTH(Txt);
#else
  R_xlen_t nChar = LENGTH(Txt);
#endif
  unsigned int val = PMurHash32(0, txt, nChar);
  return val;
  // set_seed(val, val); // TODO: how should this actually be set?
}
