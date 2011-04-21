/*-------------------------------------------------------------------
 * Define a double integer.  This replaces the horrible mess that
 * used to be the "DoubleInt" module.  Use of dlong should
 * improve the portability of magic.
 *
 * This defines "dlong" to be an 8-byte double-precision integer.
 *-------------------------------------------------------------------
 */

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
  #ifdef HAVE_SYS_PARAM_H
  #include <sys/param.h>
  #endif
#endif

#if defined(LONG_MAX) && defined(INT_MAX) 
  #if LONG_MAX == INT_MAX
  typedef long long dlong;
  #else
  typedef long dlong;
  #endif
#else
typedef long long dlong;
#endif

/* Modified by NP & Tim 7/04 */
// MAX vaule of dlong = 9223372036854775807LL [2^63]
#ifndef DLONG_MAX
  /* Note:  Linux defines LLONG_MAX but not LONG_LONG_MAX */
  #ifdef LLONG_MAX
  #define DLONG_MAX LLONG_MAX
  #else
    #ifdef LONG_LONG_MAX
    #define DLONG_MAX LONG_LONG_MAX
    #else
    #define DLONG_MAX 0x7FFFFFFFFFFFFFFFLL
    #endif
  #endif
#endif

#ifndef DLONG_MIN
  /* Note:  Linux defines LLONG_MIN but not LONG_LONG_MIN */
  #ifdef LLONG_MIN
  #define DLONG_MIN LLONG_MIN
  #else
    #ifdef LONG_LONG_MIN
    #define DLONG_MIN LONG_LONG_MIN
    #else
    #define DLONG_MIN 0x8000000000000000LL
    #endif
  #endif
#endif
