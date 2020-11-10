/*‚------------------------------------------------------------------*/
/*‚ LAGERFÜHRUNGSSYSTEM   LFS 400                                    */
/*‚------------------------------------------------------------------*/
/*‚ COPYRIGHT BY  :  EHRHARDT + PARTNER GMBH & CO. KG                */
/*‚                  SOFTWARE SYSTEME FÜR WAREHOUSE LOGISTIK         */
/*‚                  56154 BOPPARD-BUCHHOLZ                          */
/*‚                  TEL 06742 / 87270                               */
/*‚------------------------------------------------------------------*/
/*‚                                                                  */
/*‚                                                                  */
/*‚ AUTOR         :  S. FELLINGER                                    */
/*‚                                                                  */
/*‚ ERSTELLT AM   :  FEBRUAR 2011                                    */
/*‚                                                                  */
/*‚ FUNKTION      :  RSA Hash des aktuellen Datums bestimmen         */
/*‚                                                                  */
/*‚ ÄNDERUNGEN:                                                      */
/*‚ DATUM      VON            GRUND DER ÄNDERUNG                     */
/*‚------------------------------------------------------------------*/
#include "EUPGLOBAL.h"
#include <time.h>

static unsigned long long LGRSADATE_modularPow(unsigned long long base,
                                               unsigned long long exponent,
                                               unsigned long long modulus);

DLLEXPORT void LGRSADATE(int *value)
{
    struct tm *newtime;
    time_t ltime;
    unsigned long long C;
    unsigned long long N = 71744549;
    unsigned long long e = 319171;

    ltime = time(NULL);
    newtime = localtime(&ltime);
    C = newtime->tm_mday * 1000000
      + (newtime->tm_mon + 1) * 10000
      + (newtime->tm_year + 1900);
    *value = LGRSADATE_modularPow(C, e, N);
    return;
}

static unsigned long long LGRSADATE_modularPow(unsigned long long base,
                                               unsigned long long exponent,
                                               unsigned long long modulus)
{
    unsigned long long res = 1;
    unsigned long long e_prime = 1;
    for( ; e_prime <= exponent; ++e_prime)
        res = (res * base)%modulus;
    return res;
}
