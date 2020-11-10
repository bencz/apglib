/*‚*-----------------------------------------------------------------*/
/*‚* VERSCHLÜSSELN MIT CRYPT()                                       */
/*‚*---------------------------------------------------------------  */
/*‚*                                                                 */
/*‚* AUTOR         :  A. PIEGER                                      */
/*‚*                                                                 */
/*‚* ERSTELLT AM   :  26.07.2011                                     */
/*‚*                                                                 */
/*‚* FUNKTION      :  VERSCHLÜSSELN                                  */
/*‚*                                                                 */
/*‚* ÄNDERUNGEN:                                                     */
/*‚*‚DATUM      VON   GRUND DER ÄNDERUNG                             */
/*‚*                                                                 */
/*‚****************************************************************  */
#include "APGGLOBAL.h"
#include <stdio.h>
#include <stdlib.h>

#include <time.h>
#include <unistd.h>
#include <crypt.h>

void CCP0006 (char *x_string, char *x_salt, char *x_result)
{
 unsigned long seed[2];
 char salt[] = "$1$........";
 char* result;

 const char *const seedchars = "./0123456789ABCDEFGHIJKLMNOPQRST"
                               "UVWXYZabcdefghijklmnopqrstuvwxyz";
 int i;

 /* Generate a (not very) random seed.
    You should do it better than this... */
 seed[0] = time(NULL);
 seed[1] = getpid() ^ (seed[0] >> 14 & 0x30000);

 /* Turn it into printable characters from `seedchars'. */
 for (i = 0; i < 8; i++)
 {
  salt[3+i] = seedchars[(seed[i/5] >> (i%5)*6) & 0x3f];
 }

 /* Read in the user's password and encrypt it. */
 result = crypt(x_string, salt);
 memcpy(x_result, result, sizeof(result));

 return;

}
