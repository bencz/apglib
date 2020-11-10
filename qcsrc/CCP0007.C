/*‚*-----------------------------------------------------------------*/
/*‚* VERSCHLÜSSELN MIT ENCRYPT()                                     */
/*‚*---------------------------------------------------------------  */
/*‚*                                                                 */
/*‚* AUTOR         :  A. PIEGER                                      */
/*‚*                                                                 */
/*‚* ERSTELLT AM   :  29.07.2011                                     */
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

void CCP0007 (char *x_string, const char *x_key)
{
 char* result;
 int   edflag=0;

 setkey(x_key);
 encrypt(x_string, 0);
 encrypt(x_string, 1);

 return;

}
