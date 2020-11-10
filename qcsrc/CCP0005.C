/*‚*-----------------------------------------------------------------*/
/*‚* SUCHE MITTELS REGULAR EXPRESSION                                */
/*‚*---------------------------------------------------------------  */
/*‚*                                                                 */
/*‚* AUTOR         :  A. PIEGER                                      */
/*‚*                                                                 */
/*‚* ERSTELLT AM   :  30.06.2011                                     */
/*‚*                                                                 */
/*‚* FUNKTION      :  SUCHEN                                         */
/*‚*                                                                 */
/*‚* ÄNDERUNGEN:                                                     */
/*‚*‚DATUM      VON   GRUND DER ÄNDERUNG                             */
/*‚*                                                                 */
/*‚****************************************************************  */
#include "APGGLOBAL.h"
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

void CCP0005 (char *x_string, char *x_pattern, int *x_result)
{
    int status;
    regex_t re;

    //‚Initialisieren
    *x_result = 0;
    if (regcomp(&re, x_pattern, REG_EXTENDED|REG_NOSUB) != 0)
    {
      return;      /* report error */
    }
    status = regexec(&re, x_string, (size_t) 0, NULL, 0);
    regfree(&re);

    if (status != 0) {
      return;      /* report error */
    }

    *x_result = 1;
    return;

    return;
}
