/*‚*-----------------------------------------------------------------*/
/*‚* ZUFALLSZAHL ERMITTELN                                           */
/*‚*---------------------------------------------------------------  */
/*‚*                                                                 */
/*‚* AUTOR         :  A. PIEGER                                      */
/*‚*                                                                 */
/*‚* ERSTELLT AM   :  22.06.2011                                     */
/*‚*                                                                 */
/*‚* FUNKTION      :  MITTELS rand() ZAHL GENERIEREN                 */
/*‚*                                                                 */
/*‚* ÄNDERUNGEN:                                                     */
/*‚*‚DATUM      VON   GRUND DER ÄNDERUNG                             */
/*‚*                                                                 */
/*‚****************************************************************  */
#include "APGGLOBAL.h"
#include <stdio.h>
#include <stdlib.h>

DLLEXPORT void CCP0004 (int *x_seed, int *x_max, int *x_result)
{
    //‚Initialisieren
    *x_result = 0;

    //‚Jedesmal den Seed angeben, damit man ggf. die Zufallsfolge
    //‚wieder generieren kann
    if(*x_seed > 0)
    {
     srand(*x_seed);
    }

    //‚Jetzt berechnen, Beispiel von:
    //‚http://www.cplusplus.com/reference/clibrary/cstdlib/rand/
    *x_result = (rand() % *x_max) + 1;

    return;
}
