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
/*‚ AUTOR         :  L. SCZECH                                       */
/*‚                                                                  */
/*‚ ERSTELLT AM   :  FEBRUAR 2010                                    */
/*‚                                                                  */
/*‚ FUNKTION      :  Clientnamen ermitteln                           */
/*‚                                                                  */
/*‚ ÄNDERUNGEN:                                                      */
/*‚ DATUM      VON            GRUND DER ÄNDERUNG                     */
/*‚------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

          void GETHOST(char *x_hostName)
{
    char hostName[512 + 1];
    int  hostLength;

    memset(x_hostName, ' ', sizeof(hostName) - 1);

    hostLength = sizeof(hostName);
/*‚ Ermitteln und Übernehmen des Hostnamen                          */
    if (gethostname(hostName, hostLength) >= 0)
    {
       memcpy(x_hostName, hostName, strlen(hostName));
    }
    else
    {
       memcpy(x_hostName, "*ERROR", 6);
    }

    return;

}
