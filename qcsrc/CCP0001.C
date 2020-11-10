/*‚*-----------------------------------------------------------------*/
/*‚* ERMITTELN EINER KALENDERWOCHE                                   */
/*‚*---------------------------------------------------------------  */
/*‚*                                                                 */
/*‚* AUTOR         :  A. PIEGER                                      */
/*‚*                                                                 */
/*‚* ERSTELLT AM   :  25.02.2010                                     */
/*‚*                                                                 */
/*‚* FUNKTION      :  KALENDERWOCHE                                  */
/*‚*                                                                 */
/*‚* ÄNDERUNGEN:                                                     */
/*‚*‚DATUM      VON   GRUND DER ÄNDERUNG                             */
/*‚*                                                                 */
/*‚****************************************************************  */
#include "APGGLOBAL.h"
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

DLLEXPORT void CCP0001 (int *x_year, int *x_month, int *x_day,
                        char *x_week)
{
    time_t rawtime;
    struct tm * timeinfo;
    int kw_len = 0;
    char kw[10];

    /*‚AKTUELLE UHRZEIT ERMITTELN UND DIESE ÄNDERN */
    time ( &rawtime );
    timeinfo = localtime ( &rawtime );
    timeinfo->tm_year = *x_year - 1900;
    timeinfo->tm_mon = *x_month - 1;
    timeinfo->tm_mday = *x_day;

    /* call mktime: timeinfo->tm_wday will be set */
    mktime ( timeinfo );

    /*‚JETZT DIE KALENDERWOCHE AUSGEBEN */
    kw_len=strftime(kw, sizeof(kw), "%W", timeinfo);
    memcpy(x_week, kw, kw_len);

    return;
}
