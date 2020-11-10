/*‚*-----------------------------------------------------------------*/
/*‚* HEX-WERT 1:1 ÜBERNEHMEN                                         */
/*‚*---------------------------------------------------------------  */
/*‚*                                                                 */
/*‚* AUTOR         :  A. PIEGER                                      */
/*‚*                                                                 */
/*‚* ERSTELLT AM   :  02.08.2010                                     */
/*‚*                                                                 */
/*‚* FUNKTION      :  HEX-WERT AUFBEREITEN                           */
/*‚*                                                                 */
/*‚* ÄNDERUNGEN:                                                     */
/*‚*‚DATUM      VON   GRUND DER ÄNDERUNG                             */
/*‚*                                                                 */
/*‚****************************************************************  */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

void CCP0002(unsigned char *x_input, unsigned int *x_len, char *x_output, char *x_utfmode)
{
    char *tmpbuf;
    short tmp;
    short tmp2;
    int i;
    int outpos;
    if(*x_len == 0)
    {
      return;
    }

    if(*x_utfmode == '1')
    {
      memset(x_output, ' ', (*x_len / 2 * 8) );
    }else
    {
      memset(x_output, ' ', (*x_len * 2) );
    }
    tmpbuf = malloc((*x_len * 8) + 1);
    if(tmpbuf == NULL)
    {
      return;
    }
    outpos = 0;
    for(i=0;i<*x_len;i++)
    {
      tmp = x_input[i];
      if(*x_utfmode == '1')
      {
        tmp2 = x_input[i + 1];
        sprintf(tmpbuf + (outpos * 8), "&#x%02hX%02hX;", tmp, tmp2);
        outpos++;
        i++;
      }else
      {
        sprintf(tmpbuf + (outpos * 2), "%02hX", tmp);
        outpos++;
      }
    }

    if(*x_utfmode == '1')
    {
      memcpy(x_output, tmpbuf, (*x_len / 2 * 8));
    }else
    {
      memcpy(x_output, tmpbuf, (*x_len * 2));
    }
    free(tmpbuf);

    return;
}
