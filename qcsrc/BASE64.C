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
/*‚ AUTOR         :           B. GERLICH                             */
/*‚                                                                  */
/*‚ ERSTELLT AM   :           JANUAR 2009                            */
/*‚                                                                  */
/*‚ PROBLEM       :           BASE64 Handling - Decoding und Encoding*/
/*‚                           Die Parameter müssen in ASCII bzw.     */
/*‚                           UTF8 übergeben werden                  */
/*‚                                                                  */
/*‚ ÄNDERUNGEN:                                                      */
/*‚ DATUM      VON            GRUND DER ÄNDERUNG                     */
/*‚                                                                  */
/*‚------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "APGGLOBAL.h"

#define DECODE_ERROR 0xffffffff
#define uchar unsigned char
/*  ASCII codierter String
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"; */
char BASE64_chars[] = {
     0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A,
     0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50, 0x51, 0x52, 0x53, 0x54,
     0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x61, 0x62, 0x63, 0x64,
     0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E,
     0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
     0x79, 0x7A, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
     0x38, 0x39, 0x2B, 0x2F
};

int BASE64_pos(char c)
{
    char *p;
    for (p = BASE64_chars; *p; p++)
        if (*p == c)
            return p - BASE64_chars;
    return -1;
}

int BASE64_encode(uchar *x_data_in, uchar *x_data_out, int *x_len_in, int *x_len_out)
{
    uchar *s, *p;
    int i;
    int c;
    uchar *q;

    if((*x_len_in * 4 / 3 + 4) > *x_len_out)
    {
      *x_len_out = 0;
      return -1;
    }

    p = s = x_data_out;
    if (p == NULL)
    {
      *x_len_out = 0;
      return -1;
    }
    q = x_data_in;
    i = 0;
    for (i = 0; i < *x_len_in;) {
        c = q[i++];
        c *= 256;
        if (i < *x_len_in)
            c += q[i];
        i++;
        c *= 256;
        if (i < *x_len_in)
            c += q[i];
        i++;
        p[0] = BASE64_chars[(c & 0x00fc0000) >> 18];
        p[1] = BASE64_chars[(c & 0x0003f000) >> 12];
        p[2] = BASE64_chars[(c & 0x00000fc0) >> 6];
        p[3] = BASE64_chars[(c & 0x0000003f) >> 0];
        if (i > *x_len_in)
            p[3] = 0x3D; /* '=' */
        if (i > *x_len_in + 1)
            p[2] = 0x3D; /* '=' */
        p += 4;
    }
    *x_len_out = p - x_data_out;
    return 0;
}


static unsigned int BASE64_token_decode(const char *token)
{
    int i;
    unsigned int val = 0;
    int marker = 0;
    for (i = 0; i < 4; i++) {
        val *= 64;
        if (token[i] == 0x3D) /* '=' */
            marker++;
        else if (marker > 0)
            return DECODE_ERROR;
        else
            val += BASE64_pos(token[i]);
    }
    if (marker > 2)
        return DECODE_ERROR;
    return (marker << 24) | val;
}

int BASE64_decode(uchar *x_data_in, uchar *x_data_out, int *x_len_in, int *x_len_out)
{
    unsigned char *p;
    unsigned char *q;
    int i;
    unsigned int val;
    unsigned int marker;

    q = x_data_out;
    for (i = 0; i<*x_len_in; i += 4) {
        p = x_data_in + i;
        if(!(*p == 0x3D || strchr(BASE64_chars, *p)) || *x_len_out <= 0)
        {
          *x_len_out=0;
          return -1;
        }
        if ( (*x_len_in - i) < 4)
        {
          *x_len_out=0;
          return -1;
        }
        val = BASE64_token_decode(p);
        marker = (val >> 24) & 0xff;
        if (val == DECODE_ERROR)
        {
          *x_len_out=0;
          return -1;
        }
        *q++ = (val >> 16) & 0xff;
        *x_len_out = *x_len_out - 1;
        if(*x_len_out <= 0)
        {
          *x_len_out=0;
          return -1;
        }
        if (marker < 2)
        {
            *q++ = (val >> 8) & 0xff;
            *x_len_out = *x_len_out - 1;
            if(*x_len_out <= 0)
            {
              *x_len_out=0;
              return -1;
            }
        }
        if (marker < 1)
        {
            *q++ = val & 0xff;
            *x_len_out = *x_len_out - 1;
            if(*x_len_out <= 0)
            {
              *x_len_out=0;
              return -1;
            }
        }
    }
    *x_len_out = q - x_data_out;
    return 0;
}


/* Einstiegsfunktion */
/* x_data_in und x_data_out dürfen auf die gleiche Variable zeigen             */
/* x_len_in  = Länge der zu konvertierenden Daten                              */
/* x_len_out = Beim Einstieg: Zur Umsetzung verfügbarer Platz in x_data_out    */
/*           = Als Rückgabe:  Länge der generierten Daten                      */
void DLLEXPORT BASE64(char *x_action, uchar *x_data_in, uchar *x_data_out,
               int *x_len_in, int *x_len_out, char *x_retcode)
{
  uchar *data_out;
  int   outlen_max;

  *x_retcode = ' ';

  /* Die Länge muss gültig sein */
  if(*x_len_in <= 0 || *x_len_out <= 0)
  {
    *x_retcode = '1';
    return;
  }

  /* Speicher für die Zielvariable holen */
  data_out = malloc(*x_len_out);
  if(data_out == NULL)
  {
    *x_retcode = '2';
    return;
  }

  outlen_max = *x_len_out;
  memset(data_out, ' ', *x_len_out);

  if(*x_action == '0')
  {
    if(BASE64_encode(x_data_in, data_out, x_len_in, x_len_out) != 0)
    {
      *x_retcode = '3';
      memcpy(x_data_out, data_out, outlen_max);
      free(data_out);
      return;
    }
  }else
  {
    if(BASE64_decode(x_data_in, data_out, x_len_in, x_len_out) != 0)
    {
      *x_retcode = '3';
      memcpy(x_data_out, data_out, outlen_max);
      free(data_out);
      return;
    }
  }
  memcpy(x_data_out, data_out, outlen_max);

  free(data_out);
  data_out = NULL;

  return;
}
