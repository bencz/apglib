     /*****************************************************************
      *---------------------------------------------------------------*
      * COPYRIGHT BY  :  EHRHARDT + PARTNER  GMBH & CO.KG             *
      *                  SOFTWARE-SYSTEME FÜR WAREHOUSE-LOGISTIK      *
      *                  56154 BOPPARD-BUCHHOLZ                       *
      *                  TEL 06742 / 87270                            *
      *---------------------------------------------------------------*
      *                                                               *
      * AUTOR         :           B. GERLICH                          *
      *                                                               *
      * ERSTELLT AM   :           APRIL 2007                          *
      *                                                               *
      *                                                               *
      * PROBLEM       :           UMSETZUNG VON DATEN PER ICONV VON   *
      *                           EINER CCSID ZUR ANDEREN             *
      *                           --- FÜR UNICODE, ANSONSTEN LGPGM0046*
      *                               VERWENDEN                       *
      *                                                               *
      * ÄNDERUNGEN:                                                   *
      * DATUM      VON            GRUND DER ÄNDERUNG                  *
      *                                                               *
      ****************************************************************/

#include "APGGLOBAL.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef __eup_windows__
   #include "windows\libiconv\include\iconv.h"
#else
    #include <iconv.h>
#endif

/* Generieren des CCSID-Namens für Linux/Windows */
void ccp0818_generateCCSID(char *x_target_from_cp, char *x_target_to_cp,
                   char *x_from_cp, char *x_to_cp)
{
  char ccsidStr[6];
  char *getenvPtr;
  int ccsidNum;

  /* Führene Nullen entfernen */
  memcpy(&ccsidStr, x_from_cp, 5);
  ccsidStr[5] = 0;

  ccsidNum = atoi((char*)&ccsidStr);
  /* Ggf. den Defaultwert für Latin-1 setzen */
  if(ccsidNum == 0)
  {
    /* Aktuelle CCSID aus der Umgebungsvariable lesen */
    getenvPtr = getenv("LFS400_JobCCSID");
    if(getenvPtr != NULL)
    {
      ccsidNum = atoi(getenvPtr);
    }
    /* Kein Wert in Umgebungsvariable? Dann Standardwert */
    if(ccsidNum == 0)
    {
      ccsidNum = 819;
    }
  }

  /* Nun den String für die Codepage generieren */
  if(ccsidNum == 1208)
  {
    sprintf(x_target_from_cp, "utf-8");
  }else if (ccsidNum == 13488)
  {
    sprintf(x_target_from_cp, "ucs-2le");
  }else
  {
    sprintf(x_target_from_cp, "CP%i", ccsidNum);
  }

  /* Ebenfalls die führenden Nullen für die Zielcodepage entfernen */
  memcpy(&ccsidStr, x_to_cp, 5);
  ccsidStr[5] = 0;

  ccsidNum = atoi((char*)&ccsidStr);
  /* Ggf. den Defaultwert für Latin-1 setzen */
  if(ccsidNum == 0)
  {
    /* Aktuelle CCSID aus der Umgebungsvariable lesen */
    getenvPtr = getenv("LFS400_JobCCSID");
    if(getenvPtr != NULL)
    {
      ccsidNum = atoi(getenvPtr);
    }
    /* Kein Wert in Umgebungsvariable? Dann Standardwert */
    if(ccsidNum == 0)
    {
      ccsidNum = 819;
    }
  }
  /* Nun den String für die Codepage generieren */
  if(ccsidNum == 1208)
  {
    sprintf(x_target_to_cp, "utf-8");
  }else if (ccsidNum == 13488)
  {
    sprintf(x_target_to_cp, "ucs-2le");
  }else
  {
    sprintf(x_target_to_cp, "CP%i", ccsidNum);
  }

  return;
}

 /* Konvertierung von CCSID zu CCSID */
void DLLEXPORT CCP0818(char *x_data, char *x_data_out, int *x_len_in,
               int *x_len_out, char *x_from_cp, char *x_to_cp, char *x_retcode)
{
   char        tocode[33],fromcode[33];
   int         rc;
   iconv_t     iconv_ccsid;
   char        *data_out;
   char        *p_in, *p_out;
   size_t      incount, outcount;

 /*‚Nun Codepagebezeichner über iconv holen */
   memset(tocode,0,sizeof(tocode));
   memset(fromcode,0,sizeof(fromcode));
#ifdef __eup_as400__
   strcpy(fromcode,"IBMCCSID     0000000");
   strcpy(tocode,"IBMCCSID     ");

   memcpy(&fromcode[8], x_from_cp, 5);
   memcpy(&tocode[8], x_to_cp, 5);
#endif
#ifdef __eup_windows__
   ccp0818_generateCCSID((char*)&fromcode, (char*)&tocode, x_from_cp, x_to_cp);
#endif
#ifdef __eup_linux__
   ccp0818_generateCCSID((char*)&fromcode, (char*)&tocode, x_from_cp, x_to_cp);
#endif

   /* Systemfunktion AS/400 iconv_open liefert Handle für Übersetzung
     zurück */
   /* Aufruf für Umsetzung von ASCII in EBCDIC */
   iconv_ccsid = iconv_open( tocode, fromcode );
#if defined __eup_windows__
   if ( iconv_ccsid == (iconv_t)-1 )
#endif
#if defined __eup_linux__
   if ( iconv_ccsid == (iconv_t)-1 )
#endif
#ifdef __eup_as400__
   if ( iconv_ccsid.return_value < 0 )
#endif
   {
     //perror("ccp0818: iconv_open");
     x_retcode[0] = '2';
     return;
   }

   incount = *x_len_in;
   outcount = *x_len_out;

   /* Speicher für die Zielvariable holen */
   data_out = malloc(*x_len_out);
   p_in = x_data;
   p_out = data_out;

   /* Aufruf der Kovertierung */
   rc = iconv( iconv_ccsid, &p_in, &incount, &p_out, &outcount);
   *x_len_out = *x_len_out - outcount;
   if ( rc < 0 )
   {
     x_retcode[0] = '3';
     rc = iconv_close(iconv_ccsid);
     free(data_out);
     return;
   }

   memcpy(x_data_out, data_out, *x_len_out);

   rc = iconv_close(iconv_ccsid);
   free(data_out);
   x_retcode[0] = ' ';

   return;
}
