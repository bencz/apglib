     /*****************************************************************
      * KOMMUNIKATIONS-PGM xxx <-> LFS 400 über TCP/IP                *
      *---------------------------------------------------------------*
      * COPYRIGHT BY  :  EHRHARDT + PARTNER  GMBH                     *
      *                  PLANUNG + REALISIERUNG EDV-GESTÜTZTER        *
      *                  BETRIEBSABLÄUFE UND PROGRAMMIERUNG           *
      *                  56154 BOPPARD-BUCHHOLZ                       *
      *                  TEL 06742 / 87270                            *
      *---------------------------------------------------------------*
      *                                                               *
      * AUTOR         :           J. Fröhlich                         *
      *                                                               *
      * ERSTELLT AM   :           März 2000                           *
      *                                                               *
      *                                                               *
      * PROBLEM       :           Globale Definition von Variablen    *
      *                           und Funktionen für TCP/IP           *
      *                           Socketverbindung                    *
      *                           -- CLIENT + SERVER --               *
      *                                                               *
      * ÄNDERUNGEN:                                                   *
      * DATUM      VON            GRUND DER ÄNDERUNG                  *
      *                                                               *
      ****************************************************************/
  /*˜Beim Compilieren muß Parameter
 ˜   DEFINE = PRTDEB angegeben werden wenn
 ˜   Ausgabe der Meldung auf dem Bildschirm eroflgen soll
  */
  /* Parameter OUTPUT  *PRINT für Compiler-Listing */
  /* Parameter DBGVIEW *ALL für Programm zu debuggen */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <sys/un.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <iconv.h>
#include <micomput.h>
#include <decimal.h>
#include <errno.h>

#ifndef TRUE
  #define TRUE 1
  #define FALSE 0
#endif

#define DEFAULT_BUFLEN 1000
#define DEFAULT_PORT 5001
#define DEFAULT_PROTO SOCK_DGRAM

typedef int SOCKET;
#define SOCKET_ERROR -1
#define INVALID_SOCKET -1
#define closesocket(a) close(a)

SOCKET msgsock, conn_socket;
struct sockaddr_in local, from;
volatile int sigcount=0;
iconv_t  cp_in,cp_out;
char last_to_cp[5];
char last_from_cp[5];

 /* Zeitüberwachung beim Empfangen von Telegrammen */
 /* Wird aufgerufen wenn Zeit abgelaufen ist */
void catch_alarm( int sig )
{
#ifdef PRTDEB
  fprintf(stderr,"--> Signal catcher called for signal %d\n", sig);
#endif
  sigcount = 1;
  return;
}

 /*‚Abnormale Beendigung des Jobs */
 /*‚AS/400 spezifische Funktion für abnormale Beendigung */
void CancelHandler( _CNL_Hndlr_Parms_T *cancel_info )
{
#ifdef PRTDEB
  fprintf(stderr,"--> CancelHandler called with %d\n",
         *( (volatile unsigned *)cancel_info->Com_Area ) );
#endif
  if ( *( (volatile unsigned *)cancel_info->Com_Area ) != INVALID_SOCKET )
    closesocket( *( (volatile unsigned *)cancel_info->Com_Area ) );
}

 /* Konvertierung von ASCII in EBCDIC */
 /* Laden der Tabelle entsprechend übergebener Code-Page */
int load_ae_tbl(char *x_from_cp, char *x_to_cp )
{
   char        tocode[33],fromcode[33];
   int         rc;
   static int first_call = 1;

 /*‚Beim ersten Aufruf die Variablen für die alten Codepages leeren */
   if(first_call == 1)
   {
     first_call = 0;
     memset(&last_to_cp, 0, 5);
     memset(&last_from_cp, 0, 5);
   }

 /*‚Prüfen ob diese Codepage bereits einmal angefragt wurde */
   if( (memcmp(x_to_cp, (char*)&last_to_cp, 5) != 0) ||
       (memcmp(x_from_cp, (char*)&last_from_cp, 5) != 0) )
   {
 /*‚   Nur freigeben, wenn es auch eine alte Codepage gab */
       if(strlen((char*)&last_to_cp) > 0)
       {
         rc = iconv_close( cp_out );
         rc = iconv_close( cp_in );
         memset((char*)&last_to_cp, 0, 5);
         memset((char*)&last_from_cp, 0, 5);
       }
   }else
   {
     return 0;
   }

 /*‚Nun Codepagebezeichner über iconv holen */
   memset(tocode,0,sizeof(tocode));
   memset(fromcode,0,sizeof(fromcode));
   strcpy(fromcode,"IBMCCSID     0000000");
   strcpy(tocode,"IBMCCSID     ");

   memcpy(&fromcode[8], x_to_cp, 5);
   memcpy(&tocode[8], x_from_cp, 5);

   /* Systemfunktion AS/400 iconv_open liefert Handle für Übersetzung
     zurück */
   /* Aufruf für Umsetzung von ASCII in EBCDIC */
   cp_in  = iconv_open( tocode, fromcode );
   if ( cp_in.return_value < 0 )
   {
     perror("load_ae_tbl: iconv_open in");
     return( -1 );
   }

   memcpy(&tocode[8],x_to_cp,5);
   memcpy(&fromcode[8],x_from_cp,5);

   /* Aufruf für Umsetzung von EBCDIC in ASCII */
   cp_out = iconv_open( tocode, fromcode );
   if ( cp_out.return_value < 0 )
   {
     perror("load_ae_tbl: iconv_open out");
     return( -1 );
   }

   memcpy((char*)&last_from_cp, x_from_cp, 5);
   memcpy((char*)&last_to_cp, x_to_cp, 5);

   return( 0 );
}

 /* Konvertierung von ASCII in EBCDIC */
 /* Laden der Tabelle entsprechend übergebener Code-Page */
 /* Erweiterte Version für Qt, arbeitet mit längeren Übergabeparametern */
int load_ae_tbl_ext(char *x_from_cp, char *x_to_cp )
{
   char        tocode[33],fromcode[33];
   int         rc;
   static int first_call = 1;

 /* Beim ersten Aufruf die Variablen für die alten Codepages leeren */
   if(first_call == 1)
   {
     first_call = 0;
     memset(&last_to_cp, 0, 20);
     memset(&last_from_cp, 0, 20);
   }

 /* Prüfen ob diese Codepage bereits einmal angefragt wurde */
   if( (memcmp(x_to_cp, (char*)&last_to_cp, 20) != 0) ||
       (memcmp(x_from_cp, (char*)&last_from_cp, 20) != 0) )
   {
 /*    Nur freigeben, wenn es auch eine alte Codepage gab */
       if(strlen((char*)&last_to_cp) > 0)
       {
         rc = iconv_close( cp_out );
         rc = iconv_close( cp_in );
         memset((char*)&last_to_cp, 0, 20);
         memset((char*)&last_from_cp, 0, 20);
       }
   }else
   {
     return 0;
   }
#ifdef __eup_as400__
   /* Nun Codepagebezeichner über iconv holen */
   memset(tocode,0,sizeof(tocode));
   memset(fromcode,0,sizeof(fromcode));
   strcpy(fromcode,"IBMCCSID     0000000");
   strcpy(tocode,"IBMCCSID     ");

   memcpy(&fromcode[8], x_to_cp, 5);
   memcpy(&tocode[8], x_from_cp, 5);
#else
   // Nun Codepagebezeichner über iconv holen */
   memset(tocode,0,sizeof(tocode));
   memset(fromcode,0,sizeof(fromcode));
   strncpy(fromcode, x_to_cp, 20);
   strncpy(tocode, x_from_cp, 20);

   //fprintf(stderr, "cp_in vor convertCPLnxWin fromcode: <%s> tocode: <%s>\n", fromcode, tocode);

   convertCPLnxWin(fromcode);
   convertCPLnxWin(tocode);

   //fprintf(stderr, "cp_in nach convertCPLnxWin fromcode: <%s> tocode: <%s>\n", fromcode, tocode);
#endif

   /* Systemfunktion AS/400 iconv_open liefert Handle für Übersetzung
     zurück */
   /* Aufruf für Umsetzung von ASCII in EBCDIC */
   cp_in  = iconv_open( tocode, fromcode );
#ifdef __eup_as400__
   if(cp_in.return_value < 0 )
#else
   if(cp_in == (iconv_t)-1)
#endif
   {
       char tempChar[100];
       memset(tempChar, 0, 100);
       sprintf(tempChar, "load_ae_tbl: iconv_open in <%s><%s>", fromcode, tocode);
       perror(tempChar);
       return -1;
   }

#ifdef __eup_as400__
   memcpy(&tocode[8],x_to_cp,5);
   memcpy(&fromcode[8],x_from_cp,5);
#else
   memset(&tocode, 0, sizeof(tocode));
   memset(&fromcode, 0, sizeof(fromcode));

   strncpy(tocode, x_to_cp, 20);
   strncpy(fromcode, x_from_cp, 20);

   //fprintf(stderr, "cp_out vor convertCPLnxWin fromcode: <%s> tocode: <%s>\n", fromcode, tocode);

   convertCPLnxWin(fromcode);
   convertCPLnxWin(tocode);

   //fprintf(stderr, "cp_out nach convertCPLnxWin fromcode: <%s> tocode: <%s>\n", fromcode, tocode);
#endif

   /* Aufruf für Umsetzung von EBCDIC in ASCII */
   cp_out = iconv_open( tocode, fromcode );
#ifdef __eup_as400__
   if(cp_out.return_value < 0 )
#else
   if(cp_out == (iconv_t)-1)
#endif
   {
       char tempChar[100];
       memset(tempChar, 0, 100);
       sprintf(tempChar, "load_ae_tbl: iconv_open out <%s><%s>", fromcode, tocode);
       perror(tempChar);
       return -1;
   }

   strncpy(last_from_cp, x_from_cp, 20);
   strncpy(last_to_cp, x_to_cp, 20);

   return 0;
}

/* übersetzung des gesendeten Telegrams von ASCII in EBCDIC */
int asc2ebc(char *x_from, int *x_len_from, char *x_to, int *x_len_to)
{
   char        *p_in,*p_out;
   size_t      incount, outcount;
   int         rc;

   memset(x_to, 0, *x_len_to);
   memcpy(x_to, x_from, *x_len_from);

   p_in  = x_from;
   p_out = x_to;

   incount = *x_len_from;
   outcount = *x_len_to;

   /* Aufruf der Kovertierung */
   rc = iconv( cp_in, &p_in, &incount, &p_out, &outcount);
   *x_len_to = *x_len_to - outcount;
   if ( rc < 0 )
   {
     return( -1 );
   }

   return TRUE;
}

 /* übersetzung des zu sendenen Telegramms EBCDIC in ASCII */
int ebc2asc(char *x_from, int *x_len_from, char *x_to, int *x_len_to)
{
   char        *p_in,*p_out;
   size_t      incount, outcount;
   int         rc;

   memset(x_to, 0, *x_len_to);
   memcpy(x_to, x_from, *x_len_from);

   p_in  = x_from;
   p_out = x_to;

   incount = *x_len_from;
   outcount = *x_len_to;

   /* Aufruf der Kovertierung */
   rc = iconv( cp_out, &p_in, &incount, &p_out, &outcount);
   *x_len_to = *x_len_to - outcount;
   if ( rc < 0 )
   {
     return( -1 );
   }

   return TRUE;
}

void convertCPLnxWin(char *codepage)
{
    if(strncmp(codepage, "01208", 5) == 0)
    {
        strcpy(codepage, "utf-8");
        return;
    }
    if(strncmp(codepage, "00000", 5) == 0)
    {
        strcpy(codepage, "CP819");
        //strcpy(codepage, "ISO8859-15");
        return;
    }
    if(strncmp(codepage, "00", 2) == 0)
    {
        memcpy(codepage, "CP", 2);
        return;
    }
    return;
}

