     /*****************************************************************
      * COPYRIGHT BY  :  EHRHARDT + PARTNER  GMBH & Co.KG             *
      *                  SOFTWARE-SYSTEME FÜR WAREHOUSE-LOGISTIK      *
      *                  56154 BOPPARD-BUCHHOLZ                       *
      *                  TEL 06742 / 87270                            *
      *---------------------------------------------------------------*
      *                                                               *
      * AUTOR         :           B. Gerlich                          *
      *                                                               *
      * ERSTELLT AM   :           Dezember 2008                       *
      *                                                               *
      * PROBLEM       :           HANDLING VON TCP/IP VERBINDUNGEN    *
      *                                                               *
      * ÄNDERUNGEN:                                                   *
      * DATUM      VON            GRUND DER ÄNDERUNG                  *
      ****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <APGGLOBAL.h>

#define CURR_PROG "IPIO.c"
#if defined __eup_linux__ || defined __eup_as400__
  #include <ctype.h>
  #include <signal.h>
  #include <unistd.h>
  #include <errno.h>
  #include <sys/types.h>
  #include <sys/time.h>
  #include <netinet/in.h>
  #include <netinet/tcp.h>
  #include <netdb.h>
  #include <sys/socket.h>
  #include <sys/un.h>
  #include <sys/ioctl.h>
  #include <arpa/inet.h>
#endif

#if defined __eup_linux__
  #include <syslog.h>
  #include "MSGHANDLING.h"
  #include "CONSTANTS.h"
#endif

#ifdef __eup_windows__
  #include <syslog.h>
  #include "MSGHANDLING.h"
  #include "CONSTANTS.h"
  #include <Winsock2.h>
  #include <portab.h>
  #define close(a) closesocket(a)
#endif

#define MAX_DELIM 10
int IPIO_debug = 0;
int *IPIO_socket;

DLLIMPORT extern int msgsock;

typedef struct delimElement
{
  int  delimLen;
  char delimTxt[52];
};
struct delimElement IPIO_delim[MAX_DELIM];

/* Trimmfunktion */
void IPIO_strtrim(char *x_string, int x_length)
{
  int i = 0;

  if(x_string == NULL)
  {
    return;
  }
  i = x_length -1;

  for(i = i; i > -1; i--)
  {
    if(x_string[i] == ' ')
    {
      x_string[i] = 0;
    }
    else
    {
      break;
    }
  }
  return;
}


/* Verbindung öffnen */
void IPIO_open(char *x_proto, char *x_buffer, int *x_buff_len, char *x_rettxt)
{
  int tmplen;
  int ip_addr;
  struct sockaddr_in local, from;
  int flag;
  int setsockopt_ret;
  struct hostent *hp;
  struct servent *sv;
  int port;
  char *chrptr;
  char *hostptr;
  char *portptr;

  if((*x_proto != '0') && (*x_proto != '1'))
  {
    memcpy(x_rettxt, "*INVPROT  ", 10);
    return;
  }

  if(*x_buff_len <= 0)
  {
    memcpy(x_rettxt, "*INVLEN   ", 10);
    return;
  }
  /* An welcher Stelle steht der trennende Doppelpunkt */
  chrptr = memchr(x_buffer, ':', *x_buff_len);
  if(chrptr == NULL)
  {
    memcpy(x_rettxt, "*INVADR   ", 10);
    return;
  }
  /* Berechnen vor der Doppelpunkt steht */
  tmplen = chrptr - x_buffer;
  /* Wurde vor bzw. hinter dem Doppelpunkt etwas mitgegeben? */
  /* Hostname */
  if(tmplen <= 0)
  {
    memcpy(x_rettxt, "*NO_HOST  ", 10);
    return;
  }

  /* Portname */
  if((*x_buff_len - tmplen) <= 1)
  {
    memcpy(x_rettxt, "*NO_PORT  ", 10);
    return;
  }

  /* Speicher für die Variablen + 1 für Hex Null allokieren */
  hostptr = malloc(tmplen + 1);
  if(hostptr == NULL)
  {
    memcpy(x_rettxt, "*NO_MEM   ", 10);
    return;
  }
  portptr = malloc(*x_buff_len - tmplen);
  if(hostptr == NULL)
  {
    free(hostptr);
    memcpy(x_rettxt, "*NO_MEM   ", 10);
    return;
  }

  /* Host und Port übernehmen */
  memcpy(hostptr, x_buffer, tmplen);
  hostptr[tmplen] = 0;
  memcpy(portptr, x_buffer + tmplen + 1, (*x_buff_len - tmplen) - 1);
  portptr[(*x_buff_len - tmplen) - 1] = 0;

  /* Spaces abschneiden */
  IPIO_strtrim(hostptr, strlen(hostptr));
  IPIO_strtrim(portptr, strlen(portptr));

  /* Sind die Strings jetzt noch gefüllt? */
  if(strlen(hostptr) <= 0)
  {
    free(portptr);
    free(hostptr);
    memcpy(x_rettxt, "*EMPTYHOST", 10);
    return;
  }
  if(strlen(portptr) <= 0)
  {
    free(portptr);
    free(hostptr);
    memcpy(x_rettxt, "*EMPTYPORT", 10);
    return;
  }

  /* Werte sind ok, nun ggf. die Namensauflösung durchführen */
  if ( (port = atoi(portptr)) == 0 )
  {
    if(*x_proto == '0')
    {
      sv = getservbyname(portptr, "tcp");
    }else
    {
      sv = getservbyname(portptr, "udp");
    }
    if ( sv )
    {
#ifdef __eup_as400__
       port = sv->s_port;
#endif
#if defined __eup_linux__ || defined __eup_windows__
       port =ntohs(sv->s_port);
#endif
    }
  }
  if (port == 0)
  {
    memcpy(x_rettxt,"*INVSVC   ",10);
    free(portptr);
    free(hostptr);
    return;
  }

  /* Hostname */
  if ( isdigit(hostptr[0]) )
  {
    ip_addr = inet_addr(hostptr);
  }
  else
  {
    /*‚Namen aus TCP/IP-Host-Tabelle ermitteln */
    hp = gethostbyname(hostptr);
    if ( hp )
    {
       memcpy(&ip_addr,hp->h_addr,4);
    }
  }
  if ( ip_addr == -1 )
  {
    memcpy(x_rettxt,"*INVRMT   ",10);
    free(portptr);
    free(hostptr);
    return;
  }

  memset(&local, 0, sizeof(local));
  memset(&from, 0, sizeof(from));

  if(*x_proto == '0')
  {
    *IPIO_socket = socket(AF_INET, SOCK_STREAM, 0);
  }else
  {
    *IPIO_socket = socket(AF_INET, SOCK_DGRAM, 0);
  }
  if (*IPIO_socket == -1)
  {
    memcpy(x_rettxt,"*SOCKERR  ",10);
    free(portptr);
    free(hostptr);
	   return;
  }

  if(*x_proto == '0')
  {
    /* Den NAGLE-Algorithmus abschalten für schnelleres Senden */
    flag = 1;
    setsockopt_ret = setsockopt(*IPIO_socket, IPPROTO_TCP, TCP_NODELAY,
               (void *)&flag, sizeof(flag));
    if(setsockopt_ret != 0)
    {
      memcpy(x_rettxt,"*OPTERR   ",10);
      close(*IPIO_socket);
      free(portptr);
      free(hostptr);
    		return;
    }
  }

  /*‚Senden der Daten */
  from.sin_family = AF_INET;
  from.sin_addr.s_addr = ip_addr;
  from.sin_port = htons(port);

 	if (connect(*IPIO_socket, (struct sockaddr*)&from, sizeof(from))
    		== -1)
  {
    memcpy(x_rettxt, "*TCPERR   ", 10);
    close(*IPIO_socket);
    free(portptr);
    free(hostptr);
   	return;
  }

  return;
}


/* Verbindung schließen */
void IPIO_close(char *x_rettxt)
{

  if(*IPIO_socket != -1)
  {
    if(close(*IPIO_socket) != 0)
    {
      memcpy(x_rettxt, "*CLSFAIL  ", 10);
    }
  }else
  {
    memcpy(x_rettxt, "*NOTOPEN  ", 10);
  }

  *IPIO_socket = -1;
  return;
}


/* Daten lesen */
void IPIO_recv(char *x_buffer, int *x_buff_len,
                 int *x_waittime, int *x_waittime_usec,
                 char *x_proto, char *x_rettxt)
{
  int recvlen;
  int tmprecv;
  int tmplen;
  int selret;
  int i;
  int useDelim;
  struct  timeval   timeout;
  struct  timeval   *timeoutptr;
  fd_set  rfds;

  if(*IPIO_socket < 0)
  {
    memcpy(x_rettxt, "*NOSOCKET ", 10);
    return;
  }
  if(*x_buff_len <= 0)
  {
    memcpy(x_rettxt, "*INVLEN   ", 10);
    return;
  }

  /* Wurde ein Delimiter übergeben? */
  useDelim=0;
  for(i=0;i<MAX_DELIM;i++)
  {
    if(IPIO_delim[i].delimLen > 0)
    {
      if(IPIO_delim[i].delimLen > 50)
      {
        memcpy(x_rettxt, "*INVDELIM ", 10);
        return;
      }
      useDelim=1;
    }
  }

  /* Empfangspuffer leeren */
  /* memset(x_buffer, ' ', *x_buff_len); */

  FD_ZERO(&rfds);
  FD_SET(*IPIO_socket, &rfds);

  /* Wartezeit übernehmen sofern gewünscht */
  if(*x_waittime == -1 && *x_waittime_usec <= 0)
  {
    /* Unendlich lange warten */
    timeoutptr = NULL;
  }else
  {
    /* Eine bestimmte Wartezeit lang warten */
    timeout.tv_usec = *x_waittime_usec;
    timeout.tv_sec  = *x_waittime;
    timeoutptr = &timeout;
  }

  selret = select(*IPIO_socket + 1, &rfds, NULL, NULL, timeoutptr);
  /* Fehler beim Lesen vom Socket */
  if(selret == -1)
  {
    memcpy(x_rettxt, "*SELECTERR", 10);
    return;
  }
  /* Timeout, keine Daten empfangen */
  if(selret == 0)
  {
    memcpy(x_rettxt, "*TIMEOUT  ", 10);
    return;
  }

  /* Daten vorhanden nun abrufen */
  recvlen = 0;
  while(recvlen != *x_buff_len)
  {
    if( (useDelim > 0) && (*x_proto != '1') )
    {
      /* Beim Delimiterhandling für immer Byte für Byte gelesen */
      tmplen = 1;
    }else
    {
      /* Es soll eine bestimmte Anzahl an Bytes gelesen werden */
      tmplen = *x_buff_len - recvlen;
    }
    tmprecv = recv(*IPIO_socket, x_buffer + recvlen, tmplen, 0);
    /* Fehler beim Datenempfang */
    if(tmprecv <= 0)
    {
      if(tmprecv == -1)
      {
        memcpy(x_rettxt, "*RECVERR  ", 10);
        return;
      }else
      {
        memcpy(x_rettxt, "*CONCLOSE ", 10);
        return;
      }
    }
    /* Empfang der Daten festhalten */
    recvlen+= tmprecv;
    /* Bei UDP gibt es nur fixe Telegrammlängen, daher hier aufhören */
    if(*x_proto == '1') break;

    /* Prüfen ob der Delimiter empfangen wurde */
    if(useDelim > 0)
    {
      /* In einer Schleife die übergebene Delimiter- */
      /* Tabelle überprüfen */
      for(i=0;i<MAX_DELIM;i++)
      {
        if((recvlen >= IPIO_delim[i].delimLen ) &&
           (IPIO_delim[i].delimLen > 0))
        {
          /* Delimiter-String gefunden? */
          if(memcmp(x_buffer + recvlen - IPIO_delim[i].delimLen,
             IPIO_delim[i].delimTxt,
             IPIO_delim[i].delimLen) == 0)
          {
            /* Dann aufhören weiterzulesen */
            useDelim = 0;
            break;
          }
        }
      }
      if(useDelim == 0) break;
    }
  }
  *x_buff_len = recvlen;

  return;
}


/* Daten senden */
void IPIO_send(char *x_buffer, int *x_buff_len, char *x_rettxt)
{
  int sendlen;
  if(*IPIO_socket < 0)
  {
    memcpy(x_rettxt, "*NOSOCKET ", 10);
    return;
  }
  if(*x_buff_len <= 0)
  {
    memcpy(x_rettxt, "*INVLEN   ", 10);
    return;
  }

  sendlen = send(*IPIO_socket, x_buffer, *x_buff_len, 0);
  if(sendlen != *x_buff_len)
  {
    if(sendlen == -1)
    {
      memcpy(x_rettxt, "*SENDERR  ", 10);
    }else
    {
      /* Nicht vollständig gesendet */
      memcpy(x_rettxt, "*PARTSEND ", 10);
    }
    return;
  }

  return;
}


DLLEXPORT void IPIO (int *x_socket,           /* -1 wenn MSGSOCK verwendet werden soll */
                       char *x_action,        /* 0=open,1=close,2=send ,3=recv,4=debug on, */
                                              /* 5=debug off*/
                       char *x_buffer,        /* Puffer zum Senden/Emp fangen */
                                              /* Bei OPEN Hostname:Por t */
                       int *x_buff_len,       /* Länge der Daten im Pu ffer */
                                              /* Beim Empfang maximale  Länge des Puffers */
                       void *x_delim,  /* Trennstring Tabelle */
                       int *x_waittime,       /* Wartezeit beim Lesen,  -1 für unendlich */
                       int *x_waittime_usec,  /* Wartezeit in µSekunden Auflösung        */
                       char *x_proto,         /* Protokoll: 0 = TCP, 1  = UDP */
                       char *x_rettxt)        /* Return-Code PIC X(10) , SPACES bei Erfolg */
{

  /* Der Retcode ist erstmal erfolgreich */
  memcpy(x_rettxt, "          ", 10);

  /* Socket übernehmen. Je nach Übergabe wird der MSGSOCK benutzt */
  if(*x_socket == -1)
  {
    IPIO_socket = &msgsock;
  }else
  {
    IPIO_socket = x_socket;
  }

  /* Je nach Aktionscode direkt verzweigen */
  switch(*x_action)
  {
    /* Verbindung öffnen */
    case '0': IPIO_open(x_proto, x_buffer, x_buff_len, x_rettxt);
              break;
    /* Verbindung schließen */
    case '1': IPIO_close(x_rettxt);
              break;
    /* Daten senden */
    case '2': IPIO_send(x_buffer, x_buff_len, x_rettxt);
              break;
    /* Daten lesen */
    case '3': memcpy(IPIO_delim, x_delim, sizeof(IPIO_delim));
              IPIO_recv(x_buffer, x_buff_len,
                           x_waittime, x_waittime_usec,
                           x_proto, x_rettxt);
              break;
    /* Debugmeldungen aktivieren */
    case '4': IPIO_debug = 1;
              break;
    /* Debugmeldungen deaktivieren */
    case '5': IPIO_debug = 0;
              break;
    default:  memcpy(x_rettxt, "*INVACTION", 10);
              break;
  }

  return;
}
