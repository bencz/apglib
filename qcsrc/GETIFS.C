     /*****************************************************************

         ----------- WANDELN MIT *IFSIO ------------------

      *****************************************************************
      *---------------------------------------------------------------*
      * COPYRIGHT BY  :  EHRHARDT + PARTNER  GMBH & Co. KG            *
      *                  SOFTWARE SYSTEME FÜR WAREHOUSE-LOGISTIK      *
      *                  56154 BOPPARD-BUCHHOLZ                       *
      *                  TEL 06742 / 87270                            *
      *---------------------------------------------------------------*
      *                                                               *
      * AUTOR         :           B. Gerlich                          *
      *                                                               *
      * ERSTELLT AM   :           Nov 2004                            *
      *                                                               *
      *                                                               *
      * PROBLEM       :           Daten aus dem IFS lesen und         *
      *                           String füllen                       *
      *                                                               *
      * ÄNDERUNGEN:                                                   *
      * DATUM      VON            GRUND DER ÄNDERUNG                  *
      *                                                               *
      ****************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <string.h>

 #include <unistd.h>

         void GETIFS (char *x_filename, char *x_data, int *x_len_in,
                      int *x_len_out, char *ret) {
    char filename[256];
    int  i;
    int  filefd;
    int  seek_off;
    int  retcode;

    memcpy(filename, x_filename, 255);
    filename[255] = 0;

    for(i = (int)strlen(filename) - 1;i>=0;i--)
    {
      if(filename[i] == ' ')
      {
        filename[i] = 0;
      }else
      {
        break;
      }
    }

    if(strlen(filename) == 0)
    {
      ret[0] = '1';
      return;
    }

    if(*x_len_in == 0)
    {
      ret[0] = '2';
      return;
    }
    *x_len_out = 0;


    filefd = open(filename, O_RDONLY);
    if(filefd < 0)
    {
      ret[0] = '3';
      return;
    }

    seek_off = lseek(filefd, 0, SEEK_END);
    if(seek_off == -1)
    {
      ret[0] = '4';
      close(filefd);
      return;
    }

    if(seek_off > *x_len_in)
    {
      ret[0] = '5';
      close(filefd);
      return;
    }

    retcode = lseek(filefd, 0, SEEK_SET);
    if(retcode == -1)
    {
      ret[0] = '6';
      close(filefd);
      return;
    }

    retcode = read(filefd, x_data, *x_len_in);
    if(retcode < 0)
    {
      ret[0] = '7';
      close(filefd);
      return;
    }
    *x_len_out = retcode;

    close(filefd);
    ret[0] = ' ';

    return;
}
