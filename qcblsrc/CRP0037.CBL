       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0037.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SPOOL-DATEI IN PTMPSEU KOPIEREN                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.01.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SPOOL-DATEI KOPIEREN                         *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0037".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CPYSPLF-REC                   TYPE CPYSPLF-TYPE.

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SPOOLFILE                   PIC X(10).
       01  X-JOBNAME                     PIC X(10).
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SPOOLFILE
                                X-JOBNAME
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE X-RETCODE.
     ‚* CPYSPLF AUSFÜHREN
           INITIALIZE CPYSPLF-REC.
           MOVE     X-SPOOLFILE   TO SPOOLNAME OF CPYSPLF-REC.
           MOVE     X-JOBNAME     TO JOBNAME   OF CPYSPLF-REC.
           MOVE     "QTEMP"       TO TOLIB     OF CPYSPLF-REC.
           MOVE     "PTMPSEU"     TO TOFILE    OF CPYSPLF-REC.
           MOVE     "*REPLACE"    TO MBROPT    OF CPYSPLF-REC.
           CALL     "CLP0046" USING CPYSPLF-REC
                                    X-RETCODE
           END-CALL.

       ENDE.

           GOBACK.
      /
