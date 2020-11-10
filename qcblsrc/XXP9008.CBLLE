       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9008.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ULCSEU IN COBOL                                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  25.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  NACHTRÄGLICHES FÜLLEN DER KW                 *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9008".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SEU-INPUT                   USAGE POINTER.
       01  X-SEU-OUTPUT                  USAGE POINTER.
       01  X-SEU-SOURCE                  USAGE POINTER.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SEU-INPUT
                                X-SEU-OUTPUT
                                X-SEU-SOURCE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.


       ENDE.

           GOBACK.
      /
