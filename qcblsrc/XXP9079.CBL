       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9079.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LOW-VALUE/HIGH VALUE                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  28.04.2016                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST PROGRAM                                 *
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
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP9079".

      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-VARIABLE                     PIC 9(5).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-VARIABLE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           MOVE     999 TO X-VARIABLE.

       ENDE.
           GOBACK.
      /
