       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9073.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LOW-VALUE/HIGH VALUE                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.01.2015                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9063".

       01  UNSIGNED-NUM                  PIC 9(3).
       01  SIGNED-NUM                    PIC S9(3).
       01  ALPHA-VAR                     PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           MOVE     LOW-VALUE      TO ALPHA-VAR.
           MOVE     LOW-VALUE      TO UNSIGNED-NUM.
           MOVE     LOW-VALUE      TO SIGNED-NUM.

           MOVE     HIGH-VALUE     TO ALPHA-VAR.
           MOVE     HIGH-VALUE     TO UNSIGNED-NUM.
           MOVE     HIGH-VALUE     TO SIGNED-NUM.

       ENDE.

           GOBACK.
      /
