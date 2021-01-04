       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9010.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ARITHMETHIK-TEST                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.05.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST MIT RECHEN-OPERATIONEN                  *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9010".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  NUM-1                         PIC 9(4).
       01  NUM-2                         PIC 9(4).
       01  RES-1                         PIC S9(8)V9(3).

       01  BIG-NUM                       PIC 9(30).
       01  POTENZ                        PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* HIER WIRD 2 HOCH 3 GERECHNET
           MOVE     2 TO NUM-1.
           MOVE     3 TO NUM-2.

           COMPUTE  RES-1 = NUM-1 ** NUM-2.

     ‚* 2 ** 59 GEHT, 2 ** 60 NICHT, WEIL COMPUTE MAX. 18 STELLEN
     ‚* KANN
           MOVE     60 TO POTENZ.
           COMPUTE  BIG-NUM = 2 ** POTENZ.

       ENDE.

           GOBACK.
      /
