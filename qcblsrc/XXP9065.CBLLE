       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9065.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  23.12.2013                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST DER LOCAL-STORAGE                       *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9065".
       01  WORKING-VAR                   PIC 9(4).

       LOCAL-STORAGE SECTION.
       01  LOCAL-VAR                     PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    INITIALIZE WORKING-VAR, LOCAL-VAR
           END-IF.

           ADD      1 TO WORKING-VAR.
           ADD      1 TO LOCAL-VAR.

           DISPLAY  WORKING-VAR "/" LOCAL-VAR.

       ENDE.
           GOBACK.
      /
