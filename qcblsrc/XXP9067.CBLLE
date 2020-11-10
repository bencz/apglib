       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9067.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.02.2014                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST FÜR DEBUGGING                           *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9067".
       01  WORKING-VAR                   PIC 9(4).

       01  TEST-STRUCT.
        05 TESTVAR1                      PIC X(1).
        05 TESTVAR2                      PIC X(1).
        05 TESTVAR3                      PIC X(1).
        05 TESTVAR4                      PIC X(1).
        05 TESTVAR5                      PIC X(1).
        05 TESTVAR6                      PIC X(1).

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
           MOVE     "1" TO TESTVAR3 OF TEST-STRUCT.

           DISPLAY  WORKING-VAR "/" LOCAL-VAR.

       ENDE.
           GOBACK.
      /
