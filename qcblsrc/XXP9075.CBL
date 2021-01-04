       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9075.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LOW-VALUE/HIGH VALUE                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.05.2015                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9075".

       01  FORMAT1.
        15 FORMAT1-O-INDIC.
         20 IN40                         PIC 1.
        15 LIST1.
         16 LIST1-MAX                    PIC 9(2).
         16 LIST1-REC OCCURS 0 TO 10 TIMES DEPENDING ON LIST1-MAX
                                           INDEXED BY LIST1-IDX.
          17 LIST1-REC2.
           20 ITEM                       PIC X(10).
           20 SELECTED                   PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

       ENDE.
           GOBACK.
      /
