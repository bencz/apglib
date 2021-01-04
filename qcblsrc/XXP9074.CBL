       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9074.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* 2 DIM. ARRAY                                                  *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9074".

       01  GRP-ARRAY.
         05  GMAX                         PIC 9(1).
         05  GRP-REC   OCCURS 3 TIMES INDEXED BY GIDX.
           07  GRP-REC2.
             10  LAST-TIME-USED           PIC 9(4).
             10  CHK-ARRAY.
               15  CMAX                   PIC 9(3) COMP-3.
               15  CIDX                   PIC 9(3) COMP-3.
               15  CHK-REC   OCCURS 150 TIMES.
                  17  CHK-REC2.
                    20  CFFILE            PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           MOVE     CFFILE(1, 2)    TO CFFILE(2, 1).
           MOVE     CFFILE(GIDX, 2) TO CFFILE(GIDX + 1, 1).

           MOVE     CFFILE(GIDX, CIDX OF GRP-ARRAY(GIDX) ) TO
                    CFFILE(GIDX + 1, 1).

       ENDE.

           GOBACK.
      /
