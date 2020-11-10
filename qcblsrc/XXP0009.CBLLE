       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0009.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ABSTURZ-PGM                                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ABSTURZ-PGM                                  *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP0009".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  ABC-NUM                       PIC 9(5).
       01  ABC-RED                       REDEFINES ABC-NUM PIC X(5).

       01  ABC-POS                       PIC 9(2).
       01  ABC-TAB.
        05 ABC-REC                       OCCURS 10 TIMES.
         07 ABC-VAL                      PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0009"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           MOVE     11  TO ABC-POS.
           MOVE     "1" TO ABC-VAL(ABC-POS).

           MOVE     "ABC" TO ABC-RED.
           IF       ABC-NUM = 3
                    GOBACK
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
