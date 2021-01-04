       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9040.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.09.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM                                     *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9040".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  VAR-4N                        PIC 9(4).
       01  VAR-5N                        PIC 9(5).
       01  VAR-8N                        PIC 9(8).

       01  VAR-4A                        PIC X(4).
       01  VAR-8A                        PIC X(8).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9040"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* VARIABLEN GLEICHER LÄNGE
           MOVE     3       TO VAR-4N.
           MOVE     "0003"  TO VAR-4A.
           IF       VAR-4N >= VAR-4A
                    CONTINUE
           END-IF.

     ‚* NUMERISCH LÄNGER
           MOVE     3       TO VAR-5N.
           MOVE     "0003"  TO VAR-4A.
           IF       VAR-5N >= VAR-4A
                    CONTINUE
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
