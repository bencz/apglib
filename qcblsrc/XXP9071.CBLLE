       IDENTIFICATION DIVISION.
      *
         PROGRAM-ID.              XXP9071.
           COPY COPYRIGHT  OF APG-QCPYSRC.
      *
      *---------------------------------------------------------------*
      * TESTPROGRAMM                                                  *
      *---------------------------------------------------------------*
      *                                                               *
      * AUTOR         :  A. PIEGER                                    *
      *                                                               *
      * ERSTELLT AM   :  15.07.2014                                   *
      *                                                               *
      * FUNKTION      :  TEST-PGM                                     *
      *                                                               *
      * �NDERUNGEN:                                                   *
      * DATUM      VON   GRUND DER �NDERUNG                           *
      *                                                               *
      *****************************************************************
      *
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

      * STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP9071".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST".

       01  POS                            PIC 9(4).
       01  LEN                            PIC 9(4).
       01  STRING-WRK                     PIC X(150).
      /
      *-------------------------------------------------------------
       LINKAGE SECTION.
       01  DYNAMIC-RECORD                 PIC X(1).
      *-------------------------------------------------------------
       PROCEDURE DIVISION.
      *-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

      * F�LLEN PGM-WRK UND AUSF�HREN STANDARDROUTINEN
           PERFORM  COPY-PGM-INIT.

           MOVE     "HALLO" TO STRING-WRK(20:).

           SET      ADDRESS OF DYNAMIC-RECORD TO
                    ADDRESS OF STRING-WRK.

           MOVE     20          TO POS.
           MOVE     10          TO LEN.
           DISPLAY  DYNAMIC-RECORD(POS:LEN).

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
      *---------------------------------------------------------------
      * COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
      *---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
