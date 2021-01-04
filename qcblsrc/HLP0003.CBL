       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0003.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* STRING OHNE FUNCTION REVERSE() DREHEN                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  STRING UMDREHEN                              *
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
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0003".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  POS                           PIC 9(4).
       01  LEN                           PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PARM                        PIC X(150).
       01  X-RETPARM                     PIC X(150).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PARM
                                X-RETPARM.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE X-RETPARM.
     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0003" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     ZEROES TO LEN.
           MOVE     LENGTH OF X-PARM TO POS.
           PERFORM  LENGTH OF X-PARM TIMES
                    MOVE X-PARM(POS - LEN:1) TO X-RETPARM(LEN + 1:1)
                    ADD 1 TO LEN
           END-PERFORM.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
