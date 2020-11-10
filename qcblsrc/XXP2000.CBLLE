     ˆ* BAUSTELLE
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP2000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFRUF EINER API "DISPLAY LONG TEXT (QUILNGTX) API"           *
     ‚* HTTP://PUBLIB.BOULDER.IBM.COM/ISERIES/V5R2/IC2928/INFO/APIS/  *
     ‚* QUILNGTX.HTM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  28.07.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSFÜHREN DER API UND ANZEIGE WINDOW         *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP2000".
       01  PGM-TYP                       PIC X(8)  VALUE "API".

     ‚* VARIABLEN FÜR AUFRUF QUILNGTX
       01  BIN-4STELLIG                  PIC 9(8)  BINARY VALUE 1000.
       01  BIN-4ALPHA REDEFINES BIN-4STELLIG PIC X(4).

       01  ERROR-REC.
        05 BYTESPROV                     PIC 9(8) BINARY.
        05 BYTESAVAIL                    PIC 9(8) BINARY.
        05 EXCEPTID                      PIC X(7).
        05 FILLER1                       PIC X(1).
        05 FILLER2                       PIC X(16).

       01  WINDOW-TXT                    PIC X(1000).
       01  TEXT-LEN                      PIC 9(4).
       01  MSG-ID                        PIC X(7).
       01  MSG-FILE-WRK.
        05 MSG-FILE                      PIC X(10) VALUE "APGSPRACH".
        05 MSG-LIB                       PIC X(10) VALUE "QTEMP".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP2000" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* VARIABLEN FÜLLEN
           MOVE     16           TO BYTESPROV OF ERROR-REC.
           MOVE     1000         TO BIN-4STELLIG.

           MOVE     "SP00001"    TO MSG-ID.
           MOVE     "ABC"        TO WINDOW-TXT.
           CALL     "QUILNGTX" USING WINDOW-TXT
                                     BIN-4STELLIG
                                     MSG-ID
                                     MSG-FILE-WRK
                                     ERROR-REC
           END-CALL.

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
