       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP0004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* EINGABE BIBLITOHEK/PROGRAMM (WINDOW)                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  27.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  EINGABE LIB/DATEI                            *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-HMD0004DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

     ‚*----DISPLAY
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF HMD0004DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF HMD0004DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF HMD0004DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HMP0004".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

       01  PPGMLIB-WRK.
            COPY DDS-ALL-FORMATS OF PPGMLIB.

       01  LEN                           PIC 9(4).

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01 X-PLLIB                        LIKE PLLIB  OF PPGMLIB-WRK.
       01 X-PLFILE                       LIKE PLFILE OF PPGMLIB-WRK.
       01 X-F12                          PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-PLLIB
                                         X-PLFILE
                                         X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP0004" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.

           INITIALIZE X-F12.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.

     ‚* VORBELEGEN DURCH DIE ÜBERGABEWERTE
           MOVE     X-PLLIB  TO PLLIB  OF FORMAT1.
           MOVE     X-PLFILE TO PLFILE OF FORMAT1.

     ‚* CURSER SETZEN - ABER NICHT WENN EINGABE MIT "*"
           INITIALIZE LEN.
           INSPECT  PLLIB  OF FORMAT1 TALLYING LEN FOR CHARACTERS
                    BEFORE INITIAL "*".
           IF       PLLIB  OF FORMAT1 NOT = SPACES
            AND     LEN = LENGTH OF PLLIB  OF FORMAT1
                    MOVE AN  TO IN41 OF INXX
           END-IF.

       ANF010.
     ‚* AUSGABE WINDOWRAHMEN
           WRITE    WS-REC     FORMAT IS "FMT02".

     ‚* AUSGABE DISPLAY
           MOVE     CORR INXX         TO FMT01-O-INDIC.
           MOVE     CORR FORMAT1      TO FMT01-O.
           WRITE    WS-REC     FORMAT IS "FMT01".
           READ     WS-DISPLAY FORMAT IS "FMT01".
           MOVE     CORR FMT01-I      TO FORMAT1.
           MOVE     AUS TO INXX.

     ‚* F12 GEDRÜCKT ?
           IF       IN12 OF FMT01-I-INDIC  = AN
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

     ‚* BEIDE DATEN MÜSSEN ANGEGEBEN SEIN
           IF       PLLIB  OF FORMAT1 = SPACES
                    GO TO ANF010
           END-IF.

     ‚* BEIDE DATEN MÜSSEN ANGEGEBEN SEIN
           IF       PLFILE OF FORMAT1 = SPACES
                    GO TO ANF010
           END-IF.

     ‚* DATEN ÜBERNEHMEN
           MOVE     PLLIB  OF FORMAT1 TO X-PLLIB.
           MOVE     PLFILE OF FORMAT1 TO X-PLFILE.

       ENDE.
           CLOSE                 WS-DISPLAY.

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
