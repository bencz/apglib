       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0020.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRIORITÄT ÄNDERN                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.11.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRIORITÄT EINES OPL-PUNKTES ÄNDERN           *
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
                  ASSIGN       TO  WORKSTATION-XXD0020DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* POPLLST: OPPRIO
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DP
                  ASSIGN       TO  DATABASE-POPLLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

     ‚*-----BILDSCHIRMDATEI -------------------------
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF XXD0020DE.
      /
     ‚*- POPLLST: OPLFDN
       FD  POPLLST-DP
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-P.
           COPY DDS-ALL-FORMATS OF POPLLST.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0020".
       01  PGM-TYP                       PIC X(8)  VALUE "OPL    ".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0020DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0020DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  OPPRIO-SAV                LIKE OPPRIO OF POPLLST-P.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-OPLFDN                  LIKE OPLFDN OF POPLLST-P.
       01  X-F12                          PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-OPLFDN
                                X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0020" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0020DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN I-O   POPLLST-DP
           END-IF.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE X-F12.
           INITIALIZE FORMAT1.

     ‚* DATENSATZ LESEN UND PRIO VORBLENDEN (WENN VORHANDEN)
           IF       X-OPLFDN NOT = ZEROES
                    INITIALIZE POPLLSTF OF POPLLST-P
                    MOVE X-OPLFDN TO OPLFDN OF POPLLST-P
                    READ POPLLST-DP WITH NO LOCK
                    IF   FILE-STATUS = ZEROES
                         MOVE CORR POPLLSTF OF POPLLST-P TO FORMAT1
                    END-IF
           END-IF.

           MOVE     OPPRIO OF FORMAT1 TO OPPRIO-SAV.
           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* ERMITTELN DATUM/ZEIT
           PERFORM  COPY-GET-TIME.

     ‚* ÜBERSCHRIFT
       ANF010.
           MOVE     "XXP0020*1"     TO MAINUEB OF FORMAT1.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

     ‚* JETZT DEN UPDATE AUSFÜHREN
           IF       X-OPLFDN NOT = ZEROES
            AND     OPPRIO OF FORMAT1 NOT = OPPRIO-SAV
                    INITIALIZE POPLLSTF OF POPLLST-P
                    MOVE X-OPLFDN TO OPLFDN OF POPLLST-P
                    READ POPLLST-DP
                    IF   FILE-STATUS = ZEROES
                         MOVE OPPRIO OF FORMAT1 TO OPPRIO OF POPLLST-P
                         MOVE OPPRZT OF FORMAT1 TO OPPRZT OF POPLLST-P
     ‚*                  USER FÜLLEN
                         PERFORM  COPY-GET-TIME
                         MOVE T-USER            TO OPMSER OF POPLLST-P
                         MOVE T-TERM            TO OPMBS  OF POPLLST-P
                         MOVE PGM-WRK           TO OPMPGM OF POPLLST-P
                         MOVE DATE-8            TO OPMDTA OF POPLLST-P
                         MOVE TIME-6            TO OPMTIA OF POPLLST-P
                         REWRITE POPLLST-P
                    END-IF
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
