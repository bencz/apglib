       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0042.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* STARTZEIT NOTIEREN                                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  STARTZEIT FESTHALTEN                         *
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
                  ASSIGN       TO  WORKSTATION-XXD0042DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PSTRTIM: STLFDN
     ‚*-------------------------------------------------------------
           SELECT PSTRTIM-DP
                  ASSIGN       TO  DATABASE-PSTRTIM
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
            COPY DDS-ALL-FORMATS OF XXD0042DE.
      /
     ‚*- PSTRTIM: STLFDN
       FD  PSTRTIM-DP
           LABEL RECORDS ARE STANDARD.
       01  PSTRTIM-P.
           COPY DDS-ALL-FORMATS OF PSTRTIM.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0042".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0042DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0042DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0042" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0042DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PSTRTIM-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* USEREINSTELLUNGEN LESEN OB EINTRAG GESPEICHERT WERDEN SOLL
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H050"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.

           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* ERMITTELN DATUM/ZEIT
           PERFORM  COPY-GET-TIME.

     ‚* ÜBERSCHRIFT
       ANF010.
           MOVE     "XXP0042*1"     TO MAINUEB OF FORMAT1.
           MOVE     DATE-8 TO DT_S  OF FORMAT1.
           MOVE     TIME-6 TO TI_S  OF FORMAT1.

           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    IF   CFFK25 OF CFG-CPY = 1
                         PERFORM SAVE-RECORD
                    END-IF
                    GO TO ENDE
           END-IF.

     ‚* F5 GEDRÜCKT ?
           IF       IN05 OF FORMAT1-I-INDIC  = AN
                    IF   CFFK25 OF CFG-CPY = 1
                         PERFORM SAVE-RECORD
                    END-IF
     ‚*             DATUM/ZEIT AKTUALISIEREN
                    PERFORM  COPY-GET-TIME
                    GO TO ANF010
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    IF   CFFK25 OF CFG-CPY = 1
                         PERFORM SAVE-RECORD
                    END-IF
                    GO TO ENDE
           END-IF.

     ‚* F21 KOMMANDOZEILE AUFRUFEN
           IF       IN21 IN FORMAT1-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO ANF010
           END-IF.

     ‚* RECORD SPEICHERN
           IF       CFFK25 OF CFG-CPY = 1
                    PERFORM SAVE-RECORD
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PSTRTIM-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DATENSATZ SPEICHERN
     ‚*--------------------------------------------------------------
       SAVE-RECORD SECTION.
       SAV-REC-00.

           INITIALIZE PSTRTIMF OF PSTRTIM-P.
     ‚* NÄCHSTE LAUFENDE NUMMER ERMITTELN
           MOVE     "PSTRTIM" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK             TO STLFDN OF PSTRTIM-P.
     ‚* USER UND TEXT
           MOVE     T-USER               TO STUSER OF PSTRTIM-P.
           MOVE     TEXT1 OF FORMAT1     TO STTEXT OF PSTRTIM-P( 1:25).
           MOVE     TEXT2 OF FORMAT1     TO STTEXT OF PSTRTIM-P(26:25).
     ‚* STARTZEIT IST NOCH IN DATE-8/TIME-6
           MOVE     DATE-8               TO STDTS  OF PSTRTIM-P.
           MOVE     TIME-6               TO STTIS  OF PSTRTIM-P.

     ‚* ERMITTELN ENDDATUM/ZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8               TO STDTE  OF PSTRTIM-P.
           MOVE     TIME-6               TO STTIE  OF PSTRTIM-P.

     ‚* ANLAGEDATEN FÜLLEN
           MOVE     T-USER            TO STAUSR OF PSTRTIM-P.
           MOVE     T-TERM            TO STABS  OF PSTRTIM-P.
           MOVE     PGM-WRK           TO STAPGM OF PSTRTIM-P.
           MOVE     DATE-8            TO STADTA OF PSTRTIM-P.
           MOVE     TIME-6            TO STATIA OF PSTRTIM-P.

           WRITE    PSTRTIM-P.

       SAV-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOMMANDOZEILE AUFRUFEN
     ‚*--------------------------------------------------------------
       KOMMANDOZEILE SECTION.
       CMD-LIN-00.

           CALL     "XXP2001"
           END-CALL.

       CMD-LIN-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
