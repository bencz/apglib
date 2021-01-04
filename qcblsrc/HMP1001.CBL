       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP1001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ÄNDERN EINES TAGS                                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.05.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE/ÄNDERN EINES TAGS                    *
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

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-HMD1001DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TAGS
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DP
                  ASSIGN       TO  DATABASE-PTAGLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE  DATEI TAGS
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF HMD1001DE.
      /
     ‚*--- TAGS
       FD  PTAGLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-P.
           COPY DDS-ALL-FORMATS OF PTAGLIB.
      /
     ‚*--- TAGS
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL1.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN43                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  HMD1001DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  HMD1001DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HMP1001".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

     ‚* SONSTIGE VARIABLEN
       01  NEUANLAGE                     PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PTLFDN                      LIKE PTLFDN OF PTAGLIB-P.
       01  X-PGM                         LIKE PGM-WRK.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-PTLFDN
                                         X-PGM
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP1001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.
           OPEN     I-O   PTAGLIB-DP.
           OPEN     INPUT PTAGLIB-DL.

           INITIALIZE F3, F12.
           INITIALIZE X-F3, X-F12.
           INITIALIZE NEUANLAGE.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     AUS TO INXX.

       ANF020.
     ‚* PTAGLIB LESEN
           IF       X-PTLFDN NOT = ZEROES
                    INITIALIZE PTAGLIBF OF PTAGLIB-P
                    MOVE X-PTLFDN            TO PTLFDN OF PTAGLIB-P
                    READ PTAGLIB-DP WITH NO LOCK
                    IF   FILE-STATUS NOT = ZEROES
                         INITIALIZE PTAGLIBF OF PTAGLIB-P
                         MOVE 1      TO NEUANLAGE
                    ELSE
                         MOVE ZEROES TO NEUANLAGE
                    END-IF
           ELSE
                    MOVE 1      TO NEUANLAGE
           END-IF.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-DATEN.
           IF       F3 = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

       ENDE.
           CLOSE                 WS-DISPLAY
                                 PTAGLIB-DL
                                 PTAGLIB-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-DATEN SECTION.
       ANZ-DAT-00.

           INITIALIZE FORMAT1.

           MOVE     AUS    TO INXX.

       ANZ-DAT-20.
           MOVE     PGM-TYP                    TO PGMTYP OF FORMAT1.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE AUS TO IN43 OF INXX
                    MOVE CORR PTAGLIBF OF PTAGLIB-P TO FORMAT1
                    PERFORM COUNT-USED
                    IF   ANZUSED OF FORMAT1 > ZEROES
                         MOVE AN  TO IN43 OF INXX
                    END-IF
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    MOVE AUS TO IN43 OF INXX
                    MOVE X-PTLFDN               TO PTLFDN OF FORMAT1
           END-IF.

     ‚* ANZEIGE FORMAT
           MOVE     CORR   INXX     TO  FMT01-O-INDIC.
           MOVE     CORR   FORMAT1  TO  FMT01-O.
           WRITE    WS-REC FORMAT IS "FMT01".
           READ     WS-DISPLAY  FORMAT IS "FMT01".
           MOVE     CORR FMT01-I      TO FORMAT1.
           MOVE     AUS  TO INXX

     ‚* F3 ENDE
           IF       IN03 IN FMT01-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FMT01-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* NEUANLAGE/ÄNDERN
           IF       NEUANLAGE NOT = ZEROES
                    PERFORM ADD-ENTRY
           ELSE
                    PERFORM CHANGE-ENTRY
           END-IF.

       ANZ-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE-ÜBERSCHRIFTEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "HMP0000"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT1.

     ‚* ZWEITE ÜBERSCHRIFT
           MOVE     "HMP1001*1"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

           INITIALIZE PTAGLIBF OF PTAGLIB-P.
     ‚* FUNKTIONSCODES .. ETC
           MOVE     CORR FORMAT1 TO PTAGLIBF OF PTAGLIB-P.

     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PTAGLIB" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK          TO PTLFDN OF PTAGLIB-P.
           MOVE     LFDN-WRK          TO X-PTLFDN.

     ‚* DATEN FÜLLEN
           MOVE     SPACES            TO PTLIB  OF PTAGLIB-P.
           MOVE     SPACES            TO PTFILE OF PTAGLIB-P.
           MOVE     PTTEXT OF FORMAT1 TO PTTEXT OF PTAGLIB-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     DATE-8            TO PTADTA OF PTAGLIB-P.
           MOVE     TIME-6            TO PTATIA OF PTAGLIB-P.
           MOVE     T-USER            TO PTAUSR OF PTAGLIB-P.
           MOVE     T-TERM            TO PTABS  OF PTAGLIB-P.
           MOVE     PGM-WRK           TO PTAPGM OF PTAGLIB-P.

           WRITE    PTAGLIB-P.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       CHANGE-ENTRY SECTION.
       CHG-ENT-00.

           INITIALIZE PTAGLIBF OF PTAGLIB-P.
           MOVE     PTLFDN OF FORMAT1 TO PTLFDN OF PTAGLIB-P.
           READ     PTAGLIB-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-ENT-90
           END-IF.

     ‚* DATEN FÜLLEN
           MOVE     PTTEXT OF FORMAT1 TO PTTEXT OF PTAGLIB-P.

     ‚* FUNKTIONSCODES .. ETC
           MOVE     CORR FORMAT1 TO PTAGLIBF OF PTAGLIB-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO PTMSER OF PTAGLIB-P.
           MOVE     T-TERM            TO PTMBS  OF PTAGLIB-P.
           MOVE     PGM-WRK           TO PTMPGM OF PTAGLIB-P.
           MOVE     DATE-8            TO PTMDTA OF PTAGLIB-P.
           MOVE     TIME-6            TO PTMTIA OF PTAGLIB-P.

           REWRITE  PTAGLIB-P.

       CHG-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZÄHLEN WIE OFT DIESER TAG BENUTZT WIRD
     ‚*--------------------------------------------------------------
       COUNT-USED SECTION.
       CNT-USD-00.

           INITIALIZE ANZUSED OF FORMAT1.
     ‚* PTAGLIB LESEN
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     PTTAG  OF PTAGLIB-P TO PTTAG  OF PTAGLIB-L.
           MOVE     SPACES              TO PTLIB  OF PTAGLIB-L.
           MOVE     SPACES              TO PTFILE OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CNT-USD-90
           END-IF.

       CNT-USD-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CNT-USD-90
           END-IF.

           IF       PTTAG  OF PTAGLIB-L NOT = PTTAG  OF PTAGLIB-P
                    GO TO CNT-USD-90
           END-IF.

     ‚* KOPFSÄTZE NICHT ZÄHLEN
           IF       PTLIB  OF PTAGLIB-L     = SPACES
            AND     PTFILE OF PTAGLIB-L     = SPACES
                    GO TO CNT-USD-20
           END-IF.

           ADD      1 TO ANZUSED OF FORMAT1.


           GO TO    CNT-USD-20.

       CNT-USD-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
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
