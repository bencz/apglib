       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP1000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PFLEGEPROGRAMM FÜR HERMES-TAGS                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.05.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TAGPFLEGE                                    *
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
                  ASSIGN       TO  WORKSTATION-HMD1000DE
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
     ‚* LOGISCHE DATEI TAGS (KOPFSÄTZE)
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL3
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
           COPY DDS-ALL-FORMATS OF HMD1000DE.
      /
     ‚*--- TAGS
       FD  PTAGLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-P.
           COPY DDS-ALL-FORMATS OF PTAGLIB.
      /
     ‚*--- TAGS (KOPFSÄTZE)
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL3.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF HMD1000DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  HMD1000DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  HMD1000DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  HMD1000DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  HMD1000DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  HMD1000DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HMP1000".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

       01  LEN-F                         PIC 9(4).
       01  AUSWAHL                       PIC X(2).

       01  TAG-WRK                       LIKE PTTAG  OF PTAGLIB-L.
       01  PTTAG-WRK                     LIKE PTTAG  OF PTAGLIB-L.
       01  PTLFDN-WRK                    LIKE PTLFDN OF PTAGLIB-L.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-F3                          PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-F3.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP1000" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "HMD1000DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PTAGLIB-DP.
           OPEN     INPUT PTAGLIB-DL.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.
           INITIALIZE X-F3.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DER TAGS BZW. PROGRAMMNAME
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN ANHAND DER TAG-DATEI
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                         GO TO ANF010
                    END-IF
                    GO TO ANF020
           END-IF.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3 = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F5 = "1"
                    INITIALIZE F5
                    GO TO ANF020
           END-IF.
           IF       F12 = "1"
                    INITIALIZE F12
                    GO TO ANF010
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PTAGLIB-DP
                                 PTAGLIB-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SELEKTIEREN ALLER NÖTIGEN DATEN
     ‚*--------------------------------------------------------------
       VORAUSWAHL SECTION.
       VOR-AUS-00.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "HMP0000"         TO MAINUEB OF FORMAT1.
           MOVE     "HMP1000*1"       TO SUBUEB  OF FORMAT1.

       VOR-AUS-10.
           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

     ‚* FRAGEZEICHEN-FUNKTION FÜR EIN TAG
           IF       PTTAG  OF FORMAT1(1:1) = "?"
                    PERFORM TAG-SELEKTION
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    MOVE TAG-WRK              TO PTTAG   OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE PTTAG-WRK.
           INITIALIZE DATEN, ANZREC-WRK, LEN-F.
     ‚* WURDE EIN PGM MIT STERN SELEKTIERT?
           IF       PTTAG  OF FORMAT1      NOT = SPACES
            AND     PTTAG  OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN-F
                    INSPECT  PTTAG  OF FORMAT1
                             TALLYING LEN-F FOR CHARACTERS
                             BEFORE INITIAL "*"
                    MOVE     PTTAG  OF FORMAT1(1:LEN-F) TO PTTAG-WRK
           ELSE
                    MOVE     SPACES                     TO PTTAG-WRK
           END-IF.

     ‚* PPGMLIB LESEN
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     PTTAG-WRK           TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       LEN-F > ZEROES
            AND     PTTAG  OF PTAGLIB-L(1:LEN-F) NOT =
                    PTTAG  OF FORMAT1(1:LEN-F)
                    GO TO LES-DAT-20
           END-IF.

     ‚* NACH TEXT SELEKTIEREN?
           IF       TXTSEL OF FORMAT1   NOT = SPACES
                    MOVE PTAGLIB-L           TO RECORD-WRK
                    MOVE LENGTH OF PTAGLIB-L TO RECORD-LEN
                    MOVE TXTSEL OF FORMAT1   TO SELEKTION-WRK
                    CALL PROCEDURE "HLP0001" USING RECORD-WRK
                                                   RECORD-LEN
                                                   SELEKTION-WRK
                                                   RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         GO TO LES-DAT-20
                    END-IF
           END-IF.

     ‚* AUSGABE EINER SUBFILE-ZEILE
           PERFORM  AUSGABE-SFL.
           IF       SUBFILE-RECORD-NUMBER < 9999
                    GO TO LES-DAT-20
           END-IF.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER SFL-ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-SFL SECTION.
       AUS-SFL-00.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PTAGLIBF OF PTAGLIB-L TO FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

       AUS-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
           MOVE     AUS    TO INXX.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
           EVALUATE SFL-ZEILE
                    WHEN 1
                         MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                         MOVE 1 TO SUBFILE-RECORD-NUMBER
                    WHEN OTHER
                         IF   SFL-ZEILE <= ANZREC-WRK
                              MOVE SFL-ZEILE  TO SUBFILE-RECORD-NUMBER
                         ELSE
                              MOVE ANZREC-WRK TO SUBFILE-RECORD-NUMBER
                         END-IF
           END-EVALUATE.

           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AN     TO IN91  IN  INXX.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN08 IN FORMAT3-I-INDIC = AN
                    PERFORM NEUANLAGE
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                         GO TO ANZ-SFL-20
                    END-IF
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

           MOVE     WSREC-WRK      TO FORMAT2-I.
           MOVE     CORR FORMAT2-I TO FORMAT2.

           IF       AUSW OF FORMAT2 = SPACES
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* RECORD-NUMMER SICHERN, DAMIT BEI FEHLERMELDUNG DER CURSOR
     ‚* AN DER RICHTIGEN POSITION STEHT
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.

     ‚* BERECHTIGUNG FÜR DIESE FUNKTION ÜBERPRÜFEN
           IF       AUSW OF FORMAT2 NOT = SPACES
                    MOVE  SPACES          TO RET-CODE
                    MOVE  AUSW OF FORMAT2 TO AUSWAHL
                    CALL "CFP9002" USING T-USER
                                         AUSWAHL
                                         PGM-WRK
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         MOVE     SPACES TO AUSW OF FORMAT2
                         MOVE     "FMT02" TO FMT-WRK
                         MOVE     FORMAT2 TO WSREC-WRK
                         PERFORM  COPY-DISPLAY-SFLREWRITE
                         GO TO ANZ-SFL-50
                    END-IF
           END-IF.

     ‚* ÄNDERN DES DATENSATZES
           IF       AUSW OF FORMAT2 = "1"
                    CALL "HMP1001" USING PTLFDN OF FORMAT2
                                         PGM-WRK
                                         F3
                                         F12
                    END-CALL
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                    END-IF
           END-IF.

     ‚* LÖSCHEN EINES TAGS
           IF       AUSW OF FORMAT2 = "4"
                    CALL "HMP1002" USING PTLFDN OF FORMAT2
                    END-CALL
           END-IF.

     ‚* SICHTBARKEIT UMSCHALTEN
           IF       AUSW OF FORMAT2 = "6"
                    PERFORM SICHT-UMSCHALTEN
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FEHLERMELDUNG WENN KEINE SFL-ZEILEN VORHANDEN
     ‚*--------------------------------------------------------------
       FEHLERMELDUNG SECTION.
       FEH-MEL-00.

           INITIALIZE F3, F12.
           INITIALIZE FORMAT3.

     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX    TO  FORMAT3-O-INDIC.
           MOVE     "FMT03"        TO FMT-WRK.
           MOVE     FORMAT3        TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.

           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO FEH-MEL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN08 IN FORMAT3-I-INDIC = AN
                    PERFORM NEUANLAGE
                    IF   F3  = "1"
                         GO TO FEH-MEL-90
                    END-IF
                    INITIALIZE F12
                    GO TO FEH-MEL-90
           END-IF.

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "HMP0000"         TO MAINUEB OF FORMAT3.
           MOVE     "HMP1000*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUANLAGE EINES PROGRAMMS
     ‚*--------------------------------------------------------------
       NEUANLAGE SECTION.
       NEU-ANL-00.

           INITIALIZE F3, F12.
     ‚* ANLAGE EINES NEUEN TAGS
           MOVE     ZEROES TO PTLFDN-WRK.
           CALL     "HMP1001" USING PTLFDN-WRK
                                    PGM-WRK
                                    F3
                                    F12
           END-CALL.

       NEU-ANL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SICHT UMSCHALTEN
     ‚*--------------------------------------------------------------
       SICHT-UMSCHALTEN SECTION.
       SIC-UMS-00.

           INITIALIZE PTAGLIBF OF PTAGLIB-P.
           MOVE     PTLFDN OF FORMAT2 TO PTLFDN OF PTAGLIB-P.
           READ     PTAGLIB-DP.
           IF       FILE-STATUS = ZEROES
                    IF   PTFK01 OF PTAGLIB-P = ZEROES
                         MOVE 1      TO PTFK01 OF PTAGLIB-P
                    ELSE
                         MOVE ZEROES TO PTFK01 OF PTAGLIB-P
                    END-IF
                    REWRITE  PTAGLIB-P
           END-IF.

       SIC-UMS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WINDOWANZEIGE ZUR TAGAUSWAHL
     ‚*--------------------------------------------------------------
       TAG-SELEKTION SECTION.
       TAG-SEL-00.

           INITIALIZE TAG-WRK.
           CALL     "HMP0005" USING TAG-WRK
                                    F12
           END-CALL.

     ‚* WIRD HIER NICHT BENÖTIGT
           INITIALIZE F12.

       TAG-SEL-90.
           EXIT.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
