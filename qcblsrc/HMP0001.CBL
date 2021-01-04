       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP0001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUSWAHLPROGRAMM FÜR HERMES (STARTPROGRAMM)                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSWAHL + ANZEIGE PROGRAMME                  *
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
                  ASSIGN       TO  WORKSTATION-HMD0001DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PPGMLIB-DP
                  ASSIGN       TO  DATABASE-PPGMLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI TAGS SORTIERT NACH TAG
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL2
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
           COPY DDS-ALL-FORMATS OF HMD0001DE.
      /
     ‚*--- PROGRAMME
       FD  PPGMLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLIB-P.
           COPY DDS-ALL-FORMATS OF PPGMLIB.
     ‚*--- TAGS
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL2.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN40                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF HMD0001DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  HMD0001DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  HMD0001DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  HMD0001DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  HMD0001DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  HMD0001DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HMP0001".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

     ‚* INTERNE TABELLE
       01  SELTAG-TAB                    TYPE SELTAG-TYPE.

       01  LEN-L                         PIC 9(4).
       01  LEN-F                         PIC 9(4).
       01  PLLIB-WRK                     LIKE PLLIB  OF PPGMLIB-P.
       01  PLFILE-WRK                    LIKE PLFILE OF PPGMLIB-P.
       01  AUSWAHL                       PIC X(2).

       01  TAG-WRK                       LIKE PTTAG  OF PTAGLIB-L.
       01  TAG-VORHANDEN                 PIC 9(4).
       01  TAG-SELEKTIERT                PIC 9(4).

     ‚* PARAMETER FÜR DAS HMP0004
       01  LIB-WRK                       LIKE PLLIB  OF PPGMLIB-P.
       01  LIB-SAV                       LIKE PLLIB  OF PPGMLIB-P.
       01  FILE-WRK                      LIKE PLFILE OF PPGMLIB-P.

     ‚* PARAMETER FÜR DAS CRP0036
       01  SEQNBR-DUMMY                  PIC 9(4)V9(2).
       01  OPTION-WRK                    PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP0001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "HMD0001DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PPGMLIB-DP
                          PTAGLIB-DL.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.
           PERFORM  GET-VORBELEGUNG.
           PERFORM  INIT-VAR.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DER TAGS BZW. PROGRAMMNAME
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
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

           CLOSE                 PPGMLIB-DP
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
           MOVE     "HMP0001*1"       TO SUBUEB  OF FORMAT1.

     ‚* INTERNE TABELLE LÖSCHEN
           PERFORM  DEL-SELTAB.
           INITIALIZE LIB-SAV.

       VOR-AUS-10.
           MOVE     SPACES            TO RET-CODE.
     ‚* SIND INFORMATIONEN VORHANDEN?
           CALL     "CFP0010" USING PGM-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    MOVE AN  TO IN40 OF INXX
           ELSE
                    MOVE AUS TO IN40 OF INXX
           END-IF.

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

     ‚* F18 ABBRECHEN ?
           IF       IN18 OF FORMAT1-I-INDIC  = AN
                    CALL "HMP1000" USING F3
                    END-CALL
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    GO TO VOR-AUS-00
           END-IF.

     ‚* F20 PROGRAMMINFORMATIONEN
           IF       IN20 IN FORMAT1-I-INDIC = AN
                    PERFORM ANZEIGE-PGM-INFOS
                    GO TO VOR-AUS-10
           END-IF.

     ‚* F21 KOMMANDOZEILE AUFRUFEN
           IF       IN21 IN FORMAT1-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO VOR-AUS-10
           END-IF.

     ‚* FRAGEZEICHEN-FUNKTION FÜR EIN TAG
     ‚*
     ‚* TAG1
           IF       TAG1   OF FORMAT1(1:1) = "?"
                    PERFORM TAG-SELEKTION
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    MOVE TAG-WRK              TO TAG1    OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

     ‚* TAG2
           IF       TAG2   OF FORMAT1(1:1) = "?"
                    PERFORM TAG-SELEKTION
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    MOVE TAG-WRK              TO TAG2    OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

     ‚* TAG3
           IF       TAG3   OF FORMAT1(1:1) = "?"
                    PERFORM TAG-SELEKTION
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    MOVE TAG-WRK              TO TAG3    OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

     ‚* TAG4
           IF       TAG4   OF FORMAT1(1:1) = "?"
                    PERFORM TAG-SELEKTION
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    MOVE TAG-WRK              TO TAG4    OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

     ‚* TAG5
           IF       TAG5   OF FORMAT1(1:1) = "?"
                    PERFORM TAG-SELEKTION
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    MOVE TAG-WRK              TO TAG5    OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

     ‚* TAG-AUSWAHL AUFRÜCKEN
           IF       TAG5   OF FORMAT1 NOT = SPACES
            AND     TAG4   OF FORMAT1     = SPACES
                    MOVE   TAG5    OF FORMAT1 TO TAG4    OF FORMAT1
                    MOVE   SPACES             TO TAG5    OF FORMAT1
           END-IF.

           IF       TAG4   OF FORMAT1 NOT = SPACES
            AND     TAG3   OF FORMAT1     = SPACES
                    MOVE   TAG4    OF FORMAT1 TO TAG3    OF FORMAT1
                    MOVE   SPACES             TO TAG4    OF FORMAT1
           END-IF.

           IF       TAG3   OF FORMAT1 NOT = SPACES
            AND     TAG2   OF FORMAT1     = SPACES
                    MOVE   TAG3    OF FORMAT1 TO TAG2    OF FORMAT1
                    MOVE   SPACES             TO TAG3    OF FORMAT1
           END-IF.

           IF       TAG2   OF FORMAT1 NOT = SPACES
            AND     TAG1   OF FORMAT1     = SPACES
                    MOVE   TAG2    OF FORMAT1 TO TAG1    OF FORMAT1
                    MOVE   SPACES             TO TAG2    OF FORMAT1
           END-IF.

     ‚* MINDESTENS EIN TAG MUSS SELEKTIERT WORDEN SEIN
           IF       TAG1   OF FORMAT1     = "FEHLER"
                    MOVE  "ABC"           TO ERRMSG OF FORMAT1
                    MOVE  COL-GRN-RI      TO A_TAG1 OF FORMAT1
                    MOVE  AN TO IN01 OF INXX
                    MOVE  AN TO IN02 OF INXX
                    GO TO VOR-AUS-10
           END-IF.

     ‚* BERECHNEN ANZAHL DER SELEKTIERTEN TAGS
           INITIALIZE TAG-SELEKTIERT.
           IF       TAG1   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG1   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.
           IF       TAG2   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG2   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.
           IF       TAG3   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG3   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.
           IF       TAG4   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG4   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.
           IF       TAG5   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG5   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.

     ‚* WEITERE TAGS PFLEGEN
           IF       WEITERE OF FORMAT1 = "+"
                    PERFORM AENDERN-TAGSELEKTION
                    MOVE SPACES TO WEITERE OF FORMAT1
           END-IF.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE PLFILE-WRK.
           INITIALIZE DATEN, ANZREC-WRK, LEN-L, LEN-F.
     ‚* WURDE EIN PGM MIT STERN SELEKTIERT?
           IF       PLFILE OF FORMAT1      NOT = SPACES
            AND     PLFILE OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN-F
                    INSPECT  PLFILE OF FORMAT1
                             TALLYING LEN-F FOR CHARACTERS
                             BEFORE INITIAL "*"
                    MOVE     PLFILE OF FORMAT1(1:LEN-F) TO PLFILE-WRK
           ELSE
                    MOVE     SPACES                     TO PLFILE-WRK
           END-IF.

     ‚* WURDE EINE LIB MIT STERN SELEKTIERT?
           IF       PLLIB  OF FORMAT1      NOT = SPACES
            AND     PLLIB  OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN-L
                    INSPECT  PLLIB  OF FORMAT1
                             TALLYING LEN-L FOR CHARACTERS
                             BEFORE INITIAL "*"
                    MOVE     PLLIB  OF FORMAT1(1:LEN-L) TO PLLIB-WRK
           ELSE
                    MOVE     SPACES                     TO PLLIB-WRK
           END-IF.

     ‚* PPGMLIB LESEN
           INITIALIZE PPGMLIBF OF PPGMLIB-P.
           MOVE     PLLIB-WRK           TO PLLIB  OF PPGMLIB-P.
           MOVE     PLFILE-WRK          TO PLFILE OF PPGMLIB-P.
           START    PPGMLIB-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PPGMLIB-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       LEN-L > ZEROES
            AND     PLLIB  OF PPGMLIB-P(1:LEN-L) NOT =
                    PLLIB  OF FORMAT1(1:LEN-L)
                    GO TO LES-DAT-90
           END-IF.

           IF       LEN-F > ZEROES
            AND     PLFILE OF PPGMLIB-P(1:LEN-F) NOT =
                    PLFILE OF FORMAT1(1:LEN-F)
                    GO TO LES-DAT-20
           END-IF.

     ‚* NUR BESTIMMTE OBJEKTART ANZEIGEN?
           IF       PLSEU2 OF FORMAT1   NOT = SPACES
            AND     PLSEU2 OF PPGMLIB-P NOT = PLSEU2 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* NACH TEXT SELEKTIEREN?
           IF       TXTSEL OF FORMAT1   NOT = SPACES
                    MOVE PPGMLIB-P           TO RECORD-WRK
                    MOVE LENGTH OF PPGMLIB-P TO RECORD-LEN
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

     ‚* PRÜFEN ALLE SELEKTIERTEN TAGS VORHANDEN IST
           PERFORM  CHECK-TAG-VORHANDEN.
           IF       RET-CODE = SPACES
                    GO TO LES-DAT-20
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
           MOVE     CORR PPGMLIBF OF PPGMLIB-P TO FORMAT2.
     ‚* KOMMENTAR AUFRÜCKEN WENN Z.B. NUR DER ZWEITE GEFÜLLT IST
           IF       PLKOM1 OF FORMAT2 = SPACES
                    IF   PLKOM2 OF PPGMLIB-P NOT = SPACES
                         MOVE PLKOM2 OF PPGMLIB-P TO PLKOM1 OF FORMAT2
                    ELSE
     ‚*                  KOMMENTAR 3 AUFRÜCKEN BZW. TEILDATEIENTEXT
                         IF   PLKOM3 OF PPGMLIB-P NOT = SPACES
                              MOVE PLKOM3 OF PPGMLIB-P TO
                                   PLKOM1 OF FORMAT2
                         ELSE
                              MOVE PLMTXT OF PPGMLIB-P TO
                                   PLKOM1 OF FORMAT2
                         END-IF
                    END-IF
           END-IF.

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
                    PERFORM COPY-DISPLAY-CURSER
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

     ‚* F13 WEITERE SELEKTION
           IF       IN13 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    PERFORM AENDERN-TAGSELEKTION
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F21 KOMMANDOZEILE AUFRUFEN
           IF       IN21 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    PERFORM KOMMANDOZEILE
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
                    CALL "HMP0003" USING PLLIB  OF FORMAT2
                                         PLFILE OF FORMAT2
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

     ‚* LÖSCHEN DES SATZES
           IF       AUSW OF FORMAT2 = "4"
                    CALL "HMP0008" USING PLLIB  OF FORMAT2
                                         PLFILE OF FORMAT2
                    END-CALL
           END-IF.

     ‚* DETAILANZEIGE DES DATENSATZES
           IF       AUSW OF FORMAT2 = "5"
                    PERFORM START-SEU
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                    END-IF
           END-IF.

     ‚* BESCHREIBUNG ANZEIGEN
           IF       AUSW OF FORMAT2 = "6"
                    INITIALIZE F3, F12
                    MOVE "5" TO AUSWAHL
                    CALL "HMP0006" USING PLLIB  OF FORMAT2
                                         PLFILE OF FORMAT2
                                         AUSWAHL
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

     ‚* F13 WEITERE SELEKTION
           IF       IN13 IN FORMAT3-I-INDIC = AN
                    PERFORM AENDERN-TAGSELEKTION
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
           MOVE     "HMP0001*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUANLAGE EINES PROGRAMMS
     ‚*--------------------------------------------------------------
       NEUANLAGE SECTION.
       NEU-ANL-00.

           INITIALIZE F3, F12.
           INITIALIZE LIB-WRK, FILE-WRK.
     ‚* WINDOW ZUR EINGABE LIB/DATEI
           MOVE     PLLIB  OF FORMAT1 TO LIB-WRK.
           IF       LIB-SAV NOT = SPACES
                    MOVE LIB-SAV TO LIB-WRK
           END-IF.
           CALL     "HMP0004" USING LIB-WRK
                                    FILE-WRK
                                    F12
           END-CALL.
           IF       F12 = "1"
                    INITIALIZE F12
                    GO TO NEU-ANL-90
           END-IF.

     ‚* WURDE EIN PGM ANGELEGT, WIRD DIE LIB GESICHERT, DAMIT
     ‚* DIESE DANN BEI DER NÄCHSTEN ANLAGE WIEDER VORBELEGT WIRD
           MOVE     LIB-WRK TO LIB-SAV.

     ‚* ANLEGEN DES DATENSATZES
           CALL     "HMP0003" USING LIB-WRK
                                    FILE-WRK
                                    PGM-WRK
                                    F3
                                    F12
           END-CALL.

       NEU-ANL-90.
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
     ‚*--------------------------------------------------------------
     ‚* BEARNEITEN DER INT. TABELLE
     ‚*--------------------------------------------------------------
       AENDERN-TAGSELEKTION SECTION.
       AEN-TAG-00.

           INITIALIZE F12.
           CALL     "HMP0007" USING SELTAG-TAB
                                    F12
           END-CALL.
           INITIALIZE F12.

       AEN-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STARTEN DES SEU ÜBER EIN CL
     ‚*--------------------------------------------------------------
       START-SEU SECTION.
       STR-SEU-00.

           INITIALIZE RET-CODE.
           INITIALIZE FILE-WRK.
           INITIALIZE LIB-WRK.
           INITIALIZE OPTION-WRK.

     ‚* HIER KOMMT MAN EH NUR MIT "5" REIN
           MOVE     AUSW OF FORMAT2 TO OPTION-WRK.

     ‚* BIBLIOTHEK UMSETZEN
           MOVE     PLLIB OF FORMAT2 TO LIB-WRK.
           IF       LIB-WRK(4:3) = "PGM"
            OR      LIB-WRK(4:3) = "DAT"
                    MOVE "SRC" TO LIB-WRK(4:3)
           END-IF.

     ‚* DATEI UMSETZEN
           EVALUATE PLSEU2 OF FORMAT2
                    WHEN "CBLLE"
                    WHEN "CBL"
                         MOVE "QCBLSRC" TO FILE-WRK
                    WHEN "CLLE"
                    WHEN "CLP"
                         MOVE "QCLSRC"  TO FILE-WRK
                    WHEN "CLE"
                    WHEN "CPPLE"
                         MOVE "QCSRC"   TO FILE-WRK
                    WHEN "RPG"
                         MOVE "QRPGSRC" TO FILE-WRK
                    WHEN "DSPF"
                    WHEN "PRTF"
                    WHEN "PF"
                    WHEN "LF"
                         MOVE "QDDSSRC" TO FILE-WRK
                    WHEN OTHER
                         GO TO STR-SEU-90
           END-EVALUATE.

     ‚* SEU STARTEN
           MOVE     ZEROES TO SEQNBR-DUMMY.
           CALL     "CRP0036"  USING LIB-WRK
                                     FILE-WRK
                                     PLFILE OF FORMAT2
                                     SEQNBR-DUMMY
                                     OPTION-WRK
                                     F3
                                     F12
           END-CALL.

       STR-SEU-90.
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
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN VON INFORMATIONSTEXTEN
     ‚*--------------------------------------------------------------
       ANZEIGE-PGM-INFOS SECTION.
       ANZ-PGM-00.

           INITIALIZE F12.
     ‚* INFORMATIONEN ANZEIGEN
           CALL     "CFP0009" USING PGM-WRK
                                    F12
           END-CALL.
           INITIALIZE F12.

       ANZ-PGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER VORBELUNG
     ‚*--------------------------------------------------------------
       GET-VORBELEGUNG SECTION.
       GET-VOB-00.

     ‚* ERSTMAL DIE STANDARDWERTE NEHMEN
           MOVE     "*"          TO PLFILE OF FORMAT1.
           MOVE     "LFS*"       TO PLLIB  OF FORMAT1.

     ‚* WURDE EINE VORBELEGUNG IN DER CONFIG HINTERLEGT?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H100"        TO CFID   OF CFG-CPY.
           MOVE     T-USER        TO CFKEY  OF CFG-CPY.
           MOVE     SPACES        TO CFKEY2 OF CFG-CPY.
           MOVE     SPACES        TO CFKEY3 OF CFG-CPY.
           MOVE     SPACES        TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
                    MOVE CFDAT1 OF CFG-CPY TO PLFILE OF FORMAT1
                    MOVE CFDAT2 OF CFG-CPY TO PLLIB  OF FORMAT1
                    MOVE CFDAT3 OF CFG-CPY TO PLSEU2 OF FORMAT1
           END-IF.

       GET-VOB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INITIALISIEREN ALLER VARIABLEN
     ‚*--------------------------------------------------------------
       INIT-VAR SECTION.
       INI-VAR-00.

     ‚* INITIALISIEREN ALLER VARIABLEN
           INITIALIZE LIB-SAV.

     ‚* INITIALISIEREN DER INTERNEN TABELLE
           MOVE     ZEROES TO TAB-MAX OF SELTAG-TAB.
           PERFORM  DEL-SELTAB.

       INI-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES DATENSATZES IN DER INT. TABELLE
     ‚*--------------------------------------------------------------
       ADD-SELTAB SECTION.
       ADD-STB-00.

     ‚* TAG LEER? DANN NICHTS MACHEN
           IF       TAG-WRK = SPACES
                    GO TO ADD-STB-90
           END-IF.

     ‚* IST DIE TABELLE LEER, DANN SOFORT NEUEN ANLEGEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO ADD-STB-80
           END-IF.

           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.

       ADD-STB-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO ADD-STB-80
           END-IF.

     ‚* TAG SCHON VORHANDEN? DANN NICHTS MACHEN
           IF       PTTAG  OF TAB-REC(TAB-POS OF SELTAG-TAB) = TAG-WRK
                    GO TO ADD-STB-90
           END-IF.

           GO TO    ADD-STB-20.

       ADD-STB-80.
           ADD      1    TO TAB-MAX OF SELTAG-TAB.
           MOVE     TAG-WRK TO PTTAG  OF
                    TAB-REC(TAB-MAX OF SELTAG-TAB).
           MOVE     "9"     TO TAG-KZ OF
                    TAB-REC(TAB-MAX OF SELTAG-TAB).

       ADD-STB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TAG-TABELLE ZUR SELEKTION
     ‚*--------------------------------------------------------------
       DEL-SELTAB SECTION.
       DEL-STB-00.

     ‚* IST DIE TABELLE LEER, DANN MUSS NICHTS MEHR GEMACHT WERDEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO DEL-STB-90
           END-IF.

           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.

       DEL-STB-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO DEL-STB-90
           END-IF.

     ‚* INITIALISIEREN DER WERTE
           MOVE     SPACES TO PTTAG    OF
                              SELTAG-TAB(TAB-POS OF SELTAG-TAB).

           GO TO    DEL-STB-20.

       DEL-STB-90.
           INITIALIZE SELTAG-TAB.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER INT. TABELLE
     ‚*--------------------------------------------------------------
       CHK-SELTAB SECTION.
       CHK-STB-00.

     ‚* IST DIE TABELLE LEER, DANN MUSS NICHTS MEHR GEMACHT WERDEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO CHK-STB-90
           END-IF.

           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.

       CHK-STB-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO CHK-STB-90
           END-IF.

           IF       PTTAG  OF SELTAG-TAB(TAB-POS OF SELTAG-TAB) =
                    TAG-WRK
                    ADD 1 TO TAG-VORHANDEN
           END-IF.

           GO TO    CHK-STB-20.

       CHK-STB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN TAG VORHANDEN IST
     ‚*--------------------------------------------------------------
       CHECK-TAG-VORHANDEN SECTION.
       CHK-TAG-00.

           INITIALIZE RET-CODE, TAG-VORHANDEN.
     ‚* NICHTS SELEKTIERT? DANN ALLES ANZEIGEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TAG-90
           END-IF.

     ‚* AUFSETZEN MIT DEM ERSTEN TAG
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     PLLIB  OF PPGMLIB-P      TO PTLIB  OF PTAGLIB-L.
           MOVE     PLFILE OF PPGMLIB-P      TO PTFILE OF PTAGLIB-L.
           MOVE     SPACES                   TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TAG-90
           END-IF.

        CHK-TAG-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TAG-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       PTLIB  OF PTAGLIB-L    NOT = PLLIB  OF PPGMLIB-P
            OR      PTFILE OF PTAGLIB-L    NOT = PLFILE OF PPGMLIB-P
                    GO TO CHK-TAG-90
           END-IF.

     ‚* PRÜFEN DES TAGS MIT DER INT. TABELLE
           MOVE     PTTAG  OF PTAGLIB-L TO TAG-WRK.
           PERFORM  CHK-SELTAB.

     ‚* PASST DIE ANZAHL DER ÜBEREINSTIMMUNGEN?
           IF       TAG-VORHANDEN = TAB-MAX OF SELTAG-TAB
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TAG-90
           END-IF.

     ‚* WIEDERHOLEN ...
           GO TO    CHK-TAG-20.

       CHK-TAG-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
