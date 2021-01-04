       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP00401.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ÜBERSICHT ALLER BERICHTSHEFTE -> CMD "BVIEW"                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  C. FRERICHS                                  *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.03.2016                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  OFFENE BERICHTE GESAMTÜBERSICHT              *
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
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                         ATTRIBUTE-DATA IS BS-ATRIBUTE,
                         I-O-FEEDBACK   IS FILE-IO-FEEDBACK.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-XXD00401DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PAZUBER: ABUSER, ABJAHR, ABKW
     ‚*-------------------------------------------------------------
           SELECT PAZUBER-DL
                  ASSIGN       TO  DATABASE-PAZUBERL2
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
           COPY DDS-ALL-FORMATS OF XXD00401DE.
      /
     ‚*- PAZUBER: ABUSER, ABJAHR, ABKW
       FD  PAZUBER-DL
           LABEL RECORDS ARE STANDARD.
       01  PAZUBER-L.
           COPY DDS-ALL-FORMATS OF PAZUBERL2.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN03                          PIC 1.
        05 IN40                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD00401DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD00401DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD00401DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD00401DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD00401DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD00401DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP00401".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  AUSWAHL                       PIC X(2).

     ‚* PARAMETER FÜR DAS CFP8007
       01  KW-WRK                   LIKE ABKW OF PAZUBER-L.

     ‚* PARAMETER FÜR DAS XXP0045
       01  0045-AKTION                   PIC X(20).

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP00401" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD00401DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PAZUBER-DL.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.
           PERFORM  COPY-GET-TIME.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DES DATUMS UND ANZEIGE
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

       ANF020.
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3 = "1"
                    GO TO ENDE
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

           CLOSE                 PAZUBER-DL.

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
           MOVE     "XXP00401"        TO MAINUEB OF FORMAT1.
           MOVE     "XXP00401*1"      TO SUBUEB  OF FORMAT1.
           MOVE     T-USER            TO CFDAT1  OF FORMAT1.
           MOVE     DATE-8(1:4)       TO ABJAHR  OF FORMAT1.
           MOVE     DATE-8(1:4)       TO ABJAHRB OF FORMAT1.

     ‚* KW ZUM DATUM ERMITTELN
           CALL     "CFP8007" USING DATE-8
                                    KW-WRK
           END-CALL.
           MOVE     1                 TO ABKW    OF FORMAT1.
           SUBTRACT 1 FROM KW-WRK.
           MOVE     KW-WRK            TO ABKWB   OF FORMAT1.

       VOR-AUS-10.
           MOVE     AUS TO INXX.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.
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

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK.
     ‚* PAZUBER LESEN
           INITIALIZE PAZUBERF OF PAZUBER-L.
           MOVE     ABJAHR OF FORMAT1   TO ABJAHR OF PAZUBER-L.
           MOVE     ABKW   OF FORMAT1   TO ABKW   OF PAZUBER-L.
           START    PAZUBER-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PAZUBER-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       ABJAHR OF PAZUBER-L > ABJAHRB OF FORMAT1
                    GO TO LES-DAT-90
           END-IF.

           IF       ABJAHR OF PAZUBER-L = ABJAHRB OF FORMAT1
            AND     ABKW   OF PAZUBER-L > ABKWB   OF FORMAT1
                    GO TO LES-DAT-90
           END-IF.

     ‚* NACH VERANTWORTLICHEN PRÜFEN
           IF       CFDAT1 OF FORMAT1 NOT = SPACES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE     "H051"              TO CFID   OF CFG-CPY
                    MOVE     "BERICHTSHEFT"      TO CFKEY  OF CFG-CPY
                    MOVE     ABUSER OF PAZUBER-L TO CFKEY2 OF CFG-CPY
                    PERFORM  COPY-CFG-CPY
                    IF       CFG-DA OF CFG-CPY = ZEROES
                     OR      CFDAT1 OF CFG-CPY NOT = CFDAT1 OF FORMAT1
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
           MOVE     CORR PAZUBERF OF PAZUBER-L TO FORMAT2.

     ‚* ÄNDERUNGSDATEN FÜLLEN
           MOVE     ABMDTA OF FORMAT2         TO DT_M   OF FORMAT2.
           MOVE     ABMTIA OF FORMAT2         TO TI_M   OF FORMAT2.

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

       ANZ-SFL-10.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN
           PERFORM  LESE-DATEN.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

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
           IF       DATEN = ZEROES
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT05" TO FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE  AUS TO IN91 OF INXX
           ELSE
                    MOVE  AN  TO IN91 OF INXX
           END-IF.

           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    GO TO ANZ-SFL-10
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

           MOVE     SPACES TO SFL-CHG.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    IF   SFL-CHG = "1"
                         GO TO ANZ-SFL-10
                    ELSE
                         GO TO ANZ-SFL-20
                    END-IF
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

     ‚* ABSCHLIEßEN DES SATZES
           IF       AUSW OF FORMAT2 = "1"
            OR      AUSW OF FORMAT2 = "2"
                    MOVE "1"         TO SFL-CHG
     ‚*             MIT ODER OHNE MAIL?
                    IF   AUSW OF FORMAT2 = "2"
                         MOVE "NO-MAIL" TO 0045-AKTION
                    ELSE
                         MOVE SPACES    TO 0045-AKTION
                    END-IF
                    CALL "XXP0045" USING ABKW   OF FORMAT2
                                         ABJAHR OF FORMAT2
                                         ABUSER OF FORMAT2
                                         0045-AKTION
                    END-CALL
           END-IF.

     ‚* LÖSCHEN DES DATENSATZES
           IF       AUSW OF FORMAT2 = "4"
                    MOVE "1" TO SFL-CHG
                    CALL "XXP0041" USING ABJAHR OF FORMAT2
                                         ABKW   OF FORMAT2
                                         ABUSER OF FORMAT2
                    END-CALL
           END-IF.

     ‚*    SATZ ALS FEHLERHAFT KENNZEICHNEN (MIT ODER OHNE MAIL)
           IF       AUSW OF FORMAT2 = "6"
            OR      AUSW OF FORMAT2 = "5"
                    MOVE "1"         TO SFL-CHG
     ‚*             MIT ODER ODER EMAIL
                    IF   AUSW OF FORMAT2 = "6"
                         MOVE "ADD-ERROR" TO 0045-AKTION
                    ELSE
                         MOVE "MAILERROR" TO 0045-AKTION
                    END-IF
                    CALL "XXP0045" USING ABKW   OF FORMAT2
                                         ABJAHR OF FORMAT2
                                         ABUSER OF FORMAT2
                                         0045-AKTION
                    END-CALL
           END-IF.

     ‚*    ERINNERUNGS-MAIL AN AZUBI SCHREIBEN
           IF       AUSW OF FORMAT2 = "7"
                    MOVE "1"         TO SFL-CHG
                    MOVE "ER-MAIL" TO 0045-AKTION
                    CALL "XXP0045" USING ABKW   OF FORMAT2
                                         ABJAHR OF FORMAT2
                                         ABUSER OF FORMAT2
                                         0045-AKTION
                    END-CALL
           END-IF.

     ‚* PRÜFEN-EMAIL SCHREIBEN
     ‚* DER AZUBI SOLL PRÜFEN, OB DIESES BERICHTSHEFT BEREITS
     ‚* UNTERSCHRIEBEN UND ABGEHEFTET IST.
           IF       AUSW OF FORMAT2 = "8"
                    MOVE "1"       TO SFL-CHG
                    MOVE "PR-MAIL" TO 0045-AKTION
                    CALL "XXP0045" USING ABKW   OF FORMAT2
                                         ABJAHR OF FORMAT2
                                         ABUSER OF FORMAT2
                                         0045-AKTION
                    END-CALL
           END-IF.

     ‚* ABHOL-ERINNERUNG AN AZUBI SCHICKEN, DA ER SEIT LÄNGEREM
     ‚* SEINE KORRIGIERTEN BERICHTSHEFTE NICHT ABGEHOLT HAT.
           IF       AUSW OF FORMAT2 = "9"
                    MOVE "1"       TO SFL-CHG
                    MOVE "AH-MAIL" TO 0045-AKTION
                    CALL "XXP0045" USING ABKW   OF FORMAT2
                                         ABJAHR OF FORMAT2
                                         ABUSER OF FORMAT2
                                         0045-AKTION
                    END-CALL
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
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP00401"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP00401*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUANLAGE EINES PROGRAMMS
     ‚*--------------------------------------------------------------
       NEUANLAGE SECTION.
       NEU-ANL-00.

           INITIALIZE F3, F12.
           CALL     "XXP0055"
           END-CALL.

       NEU-ANL-90.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
