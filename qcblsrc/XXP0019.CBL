     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0019.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DETAILANZEIGE POPLLST-SATZ (OPLFDN)                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.03.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DETAILANSICHT OPL-PUNKT                      *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚* POPLLST: OPLFDN
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DP
                  ASSIGN       TO  DATABASE-POPLLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* POPLLSTL1: OPSHRN, OPUSER
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DL
                  ASSIGN       TO  DATABASE-POPLLSTL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚*-   TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
     ‚*-------------------------------------------------------------
           SELECT BSSORT-DAT
                  ASSIGN       TO  DISK-BSSORT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*- TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
       SD  BSSORT-DAT.
       01  BSSORT-REC.
            COPY  DDS-FMT02-O OF XXD0019DE.
      /
     ‚*--- POPLLST: OPLFDN
       FD  POPLLST-DP
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-P.
           COPY DDS-ALL-FORMATS OF POPLLST.
      /
     ‚*--- POPLLSTL2: OPSHRN, OPUSER
       FD  POPLLST-DL
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-L.
           COPY DDS-ALL-FORMATS OF POPLLSTL2.
      /
       WORKING-STORAGE SECTION.

     ‚* LISTE DER TEMPORÄREN INDIKATOREN
       01  INXX.
         02 IN02                          PIC 1.
         02 IN04                          PIC 1.
         02 IN91                          PIC 1.
         02 IN95                          PIC 1.
         02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0019DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0019DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0019DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0019DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0019".
       01  PGM-TYP                       PIC X(8)  VALUE "OPL   ".

       01  UPDATE-KZ                     PIC 9(1).
       01  AUSWAHL                       PIC X(2).
       01  OPTYPE-WRK                    LIKE OPTYPE OF POPLLST-P.
       01  ANZEIGE-MODUS                 PIC 9(1).

       01  OPLFDN-SHARE                  LIKE OPLFDN OF POPLLST-P.
       01  SHARE-COUNT                   PIC 9(4).

       01  SORT-KZ                       PIC X(2) VALUE "11".

     ‚* PARAMETER FÜR DAS XXP0018
       01   USER-WRK                     LIKE T-USER.

     ‚* PARAMETER FÜR DAS CFP0023
       01  PTXTFIT-WRK.
            COPY DDS-ALL-FORMATS OF PTXTFIT.
       01  FTFILE-WRK                    LIKE FTFILE OF PTXTFIT-WRK.
       01  FTKEY-WRK                     LIKE FTKEY  OF PTXTFIT-WRK.

     ‚* PARAMETER FÜR DAS CFP0027
       01  FILE-WRK                      PIC X(10).
       01  FORMAT-DUMMY                  PIC X(10).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-OPLFDN                      LIKE OPLFDN OF POPLLST-P.
       01  X-F3                          PIC X(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-OPLFDN
                                X-F3.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0019"   TO PGM-WRK.
           MOVE     "XXD0019DE" TO DISPLAY-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DATEI ÖFFNEN
           OPEN     I-O   POPLLST-DP
                    INPUT POPLLST-DL.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT3.
           INITIALIZE F3, F12.
           INITIALIZE ANZEIGE-MODUS.
           PERFORM  COPY-GET-TIME.

     ‚* DERZEIT IST NUR EINE ANZEIGE BZW. ÄNDERN MÖGLICH
           INITIALIZE POPLLSTF OF POPLLST-P.
           MOVE     X-OPLFDN TO OPLFDN OF POPLLST-P.
           READ     POPLLST-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           MOVE     "POPLLST"         TO FTFILE-WRK
           IF       OPSHRN OF POPLLST-P = ZEROES
                    MOVE OPLFDN OF POPLLST-P TO FTKEY-WRK
           ELSE
                    MOVE "S"                 TO FTKEY-WRK(1:1)
                    MOVE OPSHRN OF POPLLST-P TO FTKEY-WRK(2:)
           END-IF.

       ANF010.
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3  = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

           GO TO    ANF010.

       ENDE.
           CLOSE    POPLLST-DP
                    POPLLST-DL.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SORTFILE EINLESEN UND AUSGEBEN
     ‚*--------------------------------------------------------------
       LESE-DATEN-SORTFILE SECTION.
       LES-DAS-00.

           INITIALIZE DATEN.
     ‚* AUSGABE SORTIEREN
           EVALUATE SORT-KZ
                    WHEN "11"
                         SORT     BSSORT-DAT ON ASCENDING KEY
                                                  OPUSER OF BSSORT-REC
                                   WITH DUPLICATES
                                  INPUT  PROCEDURE LESE-DATEN
                                  OUTPUT PROCEDURE SUBFILE-SORT-OUTPUT
           END-EVALUATE.

       LES-DAS-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

     ‚* POPLLST LESEN
           INITIALIZE POPLLSTF OF POPLLST-L.
           MOVE     OPSHRN OF POPLLST-P TO OPSHRN OF POPLLST-L.
           MOVE     SPACES              TO OPUSER OF POPLLST-L.
           START    POPLLST-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     POPLLST-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHLÜSSELABFRAGE
           IF       OPSHRN OF POPLLST-L NOT = OPSHRN OF POPLLST-P
                    GO TO LES-DAT-90
           END-IF.

           INITIALIZE BSSORT-REC.
           MOVE     CORR POPLLSTF OF POPLLST-L TO
                    FMT02-O OF BSSORT-REC.

           RELEASE  BSSORT-REC.
           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DES SUBFILES INKL. GRUPPIERUNG
     ‚*--------------------------------------------------------------
       SUBFILE-SORT-OUTPUT SECTION.
       SUB-SOU-00.

           INITIALIZE ANZREC-WRK.

     ‚* SORTFILE AUSLESEN
       SUB-SOU-20.
           RETURN   BSSORT-DAT AT END
                    GO TO SUB-SOU-90
           END-RETURN.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
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
           MOVE     BSSORT-REC   TO FORMAT2.

           PERFORM  SET-COLOR-FMT02.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           ADD      1            TO ANZREC-WRK.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

           IF       SUBFILE-RECORD-NUMBER < 9999
                    GO TO  SUB-SOU-20
           END-IF.

       SUB-SOU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBE SETZEN
     ‚*--------------------------------------------------------------
       SET-COLOR-FMT02 SECTION.
       SET-COL-00.

     ‚* IST DAS DER SHARE-MANAGER?
           IF       OPSHRM OF FORMAT2     = OPUSER OF FORMAT2
                    MOVE     COL-RED      TO A_OPUSER OF FORMAT2
           END-IF.

     ‚* INDIKATOR AKTIVIEREN
           MOVE     AN           TO IN02     OF INXX.

       SET-COL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE F2.
           INITIALIZE FORMAT3.
           MOVE     1          TO SFL-ZEILE.

       ANZ-SFL-10.
     ‚* SELEKTION ANZEIGEN
           MOVE     AUS    TO INXX.
           IF       OPSHRN OF POPLLST-P NOT = ZEROES
                    PERFORM  LESE-DATEN-SORTFILE
           ELSE
                    MOVE     ZEROES TO DATEN
                    MOVE     ZEROES TO SUBFILE-RECORD-NUMBER
                    MOVE     ZEROES TO ANZREC-WRK
           END-IF.

       ANZ-SFL-20.
           MOVE     "FMT04"         TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
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
     ‚*             AUSGABE FEHLERMELDUNG
                    MOVE     "FMT05"         TO  FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE     AUS    TO IN91  IN  INXX
           ELSE
                    MOVE     AN     TO IN91  IN  INXX
           END-IF.

     ‚* DATEN FÜLLEN
           MOVE     CORR POPLLSTF OF POPLLST-P TO FORMAT3.
           MOVE     OPDATE OF POPLLST-P TO DT_O  OF FORMAT3.
           MOVE     OPADTA OF POPLLST-P TO DT_A  OF FORMAT3.
           MOVE     OPATIA OF POPLLST-P TO TI_A  OF FORMAT3.
           MOVE     OPMDTA OF POPLLST-P TO DT_M  OF FORMAT3.
           MOVE     OPMTIA OF POPLLST-P TO TI_M  OF FORMAT3.

     ‚* TEXT ZUR KATEGORIE LESEN, WENN VORHANDEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H102"            TO CFID   OF CFG-CPY.
           MOVE     OPTYPE OF FORMAT3 TO CFKEY  OF CFG-CPY.
           MOVE     OPUSER OF FORMAT3 TO CFKEY2 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
                    MOVE CFTXT1 OF CFG-CPY TO TYPTXT OF FORMAT3
           END-IF.

     ‚* ZUSÄTZLICHER TEXT VORHANDEN?
           CALL     "CFP0004" USING FTFILE-WRK
                                    FTKEY-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE 1      TO TXTVOR OF FORMAT3
           ELSE
                    MOVE ZEROES TO TXTVOR OF FORMAT3
           END-IF.

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

     ‚* F9 TEXT PFLEGEN
           IF       IN09 IN FORMAT3-I-INDIC = AN
                    MOVE "2" TO AUSWAHL
                    CALL "CFP0023" USING FTFILE-WRK
                                         FTKEY-WRK
                                         AUSWAHL
                                         PGM-WRK
                                         F3
                                         F12
                    END-CALL
                    INITIALIZE F12
                    IF   F3 = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

           IF       IN20 OF FORMAT3-I-INDIC = AN
                    MOVE "POPLLST" TO FILE-WRK
                    MOVE SPACES    TO FORMAT-DUMMY
                    MOVE POPLLST-P TO RECORD-WRK
                    CALL "CFP0027" USING RECORD-WRK
                                         CONST-LIB
                                         FILE-WRK
                                         FORMAT-DUMMY
                                         F3
                    END-CALL
                    IF   F3 = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    GO TO ANZ-SFL-20
           END-IF.

           MOVE     SPACES TO SFL-CHG.

           PERFORM  UPDATE-OPTEXT.

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

     ‚* ENTERNEN EINES USERS AUS DER SHARE-NUMMER
           IF       AUSW OF FORMAT2 = "4"
                    IF   OPLFDN OF FORMAT2 NOT = OPLFDN OF FORMAT3
                         PERFORM DELETE-USER-FROM-SHARE
                         MOVE "1" TO SFL-CHG
                    END-IF
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

           MOVE     "XXP0019"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0019*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEXT AKTUALISIEREN
     ‚*--------------------------------------------------------------
       UPDATE-OPTEXT SECTION.
       UPD-TXT-00.

           MOVE     ZEROES TO UPDATE-KZ.
     ‚* EINTRAG AKTUALISIEREN
           INITIALIZE POPLLSTF OF POPLLST-P.
           MOVE     X-OPLFDN TO OPLFDN OF POPLLST-P.
           READ     POPLLST-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-TXT-90
           END-IF.

           IF       OPTEXT OF FORMAT3 NOT = OPTEXT OF POPLLST-P
            OR      OPPRZT OF FORMAT3 NOT = OPPRZT OF POPLLST-P
            OR      OPPRIO OF FORMAT3 NOT = OPPRIO OF POPLLST-P
     ‚*             TEXT ÜBERNEHMEN
                    MOVE     OPTEXT OF FORMAT3   TO OPTEXT OF POPLLST-P
     ‚*             PROZENTZAHL ÜBERNEHMEN
                    MOVE     OPPRZT OF FORMAT3   TO OPPRZT OF POPLLST-P
     ‚*             BEI 100% IST DER PUNKT ABGESCHLOSSEN
                    IF   OPPRZT OF POPLLST-P >= 100
                         MOVE "9"                TO OPSTAT OF POPLLST-P
                    ELSE
                         MOVE SPACES             TO OPSTAT OF POPLLST-P
                    END-IF
     ‚*             PRIORITÄT ÜBERNEHMEN
                    MOVE     OPPRIO OF FORMAT3   TO OPPRIO OF POPLLST-P
     ‚*             USER/TERMINAL ETC NEU EINTRAGEN
                    MOVE     1                   TO UPDATE-KZ
           END-IF.

     ‚* WURDE WAS GEÄNDERT?
           IF       UPDATE-KZ = 1
     ‚*             USER FÜLLEN
                    PERFORM  COPY-GET-TIME
                    MOVE     T-USER              TO OPMSER OF POPLLST-P
                    MOVE     T-TERM              TO OPMBS  OF POPLLST-P
                    MOVE     PGM-WRK             TO OPMPGM OF POPLLST-P
                    MOVE     DATE-8              TO OPMDTA OF POPLLST-P
                    MOVE     TIME-6              TO OPMTIA OF POPLLST-P
     ‚*             SFL NEU ERSTELLEN
                    MOVE     "1"                 TO SFL-CHG
           END-IF.

     ‚* SATZ AKTUALISIEREN
           REWRITE  POPLLST-P.
           IF       OPSHRN OF POPLLST-P NOT = ZEROES
                    CALL "XXP0017" USING OPLFDN OF POPLLST-P
                                         OPSHRN OF POPLLST-P
                    END-CALL
           END-IF.

     ‚* EINTRAG KOPIEREN?
           IF       USER-WRK NOT = SPACES
                    CALL "XXP0018" USING OPLFDN OF POPLLST-P
                                         USER-WRK
                    END-CALL
           END-IF.

       UPD-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* USER AUS EINER SHARE-NUMMER ENTFERNEN
     ‚*--------------------------------------------------------------
       DELETE-USER-FROM-SHARE SECTION.
       DLT-USS-00.

     ‚* EINTRAG LÖSCHEN
           CALL     "XXP0016" USING OPLFDN OF FORMAT2
           END-CALL.

     ‚* PRÜFEN OB NOCH ANDERE USER DIESEN EINTRAG HABEN
           PERFORM  CHECK-COUNT-SHARE.
           IF       SHARE-COUNT = 1
                    INITIALIZE POPLLSTF OF POPLLST-P
                    MOVE OPLFDN-SHARE TO OPLFDN OF POPLLST-P
                    READ POPLLST-DP
                    IF   FILE-STATUS = ZEROES
     ‚*                  SHAREDATEN LÖSCHEN
                         MOVE ZEROES             TO OPSHRN OF POPLLST-P
                         MOVE SPACES             TO OPSHRM OF POPLLST-P
     ‚*                  USER FÜLLEN
                         PERFORM  COPY-GET-TIME
                         MOVE T-USER             TO OPMSER OF POPLLST-P
                         MOVE T-TERM             TO OPMBS  OF POPLLST-P
                         MOVE PGM-WRK            TO OPMPGM OF POPLLST-P
                         MOVE DATE-8             TO OPMDTA OF POPLLST-P
                         MOVE TIME-6             TO OPMTIA OF POPLLST-P
                         REWRITE  POPLLST-P
                    END-IF
           END-IF.

       DLT-USS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZÄHLEN WIEVIELE USER DIE SHARENUMMER HABEN
     ‚*--------------------------------------------------------------
       CHECK-COUNT-SHARE SECTION.
       CHK-CSH-00.

           INITIALIZE SHARE-COUNT.
     ‚* POPLLST LESEN
           INITIALIZE POPLLSTF OF POPLLST-L.
           MOVE     OPSHRN OF POPLLST-P TO OPSHRN OF POPLLST-L.
           MOVE     SPACES              TO OPUSER OF POPLLST-L.
           START    POPLLST-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-CSH-90
           END-IF.

       CHK-CSH-20.
           READ     POPLLST-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-CSH-90
           END-IF.

     ‚* SCHLÜSSELABFRAGE
           IF       OPSHRN OF POPLLST-L NOT = OPSHRN OF POPLLST-P
                    GO TO CHK-CSH-90
           END-IF.

           ADD      1 TO SHARE-COUNT.
           MOVE     OPLFDN OF POPLLST-L TO OPLFDN-SHARE.

           GO TO    CHK-CSH-20.

       CHK-CSH-90.
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
