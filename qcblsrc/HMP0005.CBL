     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HMP0005.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* WINDOW ZUR TAG-SELEKTION                                      *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  27.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  WINDOW-ANZEIGE ZUR TAG-SELEKTION             *
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
     ‚* BILDSCHIRMDATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-HMD0005DE
                  ORGANIZATION IS  TRANSACTION
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* TAGS
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DP
                  ASSIGN       TO  DATABASE-PTAGLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* TAG KOPFSÄTZE
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL3
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY
            LABEL RECORDS ARE OMITTED.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF HMD0005DE.
      /
     ‚*--- TAGS
       FD  PTAGLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-P.
           COPY DDS-ALL-FORMATS OF PTAGLIB.
      /
     ‚*--- TAG KOPFSÄTZE
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL3.
      /
       WORKING-STORAGE SECTION.

     ‚* LISTE DER TEMPORÄREN INDIKATOREN
       01  INXX.
         02 IN01                          PIC 1.
         02 IN02                          PIC 1.
         02 IN41                          PIC 1.
         02 IN42                          PIC 1.
         02 IN91                          PIC 1.
         02 IN95                          PIC 1.
         02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  HMD0005DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  HMD0005DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  HMD0005DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  HMD0005DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HMP0005".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

       01  AUSWAHL                       PIC X(2).
       01  ANZEIGE-TYP                   PIC 9(1).
       01  TAG-SELEKTION                 LIKE PTTAG  OF PTAGLIB-P.
       01  LEN                           PIC 9(4).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-PTTAG                       LIKE PTTAG  OF PTAGLIB-P.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-PTTAG
                                         X-F12.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP0005" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "HMD0005DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PTAGLIB-DP.
           OPEN     INPUT PTAGLIB-DL.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE X-PTTAG.
           INITIALIZE F3, F12.
           MOVE     ZEROES TO ANZEIGE-TYP.
           MOVE     SPACES TO TAG-SELEKTION.
           MOVE     ZEROES TO LEN.

       ANF010.
           MOVE     AUS TO INXX.
           MOVE     1 TO SFL-ZEILE.

           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
           ELSE
                    PERFORM ANZEIGE-SUBFILE
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

     ‚* SOLANGE WIEDERHOLEN BIS AUSWAHL ERFOLGT
           IF       X-PTTAG = SPACES
                    GO TO ANF010
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PTAGLIB-DP
                                 PTAGLIB-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     SPACES              TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* NUR AKTIVE ANZEIGEN?
           IF       ANZEIGE-TYP = ZEROES
            AND     PTFK01 OF PTAGLIB-L NOT = ZEROES
                    GO TO LES-DAT-20
           END-IF.

     ‚* ANZEIGE BESTIMMTER TAGS?
           IF       LEN                        > ZEROES
            AND     TAG-SELEKTION(1:1)     NOT = "*"
                    IF   TAG-SELEKTION(1:LEN) NOT =
                         PTTAG  OF PTAGLIB-L(1:LEN)
                         GO TO LES-DAT-20
                    END-IF
           END-IF.

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
     ‚* INAKTIVE TAGS PINK ANZEIGEN
           IF       PTFK01 OF PTAGLIB-L NOT = ZEROES
                    MOVE AN  TO IN42 OF INXX
           ELSE
                    MOVE AUS TO IN42 OF INXX
           END-IF.
           MOVE     CORR PTAGLIBF OF PTAGLIB-L TO FORMAT2.
           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
     ‚* SELEKTION ANZEIGEN
           IF       LEN > ZEROES
                    MOVE TAG-SELEKTION TO PTTAG OF FORMAT3
           END-IF.

           MOVE     ZEROES TO LEN.
           MOVE     AUS    TO INXX.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
           IF       SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                    MOVE SFL-ZEILE TO  SUBFILE-RECORD-NUMBER
           END-IF.

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

     ‚* BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
           MOVE     9999 TO SFL-ZEILE.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F13 ALLE ANZEIGEN
           IF       IN13 IN FORMAT3-I-INDIC = AN
                    EVALUATE ANZEIGE-TYP
                             WHEN ZEROES
                                  MOVE 1      TO ANZEIGE-TYP
                             WHEN 1
                                  MOVE ZEROES TO ANZEIGE-TYP
                    END-EVALUATE
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* ANLEGEN EINES NEUEN TAGS?
           PERFORM  ANLEGEN-TAG.
           IF       ERRFLD-WRK NOT = SPACES
     ‚*             BESCHREIBUNG FEHLT
                    MOVE     "ERR0001"         TO CPY-TXID
                    MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    PERFORM  COPY-PGM-TXT
                    MOVE     CPY-TXTEXT        TO ERRMSG   OF FORMAT3
                    MOVE     COL-GRN-RI        TO A_PTTEXT OF FORMAT3
                    MOVE     AN TO IN01 OF INXX
                    MOVE     AN TO IN02 OF INXX
     ‚*             CURSER SETZEN
                    MOVE     AN TO IN41 OF INXX
                    GO TO ANZ-SFL-20
           END-IF.

     ‚* WURDE EINE LÄNGE ERMITTELT IST EINE SELEKTION MIT "*"
     ‚* GEWÜNSCHT
     ‚*€   IF       LEN > ZEROES
     ‚*€            GO TO ANZ-SFL-90
     ‚*€   END-IF.

     ‚* ÜBERNEHMEN DER EINGABE
           IF       PTTAG  OF FORMAT3 NOT = SPACES
            AND     LEN                   = ZEROES
                    MOVE PTTAG       OF FORMAT3 TO X-PTTAG
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

     ‚* AUSWÄHLEN DES TAGS
           IF       AUSW OF FORMAT2 = "1"
                    MOVE PTTAG  OF FORMAT2 TO X-PTTAG
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* AUSWÄHLEN DES TAGS
           IF       AUSW OF FORMAT2 = "6"
                    PERFORM SWITCH-AKTIVITAET
           END-IF.

     ‚* LÖSCHEN EINES TAGS
           IF       AUSW OF FORMAT2 = "4"
                    INITIALIZE RET-CODE
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2
           MOVE     CORR FORMAT2 TO FMT02-O

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
     ‚* SELEKTION ANZEIGEN
           IF       LEN > ZEROES
                    MOVE TAG-SELEKTION TO PTTAG OF FORMAT3
           END-IF.

           INITIALIZE LEN.
     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
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

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

     ‚* F13 ALLE ANZEIGEN
           IF       IN13 IN FORMAT3-I-INDIC = AN
                    EVALUATE ANZEIGE-TYP
                             WHEN ZEROES
                                  MOVE 1      TO ANZEIGE-TYP
                             WHEN 1
                                  MOVE ZEROES TO ANZEIGE-TYP
                    END-EVALUATE
                    GO TO FEH-MEL-90
           END-IF.

     ‚* ANLEGEN EINES NEUEN TAGS?
           PERFORM  ANLEGEN-TAG.
           IF       ERRFLD-WRK NOT = SPACES
                    GO TO FEH-MEL-00
           END-IF.
           IF       LEN            > ZEROES
                    GO TO FEH-MEL-90
           END-IF.

           MOVE     PTTAG  OF FORMAT3 TO X-PTTAG.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WECHSELN DER AKTIVITÄT (AKTIV/INAKTIV)
     ‚*--------------------------------------------------------------
       SWITCH-AKTIVITAET SECTION.
       SWI-AKT-00.

           INITIALIZE PTAGLIBF OF PTAGLIB-P.
           MOVE     PTLFDN OF FORMAT2   TO PTLFDN OF PTAGLIB-P.
           READ     PTAGLIB-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO SWI-AKT-90
           END-IF.

           EVALUATE PTFK01 OF PTAGLIB-P
                    WHEN ZEROES
                         MOVE 1      TO PTFK01 OF PTAGLIB-P
                    WHEN 1
                         MOVE ZEROES TO PTFK01 OF PTAGLIB-P
           END-EVALUATE.

           REWRITE  PTAGLIB-P.

       SWI-AKT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN EINES NEUEN TAGS
     ‚*--------------------------------------------------------------
       ANLEGEN-TAG SECTION.
       ANL-TAG-00.

           INITIALIZE ERRFLD-WRK.
           MOVE     ZEROES TO LEN.
     ‚* WENN KEIN TAG EINGEGEBEN WURDE, DANN ROUTINE BEENDEN
           IF       PTTAG  OF FORMAT3 = SPACES
                    GO TO ANL-TAG-90
           END-IF.

     ‚* GIBT ES DEN TAG SCHON? DANN DIESEN ZURÜCKGEBEN
           PERFORM  CHECK-TAG.
           IF       X-PTTAG NOT = SPACES
                    GO TO ANL-TAG-90
           END-IF.

     ‚* BEINHALTET DER TAG EINEN STERN, DANN IST EINE TEILAUFLISTUNG
     ‚* GEWÜNSCHT
           IF       PTTAG  OF FORMAT3      NOT = SPACES
             AND    PTTAG  OF FORMAT3(1:1) NOT = "*"
                    MOVE    ZEROES TO LEN
                    INSPECT PTTAG  OF FORMAT3 TALLYING LEN FOR
                            CHARACTERS BEFORE INITIAL "*"
     ‚*             STERN GEFUNDEN?
                    IF   LEN     > ZEROES
                     AND LEN NOT = LENGTH OF PTTAG  OF FORMAT3
                         MOVE PTTAG  OF FORMAT3 TO TAG-SELEKTION
                         GO TO ANL-TAG-90
                    ELSE
                         INITIALIZE LEN
                    END-IF
           END-IF.

     ‚* TAG OHNE BESCHREIBUNG? NICHT ZULASSEN
           IF       PTTEXT OF FORMAT3 = SPACES
                    MOVE "XXX" TO ERRFLD-WRK
                    GO TO ANL-TAG-90
           END-IF.

     ‚* TAG SCHREIBEN - OHNE TAG
           INITIALIZE PTAGLIBF OF PTAGLIB-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PTAGLIB" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK          TO PTLFDN OF PTAGLIB-P.

     ‚* DATEN FÜLLEN
           MOVE     SPACES            TO PTLIB  OF PTAGLIB-P.
           MOVE     SPACES            TO PTFILE OF PTAGLIB-P.
           MOVE     PTTAG  OF FORMAT3 TO PTTAG  OF PTAGLIB-P.
           MOVE     PTTEXT OF FORMAT3 TO PTTEXT OF PTAGLIB-P.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO PTAUSR OF PTAGLIB-P.
           MOVE     T-TERM            TO PTABS  OF PTAGLIB-P.
           MOVE     PGM-WRK           TO PTAPGM OF PTAGLIB-P.

           WRITE    PTAGLIB-P.

       ANL-TAG-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "HMP0005*3"       TO MAINUEB OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN TAG SCHON EXISTIERT
     ‚*--------------------------------------------------------------
       CHECK-TAG SECTION.
       CHK-TAG-00.

     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     PTTAG  OF FORMAT3   TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TAG-90
           END-IF.

       CHK-TAG-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TAG-90
           END-IF.

           IF       PTTAG  OF PTAGLIB-L NOT = PTTAG  OF FORMAT3
                    GO TO CHK-TAG-90
           END-IF.

           MOVE     PTTAG  OF PTAGLIB-L TO X-PTTAG.

       CHK-TAG-90.
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
