       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              AJP0006.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* WARTUNG TEXTE                                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  04.06.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  WARTUNG DER TEXTE FÜR EIN THEMA              *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                         ATTRIBUTE-DATA IS BS-ATRIBUTE,
                         I-O-FEEDBACK   IS FILE-IO-FEEDBACK.

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRMDATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-AJD0006DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TEXTE
     ‚*-------------------------------------------------------------
           SELECT PAJXTXT-DP
                  ASSIGN       TO  DATABASE-PAJXTXT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PAJXTHM-DP
                  ASSIGN       TO  DATABASE-PAJXTHM
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY   DDS-ALL-FORMATS   OF AJD0006DE.
      /
     ‚*--- TEXTE
       FD  PAJXTXT-DP
           LABEL RECORDS ARE STANDARD.
       01  PAJXTXT-P.
           COPY DDS-ALL-FORMATS OF PAJXTXT.
      /
     ‚*--- THEMEN
       FD  PAJXTHM-DP
           LABEL RECORDS ARE STANDARD.
       01  PAJXTHM-P.
           COPY DDS-ALL-FORMATS OF PAJXTHM.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        02 IN50                          PIC 1.
        02 IN70                          PIC 1.
        02 IN91                          PIC 1.
        02 IN95                          PIC 1.
        02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  AJD0006DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  AJD0006DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  AJD0006DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  AJD0006DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "AJP0006".
       01  PGM-TYP                       PIC X(8)  VALUE "AJAX".

       01  AUSWAHL                       PIC X(2).

       01  LEER-ZEILE                    PIC X(1).

       01  POS-NR                        LIKE PSLFPO OF PAJXTXT-P.
       01  TXT-SAVE                      LIKE PSTEXT OF PAJXTXT-P.

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-PSID                        LIKE PSID   OF PAJXTXT-P.
       01  X-PSKEY                       LIKE PSKEY  OF PAJXTXT-P.
       01  X-AUSWAHL                     PIC X(2).
       01  X-PGM                         LIKE PGM-WRK.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-PSID
                                         X-PSKEY
                                         X-AUSWAHL
                                         X-PGM
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AJP0006" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "AJD0006DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PAJXTXT-DP.
           OPEN     INPUT PAJXTHM-DP.

           INITIALIZE X-F3.
           INITIALIZE X-F12.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PARAMETER RICHTIG ÜBERGEBEN?
           IF       X-PSID  = SPACES
            OR      X-PSKEY = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     AUS     TO INXX.
           MOVE     1 TO SFL-ZEILE.

       ANF020.
           MOVE     SPACES TO F3 F12.
           INITIALIZE FORMAT2.
           INITIALIZE FORMAT3.

     ‚* LESEN ALLER BISHERIGEN TEXTE
           PERFORM  LESE-DATEN.
     ‚* SCHREIBEN 4 EINGABEFÄHIGE LEERZEILEN IN SFL
           PERFORM  AUSGABE-LEERZEILEN.

     ‚* FÜLLEN SFL-KOPF
           INITIALIZE FORMAT3.

           PERFORM  ANZEIGE-SUBFILE.
           IF       F3 = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PAJXTHM-DP
                                 PAJXTXT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*----------------------------------------------------------------
     ‚* LESEN AUFTRAGSTEXTE
     ‚*----------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
           MOVE     AUS TO INXX.
     ‚* LESEN ALLER VORHANDENEN TEXTE
           INITIALIZE PAJXTXTF OF PAJXTXT-P.
           MOVE     X-PSID            TO PSID   OF PAJXTXT-P.
           MOVE     X-PSKEY           TO PSKEY  OF PAJXTXT-P.
           MOVE     ZEROES            TO PSLFPO OF PAJXTXT-P.
           START    PAJXTXT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PAJXTXT-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHLÜSSEL OK?
           IF       PSID   OF PAJXTXT-P NOT = X-PSID
            OR      PSKEY  OF PAJXTXT-P NOT = X-PSKEY
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHREIBEN DATENSATZ IN SUBFILE
           MOVE     AUS TO INXX.
           INITIALIZE LEER-ZEILE.
           PERFORM  AUSGABE-SUBFILE.

           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*----------------------------------------------------------------
     ‚* AUSGABE DER SFL-ZEILE
     ‚*----------------------------------------------------------------
       AUSGABE-SUBFILE SECTION.
       AUS-SFL-00.

     ‚* CLEAR SUBFILE KOPFSATZ
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    ZEROES  TO ANZREC-WRK
                    MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* FUELLEN  SUBFILE-EINZELSATZ
           IF       X-AUSWAHL NOT = "5"
                    MOVE AUS TO IN50 IN INXX
           ELSE
                    MOVE AN  TO IN50 IN INXX
           END-IF.

     ‚* CURSOR AUF NÄCHSTE ZEILE POSITIONIEREN, WENN EINE NEUE
     ‚* ZEILE GESCHRIEBEN WURDE
           IF       SFL-ZEILE > 1
            AND     SFL-ZEILE = SUBFILE-RECORD-NUMBER
                    MOVE AN  TO IN70 OF INXX
           END-IF.

     ‚*  SCHREIBEN SUBFILE-EINZELSATZ
           INITIALIZE FORMAT2.
           MOVE     CORR PAJXTXTF OF PAJXTXT-P TO FORMAT2.
           MOVE     LEER-ZEILE    TO LEERZ OF FORMAT2.
           MOVE     CORR INXX     TO FORMAT2-O-INDIC.
           ADD      1             TO ANZREC-WRK.
           ADD      1             TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"       TO FMT-WRK.
           MOVE     FORMAT2       TO WSREC-WRK.
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

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

           INITIALIZE PAJXTHMF OF PAJXTHM-P.
           MOVE     X-PSID           TO AXID   OF PAJXTHM-P.
           MOVE     X-PSKEY          TO AXKEY  OF PAJXTHM-P.
           READ     PAJXTHM-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE CORR PAJXTHMF OF PAJXTHM-P TO FORMAT3
           ELSE
                    MOVE     X-PSID      TO AXID   OF FORMAT3
                    MOVE     X-PSKEY     TO AXKEY  OF FORMAT3
           END-IF.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
           PERFORM  SFL-HEADER.

           IF       X-AUSWAHL NOT = "5"
                    MOVE AUS TO IN50 IN INXX
           ELSE
                    MOVE AN  TO IN50 IN INXX
           END-IF.

     ‚* ANZEIGE SUBFILE
           IF       SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                    MOVE SFL-ZEILE TO  SUBFILE-RECORD-NUMBER
           END-IF.
           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AN     TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.

     ‚* BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
           MOVE     9999 TO SFL-ZEILE.

     ‚* F3 ENDE
           IF       IN03 OF FORMAT3-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT3-I-INDIC = AN
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

     ‚* DARF ETWAS GEÄNDERT WERDEN?
           IF       X-AUSWAHL = "5"
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

     ‚* BEI 1, ZEILE EINFÜGEN - JEDOCH NICHT BEI DEN LETZTEN ZEILEN
           IF       AUSW  OF FORMAT2      = "1"
            AND     LEERZ OF FORMAT2      = SPACES
                    MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                    IF   SFL-ZEILE > 1
                         SUBTRACT 1 FROM SFL-ZEILE
                    END-IF
     ‚*             EINFÜGEN LEERZEILE
                    PERFORM EINFUEGEN-ZEILE
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* BEI 4, ZEILE LÖSCHEN
           IF       AUSW  OF FORMAT2      = "4"
            AND     LEERZ OF FORMAT2      = SPACES
                    MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
     ‚*             LÖSCHEN AUFTRAGSTEXTE
                    PERFORM LOESCHEN-TEXTE
                    MOVE     "FMT02" TO FMT-WRK
                    MOVE     FORMAT2 TO WSREC-WRK
                    PERFORM  COPY-DISPLAY-SFLREWRITE
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* DIE LETZTEN LEERZEILEN WERDEN BEI NICHTBEBRAUCH ÜBERLESEN
           IF       PSTEXT OF FORMAT2     = SPACES
            AND     LEERZ  OF FORMAT2 NOT = SPACES
                    MOVE     "FMT02" TO FMT-WRK
                    MOVE     FORMAT2 TO WSREC-WRK
                    PERFORM  COPY-DISPLAY-SFLREWRITE
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* ANLEGEN NEUEN TEXT
           PERFORM  ANLEGEN-TEXT.

           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*----------------------------------------------------------------
     ‚* LÖSCHEN ZUSATZTEXTE
     ‚*----------------------------------------------------------------
       LOESCHEN-TEXTE SECTION.
       LOE-SCH-00.

           INITIALIZE PAJXTXTF OF PAJXTXT-P.
           MOVE     PSID   OF FORMAT2   TO PSID  OF PAJXTXT-P.
           MOVE     PSKEY  OF FORMAT2   TO PSKEY OF PAJXTXT-P.
           MOVE     PSLFPO OF FORMAT2   TO PSLFPO OF PAJXTXT-P.
           READ     PAJXTXT-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PAJXTXT-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PAJXTXT"   TO FILE-WRK
                    MOVE PAJXTXT-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       LOE-SCH-90.
           EXIT.
      /
     ‚*----------------------------------------------------------------
     ‚* EINFÜGEN EINER LEERZEILEN
     ‚*----------------------------------------------------------------
       EINFUEGEN-ZEILE SECTION.
       EIN-ZLE-00.

     ‚* ZUNÄCHST DIE LETZTE POSITION DIESES TEXTES LESEN
           PERFORM  ERMITTELN-POSITION.

     ‚* VON DER LETZTEN POSITION BIS ZUR AKTUELLEN POSITION JEWEILS
     ‚* UM EINE POSITION ERHÖHEN
           PERFORM  WITH TEST BEFORE UNTIL POS-NR < PSLFPO OF FORMAT2
                    INITIALIZE PAJXTXTF OF PAJXTXT-P
                    MOVE X-PSID            TO PSID  OF PAJXTXT-P
                    MOVE X-PSKEY           TO PSKEY OF PAJXTXT-P
                    MOVE POS-NR            TO PSLFPO OF PAJXTXT-P
                    READ PAJXTXT-DP
                    IF   FILE-STATUS = ZEROES
                         IF   POS-NR NOT = PSLFPO OF FORMAT2
                              DELETE PAJXTXT-DP
                              ADD   1 TO  PSLFPO OF PAJXTXT-P
                              WRITE PAJXTXT-P
                         ELSE
                              MOVE  PSTEXT OF PAJXTXT-P TO TXT-SAVE
                              MOVE  SPACES TO PSTEXT OF PAJXTXT-P
                              REWRITE PAJXTXT-P
                              MOVE  TXT-SAVE TO PSTEXT OF PAJXTXT-P
                              ADD   1 TO  PSLFPO OF PAJXTXT-P
                              WRITE PAJXTXT-P
                         END-IF
                    END-IF
                    SUBTRACT 1 FROM POS-NR
           END-PERFORM.

       EIN-ZLE-90.
           EXIT.
      /
     ‚*----------------------------------------------------------------
     ‚* ERMITTELN DER GRÖßTEN POS-NR ZU DIESEM PROGRAMM
     ‚*----------------------------------------------------------------
       ERMITTELN-POSITION SECTION.
       ERM-POS-00.

           MOVE     PSLFPO OF FORMAT2  TO POS-NR.

           INITIALIZE PAJXTXTF OF PAJXTXT-P.
           MOVE     X-PSID              TO PSID  OF PAJXTXT-P.
           MOVE     X-PSKEY             TO PSKEY OF PAJXTXT-P.
           MOVE     PSLFPO OF FORMAT2   TO PSLFPO OF PAJXTXT-P.
           START    PAJXTXT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ERM-POS-90
           END-IF.

       ERM-POS-20.
           READ     PAJXTXT-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ERM-POS-90
           END-IF.

           IF       PSID  OF PAJXTXT-P = X-PSID
            AND     PSKEY OF PAJXTXT-P = X-PSKEY
                    MOVE  PSLFPO OF PAJXTXT-P TO POS-NR
                    GO TO ERM-POS-20
           END-IF.

       ERM-POS-90.
           EXIT.
      /
     ‚*----------------------------------------------------------------
     ‚* AUSGEBEN LEERZEILEN EINFÜGEN
     ‚*----------------------------------------------------------------
       AUSGABE-LEERZEILEN SECTION.
       AUS-LEE-00.

           MOVE     AUS TO INXX.

     ‚* ZUNÄCHST DIE LETZTE POSITION DIESES TEXTS LESEN
           PERFORM  ERMITTELN-POSITION.

     ‚* LERRZEILEN WERDEN MARKIERT, DAMIT DIESE SPÄTER WIEDER
     ‚* ENTFERNT WERDEN KÖNNEN.
           MOVE     "L" TO  LEER-ZEILE.

           INITIALIZE PAJXTXTF OF PAJXTXT-P.
           MOVE     X-PSID       TO PSID  OF PAJXTXT-P.
           MOVE     X-PSKEY      TO PSKEY OF PAJXTXT-P.
           MOVE     SPACES       TO PSTEXT OF PAJXTXT-P.

     ‚* BEIM ERSTEN SFL-AUFBAU WIR DER CURSOR AUF DER ERSTEN
     ‚* DER DREI LEERZEILEN POSITIONIERT
           IF       SFL-ZEILE = 1
                    MOVE AN  TO IN70 OF INXX
           ELSE
                    MOVE AUS TO IN70 OF INXX
           END-IF.

           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO PSLFPO OF PAJXTXT-P.
           PERFORM  AUSGABE-SUBFILE.

           MOVE     AUS TO IN70 IN INXX.
           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO PSLFPO OF PAJXTXT-P.
           PERFORM  AUSGABE-SUBFILE.

           MOVE     AUS TO IN70 IN INXX.
           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO PSLFPO OF PAJXTXT-P.
           PERFORM  AUSGABE-SUBFILE.
           MOVE     AUS TO INXX.

       AUS-LEE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN/ÄNDERN EINES TEXTES
     ‚*--------------------------------------------------------------
       ANLEGEN-TEXT SECTION.
       ANL-TXT-00.

           INITIALIZE PAJXTXTF OF PAJXTXT-P.
           MOVE     PSID   OF FORMAT2   TO PSID  OF PAJXTXT-P.
           MOVE     PSKEY  OF FORMAT2   TO PSKEY OF PAJXTXT-P.
           MOVE     PSLFPO OF FORMAT2   TO PSLFPO OF PAJXTXT-P.
           READ     PAJXTXT-DP.
           IF       FILE-STATUS = ZEROES
                    IF   PSTEXT OF FORMAT2 NOT = PSTEXT OF PAJXTXT-P
                         MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                         MOVE PSTEXT OF FORMAT2 TO PSTEXT OF PAJXTXT-P
                         MOVE T-USER            TO PSMSER OF PAJXTXT-P
                         MOVE T-TERM            TO PSMBS  OF PAJXTXT-P
                         MOVE PGM-WRK           TO PSMPGM OF PAJXTXT-P
                    END-IF
                    REWRITE PAJXTXT-P
           ELSE
     ‚*             NEUEN DATENSATZ SCHREIBEN
                    MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                    INITIALIZE PAJXTXTF OF PAJXTXT-P
                    MOVE PSID  OF FORMAT2       TO PSID  OF PAJXTXT-P
                    MOVE PSKEY OF FORMAT2       TO PSKEY OF PAJXTXT-P
                    MOVE PSLFPO OF FORMAT2      TO PSLFPO OF PAJXTXT-P
                    MOVE PSTEXT OF FORMAT2      TO PSTEXT OF PAJXTXT-P
                    MOVE T-USER                 TO PSAUSR OF PAJXTXT-P
                    MOVE T-TERM                 TO PSABS  OF PAJXTXT-P
                    MOVE PGM-WRK                TO PSAPGM OF PAJXTXT-P
                    WRITE PAJXTXT-P
           END-IF.

       ANL-TXT-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "AJP0000"         TO MAINUEB OF FORMAT3.
           MOVE     "AJP0006*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
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
