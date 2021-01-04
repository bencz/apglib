       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP0006.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* WARTUNG PROGRAMMTEXTE                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  WARTUNG DER TEXTE FÜR EIN PROGRAMM           *
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
                  ASSIGN       TO  WORKSTATION-HMD0006DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TEXTE
     ‚*-------------------------------------------------------------
           SELECT PPGMLST-DP
                  ASSIGN       TO  DATABASE-PPGMLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PPGMLIB-DP
                  ASSIGN       TO  DATABASE-PPGMLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY   DDS-ALL-FORMATS   OF HMD0006DE.
      /
     ‚*--- TEXTE
       FD  PPGMLST-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLST-P.
           COPY DDS-ALL-FORMATS OF PPGMLST.
      /
     ‚*--- PROGRAMME
       FD  PPGMLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLIB-P.
           COPY DDS-ALL-FORMATS OF PPGMLIB.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        02 IN50                          PIC 1.
        02 IN70                          PIC 1.
        02 IN91                          PIC 1.
        02 IN95                          PIC 1.
        02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  HMD0006DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  HMD0006DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  HMD0006DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  HMD0006DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HMP0006".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

       01  AUSWAHL                       PIC X(2).

       01  LEER-ZEILE                    PIC X(1).

       01  POS-NR                        LIKE PSLFPO OF PPGMLST-P.
       01  TXT-SAVE                      LIKE PSTEXT OF PPGMLST-P.

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-PSLIB                       LIKE PSLIB  OF PPGMLST-P.
       01  X-PSFILE                      LIKE PSFILE OF PPGMLST-P.
       01  X-AUSWAHL                     PIC X(2).
       01  X-PGM                         LIKE PGM-WRK.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-PSLIB
                                         X-PSFILE
                                         X-AUSWAHL
                                         X-PGM
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP0006" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.
           OPEN     I-O   PPGMLST-DP.
           OPEN     INPUT PPGMLIB-DP.

           INITIALIZE X-F3.
           INITIALIZE X-F12.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PARAMETER RICHTIG ÜBERGEBEN?
           IF       X-PSLIB  = SPACES
            OR      X-PSFILE = SPACES
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
           CLOSE                 WS-DISPLAY
                                 PPGMLIB-DP
                                 PPGMLST-DP.

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
           INITIALIZE PPGMLSTF OF PPGMLST-P.
           MOVE     X-PSLIB           TO PSLIB  OF PPGMLST-P.
           MOVE     X-PSFILE          TO PSFILE OF PPGMLST-P.
           MOVE     ZEROES            TO PSLFPO OF PPGMLST-P.
           START    PPGMLST-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PPGMLST-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHLÜSSEL OK?
           IF       PSLIB  OF PPGMLST-P NOT = X-PSLIB
            OR      PSFILE OF PPGMLST-P NOT = X-PSFILE
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
                    MOVE    CORR INXX  TO FMT03-O-INDIC
                    WRITE   SUBFILE WS-REC FORMAT IS "FMT03"
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
           MOVE     CORR PPGMLSTF OF PPGMLST-P TO FORMAT2.
           MOVE     LEER-ZEILE    TO LEERZ OF FORMAT2.
           MOVE     CORR INXX     TO FMT02-O-INDIC.
           MOVE     CORR FORMAT2  TO FMT02-O.
           ADD      1             TO ANZREC-WRK.
           ADD      1             TO SUBFILE-RECORD-NUMBER.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT02".
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
           WRITE    WS-REC FORMAT IS "FMT04".
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

           INITIALIZE PPGMLIBF OF PPGMLIB-P.
           MOVE     X-PSLIB           TO PLLIB  OF PPGMLIB-P.
           MOVE     X-PSFILE          TO PLFILE OF PPGMLIB-P.
           READ     PPGMLIB-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE CORR PPGMLIBF OF PPGMLIB-P TO FORMAT3
           ELSE
                    MOVE     X-PSLIB      TO PLLIB  OF FORMAT3
                    MOVE     X-PSFILE     TO PLFILE OF FORMAT3
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
           MOVE     CORR   INXX     TO  FMT03-O-INDIC.
           MOVE     CORR   FORMAT3  TO  FMT03-O.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT03".
           MOVE     1      TO  SUBFILE-RECORD-NUMBER.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           READ     WS-DISPLAY  FORMAT IS "FMT03".
           MOVE     AUS  TO INXX

     ‚* BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
           MOVE     9999 TO SFL-ZEILE.

     ‚* F3 ENDE
           IF       IN03 IN FMT03-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FMT03-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FMT03-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           READ     SUBFILE WS-DISPLAY NEXT MODIFIED RECORD
                    FORMAT IS "FMT02".
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

           MOVE     CORR FMT02-I TO FORMAT2.

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
                         MOVE     CORR FORMAT2 TO FMT02-O
                         REWRITE  SUBFILE WS-REC FORMAT IS "FMT02"
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
                    MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                    REWRITE  SUBFILE WS-REC FORMAT IS "FMT02"
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* DIE LETZTEN LEERZEILEN WERDEN BEI NICHTBEBRAUCH ÜBERLESEN
           IF       PSTEXT OF FORMAT2     = SPACES
            AND     LEERZ  OF FORMAT2 NOT = SPACES
                    MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                    REWRITE  SUBFILE WS-REC FORMAT IS "FMT02"
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* ANLEGEN NEUEN TEXT
           PERFORM  ANLEGEN-TEXT.

           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           REWRITE  SUBFILE WS-REC FORMAT IS "FMT02".

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*----------------------------------------------------------------
     ‚* LÖSCHEN ZUSATZTEXTE
     ‚*----------------------------------------------------------------
       LOESCHEN-TEXTE SECTION.
       LOE-SCH-00.

           INITIALIZE PPGMLSTF OF PPGMLST-P.
           MOVE     PSLIB  OF FORMAT2   TO PSLIB  OF PPGMLST-P.
           MOVE     PSFILE OF FORMAT2   TO PSFILE OF PPGMLST-P.
           MOVE     PSLFPO OF FORMAT2   TO PSLFPO OF PPGMLST-P.
           READ     PPGMLST-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PPGMLST-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PPGMLST"   TO FILE-WRK
                    MOVE PPGMLST-P   TO RECORD-WRK
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
                    INITIALIZE PPGMLSTF OF PPGMLST-P
                    MOVE X-PSLIB           TO PSLIB  OF PPGMLST-P
                    MOVE X-PSFILE          TO PSFILE OF PPGMLST-P
                    MOVE POS-NR            TO PSLFPO OF PPGMLST-P
                    READ PPGMLST-DP
                    IF   FILE-STATUS = ZEROES
                         IF   POS-NR NOT = PSLFPO OF FORMAT2
                              DELETE PPGMLST-DP
                              ADD   1 TO  PSLFPO OF PPGMLST-P
                              WRITE PPGMLST-P
                         ELSE
                              MOVE  PSTEXT OF PPGMLST-P TO TXT-SAVE
                              MOVE  SPACES TO PSTEXT OF PPGMLST-P
                              REWRITE PPGMLST-P
                              MOVE  TXT-SAVE TO PSTEXT OF PPGMLST-P
                              ADD   1 TO  PSLFPO OF PPGMLST-P
                              WRITE PPGMLST-P
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

           INITIALIZE PPGMLSTF OF PPGMLST-P.
           MOVE     X-PSLIB             TO PSLIB  OF PPGMLST-P.
           MOVE     X-PSFILE            TO PSFILE OF PPGMLST-P.
           MOVE     PSLFPO OF FORMAT2   TO PSLFPO OF PPGMLST-P.
           START    PPGMLST-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ERM-POS-90
           END-IF.

       ERM-POS-20.
           READ     PPGMLST-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ERM-POS-90
           END-IF.

           IF       PSLIB  OF PPGMLST-P = X-PSLIB
            AND     PSFILE OF PPGMLST-P = X-PSFILE
                    MOVE  PSLFPO OF PPGMLST-P TO POS-NR
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

           INITIALIZE PPGMLSTF OF PPGMLST-P.
           MOVE     X-PSLIB      TO PSLIB  OF PPGMLST-P.
           MOVE     X-PSFILE     TO PSFILE OF PPGMLST-P.
           MOVE     SPACES       TO PSTEXT OF PPGMLST-P.

     ‚* BEIM ERSTEN SFL-AUFBAU WIR DER CURSOR AUF DER ERSTEN
     ‚* DER DREI LEERZEILEN POSITIONIERT
           IF       SFL-ZEILE = 1
                    MOVE AN  TO IN70 OF INXX
           ELSE
                    MOVE AUS TO IN70 OF INXX
           END-IF.

           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO PSLFPO OF PPGMLST-P.
           PERFORM  AUSGABE-SUBFILE.

           MOVE     AUS TO IN70 IN INXX.
           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO PSLFPO OF PPGMLST-P.
           PERFORM  AUSGABE-SUBFILE.

           MOVE     AUS TO IN70 IN INXX.
           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO PSLFPO OF PPGMLST-P.
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

           INITIALIZE PPGMLSTF OF PPGMLST-P.
           MOVE     PSLIB  OF FORMAT2   TO PSLIB  OF PPGMLST-P.
           MOVE     PSFILE OF FORMAT2   TO PSFILE OF PPGMLST-P.
           MOVE     PSLFPO OF FORMAT2   TO PSLFPO OF PPGMLST-P.
           READ     PPGMLST-DP.
           IF       FILE-STATUS = ZEROES
                    IF   PSTEXT OF FORMAT2 NOT = PSTEXT OF PPGMLST-P
                         MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                         MOVE PSTEXT OF FORMAT2 TO PSTEXT OF PPGMLST-P
                         MOVE T-USER            TO PSMSER OF PPGMLST-P
                         MOVE T-TERM            TO PSMBS  OF PPGMLST-P
                         MOVE PGM-WRK           TO PSMPGM OF PPGMLST-P
                    END-IF
                    REWRITE PPGMLST-P
           ELSE
     ‚*             NEUEN DATENSATZ SCHREIBEN
                    MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                    INITIALIZE PPGMLSTF OF PPGMLST-P
                    MOVE PSLIB  OF FORMAT2      TO PSLIB  OF PPGMLST-P
                    MOVE PSFILE OF FORMAT2      TO PSFILE OF PPGMLST-P
                    MOVE PSLFPO OF FORMAT2      TO PSLFPO OF PPGMLST-P
                    MOVE PSTEXT OF FORMAT2      TO PSTEXT OF PPGMLST-P
                    MOVE T-USER                 TO PSAUSR OF PPGMLST-P
                    MOVE T-TERM                 TO PSABS  OF PPGMLST-P
                    MOVE PGM-WRK                TO PSAPGM OF PPGMLST-P
                    WRITE PPGMLST-P
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
           MOVE     "HMP0000"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT3.

     ‚* ZWEITE ÜBERSCHRIFT
           MOVE     "HMP0006*3"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT3.

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
