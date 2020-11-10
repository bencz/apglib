       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0023.
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
                  ASSIGN       TO  WORKSTATION-CFD0023DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TEXTE
     ‚*-------------------------------------------------------------
           SELECT PTXTFIT-DP
                  ASSIGN       TO  DATABASE-PTXTFIT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY   DDS-ALL-FORMATS   OF CFD0023DE.
      /
     ‚*--- TEXTE
       FD  PTXTFIT-DP
           LABEL RECORDS ARE STANDARD.
       01  PTXTFIT-P.
           COPY DDS-ALL-FORMATS OF PTXTFIT.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        02 IN50                          PIC 1.
        02 IN70                          PIC 1.
        02 IN91                          PIC 1.
        02 IN95                          PIC 1.
        02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  CFD0023DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CFD0023DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CFD0023DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CFD0023DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0023".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  AUSWAHL                       PIC X(2).

       01  LEER-ZEILE                    PIC X(1).

       01  POS-NR                        LIKE FTLFPO OF PTXTFIT-P.
       01  TXT-SAVE                      LIKE FTTEXT OF PTXTFIT-P.

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-FTFILE                      LIKE FTFILE OF PTXTFIT-P.
       01  X-FTKEY                       LIKE FTKEY  OF PTXTFIT-P.
       01  X-AUSWAHL                     PIC X(2).
       01  X-PGM                         LIKE PGM-WRK.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-FTFILE
                                         X-FTKEY
                                         X-AUSWAHL
                                         X-PGM
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0023" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0023DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PTXTFIT-DP.

           INITIALIZE X-F3.
           INITIALIZE X-F12.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PARAMETER RICHTIG ÜBERGEBEN?
           IF       X-FTFILE = SPACES
            OR      X-FTKEY  = SPACES
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

           CLOSE                 PTXTFIT-DP.

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
           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     X-FTFILE          TO FTFILE OF PTXTFIT-P.
           MOVE     X-FTKEY           TO FTKEY  OF PTXTFIT-P.
           MOVE     ZEROES            TO FTLFPO OF PTXTFIT-P.
           START    PTXTFIT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PTXTFIT-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHLÜSSEL OK?
           IF       FTFILE OF PTXTFIT-P NOT = X-FTFILE
            OR      FTKEY  OF PTXTFIT-P NOT = X-FTKEY
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
           MOVE     CORR PTXTFITF OF PTXTFIT-P TO FORMAT2.
           MOVE     LEER-ZEILE   TO LEERZ OF FORMAT2.

           IF       FTMDTA OF PTXTFIT-P NOT = ZEROES
                    MOVE FTMDTA OF PTXTFIT-P TO DT_M   OF FORMAT2
                    MOVE FTMTIA OF PTXTFIT-P TO TI_M   OF FORMAT2
           ELSE
                    MOVE FTADTA OF PTXTFIT-P TO DT_M   OF FORMAT2
                    MOVE FTATIA OF PTXTFIT-P TO TI_M   OF FORMAT2
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

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.
           MOVE     X-FTFILE          TO FTFILE OF FORMAT3.
           MOVE     X-FTKEY           TO FTKEY  OF FORMAT3.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
           PERFORM  SFL-HEADER.

           IF       X-AUSWAHL NOT = "5"
                    MOVE AUS TO IN50 IN INXX
           ELSE
                    MOVE AN  TO IN50 IN INXX
           END-IF.

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
           INITIALIZE DSP-ATTR.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT3-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F21 ALLES LÖSCHEN
           IF       IN21 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    PERFORM DELETE-ALL
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
                    MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                    MOVE     "FMT02" TO FMT-WRK
                    MOVE     FORMAT2 TO WSREC-WRK
                    PERFORM  COPY-DISPLAY-SFLREWRITE
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* DIE LETZTEN LEERZEILEN WERDEN BEI NICHTBEBRAUCH ÜBERLESEN
           IF       FTTEXT OF FORMAT2     = SPACES
            AND     LEERZ  OF FORMAT2 NOT = SPACES
                    MOVE     "FMT02" TO FMT-WRK
                    MOVE     FORMAT2 TO WSREC-WRK
                    PERFORM  COPY-DISPLAY-SFLREWRITE
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* ANLEGEN NEUEN TEXT
           PERFORM  ANLEGEN-TEXT.

           MOVE     SPACES TO AUSW OF FORMAT2.
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

           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     FTFILE OF FORMAT2   TO FTFILE OF PTXTFIT-P.
           MOVE     FTKEY  OF FORMAT2   TO FTKEY  OF PTXTFIT-P.
           MOVE     FTLFPO OF FORMAT2   TO FTLFPO OF PTXTFIT-P.
           READ     PTXTFIT-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PTXTFIT-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PTXTFIT"   TO FILE-WRK
                    MOVE PTXTFIT-P   TO RECORD-WRK
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
           PERFORM  WITH TEST BEFORE UNTIL POS-NR < FTLFPO OF FORMAT2
                    INITIALIZE PTXTFITF OF PTXTFIT-P
                    MOVE X-FTFILE          TO FTFILE OF PTXTFIT-P
                    MOVE X-FTKEY           TO FTKEY  OF PTXTFIT-P
                    MOVE POS-NR            TO FTLFPO OF PTXTFIT-P
                    READ PTXTFIT-DP
                    IF   FILE-STATUS = ZEROES
                         IF   POS-NR NOT = FTLFPO OF FORMAT2
                              DELETE PTXTFIT-DP
                              ADD   1 TO  FTLFPO OF PTXTFIT-P
     ‚*                       DATUM/UHRZEIT
                              PERFORM  COPY-GET-TIME
     ‚*                       USER FÜLLEN
                              MOVE  DATE-8    TO FTADTA OF PTXTFIT-P
                              MOVE  TIME-6    TO FTATIA OF PTXTFIT-P
                              MOVE  T-USER    TO FTAUSR OF PTXTFIT-P
                              MOVE  T-TERM    TO FTABS  OF PTXTFIT-P
                              MOVE  PGM-WRK   TO FTAPGM OF PTXTFIT-P
                              WRITE PTXTFIT-P
                         ELSE
                              MOVE  FTTEXT OF PTXTFIT-P TO TXT-SAVE
                              MOVE  SPACES TO FTTEXT OF PTXTFIT-P
     ‚*                       DATUM/UHRZEIT
                              PERFORM  COPY-GET-TIME
     ‚*                       USER FÜLLEN
                              MOVE  DATE-8    TO FTADTA OF PTXTFIT-P
                              MOVE  TIME-6    TO FTATIA OF PTXTFIT-P
                              MOVE  T-USER    TO FTAUSR OF PTXTFIT-P
                              MOVE  T-TERM    TO FTABS  OF PTXTFIT-P
                              MOVE  PGM-WRK   TO FTAPGM OF PTXTFIT-P
                              REWRITE PTXTFIT-P
                              MOVE  TXT-SAVE  TO FTTEXT OF PTXTFIT-P
     ‚*                       USER FÜLLEN
                              MOVE  DATE-8    TO FTMDTA OF PTXTFIT-P
                              MOVE  TIME-6    TO FTMTIA OF PTXTFIT-P
                              MOVE  T-USER    TO FTMSER OF PTXTFIT-P
                              MOVE  T-TERM    TO FTMBS  OF PTXTFIT-P
                              MOVE  PGM-WRK   TO FTMPGM OF PTXTFIT-P
                              ADD   1         TO FTLFPO OF PTXTFIT-P
                              WRITE PTXTFIT-P
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

           MOVE     FTLFPO OF FORMAT2  TO POS-NR.

           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     X-FTFILE            TO FTFILE OF PTXTFIT-P.
           MOVE     X-FTKEY             TO FTKEY  OF PTXTFIT-P.
           MOVE     FTLFPO OF FORMAT2   TO FTLFPO OF PTXTFIT-P.
           START    PTXTFIT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ERM-POS-90
           END-IF.

       ERM-POS-20.
           READ     PTXTFIT-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ERM-POS-90
           END-IF.

           IF       FTFILE OF PTXTFIT-P = X-FTFILE
            AND     FTKEY  OF PTXTFIT-P = X-FTKEY
                    MOVE  FTLFPO OF PTXTFIT-P TO POS-NR
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

           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     X-FTFILE     TO FTFILE OF PTXTFIT-P.
           MOVE     X-FTKEY      TO FTKEY  OF PTXTFIT-P.
           MOVE     SPACES       TO FTTEXT OF PTXTFIT-P.

     ‚* BEIM ERSTEN SFL-AUFBAU WIR DER CURSOR AUF DER ERSTEN
     ‚* DER DREI LEERZEILEN POSITIONIERT
           IF       SFL-ZEILE = 1
                    MOVE AN  TO IN70 OF INXX
           ELSE
                    MOVE AUS TO IN70 OF INXX
           END-IF.

           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO FTLFPO OF PTXTFIT-P.
           PERFORM  AUSGABE-SUBFILE.

           MOVE     AUS TO IN70 IN INXX.
           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO FTLFPO OF PTXTFIT-P.
           PERFORM  AUSGABE-SUBFILE.

           MOVE     AUS TO IN70 IN INXX.
           ADD      1 TO POS-NR.
           MOVE     POS-NR       TO FTLFPO OF PTXTFIT-P.
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

           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     FTFILE OF FORMAT2   TO FTFILE OF PTXTFIT-P.
           MOVE     FTKEY  OF FORMAT2   TO FTKEY  OF PTXTFIT-P.
           MOVE     FTLFPO OF FORMAT2   TO FTLFPO OF PTXTFIT-P.
           READ     PTXTFIT-DP.
           IF       FILE-STATUS = ZEROES
                    IF   FTTEXT OF FORMAT2 NOT = FTTEXT OF PTXTFIT-P
                         MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
     ‚*                  DATUM/UHRZEIT
                         PERFORM  COPY-GET-TIME
     ‚*                  USER FÜLLEN
                         MOVE DATE-8            TO FTMDTA OF PTXTFIT-P
                         MOVE TIME-6            TO FTMTIA OF PTXTFIT-P
                         MOVE T-USER            TO FTMSER OF PTXTFIT-P
                         MOVE T-TERM            TO FTMBS  OF PTXTFIT-P
                         MOVE PGM-WRK           TO FTMPGM OF PTXTFIT-P
                         MOVE FTTEXT OF FORMAT2 TO FTTEXT OF PTXTFIT-P
                    END-IF
                    REWRITE PTXTFIT-P
           ELSE
     ‚*             NEUEN DATENSATZ SCHREIBEN
                    MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                    INITIALIZE PTXTFITF OF PTXTFIT-P
                    MOVE FTFILE OF FORMAT2      TO FTFILE OF PTXTFIT-P
                    MOVE FTKEY  OF FORMAT2      TO FTKEY  OF PTXTFIT-P
                    MOVE FTLFPO OF FORMAT2      TO FTLFPO OF PTXTFIT-P
                    MOVE FTTEXT OF FORMAT2      TO FTTEXT OF PTXTFIT-P
     ‚*             DATUM/UHRZEIT
                    PERFORM  COPY-GET-TIME
     ‚*             USER FÜLLEN
                    MOVE DATE-8                 TO FTADTA OF PTXTFIT-P
                    MOVE TIME-6                 TO FTATIA OF PTXTFIT-P
                    MOVE T-USER                 TO FTAUSR OF PTXTFIT-P
                    MOVE T-TERM                 TO FTABS  OF PTXTFIT-P
                    MOVE PGM-WRK                TO FTAPGM OF PTXTFIT-P
                    WRITE PTXTFIT-P
           END-IF.

       ANL-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE TEXTE LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-ALL SECTION.
       DEL-ALL-00.

           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     X-FTFILE            TO FTFILE OF PTXTFIT-P.
           MOVE     X-FTKEY             TO FTKEY  OF PTXTFIT-P.
           MOVE     ZEROES              TO FTLFPO OF PTXTFIT-P.
           START    PTXTFIT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-ALL-90
           END-IF.

       DEL-ALL-20.
           READ     PTXTFIT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-ALL-90
           END-IF.

           IF       FTFILE OF PTXTFIT-P NOT = X-FTFILE
            OR      FTKEY  OF PTXTFIT-P NOT = X-FTKEY
                    REWRITE PTXTFIT-P
                    GO TO DEL-ALL-90
           END-IF.

     ‚* SATZ LÖSCHEN
           DELETE   PTXTFIT-DP.

           GO TO    DEL-ALL-20.

       DEL-ALL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

           MOVE     "CFP0023"         TO MAINUEB OF FORMAT3.
           MOVE     "CFP0023*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
