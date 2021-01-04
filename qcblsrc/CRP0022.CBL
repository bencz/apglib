       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0022.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUSWAHLPROGRAMM FÜR SOURCE-HIGHLIGHTING                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SOURCE-OBERWRITE                             *
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
                  ASSIGN       TO  WORKSTATION-CRD0022DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI SYNTAX-HIGHLIGHT
     ‚*-------------------------------------------------------------
           SELECT PTYPHGH-DP
                  ASSIGN       TO  DATABASE-PTYPHGH
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF CRD0022DE.
      /
     ‚*--- SYNTAX-HIGHLIGHTING
       FD  PTYPHGH-DP
           LABEL RECORDS ARE STANDARD.
       01  PTYPHGH-P.
           COPY DDS-ALL-FORMATS OF PTYPHGH.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  CRD0022DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CRD0022DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CRD0022DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CRD0022DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  FORMAT6X.
           COPY  DDS-FMT06-O OF  CRD0022DE
           REPLACING FMT06-O BY FORMAT6
                     FMT06-O-INDIC BY FORMAT6-O-INDIC.

       01  FORMAT6Y.
           COPY  DDS-FMT06-I OF  CRD0022DE
           REPLACING FMT06-I BY FORMAT6-I
                     FMT06-I-INDIC BY FORMAT6-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0022".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU".

       01  TYTYP-WRK                     LIKE TYTYP  OF PTYPHGH-P.
       01  AUSWAHL                       PIC X(2).

       01  NEUANLAGE-WRK                 PIC 9(1).

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-F3
                                X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0022" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.
           OPEN     I-O   PTYPHGH-DP.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.
           INITIALIZE X-F3, X-F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* SFL-ZEILE SETZEN
           MOVE     1 TO SFL-ZEILE.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         MOVE "1" TO X-F3
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         MOVE "1" TO X-F12
                         GO TO ENDE
                    END-IF
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
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
           CLOSE                 WS-DISPLAY
                                 PTYPHGH-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK.
     ‚* DATEN JETZT LESEN
           INITIALIZE PTYPHGHF OF PTYPHGH-P.
           MOVE     T-USER              TO TYUSER OF PTYPHGH-P.
           MOVE     SPACES              TO TYTYP  OF PTYPHGH-P.
           START    PTYPHGH-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PTYPHGH-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* KEYS PRÜFEN
           IF       TYUSER OF PTYPHGH-P NOT = T-USER
                    GO TO LES-DAT-90
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
                    MOVE    CORR INXX  TO FMT03-O-INDIC
                    WRITE   SUBFILE WS-REC FORMAT IS "FMT03"
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PTYPHGHF OF PTYPHGH-P TO FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           MOVE     CORR FORMAT2 TO FMT02-O.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
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
           MOVE     AUS    TO INXX.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           WRITE    WS-REC FORMAT IS "FMT04".
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

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
           MOVE     AUS  TO INXX.

     ‚* F3 ENDE
           IF       IN03 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN08 IN FMT03-I-INDIC = AN
                    MOVE SPACES TO TYTYP-WRK
                    PERFORM NEUANLAGE-AENDERN
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
           IF       IN12 IN FMT03-I-INDIC = AN
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
                         MOVE     CORR FORMAT2 TO FMT02-O
                         REWRITE  SUBFILE WS-REC FORMAT IS "FMT02"
                         GO TO ANZ-SFL-50
                    END-IF
           END-IF.

     ‚* ÄNDERN DES DATENSATZES
           IF       AUSW OF FORMAT2 = "1"
                    MOVE TYTYP  OF FORMAT2 TO TYTYP-WRK
                    PERFORM NEUANLAGE-AENDERN
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                    END-IF
           END-IF.

     ‚* LÖSCHEN DES SATZES
           IF       AUSW OF FORMAT2 = "4"
                    PERFORM LOESCHEN-EINTRAG
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     CORR FORMAT2 TO FMT02-O.

           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           REWRITE  SUBFILE WS-REC FORMAT IS "FMT02".

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
           WRITE    WS-REC FORMAT IS "FMT04".
           WRITE    WS-REC FORMAT IS "FMT05".
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FMT03-O-INDIC.
           MOVE     CORR   FORMAT3  TO  FMT03-O.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT03".
           MOVE     1      TO  SUBFILE-RECORD-NUMBER.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           READ     WS-DISPLAY  FORMAT IS "FMT03".

           IF       IN03 OF FMT03-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO FEH-MEL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN08 IN FMT03-I-INDIC = AN
                    MOVE SPACES TO TYTYP-WRK
                    PERFORM NEUANLAGE-AENDERN
                    IF   F3  = "1"
                         GO TO FEH-MEL-90
                    END-IF
                    INITIALIZE F12
                    GO TO FEH-MEL-90
           END-IF.

           IF       IN12 OF FMT03-I-INDIC = AN
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
           MOVE     "CRP0022"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT3.

     ‚* ZWEITE ÜBERSCHRIFT
           MOVE     "CRP0022*3"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT3.

     ‚* TYP
           MOVE     "SFT0024"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0024 OF FORMAT3.

     ‚* OVERWRITE
           MOVE     "SFT0027"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0027 OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUANLAGE EINES PROGRAMMS
     ‚*--------------------------------------------------------------
       NEUANLAGE-AENDERN SECTION.
       NEU-ANL-00.

           INITIALIZE F3, F12.
           INITIALIZE FORMAT6.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CRP0022"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT6.
     ‚* LESEN ZWEITE ÜBERSCHRIFT
           MOVE     "CRP0022*6"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT6.

           MOVE     1 TO NEUANLAGE-WRK.
     ‚* NEUANLAGE ODER NICHT?
           INITIALIZE PTYPHGHF OF PTYPHGH-P.
           IF       TYTYP-WRK NOT = SPACES
                    MOVE ZEROES TO NEUANLAGE-WRK
                    INITIALIZE PTYPHGHF OF PTYPHGH-P
                    MOVE T-USER    TO TYUSER OF PTYPHGH-P
                    MOVE TYTYP-WRK TO TYTYP  OF PTYPHGH-P
                    READ PTYPHGH-DP WITH NO LOCK
                    IF   FILE-STATUS NOT = ZEROES
                         INITIALIZE PTYPHGHF OF PTYPHGH-P
                    END-IF
           END-IF.

           MOVE     CORR PTYPHGHF OF PTYPHGH-P TO FORMAT6.

       NEU-ANL-10.
     ‚* FELDER PROTECTEN?
           IF       NEUANLAGE-WRK NOT = ZEROES
                    MOVE AUS TO IN41 OF INXX
           ELSE
                    MOVE AN  TO IN41 OF INXX
           END-IF.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT6.
           MOVE     CORR INXX         TO FMT06-O-INDIC.
           MOVE     CORR FORMAT6      TO FMT06-O.
           WRITE    WS-REC     FORMAT IS "FMT06".
           READ     WS-DISPLAY FORMAT IS "FMT06".
           MOVE     CORR FMT06-I      TO FORMAT6.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FMT06-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO NEU-ANL-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FMT06-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO NEU-ANL-90
           END-IF.

     ‚* UPDATEN DES BEFEHLS
           INITIALIZE PTYPHGHF OF PTYPHGH-P.
           MOVE     T-USER              TO TYUSER OF PTYPHGH-P.
           MOVE     TYTYP  OF FORMAT6   TO TYTYP  OF PTYPHGH-P.
           READ     PTYPHGH-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE CORR FORMAT6 TO PTYPHGHF OF PTYPHGH-P
     ‚*             USER/DATUM FÜLLEN
                    PERFORM  COPY-GET-TIME
                    MOVE T-USER      TO TYMSER OF PTYPHGH-P
                    MOVE T-TERM      TO TYMBS  OF PTYPHGH-P
                    MOVE PGM-WRK     TO TYMPGM OF PTYPHGH-P
                    MOVE DATE-8      TO TYMDTA OF PTYPHGH-P
                    MOVE TIME-6      TO TYMTIA OF PTYPHGH-P
                    REWRITE PTYPHGH-P
           ELSE
                    MOVE CORR FORMAT6 TO PTYPHGHF OF PTYPHGH-P
     ‚*             USER/DATUM FÜLLEN
                    PERFORM  COPY-GET-TIME
                    MOVE T-USER      TO TYAUSR OF PTYPHGH-P
                    MOVE T-TERM      TO TYABS  OF PTYPHGH-P
                    MOVE PGM-WRK     TO TYAPGM OF PTYPHGH-P
                    MOVE DATE-8      TO TYADTA OF PTYPHGH-P
                    MOVE TIME-6      TO TYATIA OF PTYPHGH-P
                    WRITE PTYPHGH-P
           END-IF.

       NEU-ANL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN EINES PTYPHGH-EINTRAGS
     ‚*--------------------------------------------------------------
       LOESCHEN-EINTRAG SECTION.
       LOE-EIN-00.

           INITIALIZE PTYPHGHF OF PTYPHGH-P.
           MOVE     TYUSER OF FORMAT2 TO TYUSER OF PTYPHGH-P.
           MOVE     TYTYP  OF FORMAT2 TO TYTYP  OF PTYPHGH-P.
           READ     PTYPHGH-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PTYPHGH-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PTYPHGH"   TO FILE-WRK
                    MOVE PTYPHGH-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       LOE-EIN-90.
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
