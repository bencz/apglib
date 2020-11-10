       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              DRP0100.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DRUCKEN TIPPS FÜR EINEN SPIELTAG                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.03.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DRUCKEN DER TIPPÜBERSICHT                    *
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

     ‚*---------------------------------------------------------------*
     ‚*    PRINTERFILE
     ‚*---------------------------------------------------------------*
           SELECT DRUCK-DAT
                  ASSIGN        TO FORMATFILE-DRF0100
                  ORGANIZATION  IS SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI MITSPIELER
     ‚*-------------------------------------------------------------
           SELECT PMITSPL-DP
                  ASSIGN       TO  DATABASE-PMITSPL
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TIPPS
     ‚*-------------------------------------------------------------
           SELECT PTIPPEN-DP
                  ASSIGN       TO  DATABASE-PTIPPEN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI VEREINE
     ‚*-------------------------------------------------------------
           SELECT PVEREIN-DP
                  ASSIGN       TO  DATABASE-PVEREIN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI SPIELTAGE
     ‚*-------------------------------------------------------------
           SELECT PSPITAG-DL
                  ASSIGN       TO  DATABASE-PSPITAGL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- DRUCKAUSGABE
       FD  DRUCK-DAT
           LABEL RECORDS ARE OMITTED.
       01  DRUCK-REC.
            COPY DDS-ALL-FORMATS OF DRF0100.
      /
     ‚*--- PMITSPL: MSLIGA, MSJAHR, MSTAG, MSTUSR
       FD  PMITSPL-DP
           LABEL RECORDS ARE STANDARD.
       01  PMITSPL-P.
           COPY DDS-ALL-FORMATS OF AMU-PMITSPL.
      /
     ‚*--- PTIPPEN: TPLFDN, TPTUSR
       FD  PTIPPEN-DP
           LABEL RECORDS ARE STANDARD.
       01  PTIPPEN-P.
           COPY DDS-ALL-FORMATS OF AMU-PTIPPEN.
      /
     ‚*--- PVEREIN: FVLFDN
       FD  PVEREIN-DP
           LABEL RECORDS ARE STANDARD.
       01  PVEREIN-P.
           COPY DDS-ALL-FORMATS OF AMU-PVEREIN.
      /
     ‚*--- PSPITAGL1: STLIGA, STJAHR, STTAG, STLFDN
       FD  PSPITAG-DL
           LABEL RECORDS ARE STANDARD.
       01  PSPITAG-L.
           COPY DDS-ALL-FORMATS OF AMU-PSPITAGL1.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN40                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  DRF0100
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  DRF0100
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  DRF0100
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  DRF0100
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  DRF0100
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  DRF0100
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "DRP0100".
       01  PGM-TYP                       PIC X(8)  VALUE "PRINT".

       01  HEIM-NAME                     LIKE FVNAME OF PVEREIN-P.
       01  AUSW-NAME                     LIKE FVNAME OF PVEREIN-P.

       01  AUSWAHL                       PIC X(2).
       01  IND                           PIC 9(4).
       01  MAX-ANZAHL-TAB                PIC 9(4)  VALUE 9.

       01  SPIELE-TAB.
        05 SPIELE-REC                    OCCURS 20 TIMES.
         07 LFDN                         LIKE STLFDN OF PSPITAG-L.
         07 HEIM                         LIKE FVLFDN OF PVEREIN-P.
         07 AUSW                         LIKE FVLFDN OF PVEREIN-P.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-LIGA                        LIKE STLIGA OF PSPITAG-L.
       01  X-SAISON                      LIKE STJAHR OF PSPITAG-L.
       01  X-SPIELTAG                    LIKE STTAG  OF PSPITAG-L.
       01  X-OUTQ                        PIC X(10).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-LIGA
                                X-SAISON
                                X-SPIELTAG
                                X-OUTQ.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* AMU-BIBLIOTHEK VORSCHALTEN
           PERFORM  ADD-AMU-LIB.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "DRP0100" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE AMU-DATEIEN ÖFFNEN
           OPEN     INPUT PSPITAG-DL
                          PMITSPL-DP
                          PTIPPEN-DP
                          PVEREIN-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           MOVE     AUS TO INXX.
     ‚* INTERNE TABELLE LEEREN
           MOVE     ZEROES TO IND.
           PERFORM  MAX-ANZAHL-TAB TIMES
                    ADD  1      TO IND
                    MOVE ZEROES TO HEIM OF SPIELE-REC(IND)
                    MOVE ZEROES TO AUSW OF SPIELE-REC(IND)
           END-PERFORM.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN ANHAND DER TAG-DATEI
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    GO TO ENDE
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

           CLOSE                 PSPITAG-DL
                                 PMITSPL-DP
                                 PTIPPEN-DP
                                 PVEREIN-DP.

           PERFORM  COPY-PGM-EXIT.

     ‚* AMU-BIBLIOTHEK LÖSCHEN
           PERFORM  DEL-AMU-LIB.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK.
     ‚* ERMITTELN ALLER BEGEGNUNGEN UND SPEICHERN IN EINER INT. TABELLE
           PERFORM  GET-BEGEGNUNGEN.
           IF       IND = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* DATENBANK LESEN
           INITIALIZE PMITSPLF OF PMITSPL-P.
           MOVE     X-LIGA              TO MSLIGA OF PMITSPL-P.
           MOVE     X-SAISON            TO MSJAHR OF PMITSPL-P.
           MOVE     X-SPIELTAG          TO MSTAG  OF PMITSPL-P.
           MOVE     SPACES              TO MSTUSR OF PMITSPL-P.
           START    PMITSPL-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PMITSPL-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* RICHTIGER KEY?
           IF       MSLIGA OF PMITSPL-P NOT = X-LIGA
            OR      MSJAHR OF PMITSPL-P NOT = X-SAISON
            OR      MSTAG  OF PMITSPL-P NOT = X-SPIELTAG
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
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PMITSPLF OF PMITSPL-P TO FORMAT2.
     ‚* JETZT ZU ALLEN BEGEGNUNGEN DEN JEWEILIGEN TIPP ERMITTELN
           PERFORM  GET-BEGEGNUNGEN-TIPP.

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
                    MOVE "1" TO F5
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

     ‚* AUSGABE INFOZEILE DASS DISPLAY LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

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
           MOVE     "DRP0100"         TO MAINUEB OF FORMAT3.
           MOVE     "DRP0100*3"       TO SUBUEB  OF FORMAT3.
           PERFORM  GET-BEGEGNUNGEN-VEREIN.

     ‚* HEADERDATEN FÜLLEN
           MOVE     X-LIGA         TO STLIGA OF FORMAT3.
           MOVE     X-SAISON       TO STJAHR OF FORMAT3.
           MOVE     X-SPIELTAG     TO STTAG  OF FORMAT3.
           MOVE     T-USER         TO TUSER  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN ALLER BEGEGNUNGEN
     ‚*--------------------------------------------------------------
       GET-BEGEGNUNGEN SECTION.
       GET-BEG-00.

           INITIALIZE IND.
     ‚* DATENBANK LESEN
           INITIALIZE PSPITAGF OF PSPITAG-L.
           MOVE     X-LIGA              TO STLIGA OF PSPITAG-L.
           MOVE     X-SAISON            TO STJAHR OF PSPITAG-L.
           MOVE     X-SPIELTAG          TO STTAG  OF PSPITAG-L.
           MOVE     SPACES              TO STLFDN OF PSPITAG-L.
           START    PSPITAG-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-BEG-90
           END-IF.

       GET-BEG-20.
           READ     PSPITAG-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-BEG-90
           END-IF.

     ‚* RICHTIGER KEY?
           IF       STLIGA OF PSPITAG-L NOT = X-LIGA
            OR      STJAHR OF PSPITAG-L NOT = X-SAISON
            OR      STTAG  OF PSPITAG-L NOT = X-SPIELTAG
                    GO TO GET-BEG-90
           END-IF.

           ADD      1 TO IND.
           MOVE     STLFDN OF PSPITAG-L TO LFDN OF SPIELE-REC(IND).
           MOVE     STHEIM OF PSPITAG-L TO HEIM OF SPIELE-REC(IND).
           MOVE     STAUSW OF PSPITAG-L TO AUSW OF SPIELE-REC(IND).

           GO TO    GET-BEG-20.

       GET-BEG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN ALLER BEGEGNUNGEN
     ‚*--------------------------------------------------------------
       GET-BEGEGNUNGEN-TIPP SECTION.
       GET-BGT-00.

           INITIALIZE IND.
     ‚* TABELLE DURCHLAUFEN
           PERFORM  MAX-ANZAHL-TAB TIMES
                    ADD 1 TO IND
                    INITIALIZE PTIPPENF OF PTIPPEN-P
                    MOVE LFDN OF SPIELE-REC(IND) TO TPLFDN OF PTIPPEN-P
                    MOVE MSTUSR OF PMITSPL-P     TO TPTUSR OF PTIPPEN-P
                    READ PTIPPEN-DP
                    IF   FILE-STATUS = ZEROES
                         PERFORM FILL-BEGEGNUNGEN
                    END-IF
           END-PERFORM.

       GET-BGT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TIPPS FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-BEGEGNUNGEN SECTION.
       FIL-BEG-00.

           EVALUATE IND
                    WHEN 1
                         MOVE TPTORH OF PTIPPEN-P TO TIP1H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP1A OF FORMAT2
                    WHEN 2
                         MOVE TPTORH OF PTIPPEN-P TO TIP2H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP2A OF FORMAT2
                    WHEN 3
                         MOVE TPTORH OF PTIPPEN-P TO TIP3H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP3A OF FORMAT2
                    WHEN 4
                         MOVE TPTORH OF PTIPPEN-P TO TIP4H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP4A OF FORMAT2
                    WHEN 5
                         MOVE TPTORH OF PTIPPEN-P TO TIP5H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP5A OF FORMAT2
                    WHEN 6
                         MOVE TPTORH OF PTIPPEN-P TO TIP6H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP6A OF FORMAT2
                    WHEN 7
                         MOVE TPTORH OF PTIPPEN-P TO TIP7H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP7A OF FORMAT2
                    WHEN 8
                         MOVE TPTORH OF PTIPPEN-P TO TIP8H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP8A OF FORMAT2
                    WHEN 9
                         MOVE TPTORH OF PTIPPEN-P TO TIP9H OF FORMAT2
                         MOVE TPTORA OF PTIPPEN-P TO TIP9A OF FORMAT2
           END-EVALUATE.

       FIL-BEG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN ALLER BEGEGNUNGEN
     ‚*--------------------------------------------------------------
       GET-BEGEGNUNGEN-VEREIN SECTION.
       GET-BVE-00.

           INITIALIZE IND.
     ‚* TABELLE DURCHLAUFEN
           PERFORM  MAX-ANZAHL-TAB TIMES
                    ADD 1 TO IND
                    INITIALIZE HEIM-NAME, AUSW-NAME
     ‚*             HEIMMANNSCHAFT
                    INITIALIZE PVEREINF OF PVEREIN-P
                    MOVE HEIM OF SPIELE-REC(IND) TO FVLFDN OF PVEREIN-P
                    READ PVEREIN-DP
                    IF   FILE-STATUS = ZEROES
                         MOVE FVNAME OF PVEREIN-P TO HEIM-NAME
                    END-IF
     ‚*             AUSWÄRTSMANNSCHAFT
                    INITIALIZE PVEREINF OF PVEREIN-P
                    MOVE AUSW OF SPIELE-REC(IND) TO FVLFDN OF PVEREIN-P
                    READ PVEREIN-DP
                    IF   FILE-STATUS = ZEROES
                         MOVE FVNAME OF PVEREIN-P TO AUSW-NAME
                    END-IF
     ‚*             FORMAT FÜLLEN
                    PERFORM FILL-BEGEGNUNGEN-VEREIN
           END-PERFORM.

       GET-BVE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TIPPS FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-BEGEGNUNGEN-VEREIN SECTION.
       FIL-VER-00.

           EVALUATE IND
                    WHEN 1
                         MOVE HEIM-NAME TO TIP1 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP1 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP1 OF FORMAT3(5:3)
                    WHEN 2
                         MOVE HEIM-NAME TO TIP2 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP2 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP2 OF FORMAT3(5:3)
                    WHEN 3
                         MOVE HEIM-NAME TO TIP3 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP3 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP3 OF FORMAT3(5:3)
                    WHEN 4
                         MOVE HEIM-NAME TO TIP4 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP4 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP4 OF FORMAT3(5:3)
                    WHEN 5
                         MOVE HEIM-NAME TO TIP5 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP5 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP5 OF FORMAT3(5:3)
                    WHEN 6
                         MOVE HEIM-NAME TO TIP6 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP6 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP6 OF FORMAT3(5:3)
                    WHEN 7
                         MOVE HEIM-NAME TO TIP7 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP7 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP7 OF FORMAT3(5:3)
                    WHEN 8
                         MOVE HEIM-NAME TO TIP8 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP8 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP8 OF FORMAT3(5:3)
                    WHEN 9
                         MOVE HEIM-NAME TO TIP9 OF FORMAT3(1:3)
                         MOVE ":"       TO TIP9 OF FORMAT3(4:1)
                         MOVE AUSW-NAME TO TIP9 OF FORMAT3(5:3)
           END-EVALUATE.

       FIL-VER-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AMU-BIBLIOTHEK VORSCHALTEN
     ‚*--------------------------------------------------------------
       ADD-AMU-LIB SECTION.
       ADD-AMU-00.

     ‚* "AMU" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE ADDLIBLE-REC.
           MOVE     "AMU"        TO LIB     OF ADDLIBLE-REC.
           MOVE     "*LAST"      TO LIBPOS  OF ADDLIBLE-REC.
           CALL     "CLP0007" USING ADDLIBLE-REC
                                    RET-CODE
           END-CALL.

       ADD-AMU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AMU-BIBLIOTHEK ENTFERNEN
     ‚*--------------------------------------------------------------
       DEL-AMU-LIB SECTION.
       DEL-AMU-00.

     ‚* "AMU" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE RMVLIBLE-REC.
           MOVE     "AMU"        TO LIB     OF RMVLIBLE-REC.
           CALL     "CLP0009" USING RMVLIBLE-REC
           END-CALL.

       DEL-AMU-90.
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
