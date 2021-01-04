       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0203.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DYNAMISCHER WOCHENPLAN                                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DYNAMISCHER WOCHENPLAN IM SUBFILE            *
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
                  ASSIGN       TO  WORKSTATION-XXD0203DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE  DATEI KÜCHENPLAN
     ‚*-------------------------------------------------------------
           SELECT PKDPLAN-DL
                  ASSIGN       TO  DATABASE-PKDPLANL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI DATUM
     ‚*-------------------------------------------------------------
           SELECT PCFGDAT-DP
                  ASSIGN       TO  DATABASE-PCFGDAT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI KONFIG
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
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
           COPY DDS-ALL-FORMATS OF XXD0203DE.
      /
     ‚*--- KÜCHENPLAN
       FD  PKDPLAN-DL
           LABEL RECORDS ARE STANDARD.
       01  PKDPLAN-L.
           COPY DDS-ALL-FORMATS OF PKDPLANL1.
      /
     ‚*--- DATUMS-TABELLE
       FD  PCFGDAT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGDAT-P.
           COPY DDS-ALL-FORMATS OF PCFGDAT.
      /
     ‚*--- DATUMS-KONFIGURATION
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN43                          PIC 1.
        05 IN44                          PIC 1.
        05 IN45                          PIC 1.
        05 IN46                          PIC 1.
        05 IN47                          PIC 1.
        05 IN48                          PIC 1.
        05 IN49                          PIC 1.
        05 IN50                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0203DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0203DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0203DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0203DE
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP0203".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  AUSWAHL                       PIC X(2).
       01  COUNTER                       PIC 9(4).
       01  WOTA-WRK                      LIKE DTWOTA OF PCFGDAT-P.
       01  FIRST-DAY                     LIKE DTLFDN OF PCFGDAT-P.

       01  MAX-TAGE                      PIC 9(4) VALUE 5.
       01  MAX-TAGE-REAL                 PIC 9(4) VALUE 7.
       01  MAX-DATE                      LIKE DATE-8.
       01  MAX-DATE-REAL                 LIKE DATE-8.
       01  DATUM-SAV                     LIKE DATE-8.

       01  KD-TAB                        TYPE KDPLAN-TYPE.
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
           MOVE     "XXP0203" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0203DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PKDPLAN-DL
                          PCONFIG-DP
                          PCFGDAT-DP.

           MOVE     AUS TO INXX.
           INITIALIZE X-F3.
           INITIALIZE F3, F12.
     ‚* INTERNE TABELLE LÖSCHEN
           MOVE     ZEROES TO TAB-MAX OF KD-TAB.
           PERFORM  DEL-INTTAB.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ERMITTELN DATUM/ZEIT
           PERFORM  COPY-GET-TIME.
     ‚* DAS DATUM KONVERTIEREN
           MOVE     DATE-8            TO DT_WRK OF FORMAT3.
           MOVE     1 TO SFL-ZEILE.

       ANF020.
     ‚* DAS DATUM AUS DER AUSWAHL KONVERIEREN
           MOVE     DT_WRK OF FORMAT3(1:8) TO DATUM-SAV.
           MOVE     DATUM-SAV              TO DATE-DB.

     ‚* ERMITTELN DES ERSTEN TAGES IN DIESER WOCHE
           PERFORM  GET-FIRST-DAY-IN-WEEK.

     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN ANHAND DER TAG-DATEI
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         MOVE "1" TO X-F3
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         GO TO ENDE
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
                    GO TO ENDE
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PKDPLAN-DL
                                 PCONFIG-DP
                                 PCFGDAT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK.
     ‚* PCFGDAT LESEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "K110"              TO CFID   OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHLÜSSEL PRÜFEN
           IF       CFID   OF PCONFIG-P NOT = "K110"
                    GO TO LES-DAT-90
           END-IF.

     ‚* EIN USER-KEY? KOPFSÄTZE WEGLASSEN
           IF       CFKEY2 OF PCONFIG-P NOT = SPACES
            OR      CFKOPF OF PCONFIG-P NOT = ZEROES
                    GO TO LES-DAT-20
           END-IF.

     ‚* PRÜFEN OB ES EINEN PASSENDEN USER-KEY GIBT UND OB DORT DIE
     ‚* ANZEIGE AUSGESCHALTET IST
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     CFID    OF PCONFIG-P TO CFID   OF CFG-CPY.
           MOVE     CFKEY   OF PCONFIG-P TO CFKEY  OF CFG-CPY.
           MOVE     T-USER               TO CFKEY2 OF CFG-CPY.
           MOVE     SPACES               TO CFKEY3 OF CFG-CPY.
           MOVE     SPACES               TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
     ‚*             DANN ZIEHT DIESE EINSTELLUNG ...
                    IF   CFFK01 OF CFG-CPY NOT = ZEROES
                         GO TO LES-DAT-20
                    END-IF
           ELSE
     ‚*             ... ANSONSTEN ZIEHT DIE EINSTELLUNG
                    IF   CFFK01 OF PCONFIG-P NOT = ZEROES
                         GO TO LES-DAT-20
                    END-IF
           END-IF.

     ‚* ERSTMAL DIE INTERNE TABELLE LÖSCHEN
           PERFORM  DEL-INTTAB.

           PERFORM  LESEN-PKDPLAN.
     ‚* KEIN USER EINGETRAGEN?
           IF       TAB-MAX    OF KD-TAB  = ZEROES
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
     ‚* LESEN ALLER DIENSTE AN DIESEM TAG
     ‚*--------------------------------------------------------------
       LESEN-PKDPLAN SECTION.
       LES-PKD-00.

     ‚* PKDPLAN LESEN
           INITIALIZE PKDPLANF OF PKDPLAN-L.
           MOVE     CFKEY  OF PCONFIG-P TO KDTYP  OF PKDPLAN-L.
           MOVE     FIRST-DAY           TO KDDATE OF PKDPLAN-L.
           START    PKDPLAN-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-PKD-90
           END-IF.

       LES-PKD-20.
           READ     PKDPLAN-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-PKD-90
           END-IF.

     ‚* SCHLÜSSEL PRÜFEN
           IF       KDTYP  OF PKDPLAN-L NOT = CFKEY  OF PCONFIG-P
                    GO TO LES-PKD-90
           END-IF.

     ‚* MAX. DATUM ERREICHT?
           IF       KDDATE OF PKDPLAN-L     > MAX-DATE
                    GO TO LES-PKD-90
           END-IF.

     ‚* WURDE DER DIENST GETAUSCHT?
           IF       KDGUSR OF PKDPLAN-L NOT = SPACES
                    MOVE KDGUSR OF PKDPLAN-L TO KDUSER OF PKDPLAN-L
           END-IF.

     ‚* ERSTMAL EINE INTERNE TABELLE FÜLLEN
           PERFORM  UPDATE-INTTAB.
           IF       RET-CODE NOT = SPACES
                    PERFORM ADD-INTTAB
           END-IF.

           GO TO    LES-PKD-20.

       LES-PKD-90.
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

           MOVE     ZEROES TO TAB-POS OF KD-TAB.

       AUS-SFL-20.
           ADD      1  TO TAB-POS OF KD-TAB.
           IF       TAB-POS OF KD-TAB > TAB-MAX OF KD-TAB
                    GO TO AUS-SFL-90
           END-IF.

     ‚* GLÜCKLICHERWEISE HEIßEN DIE FELDER IN DER INTERNEN TABELLE
     ‚* GLEICH DENEN IM DISPLAY
           INITIALIZE FORMAT2.
           MOVE     CORR TAB-REC OF KD-TAB(TAB-POS OF KD-TAB) TO
                    FORMAT2.
           EVALUATE WOTA-WRK
                    WHEN 1
                         MOVE AN  TO IN46 OF INXX
                    WHEN 2
                         MOVE AN  TO IN47 OF INXX
                    WHEN 3
                         MOVE AN  TO IN48 OF INXX
                    WHEN 4
                         MOVE AN  TO IN49 OF INXX
                    WHEN 5
                         MOVE AN  TO IN50 OF INXX
           END-EVALUATE.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

           GO TO    AUS-SFL-20.

       AUS-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
           MOVE     DATUM-SAV         TO DT_WRK OF FORMAT3.
     ‚* VON-DATUM KONVERTIEREN
           MOVE     FIRST-DAY         TO DT_VON OF FORMAT3.
     ‚* BIS-DATUM KONVERTIEREN
           MOVE     MAX-DATE-REAL     TO DT_BIS OF FORMAT3.

           MOVE     AUS    TO INXX.
  ******   MOVE     1      TO  SUBFILE-RECORD-NUMBER.

       ANZ-SFL-10.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.
           EVALUATE WOTA-WRK
                    WHEN 1
                         MOVE AN  TO IN46 OF INXX
                    WHEN 2
                         MOVE AN  TO IN47 OF INXX
                    WHEN 3
                         MOVE AN  TO IN48 OF INXX
                    WHEN 4
                         MOVE AN  TO IN49 OF INXX
                    WHEN 5
                         MOVE AN  TO IN50 OF INXX
           END-EVALUATE.

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
           IF       IN03 OF FORMAT3-I-INDIC = AN
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

     ‚* DATUM AUF GÜLTIGKEIT PRÜFEN
           MOVE     DT_WRK OF FORMAT3(1:8) TO DATE-DB.
           PERFORM  COPY-CHECK-DATE.
           IF       DATUM-RETCODE NOT = SPACES
                    GO TO ANZ-SFL-10
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

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     CORR FORMAT2 TO FMT02-O.

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
           MOVE     DATUM-SAV         TO DT_WRK OF FORMAT3.
     ‚* VON-DATUM KONVERTIEREN
           MOVE     FIRST-DAY         TO DT_VON OF FORMAT3.
     ‚* BIS-DATUM KONVERTIEREN
           MOVE     MAX-DATE-REAL     TO DT_BIS OF FORMAT3.

     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           PERFORM  SFL-HEADER.
           EVALUATE WOTA-WRK
                    WHEN 1
                         MOVE AN  TO IN46 OF INXX
                    WHEN 2
                         MOVE AN  TO IN47 OF INXX
                    WHEN 3
                         MOVE AN  TO IN48 OF INXX
                    WHEN 4
                         MOVE AN  TO IN49 OF INXX
                    WHEN 5
                         MOVE AN  TO IN50 OF INXX
           END-EVALUATE.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FORMAT3-O-INDIC.
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

     ‚* DATUM AUF GÜLTIGKEIT PRÜFEN
           MOVE     DT_WRK OF FORMAT3(1:8) TO DATE-DB.
           PERFORM  COPY-CHECK-DATE.
           IF       DATUM-RETCODE NOT = SPACES
                    GO TO FEH-MEL-00
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
           MOVE     "XXP0200"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0203*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WOCHENTAGE RUNTERRECHNEN
     ‚*--------------------------------------------------------------
       WOCHENTAG-RUNTERRECHNEN SECTION.
       WOT-REC-00.

           INITIALIZE COUNTER.
     ‚* READ PRIOR
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-DB     TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WOT-REC-90
           END-IF.

       WOT-REC-20.
           READ     PCFGDAT-DP PRIOR RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WOT-REC-90
           END-IF.

     ‚* MONTAG - FREITAG FÜLLEN, NICHT MEHR
           ADD      1 TO COUNTER.
           IF       COUNTER >= WOTA-WRK
                    GO TO WOT-REC-90
           END-IF.

           MOVE     DTLFDN OF PCFGDAT-P TO FIRST-DAY.
           GO TO    WOT-REC-20.

       WOT-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES ERSTEN TAGES IN DER WOCHE
     ‚*--------------------------------------------------------------
       GET-FIRST-DAY-IN-WEEK SECTION.
       GET-FWD-00.

           INITIALIZE FIRST-DAY, WOTA-WRK.
     ‚* ERMITTELN DES AKTUELLEN TAGES
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-DB   TO DTLFDN OF PCFGDAT-P.
           READ     PCFGDAT-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

     ‚* ERSTEN TAG BERECHNEN
           MOVE     DATE-DB             TO FIRST-DAY.
           MOVE     DTWOTA OF PCFGDAT-P TO WOTA-WRK.
           PERFORM  WOCHENTAG-RUNTERRECHNEN.

           INITIALIZE COUNTER.
     ‚* JETZT VOM ERSTEN TAG AN, DIE GANZE WOCHE LESEN
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     FIRST-DAY TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

       GET-FWD-20.
           READ     PCFGDAT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

     ‚* MONTAG - FREITAG FÜLLEN, NICHT MEHR
           ADD      1 TO COUNTER.
           IF       COUNTER <= MAX-TAGE
                    MOVE     DTLFDN OF PCFGDAT-P TO MAX-DATE
                    GO TO GET-FWD-20
           END-IF.

           IF       COUNTER <= MAX-TAGE-REAL
                    MOVE     DTLFDN OF PCFGDAT-P TO MAX-DATE-REAL
                    GO TO GET-FWD-20
           END-IF.

       GET-FWD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN EINES DATENSATZES IN DER INT. TABELLE
     ‚*--------------------------------------------------------------
       UPDATE-INTTAB SECTION.
       UPD-KDT-00.

           INITIALIZE RET-CODE.
           MOVE     ZEROES TO TAB-POS OF KD-TAB.

       UPD-KDT-20.
           ADD      1 TO TAB-POS OF KD-TAB.
           IF       TAB-POS OF KD-TAB > TAB-MAX OF KD-TAB
                    MOVE "1" TO RET-CODE
                    GO TO UPD-KDT-90
           END-IF.

     ‚* WENN FÜR DEN WOCHENTAG SCHON JEMAND EINGETRAGEN IST, DANN
     ‚* NICHTS MACHEN, ANSONSTEN EINTRAGEN
           IF       MONTAG   OF TAB-REC(TAB-POS OF KD-TAB) = SPACES
            AND     KDWOTA   OF PKDPLAN-L                  = 1
                    MOVE KDTYP  OF PKDPLAN-L TO
                         KDTYP     OF KD-TAB(TAB-POS OF KD-TAB)
                    MOVE KDUSER OF PKDPLAN-L TO
                         MONTAG    OF KD-TAB(TAB-POS OF KD-TAB)
                    GO TO UPD-KDT-90
           END-IF.

           IF       DIENSTAG OF TAB-REC(TAB-POS OF KD-TAB) = SPACES
            AND     KDWOTA   OF PKDPLAN-L                  = 2
                    MOVE KDTYP  OF PKDPLAN-L TO
                         KDTYP     OF KD-TAB(TAB-POS OF KD-TAB)
                    MOVE KDUSER OF PKDPLAN-L TO
                         DIENSTAG  OF KD-TAB(TAB-POS OF KD-TAB)
                    GO TO UPD-KDT-90
           END-IF.

           IF       MITTWOCH OF TAB-REC(TAB-POS OF KD-TAB) = SPACES
            AND     KDWOTA   OF PKDPLAN-L                  = 3
                    MOVE KDTYP  OF PKDPLAN-L TO
                         KDTYP     OF KD-TAB(TAB-POS OF KD-TAB)
                    MOVE KDUSER OF PKDPLAN-L TO
                         MITTWOCH  OF KD-TAB(TAB-POS OF KD-TAB)
                    GO TO UPD-KDT-90
           END-IF.

           IF       DONNERST OF TAB-REC(TAB-POS OF KD-TAB) = SPACES
            AND     KDWOTA   OF PKDPLAN-L                  = 4
                    MOVE KDTYP  OF PKDPLAN-L TO
                         KDTYP     OF KD-TAB(TAB-POS OF KD-TAB)
                    MOVE KDUSER OF PKDPLAN-L TO
                         DONNERST  OF KD-TAB(TAB-POS OF KD-TAB)
                    GO TO UPD-KDT-90
           END-IF.

           IF       FREITAG  OF TAB-REC(TAB-POS OF KD-TAB) = SPACES
            AND     KDWOTA   OF PKDPLAN-L                  = 5
                    MOVE KDTYP  OF PKDPLAN-L TO
                         KDTYP     OF KD-TAB(TAB-POS OF KD-TAB)
                    MOVE KDUSER OF PKDPLAN-L TO
                         FREITAG   OF KD-TAB(TAB-POS OF KD-TAB)
                    GO TO UPD-KDT-90
           END-IF.

           GO TO    UPD-KDT-20.

       UPD-KDT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES DATENSATZES IN DER INT. TABELLE
     ‚*--------------------------------------------------------------
       ADD-INTTAB SECTION.
       ADD-KDT-00.

           ADD      1    TO TAB-MAX OF KD-TAB.
           MOVE     KDTYP  OF PKDPLAN-L TO
                    KDTYP  OF KD-TAB(TAB-MAX OF KD-TAB).
           EVALUATE KDWOTA OF PKDPLAN-L
                    WHEN 1
                         MOVE KDUSER OF PKDPLAN-L TO
                              MONTAG  OF KD-TAB(TAB-MAX OF KD-TAB)
                    WHEN 2
                         MOVE KDUSER OF PKDPLAN-L TO
                              DIENSTAG OF KD-TAB(TAB-MAX OF KD-TAB)
                    WHEN 3
                         MOVE KDUSER OF PKDPLAN-L TO
                              MITTWOCH OF KD-TAB(TAB-MAX OF KD-TAB)
                    WHEN 4
                         MOVE KDUSER OF PKDPLAN-L TO
                              DONNERST OF KD-TAB(TAB-MAX OF KD-TAB)
                    WHEN 5
                         MOVE KDUSER OF PKDPLAN-L TO
                              FREITAG OF KD-TAB(TAB-MAX OF KD-TAB)
           END-EVALUATE.

       ADD-KDT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TAG-TABELLE ZUR SELEKTION
     ‚*--------------------------------------------------------------
       DEL-INTTAB SECTION.
       DEL-ITB-00.

     ‚* IST DIE TABELLE LEER, DANN MUSS NICHTS MEHR GEMACHT WERDEN
           IF       TAB-MAX OF KD-TAB = ZEROES
                    GO TO DEL-ITB-90
           END-IF.

           MOVE     ZEROES TO TAB-POS OF KD-TAB.

       DEL-ITB-20.
           ADD      1 TO TAB-POS OF KD-TAB.
           IF       TAB-POS OF KD-TAB > TAB-MAX OF KD-TAB
                    GO TO DEL-ITB-90
           END-IF.

     ‚* INITIALISIEREN DER WERTE
           MOVE     SPACES TO KDTYP    OF KD-TAB(TAB-POS OF KD-TAB).
           MOVE     SPACES TO MONTAG   OF KD-TAB(TAB-POS OF KD-TAB).
           MOVE     SPACES TO DIENSTAG OF KD-TAB(TAB-POS OF KD-TAB).
           MOVE     SPACES TO MITTWOCH OF KD-TAB(TAB-POS OF KD-TAB).
           MOVE     SPACES TO DONNERST OF KD-TAB(TAB-POS OF KD-TAB).
           MOVE     SPACES TO FREITAG  OF KD-TAB(TAB-POS OF KD-TAB).

           GO TO    DEL-ITB-20.

       DEL-ITB-90.
           INITIALIZE KD-TAB.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CHECK-DATE: DATUMSFELDER PRÜFEN
     ‚*---------------------------------------------------------------

           COPY     CHKDATE    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CONVERT-DATE: DATUMSFELDER AUFBEREITEN/UMSETZEN
     ‚*---------------------------------------------------------------

           COPY     CNVDATE    OF APG-QCPYSRC.
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
