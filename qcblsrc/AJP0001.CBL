       IDENTIFICATION DIVISION.
      *
         PROGRAM-ID.              AJP0001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
      *
      *---------------------------------------------------------------*
      * AUSWAHLPROGRAMM FÜR AJAX                                      *
      *---------------------------------------------------------------*
      *                                                               *
      * AUTOR         :  A. PIEGER                                    *
      *                                                               *
      * ERSTELLT AM   :  04.06.2009                                   *
      *                                                               *
      * FUNKTION      :  AUSWAHL + ANZEIGE THEMEN                     *
      *                                                               *
      * ÄNDERUNGEN:                                                   *
      * DATUM      VON   GRUND DER ÄNDERUNG                           *
      *                                                               *
      *****************************************************************
      *
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

      *-------------------------------------------------------------
      *-   BILDSCHIRM-DATEI
      *-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-AJD0001DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

      *-------------------------------------------------------------
      * PHYSISCHE DATEI THEMEN
      *-------------------------------------------------------------
           SELECT PAJXTHM-DP
                  ASSIGN       TO  DATABASE-PAJXTHM
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
      *----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF AJD0001DE.
      /
      *--- THEMEN
       FD  PAJXTHM-DP
           LABEL RECORDS ARE STANDARD.
       01  PAJXTHM-P.
           COPY DDS-ALL-FORMATS OF PAJXTHM.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF AJD0001DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  AJD0001DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  AJD0001DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  AJD0001DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  AJD0001DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  AJD0001DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

      * STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

      * STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

      * STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "AJP0001".
       01  PGM-TYP                       PIC X(8)  VALUE "AJAX".

       01  AUSWAHL                       PIC X(2).

       01  AXID-WRK                      LIKE AXID   OF PAJXTHM-P.

      * PARAMETER FÜR DAS AJP0002
       01  THEMEN-MODUS                  PIC X(1).
       01  AXID-DUMMY                    LIKE AXID   OF PAJXTHM-P.
       01  AXKEY-DUMMY                   LIKE AXKEY  OF PAJXTHM-P.
      /
      *-------------------------------------------------------------
       LINKAGE SECTION.
      *-------------------------------------------------------------
       PROCEDURE DIVISION.
      *-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

      * FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AJP0001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

      * DISPLAY ÖFFNEN
           MOVE     "AJD0001DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

      * ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PAJXTHM-DP.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

      * DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.

       ANF010.
      * VORAUSWAHL: SELEKTIEREN DES HAUPTTHEMAS
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

       ANF020.
      * LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN ANHAND DER TAG-DATEI
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

      * ANZEIGEN SUBFILE
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

      * WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
      * DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PAJXTHM-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
      *--------------------------------------------------------------
      * SELEKTIEREN ALLER NÖTIGEN DATEN
      *--------------------------------------------------------------
       VORAUSWAHL SECTION.
       VOR-AUS-00.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
      * LESEN ÜBERSCHRIFT
           MOVE     "AJP0000"         TO MAINUEB OF FORMAT1.
           MOVE     "AJP0001*1"       TO SUBUEB  OF FORMAT1.

       VOR-AUS-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.

      * F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

      * F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

      * F8 NEUANLAGE HAUPTTHEMA ?
           IF       IN08 OF FORMAT1-I-INDIC  = AN
                    PERFORM NEUANLAGE-HAUPTTHEMA
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    GO TO VOR-AUS-00
           END-IF.

      * FRAGEZEICHEN-FUNKTION FÜR THEMEN
           IF       AXID   OF FORMAT1(1:1) = "?"
      *€            PERFORM THEMA-SELEKTION
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    MOVE AXID-WRK             TO AXID    OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

      * THEMA MUSS ANGEGEBEN SEIN
           IF       AXID   OF FORMAT1 = SPACES
                    GO TO VOR-AUS-10
           END-IF.

       VOR-AUS-90.
           EXIT.
      /
      *--------------------------------------------------------------
      * LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
      *--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
      * PAJXTHM LESEN
           INITIALIZE PAJXTHMF OF PAJXTHM-P.
           MOVE     AXID   OF FORMAT1   TO AXID   OF PAJXTHM-P.
           MOVE     SPACES              TO AXKEY  OF PAJXTHM-P.
           START    PAJXTHM-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PAJXTHM-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       AXID   OF PAJXTHM-P NOT = AXID   OF FORMAT1
            AND     AXID   OF FORMAT1   NOT = SPACES
                    GO TO LES-DAT-90
           END-IF.

      * DAS HAUPTTHEMA SELBER NICHT ANZEIGEN
           IF       AXKEY  OF PAJXTHM-P     = SPACES
                    GO TO LES-DAT-20
           END-IF.

      * NACH TEXT SELEKTIEREN?
           IF       TXTSEL OF FORMAT1   NOT = SPACES
                    MOVE PAJXTHM-P           TO RECORD-WRK
                    MOVE LENGTH OF PAJXTHM-P TO RECORD-LEN
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

      * AUSGABE EINER SUBFILE-ZEILE
           PERFORM  AUSGABE-SFL.
           IF       SUBFILE-RECORD-NUMBER < 9999
                    GO TO LES-DAT-20
           END-IF.

       LES-DAT-90.
           EXIT.
      /
      *--------------------------------------------------------------
      * AUSGABE DER SFL-ZEILE
      *--------------------------------------------------------------
       AUSGABE-SFL SECTION.
       AUS-SFL-00.

      * SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    ZEROES  TO ANZREC-WRK
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

      * SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PAJXTHMF OF PAJXTHM-P TO FORMAT2.
      * KOMMENTAR AUFRÜCKEN WENN Z.B. NUR DER ZWEITE GEFÜLLT IST
           IF       AXKOM1 OF FORMAT2 = SPACES
                    IF   AXKOM2 OF PAJXTHM-P NOT = SPACES
                         MOVE AXKOM2 OF PAJXTHM-P TO AXKOM1 OF FORMAT2
                    ELSE
      *                  KOMMENTAR 3 AUFRÜCKEN BZW. TEILDATEIENTEXT
                         IF   AXKOM3 OF PAJXTHM-P NOT = SPACES
                              MOVE AXKOM3 OF PAJXTHM-P TO
                                   AXKOM1 OF FORMAT2
                         ELSE
                              MOVE AXMTXT OF PAJXTHM-P TO
                                   AXKOM1 OF FORMAT2
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
      *--------------------------------------------------------------
      * ANZEIGEN DES SUBFILES
      *--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.

           MOVE     AUS    TO INXX.
  ******   MOVE     1      TO  SUBFILE-RECORD-NUMBER.

       ANZ-SFL-20.
      * AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

      * SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

      * ANZEIGE SUBFILE
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

      * BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
      *€   MOVE     9999 TO SFL-ZEILE.

      * F3 ENDE
           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

      * F5 AKTUALISIEREN
           IF       IN05 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

      * F8 NEUANLAGE
           IF       IN08 OF FORMAT3-I-INDIC = AN
                    PERFORM NEUANLAGE-UNTERTHEMA
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                         GO TO ANZ-SFL-20
                    END-IF
                    GO TO ANZ-SFL-90
           END-IF.

      * F12 ABBRECHEN
           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

       ANZ-SFL-50.
      * LESEN VON SUBFILE NEXT MODIFIED RECORD
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

      * RECORD-NUMMER SICHERN, DAMIT BEI FEHLERMELDUNG DER CURSOR
      * AN DER RICHTIGEN POSITION STEHT
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.

      * BERECHTIGUNG FÜR DIESE FUNKTION ÜBERPRÜFEN
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

      * ÄNDERN DES DATENSATZES
           IF       AUSW OF FORMAT2 = "1"
                    MOVE SPACES TO THEMEN-MODUS
                    CALL "AJP0002" USING AXID   OF FORMAT2
                                         AXKEY  OF FORMAT2
                                         THEMEN-MODUS
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

      * LÖSCHEN DES SATZES
           IF       AUSW OF FORMAT2 = "4"
                    CALL "AJP0008" USING AXID   OF FORMAT2
                                         AXKEY  OF FORMAT2
                    END-CALL
           END-IF.

      * BESCHREIBUNG ANZEIGEN
           IF       AUSW OF FORMAT2 = "6"
                    INITIALIZE F3, F12
                    MOVE "5" TO AUSWAHL
                    CALL "AJP0006" USING AXID   OF FORMAT2
                                         AXKEY  OF FORMAT2
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
      *--------------------------------------------------------------
      * FEHLERMELDUNG WENN KEINE SFL-ZEILEN VORHANDEN
      *--------------------------------------------------------------
       FEHLERMELDUNG SECTION.
       FEH-MEL-00.

           INITIALIZE F3, F12.
           INITIALIZE FORMAT3.

      * AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           PERFORM  SFL-HEADER.

      * ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
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

      * F8 NEUANLAGE
           IF       IN08 IN FORMAT3-I-INDIC = AN
                    PERFORM NEUANLAGE-UNTERTHEMA
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

       FEH-MEL-90.
           EXIT.
      /
      *--------------------------------------------------------------
      * SUBFILEÜBERSCHRIFTEN FÜLLEN
      *--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

      * ÜBERSCHRIFTEN
           MOVE     "AJP0000"         TO MAINUEB OF FORMAT3.
           MOVE     "AJP0001*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
      *--------------------------------------------------------------
      * SELEKTION EINES THEMAS MITTELS WINDOW
      *--------------------------------------------------------------
       THEMA-SELEKTON SECTION.
       THM-SEL-00.

           INITIALIZE RET-CODE.

       THM-SEL-90.
           EXIT.
      /
      *--------------------------------------------------------------
      * HAUPTTHEMA ANLEGEN
      *--------------------------------------------------------------
       NEUANLAGE-HAUPTTHEMA SECTION.
       HAU-THM-00.

           MOVE     "H"     TO THEMEN-MODUS.
           MOVE     SPACES  TO AXID-DUMMY.
           MOVE     SPACES  TO AXKEY-DUMMY.

           CALL     "AJP0002" USING AXID-DUMMY
                                    AXKEY-DUMMY
                                    THEMEN-MODUS
                                    PGM-WRK
                                    F3
                                    F12
           END-CALL.

       HAU-THM-90.
           EXIT.
      /
      *--------------------------------------------------------------
      * UNTERTHEMA ANLEGEN
      *--------------------------------------------------------------
       NEUANLAGE-UNTERTHEMA SECTION.
       HAU-UTM-00.

           MOVE     "U"     TO THEMEN-MODUS.
           MOVE     SPACES  TO AXID-DUMMY.
           MOVE     SPACES  TO AXKEY-DUMMY.

           CALL     "AJP0002" USING AXID   OF FORMAT1
                                    AXKEY-DUMMY
                                    THEMEN-MODUS
                                    PGM-WRK
                                    F3
                                    F12
           END-CALL.

       HAU-UTM-90.
           EXIT.
      /
      *---------------------------------------------------------------
      * COPY-PGM-TXT: PROGRAMMTEXTE LESEN
      *---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
      /
      *---------------------------------------------------------------
      * COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
      *---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
      *---------------------------------------------------------------
      * COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
      *---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
      *---------------------------------------------------------------
      * COPY-DSP-*: ALLE DISPLAYROUTINEN
      *---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
