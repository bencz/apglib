       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0304.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE ALLER ÜBUNGEN                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE ALLER ÜBUNGEN EINES USERS            *
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
                  ASSIGN       TO  WORKSTATION-XXD0304DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PAZUUEB: AULFDN
     ‚*-------------------------------------------------------------
           SELECT PAZUUEB-DP
                  ASSIGN       TO  DATABASE-PAZUUEB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PAZUSRC: ASUSER, ASLFDN, ASSLIB, ASSRCN, ASSMBR
     ‚*-------------------------------------------------------------
           SELECT PAZUSRC-DP
                  ASSIGN       TO  DATABASE-PAZUSRC
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
           COPY DDS-ALL-FORMATS OF XXD0304DE.
      /
     ‚*--- PAZUUEB: AULFDN
       FD  PAZUUEB-DP
           LABEL RECORDS ARE STANDARD.
       01  PAZUUEB-P.
           COPY DDS-ALL-FORMATS OF PAZUUEB.
     ‚*--- PAZUSRC: ASUSER, ASLFDN, ASSLIB, ASSRCN, ASSMBR
       FD  PAZUSRC-DP
           LABEL RECORDS ARE STANDARD.
       01  PAZUSRC-P.
           COPY DDS-ALL-FORMATS OF PAZUSRC.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  XXD0304DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0304DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0304DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0304DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0304DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0304DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0304".
       01  PGM-TYP                       PIC X(8)  VALUE "AZUBI".

       01  AUSWAHL                       PIC X(2).

     ‚* PARAMETER FÜR DAS XXP0302
       01  AULFDN-WRK                    LIKE AULFDN OF PAZUUEB-P.

     ‚* PARAMETER FÜR DAS XXP0303
       01  USER-WRK                      LIKE T-USER.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0304" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0304DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PAZUUEB-DP
                          PAZUSRC-DP.

           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.
           MOVE     1 TO SFL-ZEILE.
           MOVE     AUS TO INXX.

       ANF010.
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
            OR      F12 = "1"
                    GO TO ENDE
           END-IF.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
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
                    GO TO ENDE
           END-IF.
           IF       F5 = "1"
                    INITIALIZE F5
                    GO TO ANF020
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PAZUUEB-DP
                                 PAZUSRC-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* VORAUSWAHLBILDSCHIRM ANZEIGEN
     ‚*--------------------------------------------------------------
       VORAUSWAHL SECTION.
       VOR-AUS-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0300"         TO MAINUEB OF FORMAT1.
           MOVE     "XXP0304*1"       TO SUBUEB  OF FORMAT1.

           MOVE     CORR INXX TO FORMAT1-O-INDIC.
           MOVE     "FMT01" TO FMT-WRK.
           MOVE     FORMAT1 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           MOVE     AUS  TO INXX.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT1-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F8 NEUE ÜBUNGEN ANLEGEN
     ‚*€   IF       IN08 IN FORMAT1-I-INDIC = AN
     ‚*€            GO TO VOR-AUS-00
     ‚*€   END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
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

           INITIALIZE DATEN.
     ‚* DIE GENERELLEN SOURCEN SIND MIT *DEFAULT GEKENNZEICHNET
           INITIALIZE PAZUSRCF OF PAZUSRC-P.
           MOVE     ASUSER OF FORMAT1        TO ASUSER OF PAZUSRC-P.
           MOVE     ZEROES                   TO ASLFDN OF PAZUSRC-P.
           MOVE     ZEROES                   TO ASSLIB OF PAZUSRC-P.
           MOVE     ZEROES                   TO ASSRCN OF PAZUSRC-P.
           MOVE     ZEROES                   TO ASSMBR OF PAZUSRC-P.
           START    PAZUSRC-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

        LES-DAT-20.
           READ     PAZUSRC-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* USER ABFRAGEN
           IF       ASUSER OF PAZUSRC-P NOT = ASUSER OF FORMAT1
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
           MOVE     CORR PAZUSRCF OF PAZUSRC-P TO FORMAT2.
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

           MOVE     PGM-TYP                    TO PGMTYP OF FORMAT3.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
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

     ‚* F8 NEUE SOURCE HINZUFÜGEN
     ‚*€   IF       IN08 IN FORMAT3-I-INDIC = AN
     ‚*€            MOVE ZEROES TO AULFDN-WRK
     ‚*€            PERFORM SHOW-ENTRY
     ‚*€            MOVE "1" TO F5
     ‚*€            GO TO ANZ-SFL-90
     ‚*€   END-IF.

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
                    MOVE SPACES            TO RET-CODE
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

     ‚* ANZEIGEN DER ÜBUNG
           IF       AUSW OF FORMAT2 = "1"
                    MOVE AULFDN OF FORMAT2 TO AULFDN-WRK
                    PERFORM SHOW-ENTRY
                    IF   F3 = "1"
                         GO TO ENDE
                    END-IF
           END-IF.

     ‚* LÖSCHEN DER ÜBUNGE
           IF       AUSW OF FORMAT2 = "4"
                    MOVE AULFDN OF FORMAT2 TO AULFDN-WRK
                    PERFORM DELETE-ENTRY
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

     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP                    TO PGMTYP OF FORMAT3.

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

     ‚* F8 NEUANLAGE
     ‚*€   IF       IN08 IN FORMAT3-I-INDIC = AN
     ‚*€            MOVE ZEROES TO AULFDN-WRK
     ‚*€            PERFORM SHOW-ENTRY
     ‚*€            IF   F3  = "1"
     ‚*€                 GO TO FEH-MEL-90
     ‚*€            END-IF
     ‚*€            INITIALIZE F12
     ‚*€            GO TO FEH-MEL-90
     ‚*€   END-IF.

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE-ÜBERSCHRIFTEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0300"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0304*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINTRAG ANZEIGEN/ANLEGEN
     ‚*--------------------------------------------------------------
       SHOW-ENTRY SECTION.
       SHW-ENT-00.

     ‚* PROGRAMM AUFRUFEN
           CALL     "XXP0302" USING AULFDN-WRK
                                    F3
                                    F12
           END-CALL.

       SHW-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINTRAG LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-ENTRY SECTION.
       DEL-ENT-00.

           INITIALIZE RET-CODE.

       DEL-ENT-90.
           EXIT.
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
