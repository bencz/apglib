     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HMP0007.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* WINDOW ZUR TAG-SELEKTION                                      *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.02.2009                                   *
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
                  ASSIGN       TO  WORKSTATION-HMD0007DE
                  ORGANIZATION IS  TRANSACTION
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY
            LABEL RECORDS ARE OMITTED.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF HMD0007DE.
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
           COPY  DDS-FMT02-O OF  HMD0007DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  HMD0007DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  HMD0007DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  HMD0007DE
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

       01  PGM-WRK                       PIC X(10) VALUE "HMP0007".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

     ‚* INTERNE TABELLE
       01  SELTAG-TAB                    TYPE SELTAG-TYPE.

       01  KZ-WRK                        PIC X(1).
       01  TAG-WRK                       LIKE PTTAG   OF PTAGLIB-COPY.
       01  AKT-POS                       LIKE TAB-MAX OF SELTAG-TAB.

       01  AKTION                        PIC 9(1).
       01  AUSWAHL                       PIC X(2).
       01  ANZEIGE-TYP                   PIC 9(1).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-SELTAG                      TYPE SELTAG-TYPE.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-SELTAG
                                         X-F12.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP0007" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "HMD0007DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           INITIALIZE SELTAG-TAB, X-F12.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE F3, F12.
           MOVE     X-SELTAG TO SELTAG-TAB.

       ANF010.
           MOVE     ZEROES TO AKTION.
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

     ‚* BEI EINER EINGABE ZURÜCK
           IF       AKTION NOT = ZEROES
                    GO TO ANF010
           END-IF.

       ENDE.
           MOVE     SELTAG-TAB TO X-SELTAG.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO LES-DAT-90
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
           IF       TAG-KZ OF SELTAG-TAB(TAB-POS OF SELTAG-TAB) = "9"
                    MOVE AN  TO IN42 OF INXX
           ELSE
                    MOVE AUS TO IN42 OF INXX
           END-IF.
           MOVE     PTTAG  OF SELTAG-TAB(TAB-POS OF SELTAG-TAB) TO
                    PTTAG  OF FORMAT2.

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
           MOVE     AUS    TO INXX.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

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

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* ANLEGEN EINES NEUEN TAGS?
           PERFORM  ANLEGEN-TAG.

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

     ‚* LÖSCHEN DES TAGS
           IF       AUSW OF FORMAT2 = "4"
                    MOVE     1 TO AKTION
                    PERFORM DELETE-TAG
                    GO TO ANZ-SFL-90
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

     ‚* ANLEGEN EINES NEUEN TAGS?
           PERFORM  ANLEGEN-TAG.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN EINES NEUEN TAGS
     ‚*--------------------------------------------------------------
       ANLEGEN-TAG SECTION.
       ANL-TAG-00.

           INITIALIZE ERRFLD-WRK.
     ‚* WENN KEIN TAG EINGEGEBEN WURDE, DANN ROUTINE BEENDEN
           IF       PTTAG  OF FORMAT3 = SPACES
                    GO TO ANL-TAG-90
           END-IF.

           MOVE     1 TO AKTION.
     ‚* GIBT ES DEN TAG SCHON? DANN DIESEN ZURÜCKGEBEN
           PERFORM  CHK-SELTAB.
           IF       RET-CODE NOT = SPACES
                    GO TO ANL-TAG-90
           ELSE
                    PERFORM ADD-SELTAB
           END-IF.

       ANL-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN EINES TAGS
     ‚*--------------------------------------------------------------
       DELETE-TAG SECTION.
       DEL-TAG-00.

     ‚* TAG LEER? DANN NICHTS MACHEN
           IF       PTTAG  OF FORMAT2 = SPACES
                    GO TO DEL-TAG-90
           END-IF.

     ‚* IST DIE TABELLE LEER, DANN SOFORT NEUEN ANLEGEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO DEL-TAG-90
           END-IF.

           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.

       DEL-TAG-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO DEL-TAG-90
           END-IF.

     ‚* TAG GEFUNDEN? DANN LÖSCHEN.
           IF       PTTAG  OF SELTAG-TAB(TAB-POS OF SELTAG-TAB) NOT =
                    PTTAG  OF FORMAT2
                    GO TO DEL-TAG-20
           END-IF.

           MOVE     SPACES TO
                    PTTAG  OF SELTAG-TAB(TAB-POS OF SELTAG-TAB).

     ‚* TABELLE NEU ORDNEN
           PERFORM  TABELLE-ORDNEN.

       DEL-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TABELLE ORDNEN
     ‚*--------------------------------------------------------------
       TABELLE-ORDNEN SECTION.
       TAB-ORD-00.

     ‚* IST DIE TABELLE LEER, DANN SOFORT NEUEN ANLEGEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO TAB-ORD-90
           END-IF.

           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.

       TAB-ORD-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO TAB-ORD-90
           END-IF.

     ‚* IST DER TAG GEFÜLLT?
           IF       PTTAG  OF SELTAG-TAB(TAB-POS OF SELTAG-TAB) =
                    SPACES
                    PERFORM SUCHEN-TAG
                    SUBTRACT 1 FROM TAB-MAX OF SELTAG-TAB
                    IF   TAG-WRK NOT = SPACES
                         MOVE TAG-WRK TO PTTAG OF SELTAG-TAB(TAB-POS OF
                                         SELTAG-TAB)
                         MOVE KZ-WRK TO TAG-KZ OF SELTAG-TAB(TAB-POS OF
                                         SELTAG-TAB)
                         GO TO TAB-ORD-00
                    ELSE
                         GO TO TAB-ORD-90
                    END-IF
           END-IF.

           GO TO    TAB-ORD-20.

       TAB-ORD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHEN EINES TAGS VOM ENDE DER TABELLE
     ‚*--------------------------------------------------------------
       SUCHEN-TAG SECTION.
       SUC-TAG-00.

           INITIALIZE TAG-WRK, KZ-WRK.
           MOVE     TAB-MAX OF SELTAG-TAB TO AKT-POS.

       SUC-TAG-20.
           IF       AKT-POS <= TAB-POS OF SELTAG-TAB
                    GO TO SUC-TAG-90
           END-IF.

     ‚* IST DER TAG GEFÜLLT?
           IF       PTTAG  OF SELTAG-TAB(AKT-POS) NOT = SPACES
                    MOVE PTTAG  OF SELTAG-TAB(AKT-POS) TO TAG-WRK
                    MOVE TAG-KZ OF SELTAG-TAB(AKT-POS) TO KZ-WRK
                    MOVE SPACES TO PTTAG  OF SELTAG-TAB(AKT-POS)
                    GO TO SUC-TAG-90
           END-IF.

           SUBTRACT 1 FROM AKT-POS.
           GO TO    SUC-TAG-20.

       SUC-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES DATENSATZES IN DER INT. TABELLE
     ‚*--------------------------------------------------------------
       ADD-SELTAB SECTION.
       ADD-STB-00.

     ‚* TAG LEER? DANN NICHTS MACHEN
           IF       PTTAG  OF FORMAT3 = SPACES
                    GO TO ADD-STB-90
           END-IF.

     ‚* IST DIE TABELLE LEER, DANN SOFORT NEUEN ANLEGEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO ADD-STB-80
           END-IF.

           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.

       ADD-STB-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO ADD-STB-80
           END-IF.

     ‚* TAG SCHON VORHANDEN? DANN NICHTS MACHEN
           IF       PTTAG  OF SELTAG-TAB(TAB-POS OF SELTAG-TAB) =
                    PTTAG  OF FORMAT3
                    GO TO ADD-STB-90
           END-IF.

           GO TO    ADD-STB-20.

       ADD-STB-80.
           ADD      1    TO TAB-MAX OF SELTAG-TAB.
           MOVE     PTTAG  OF FORMAT3 TO PTTAG  OF
                    SELTAG-TAB(TAB-MAX OF SELTAG-TAB).
           MOVE     "1"               TO TAG-KZ OF
                    SELTAG-TAB(TAB-MAX OF SELTAG-TAB).

       ADD-STB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TAG-TABELLE ZUR SELEKTION
     ‚*--------------------------------------------------------------
       DEL-SELTAB SECTION.
       DEL-STB-00.

     ‚* IST DIE TABELLE LEER, DANN MUSS NICHTS MEHR GEMACHT WERDEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO DEL-STB-90
           END-IF.

           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.

       DEL-STB-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO DEL-STB-90
           END-IF.

     ‚* INITIALISIEREN DER WERTE
           MOVE     SPACES TO PTTAG    OF
                              SELTAG-TAB(TAB-POS OF SELTAG-TAB).

           GO TO    DEL-STB-20.

       DEL-STB-90.
           INITIALIZE SELTAG-TAB.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER INT. TABELLE
     ‚*--------------------------------------------------------------
       CHK-SELTAB SECTION.
       CHK-STB-00.

           INITIALIZE RET-CODE.
     ‚* IST DIE TABELLE LEER, DANN MUSS NICHTS MEHR GEMACHT WERDEN
           IF       TAB-MAX OF SELTAG-TAB = ZEROES
                    GO TO CHK-STB-90
           END-IF.

           MOVE     ZEROES TO TAB-POS OF SELTAG-TAB.

       CHK-STB-20.
           ADD      1 TO TAB-POS OF SELTAG-TAB.
           IF       TAB-POS OF SELTAG-TAB > TAB-MAX OF SELTAG-TAB
                    GO TO CHK-STB-90
           END-IF.

           IF       PTTAG  OF SELTAG-TAB(TAB-POS OF SELTAG-TAB) =
                    PTTAG  OF FORMAT3
                    MOVE "1" TO RET-CODE
                    GO TO CHK-STB-90
           END-IF.

           GO TO    CHK-STB-20.

       CHK-STB-90.
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
