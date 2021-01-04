       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LED-TELEGRAMMANALYSE                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.05.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANALYSE LED-TELEGRAMM                        *
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
                  ASSIGN       TO  WORKSTATION-XXD0001DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF XXD0001DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN51                          PIC 1.
        05 IN52                          PIC 1.
        05 IN53                          PIC 1.
        05 IN54                          PIC 1.
        05 IN55                          PIC 1.
        05 IN56                          PIC 1.
        05 IN57                          PIC 1.
        05 IN58                          PIC 1.
        05 IN59                          PIC 1.
        05 IN60                          PIC 1.
        05 IN61                          PIC 1.
        05 IN62                          PIC 1.
        05 IN63                          PIC 1.
        05 IN64                          PIC 1.
        05 IN65                          PIC 1.
        05 IN66                          PIC 1.
        05 IN67                          PIC 1.
        05 IN68                          PIC 1.
        05 IN69                          PIC 1.
        05 IN70                          PIC 1.
        05 IN71                          PIC 1.
        05 IN72                          PIC 1.
        05 IN73                          PIC 1.
        05 IN74                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0001DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0001DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0001DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0001DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0001DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0001DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  FORMAT7X.
           COPY  DDS-FMT07-O OF  XXD0001DE
           REPLACING FMT07-O BY FORMAT7
                     FMT07-O-INDIC BY FORMAT7-O-INDIC.

       01  FORMAT7Y.
           COPY  DDS-FMT07-I OF  XXD0001DE
           REPLACING FMT07-I BY FORMAT7-I
                     FMT07-I-INDIC BY FORMAT7-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0001".
       01  PGM-TYP                       PIC X(8)  VALUE "LED".
       01  AUSWAHL                       PIC X(2).

       01  LEDTLG-WRK                    PIC X(35).

       01  LED                           PIC 9(3).
       01  POS                           PIC 9(4).

       01  LED-LEISTE.
        05 LEDNR1                        PIC 9(1).
        05 LEDNR2                        PIC 9(1).
        05 LEDNR3                        PIC 9(1).
        05 LEDNR4                        PIC 9(1).
        05 LEDNR5                        PIC 9(1).
        05 LEDNR6                        PIC 9(1).
        05 LEDNR7                        PIC 9(1).
        05 LEDNR8                        PIC 9(1).
        05 LEDNR9                        PIC 9(1).
        05 LEDNR10                       PIC 9(1).
        05 LEDNR11                       PIC 9(1).
        05 LEDNR12                       PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0001DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DER TAGS BZW. PROGRAMMNAME
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* TELEGRAMM ÜBERNEHMEN
           MOVE     LEDTLG OF FORMAT1 TO LEDTLG-WRK.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN ANHAND DER TAG-DATEI
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

     ‚* ANZEIGEMODUS AUSWÄHLEN
           EVALUATE MODUS OF FORMAT1
                    WHEN ZEROES
     ‚*                  ANZEIGEN SUBFILE
                         PERFORM  ANZEIGE-SUBFILE
                    WHEN 2
     ‚*                  SORTIERREGAL SIMULIEREN
                         PERFORM  ANZEIGE-SORTIERREGAL
           END-EVALUATE.
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

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SELEKTIEREN ALLER NÖTIGEN DATEN
     ‚*--------------------------------------------------------------
       VORAUSWAHL SECTION.
       VOR-AUS-00.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "XXP0001"       TO MAINUEB OF FORMAT1.
           MOVE     "XXP0001*1"     TO SUBUEB  OF FORMAT1.

       VOR-AUS-10.
           MOVE     PGM-TYP         TO PGMTYP OF FORMAT1.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

     ‚* TELEGRAMM VORHANDEN?
           IF       LEDTLG OF FORMAT1 = SPACES
                    GO TO VOR-AUS-10
           END-IF.

##APG‚* DERZEIT GEHTS NICHT ...
##APG      IF       MODUS OF FORMAT1 = 1
##APG               GO TO VOR-AUS-10
##APG      END-IF.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

     ‚* +++LEDXZZAZZZZZZZZZZZZZZZZZZZZAX---
     ‚* 1...5....0....5....0....5....0....5
     ‚*          10        20        30

           INITIALIZE DATEN.
           INITIALIZE LED-LEISTE.
           MOVE     8 TO POS.
           MOVE     1 TO LED.

       LES-DAT-20.
           IF       POS > 31
                    GO TO LES-DAT-90
           END-IF.

           INITIALIZE FORMAT2.
           EVALUATE LEDTLG-WRK(POS:1)
     ‚*             BEIDE LEDS AUS
                    WHEN "Z"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE SPACES     TO KURZSTS OF FORMAT2
                         MOVE "AUS"      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE SPACES     TO KURZSTS OF FORMAT2
                         MOVE "AUS"      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*             ERSTE LED AN, ZWEITE AUS
                    WHEN "D"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "1"        TO KURZSTS OF FORMAT2
                         MOVE "AN "      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE SPACES     TO KURZSTS OF FORMAT2
                         MOVE "AUS"      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*             ERSTE LED AUS, ZWEITE AN
                    WHEN "A"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE SPACES     TO KURZSTS OF FORMAT2
                         MOVE "AUS"      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "1"        TO KURZSTS OF FORMAT2
                         MOVE "AN "      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*             ERSTE LED BLINKT, ZWEITE AUS
                    WHEN "H"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "2"        TO KURZSTS OF FORMAT2
                         MOVE "BLINKEN"  TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE SPACES     TO KURZSTS OF FORMAT2
                         MOVE "AUS"      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*             ERSTE LED AUS, ZWEITE BLINKT
                    WHEN "B"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE SPACES     TO KURZSTS OF FORMAT2
                         MOVE "AUS"      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "2"        TO KURZSTS OF FORMAT2
                         MOVE "BLINKEN"  TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*             ERSTE LED AN, ZWEITE AN
                    WHEN "E"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "1"        TO KURZSTS OF FORMAT2
                         MOVE "AN "      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "1"        TO KURZSTS OF FORMAT2
                         MOVE "AN "      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*             ERSTE LED AN, ZWEITE BLINKT
                    WHEN "F"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "1"        TO KURZSTS OF FORMAT2
                         MOVE "AN "      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "2"        TO KURZSTS OF FORMAT2
                         MOVE "BLINKEN"  TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*             ERSTE LED BLINKT, ZWEITE AN
                    WHEN "I"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "2"        TO KURZSTS OF FORMAT2
                         MOVE "BLINKEN"  TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "1"        TO KURZSTS OF FORMAT2
                         MOVE "AN "      TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*             ERSTE LED BLINKT, ZWEITE BLINKT
                    WHEN "J"
     ‚*                  ERSTE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "2"        TO KURZSTS OF FORMAT2
                         MOVE "BLINKEN"  TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
     ‚*                  ZWEITE LED
                         MOVE LED        TO LEDNR   OF FORMAT2
                         MOVE "2"        TO KURZSTS OF FORMAT2
                         MOVE "BLINKEN"  TO LANGSTS OF FORMAT2
                         PERFORM  AUSGABE-SFL
                         ADD      1 TO LED
           END-EVALUATE.

           ADD      1 TO POS.
           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER SFL-ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-SFL SECTION.
       AUS-SFL-00.

     ‚* SUBFILE FÜLLEN ODER LEISTE FÜLLEN
           EVALUATE MODUS OF FORMAT1
                    WHEN ZEROES
     ‚*             SUBFILE-CLEAR
                    IF   DATEN = ZEROES
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
                    END-IF

     ‚*             BLINKENDE LED?
                    IF   KURZSTS OF FORMAT2 = "2"
                         MOVE AN  TO IN41 OF INXX
                    ELSE
                         MOVE AUS TO IN41 OF INXX
                    END-IF

     ‚*             SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
                    MOVE     SPACES       TO AUSW OF FORMAT2
                    MOVE     CORR INXX    TO FORMAT2-O-INDIC
                    ADD      1            TO ANZREC-WRK
                    ADD      1            TO SUBFILE-RECORD-NUMBER
                    MOVE     "FMT02"      TO FMT-WRK
                    MOVE     FORMAT2      TO WSREC-WRK
                    PERFORM  COPY-DISPLAY-SFLWRITE
                    MOVE     AUS TO INXX
     ‚*             LED-LEISTE FÜLLEN
                    WHEN 1
                    WHEN 2
                         MOVE    1 TO DATEN
                         PERFORM FILL-LED-LEISTE
           END-EVALUATE.

       AUS-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
           MOVE     LEDTLG-WRK TO LEDTLG OF FORMAT3.

           MOVE     AUS    TO INXX.
  ******   MOVE     1      TO  SUBFILE-RECORD-NUMBER.

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
           MOVE     AUS  TO INXX

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
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* LEDTLG GEÄNDERT?
           IF       LEDTLG OF FORMAT3 NOT = LEDTLG-WRK
                    MOVE LEDTLG OF FORMAT3 TO LEDTLG-WRK
                    MOVE "1" TO F5
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
     ‚* SORTIERREGAL VON NECKERMANN SIMULIEREN
     ‚*--------------------------------------------------------------
       ANZEIGE-SORTIERREGAL SECTION.
       ANZ-SRT-00.

           INITIALIZE FORMAT7.
           MOVE     LEDTLG-WRK TO LEDTLG OF FORMAT7.

           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "XXP0001"         TO MAINUEB OF FORMAT7.
           MOVE     "XXP0001*7"       TO SUBUEB  OF FORMAT7.

     ‚* MAPPEN DER LEISTE
           PERFORM  MAPPEN-LED-LEISTE.

       ANZ-SRT-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT7.

           MOVE     CORR INXX       TO  FORMAT7-O-INDIC.
           MOVE     "FMT07"         TO  FMT-WRK.
           MOVE     FORMAT7         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT7-I.
           MOVE     CORR FORMAT7-I  TO FORMAT7.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT7-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SRT-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT7-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SRT-90
           END-IF.

     ‚* LEDTLG GEÄNDERT?
           IF       LEDTLG OF FORMAT7 NOT = LEDTLG-WRK
                    MOVE LEDTLG OF FORMAT7 TO LEDTLG-WRK
                    MOVE "1" TO F5
                    GO TO ANZ-SRT-90
           END-IF.

       ANZ-SRT-90.
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
           MOVE     "XXP0001"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0001*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LED-LEISTE FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-LED-LEISTE SECTION.
       FIL-LED-00.

           EVALUATE LED ALSO KURZSTS OF FORMAT2
     ‚*             LED1
                    WHEN 1 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR1  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR1  OF LED-LEISTE
                         END-IF
                    WHEN 2 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR2  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR2  OF LED-LEISTE
                         END-IF
                    WHEN 3 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR3  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR3  OF LED-LEISTE
                         END-IF
                    WHEN 4 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR4  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR4  OF LED-LEISTE
                         END-IF
                    WHEN 5 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR5  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR5  OF LED-LEISTE
                         END-IF
                    WHEN 6 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR6  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR6  OF LED-LEISTE
                         END-IF
                    WHEN 7 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR7  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR7  OF LED-LEISTE
                         END-IF
                    WHEN 8 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR8  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR8  OF LED-LEISTE
                         END-IF
                    WHEN 9 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR9  OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR9  OF LED-LEISTE
                         END-IF
                    WHEN 10 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR10 OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR10 OF LED-LEISTE
                         END-IF
                    WHEN 11 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR11 OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR11 OF LED-LEISTE
                         END-IF
                    WHEN 12 ALSO NOT SPACES
     ‚*                  AN ODER BLINKEN?
                         IF   KURZSTS OF FORMAT2 = "1"
                              MOVE 1 TO LEDNR12 OF LED-LEISTE
                         ELSE
                              MOVE 2 TO LEDNR12 OF LED-LEISTE
                         END-IF
           END-EVALUATE.

       FIL-LED-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAPPEN DER LEISTE AUF INDIKATOREN
     ‚*--------------------------------------------------------------
       MAPPEN-LED-LEISTE SECTION.
       MAP-LEI-00.

     ‚* LED1
           IF       LEDNR1  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN51 OF INXX
           ELSE
                    IF   LEDNR1  OF LED-LEISTE = 1
                         MOVE AN  TO IN51 OF INXX
                         MOVE AUS TO IN63 OF INXX
                    ELSE
                         MOVE AN  TO IN51 OF INXX
                         MOVE AN  TO IN63 OF INXX
                    END-IF
           END-IF.

     ‚* LED2
           IF       LEDNR2  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN52 OF INXX
           ELSE
                    IF   LEDNR2  OF LED-LEISTE = 1
                         MOVE AN  TO IN52 OF INXX
                         MOVE AUS TO IN64 OF INXX
                    ELSE
                         MOVE AN  TO IN52 OF INXX
                         MOVE AN  TO IN64 OF INXX
                    END-IF
           END-IF.

     ‚* LED3
           IF       LEDNR3  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN53 OF INXX
           ELSE
                    IF   LEDNR3  OF LED-LEISTE = 1
                         MOVE AN  TO IN53 OF INXX
                         MOVE AUS TO IN65 OF INXX
                    ELSE
                         MOVE AN  TO IN53 OF INXX
                         MOVE AN  TO IN65 OF INXX
                    END-IF
           END-IF.

     ‚* LED4
           IF       LEDNR4  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN54 OF INXX
           ELSE
                    IF   LEDNR4  OF LED-LEISTE = 1
                         MOVE AN  TO IN54 OF INXX
                         MOVE AUS TO IN66 OF INXX
                    ELSE
                         MOVE AN  TO IN54 OF INXX
                         MOVE AN  TO IN66 OF INXX
                    END-IF
           END-IF.

     ‚* LED5
           IF       LEDNR5  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN55 OF INXX
           ELSE
                    IF   LEDNR5  OF LED-LEISTE = 1
                         MOVE AN  TO IN55 OF INXX
                         MOVE AUS TO IN67 OF INXX
                    ELSE
                         MOVE AN  TO IN55 OF INXX
                         MOVE AN  TO IN67 OF INXX
                    END-IF
           END-IF.

     ‚* LED6
           IF       LEDNR6  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN56 OF INXX
           ELSE
                    IF   LEDNR6  OF LED-LEISTE = 1
                         MOVE AN  TO IN56 OF INXX
                         MOVE AUS TO IN68 OF INXX
                    ELSE
                         MOVE AN  TO IN56 OF INXX
                         MOVE AN  TO IN68 OF INXX
                    END-IF
           END-IF.

     ‚* LED7
           IF       LEDNR7  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN57 OF INXX
           ELSE
                    IF   LEDNR7  OF LED-LEISTE = 1
                         MOVE AN  TO IN57 OF INXX
                         MOVE AUS TO IN69 OF INXX
                    ELSE
                         MOVE AN  TO IN57 OF INXX
                         MOVE AN  TO IN69 OF INXX
                    END-IF
           END-IF.

     ‚* LED8
           IF       LEDNR8  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN58 OF INXX
           ELSE
                    IF   LEDNR8  OF LED-LEISTE = 1
                         MOVE AN  TO IN58 OF INXX
                         MOVE AUS TO IN70 OF INXX
                    ELSE
                         MOVE AN  TO IN58 OF INXX
                         MOVE AN  TO IN70 OF INXX
                    END-IF
           END-IF.

     ‚* LED9
           IF       LEDNR9  OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN59 OF INXX
           ELSE
                    IF   LEDNR9  OF LED-LEISTE = 1
                         MOVE AN  TO IN59 OF INXX
                         MOVE AUS TO IN71 OF INXX
                    ELSE
                         MOVE AN  TO IN59 OF INXX
                         MOVE AN  TO IN71 OF INXX
                    END-IF
           END-IF.

     ‚* LED10
           IF       LEDNR10 OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN60 OF INXX
           ELSE
                    IF   LEDNR10 OF LED-LEISTE = 1
                         MOVE AN  TO IN60 OF INXX
                         MOVE AUS TO IN72 OF INXX
                    ELSE
                         MOVE AN  TO IN60 OF INXX
                         MOVE AN  TO IN72 OF INXX
                    END-IF
           END-IF.

     ‚* LED11
           IF       LEDNR11 OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN61 OF INXX
           ELSE
                    IF   LEDNR11 OF LED-LEISTE = 1
                         MOVE AN  TO IN61 OF INXX
                         MOVE AUS TO IN73 OF INXX
                    ELSE
                         MOVE AN  TO IN61 OF INXX
                         MOVE AN  TO IN73 OF INXX
                    END-IF
           END-IF.

     ‚* LED12
           IF       LEDNR12 OF LED-LEISTE = ZEROES
                    MOVE AUS TO IN62 OF INXX
           ELSE
                    IF   LEDNR12 OF LED-LEISTE = 1
                         MOVE AN  TO IN62 OF INXX
                         MOVE AUS TO IN74 OF INXX
                    ELSE
                         MOVE AN  TO IN62 OF INXX
                         MOVE AN  TO IN74 OF INXX
                    END-IF
           END-IF.

       MAP-LEI-90.
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
