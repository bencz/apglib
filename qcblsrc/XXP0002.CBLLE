       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DISPLAYTEST                                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DISPLAY-TESTS                                *
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
                  ASSIGN       TO  WORKSTATION-XXD0002DE
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
           COPY DDS-ALL-FORMATS OF XXD0002DE.
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
        05 IN75                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0002DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0002DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0002DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0002DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0002DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0002DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0002".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".
       01  AUSWAHL                       PIC X(2).
       01  CHOICE-WRK                    PIC 9(2).

       01  ZEILE-ANZEIGEN                PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0002DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.
           INITIALIZE CHOICE-WRK.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DER TAGS BZW. PROGRAMMNAME
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* JE NACH AUSWAHL IN VERSCHIEDENE SELEKTIONEN VERZWEIGEN
           EVALUATE CHOICE-WRK
                    WHEN 3     INITIALIZE ZEILE-ANZEIGEN
                               PERFORM  ANZEIGE-SUBFILE
                    WHEN 99    GO TO ENDE
                    WHEN OTHER GO TO ANF010
           END-EVALUATE.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF010.

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
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "XXP0002"         TO MAINUEB OF FORMAT1.
           MOVE     "XXP0002*1"       TO SUBUEB  OF FORMAT1.

      *
           MOVE     "TEST" TO C_TEST OF FORMAT1.
           MOVE     "https://support.epg.com/browse/LFSSTDLOG-13051"
             TO     URLVAR OF FORMAT1.

       VOR-AUS-10.
           PERFORM  FILL-HEX.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

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

     ‚* GEÄNDERT?
           IF       IN51 OF FORMAT1-I-INDIC  = AN
                    MOVE "CHG"  TO AENDERN OF FORMAT1
           ELSE
                    MOVE SPACES TO AENDERN OF FORMAT1
           END-IF.

           MOVE     WAHL OF FORMAT1 TO CHOICE-WRK.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
           INITIALIZE ANZREC-WRK.
           MOVE     AUS    TO INXX.
           MOVE     1      TO SFL-ZEILE.

       ANZ-SFL-20.
           INITIALIZE DATEN.
           IF       ZEILE-ANZEIGEN = 1
                    PERFORM AUSGABE-SFL
           ELSE
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

           MOVE     AN TO IN75 OF INXX.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

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
           IF       DATEN = ZEROES
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT05" TO FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE  AUS TO IN91 OF INXX
           ELSE
                    MOVE  AN  TO IN91 OF INXX
           END-IF.

           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
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
                    PERFORM COPY-DISPLAY-CURSER
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN20 IN FORMAT3-I-INDIC = AN
                    EVALUATE ZEILE-ANZEIGEN
                             WHEN ZEROES MOVE 1      TO ZEILE-ANZEIGEN
                             WHEN 1      MOVE ZEROES TO ZEILE-ANZEIGEN
                    END-EVALUATE
                    GO TO ANZ-SFL-00
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
                    MOVE    ZEROES  TO ANZREC-WRK
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
           MOVE     "TEST" TO TESTFELD OF FORMAT2.

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
     ‚* HEX-WERTE FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-HEX SECTION.
       FIL-HEX-00.

     ‚* 4*
           MOVE     X"404142434445464748494A4B4C4D4E4F" TO
                    HEX4 OF FORMAT1.
     ‚* 5*
           MOVE     X"505152535455565758595A5B5C5D5E5F" TO
                    HEX5 OF FORMAT1.
     ‚* 6*
           MOVE     X"606162636465666768696A6B6C6D6E6F" TO
                    HEX6 OF FORMAT1.
     ‚* 7*
           MOVE     X"707172737475767778797A7B7C7D7E7F" TO
                    HEX7 OF FORMAT1.
     ‚* 8*
           MOVE     X"808182838485868788898A8B8C8D8E8F" TO
                    HEX8 OF FORMAT1.
     ‚* 9*
           MOVE     X"909192939495969798999A9B9C9D9E9F" TO
                    HEX9 OF FORMAT1.
     ‚* A*
           MOVE     X"A0A1A2A3A4A5A6A7A8A9AAABACADAEAF" TO
                    HEXA OF FORMAT1.
     ‚* B*
           MOVE     X"B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF" TO
                    HEXB OF FORMAT1.
     ‚* C*
           MOVE     X"C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF" TO
                    HEXC OF FORMAT1.
     ‚* D*
           MOVE     X"D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF" TO
                    HEXD OF FORMAT1.
     ‚* E*
           MOVE     X"E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF" TO
                    HEXE OF FORMAT1.
     ‚* F*
           MOVE     X"F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF" TO
                    HEXF OF FORMAT1.

       FIL-HEX-90.
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
