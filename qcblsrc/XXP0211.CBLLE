       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0211.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE KAFFEEMASCHINENSTATUS                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  17.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE STATUS                               *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-XXD0211DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

     ‚*----DISPLAY
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF XXD0211DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN40                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN43                          PIC 1.
        05 IN44                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0211DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0211DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF XXD0211DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF XXD0211DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0211".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  ANZMODE-WRK                   PIC 9(1).
       01  EDTMODE-WRK                   PIC 9(1).
       01  EUP-TXT                       LIKE CFIN21 OF CFG-CPY.
       01  IAW-TXT                       LIKE CFIN21 OF CFG-CPY.

     ‚* PARAMETER FÜR XXP0211
       01  KEY-WRK                       LIKE CFKEY  OF CFG-CPY.
       01  FK01-WRK                      LIKE CFFK01 OF CFG-CPY.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0211" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0211DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.
           INITIALIZE FORMAT2.
           INITIALIZE EUP-TXT, IAW-TXT.
           INITIALIZE ANZMODE-WRK.
           INITIALIZE EDTMODE-WRK.

       ANF010.
           INITIALIZE KEY-WRK, FK01-WRK.

     ‚* AKTUELLE WERTE FÜR EUP ERMITTELN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "K111"                TO CFID   OF CFG-CPY.
           MOVE     "EUP"                 TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
                    MOVE CFFK01 OF CFG-CPY TO EUP_NUM  OF FORMAT1
                    MOVE CFMSER OF CFG-CPY TO EUP_USER OF FORMAT1
                    MOVE CFMDTA OF CFG-CPY TO DT_EUP   OF FORMAT1
                    MOVE CFMTIA OF CFG-CPY TO TI_EUP   OF FORMAT1
                    MOVE CFIN21 OF CFG-CPY TO EUP_TXT  OF FORMAT2
                    MOVE CFIN21 OF CFG-CPY TO EUP_TXE  OF FORMAT2
           END-IF.

     ‚* JETZT IAW-KAFFEEMASCHINE
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "K111"                TO CFID   OF CFG-CPY.
           MOVE     "IAW"                 TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
                    MOVE CFFK01 OF CFG-CPY TO IAW_NUM  OF FORMAT1
                    MOVE CFMSER OF CFG-CPY TO IAW_USER OF FORMAT1
                    MOVE CFMDTA OF CFG-CPY TO DT_IAW   OF FORMAT1
                    MOVE CFMTIA OF CFG-CPY TO TI_IAW   OF FORMAT1
                    MOVE CFIN21 OF CFG-CPY TO IAW_TXT  OF FORMAT2
                    MOVE CFIN21 OF CFG-CPY TO IAW_TXE  OF FORMAT2
           END-IF.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0211"       TO MAINUEB OF FORMAT1.

     ‚* FARBE SETZEN EUP
           EVALUATE EUP_NUM OF FORMAT1
                    WHEN 4      MOVE AN  TO IN40 OF INXX
                                MOVE AUS TO IN41 OF INXX
                                MOVE "PGM0028" TO CPY-TXID
                    WHEN 2      MOVE AUS TO IN40 OF INXX
                                MOVE AN  TO IN41 OF INXX
                                MOVE "PGM0029" TO CPY-TXID
                    WHEN 1      MOVE AUS TO IN40 OF INXX
                                MOVE AUS TO IN41 OF INXX
                                MOVE "PGM0027" TO CPY-TXID
           END-EVALUATE.

     ‚* TEXT NUN LESEN
           MOVE     SPRACHE-WRK              TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT               TO EUP_STAT OF FORMAT1.

     ‚* FARBE SETZEN IAW
           EVALUATE IAW_NUM OF FORMAT1
                    WHEN 4      MOVE AN  TO IN42 OF INXX
                                MOVE AUS TO IN43 OF INXX
                                MOVE "PGM0028" TO CPY-TXID
                    WHEN 2      MOVE AUS TO IN42 OF INXX
                                MOVE AN  TO IN43 OF INXX
                                MOVE "PGM0029" TO CPY-TXID
                    WHEN 1      MOVE AUS TO IN42 OF INXX
                                MOVE AUS TO IN43 OF INXX
                                MOVE "PGM0027" TO CPY-TXID
           END-EVALUATE.

     ‚* TEXT NUN LESEN
           MOVE     SPRACHE-WRK              TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT               TO IAW_STAT OF FORMAT1.

     ‚* EDIT-MODE?
           EVALUATE EDTMODE-WRK
                    WHEN ZEROES MOVE AUS TO IN44 OF INXX
                    WHEN 1      MOVE AN  TO IN44 OF INXX
           END-EVALUATE.

     ‚* NUN PRÜFEN WELCHES FORMAT ANGEZEIGT WIRD
           EVALUATE ANZMODE-WRK
                    WHEN ZEROES PERFORM SHOW-SMALL-WINDOW
                    WHEN 1      PERFORM SHOW-LARGE-WINDOW
                    WHEN OTHER  PERFORM SHOW-SMALL-WINDOW
           END-EVALUATE.

     ‚* F2 EUP AKTIV/DEFEKT
           IF       F2 = "1"
                    IF   EUP_NUM OF FORMAT1 = 1
                         MOVE 4      TO FK01-WRK
                    ELSE
                         MOVE 1      TO FK01-WRK
                    END-IF
     ‚*             STATUS JETZT AKTUALISIEREN
                    MOVE    "EUP" TO KEY-WRK
                    PERFORM UPDATE-STATUS
           END-IF.

     ‚* F3 ENDE ?
           IF       F3 = "1"
                    GO TO ENDE
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       F5 = "1"
                    GO TO ANF010
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* F14 IAW AKTIV/DEFEKT
           IF       F14 = "1"
                    IF   IAW_NUM OF FORMAT1 = 1
                         MOVE 4      TO FK01-WRK
                    ELSE
                         MOVE 1      TO FK01-WRK
                    END-IF
     ‚*             STATUS JETZT AKTUALISIEREN
                    MOVE    "IAW" TO KEY-WRK
                    PERFORM UPDATE-STATUS
           END-IF.

     ‚* F17 EUP KD AKTIV
           IF       F17 = "1"
                    IF   EUP_NUM OF FORMAT1 = 2
                         MOVE 1      TO FK01-WRK
                    ELSE
                         MOVE 2      TO FK01-WRK
                    END-IF
     ‚*             STATUS JETZT AKTUALISIEREN
                    MOVE    "EUP" TO KEY-WRK
                    PERFORM UPDATE-STATUS
           END-IF.

     ‚* F18 IAW KD AKTIV
           IF       F18 = "1"
                    IF   IAW_NUM OF FORMAT1 = 2
                         MOVE 1      TO FK01-WRK
                    ELSE
                         MOVE 2      TO FK01-WRK
                    END-IF
     ‚*             STATUS JETZT AKTUALISIEREN
                    MOVE    "IAW" TO KEY-WRK
                    PERFORM UPDATE-STATUS
           END-IF.

     ‚* F22 ANZEIGE WECHSELN
           IF       F22 = "1"
                    EVALUATE ANZMODE-WRK
                             WHEN ZEROES MOVE 1      TO ANZMODE-WRK
                             WHEN 1      MOVE ZEROES TO ANZMODE-WRK
                                         MOVE ZEROES TO EDTMODE-WRK
                    END-EVALUATE
           END-IF.

     ‚* F23 TEXT EDITIEREN/SPEICHERN
           IF       F23 = "1"
                    EVALUATE EDTMODE-WRK
                             WHEN ZEROES MOVE 1      TO EDTMODE-WRK
                             WHEN 1      MOVE ZEROES TO EDTMODE-WRK
                                  PERFORM UPDATE-STATUS
                    END-EVALUATE
           END-IF.

     ‚* TEXT BEI SCHUSS AKTIVIEREN
           IF       EDTMODE-WRK     = 1
            AND     F2          NOT = "1"
            AND     F3          NOT = "1"
            AND     F5          NOT = "1"
            AND     F12         NOT = "1"
            AND     F14         NOT = "1"
            AND     F17         NOT = "1"
            AND     F18         NOT = "1"
            AND     F22         NOT = "1"
            AND     F23         NOT = "1"
                    PERFORM UPDATE-STATUS
           END-IF.

           GO TO    ANF010.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* KLEINES WINDOW OHNE TEXT ANZEIGEN
     ‚*--------------------------------------------------------------
       SHOW-SMALL-WINDOW SECTION.
       SHW-SWD-00.

           INITIALIZE F-KEYS.
     ‚* RAHMEN ANZEIGEN
           MOVE     "FMT03"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           MOVE     AUS TO INXX.

     ‚* F2 EUP AKTIV/DEFEKT
           IF       IN02 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F2
           END-IF.

     ‚* F3 ENDE ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F5
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
           END-IF.

     ‚* F14 IAW AKTIV/DEFEKT
           IF       IN14 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F14
           END-IF.

     ‚* F17 EUP KD AKTIV
           IF       IN17 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F17
           END-IF.

     ‚* F18 IAW KD AKTIV
           IF       IN18 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F18
           END-IF.

     ‚* F22 STATUS ANZEIGE WECHSELN
           IF       IN22 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F22
           END-IF.

       SHW-SWD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SHOW-LARGE-WINDOW
     ‚*--------------------------------------------------------------
       SHOW-LARGE-WINDOW SECTION.
       SHW-LDW-00.

           INITIALIZE F-KEYS.
           MOVE     CORR FORMAT1 TO FORMAT2.
     ‚* RAHMEN ANZEIGEN
           MOVE     "FMT04"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     CORR INXX       TO  FORMAT2-O-INDIC.
           MOVE     "FMT02"         TO  FMT-WRK.
           MOVE     FORMAT2         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT2-I.
           MOVE     CORR FORMAT2-I TO FORMAT2.
           MOVE     AUS TO INXX.

     ‚* F2 EUP AKTIV/DEFEKT
           IF       IN02 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F2
           END-IF.

     ‚* F3 ENDE ?
           IF       IN03 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F3
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F5
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F12
           END-IF.

     ‚* F14 IAW AKTIV/DEFEKT
           IF       IN14 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F14
           END-IF.

     ‚* F17 EUP KD AKTIV
           IF       IN17 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F17
           END-IF.

     ‚* F18 IAW KD AKTIV
           IF       IN18 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F18
           END-IF.

     ‚* F22 STATUS ANZEIGE WECHSELN
           IF       IN22 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F22
           END-IF.

     ‚* F23 EDITIERMODUS
           IF       IN23 OF FORMAT2-I-INDIC  = AN
                    MOVE "1" TO F23
           END-IF.

     ‚* TEXTE SPEICHERN
           MOVE     EUP_TXE OF FORMAT2 TO EUP-TXT.
           MOVE     IAW_TXE OF FORMAT2 TO IAW-TXT.

       SHW-LDW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STATUS AKTUALISIEREN
     ‚*--------------------------------------------------------------
       UPDATE-STATUS SECTION.
       UPD-STS-00.

           CALL     "XXP0212" USING KEY-WRK
                                    FK01-WRK
                                    EUP-TXT
                                    IAW-TXT
           END-CALL.

       UPD-STA-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
