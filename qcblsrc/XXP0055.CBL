       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0055.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* BERICHTSHEFTDATEN                                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.01.2014                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANLEGEN VON BERICHTSHEFTDATEN                *
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
                  ASSIGN       TO  WORKSTATION-XXD0055DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

     ‚*-----BILDSCHIRMDATEI -------------------------
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF XXD0055DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP0055".
       01  PGM-TYP                        PIC X(8)  VALUE "PLANUNG".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0055DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0055DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  KW-WRK                    LIKE VONKW  OF FORMAT1.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0055" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0055DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.

           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* ERMITTELN DATUM/ZEIT
           PERFORM  COPY-GET-TIME.
     ‚* DEFAULTDATEN
           MOVE     "BERICHTSHEFT"  TO CFKEY  OF FORMAT1.
           MOVE     T-USER          TO CFDAT1  OF FORMAT1.
           MOVE     DATE-8(1:4)     TO CFNM04 OF FORMAT1.
           MOVE     DATE-8(1:4)     TO JAHR   OF FORMAT1.
           MOVE     1               TO VONKW  OF FORMAT1.
           MOVE     52              TO BISKW  OF FORMAT1.

     ‚* ÜBERSCHRIFT
       ANF010.
           MOVE     "XXP0055*1"     TO MAINUEB OF FORMAT1.

           MOVE     "FMT01"         TO FMT-WRK.
           MOVE     FORMAT1         TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB ES DIE GRUPPE GIBT
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H051"              TO CFID   OF CFG-CPY.
           MOVE     CFKEY  OF FORMAT1   TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
     ˆ* TODO: FEHLERMELDUNG
                    GO TO ANF010
           END-IF.

     ‚* PRÜFEN OB KW-EINGABE SINNVOLL IST
           IF       VONKW  OF FORMAT1 = ZEROES
            OR      BISKW  OF FORMAT1 < VONKW  OF FORMAT1
     ˆ* TODO: FEHLERMELDUNG
                    GO TO ANF010
           END-IF.

     ‚* NUN FÜR JEDE KW AUFRUFEN
           MOVE     VONKW  OF FORMAT1 TO KW-WRK.
           IF       BISKW  OF FORMAT1 = ZEROES
                    MOVE VONKW  OF FORMAT1 TO BISKW  OF FORMAT1
           END-IF.

     ‚* BERICHTSHEFTDATEN NUN ANLEGEN
           PERFORM  UNTIL KW-WRK > BISKW  OF FORMAT1
                    CALL "XXP0012" USING CFKEY  OF FORMAT1
                                         KW-WRK
                                         JAHR   OF FORMAT1
                                         CFDAT1 OF FORMAT1
                                         CFNM04 OF FORMAT1
                    END-CALL
                    ADD 1 TO KW-WRK
           END-PERFORM.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
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
