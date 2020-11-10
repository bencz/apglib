       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9008.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE WINDOW DAS QRPLOBJ IM STACK IST                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  18.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  WINDOW ZUR ANZEIGE QRPLOBJ                   *
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
                  ASSIGN       TO  WORKSTATION-CFD9008DE
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
            COPY DDS-ALL-FORMATS OF CFD9008DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9008".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CFD9008DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF CFD9008DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  DISPLAY-OPENED                PIC 9(1).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-NAPGM                       PIC X(10).
       01  X-NALIB                       PIC X(10).
       01  X-OLPGM                       PIC X(10).
       01  X-OLLIB                       PIC X(10).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-NAPGM
                                X-NALIB
                                X-OLPGM
                                X-OLLIB.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE DISPLAY-OPENED.
     ‚* SOLL DAS WINDOW ANGEZEIGT WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H000"              TO CFID   OF CFG-CPY.
           MOVE     X-NAPGM             TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
                    IF   CFFK01 OF CFG-CPY NOT = ZEROES
                         GO TO ENDE
                    END-IF
           END-IF.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.
           MOVE     1     TO DISPLAY-OPENED.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.
           MOVE     X-NAPGM           TO NAPGM  OF FORMAT1.
           MOVE     X-NALIB           TO NALIB  OF FORMAT1.
           MOVE     X-OLPGM           TO OLPGM  OF FORMAT1.
           MOVE     X-OLLIB           TO OLLIB  OF FORMAT1.

           MOVE     CORR FORMAT1      TO FMT01-O.
           WRITE    WS-REC     FORMAT IS "FMT02".
           WRITE    WS-REC     FORMAT IS "FMT01".
           READ     WS-DISPLAY FORMAT IS "FMT01".
           MOVE     CORR FMT01-I      TO FORMAT1.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FMT01-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FMT01-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

       ENDE.
           IF       DISPLAY-OPENED NOT = ZEROES
                    CLOSE WS-DISPLAY
           END-IF.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
