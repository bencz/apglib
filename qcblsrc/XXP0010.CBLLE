       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0010.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* BINÄRE ADDITION                                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BINÄRE ADDITION (SCPGM051)                   *
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
                  ASSIGN       TO  WORKSTATION-XXD0010DE
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
           COPY DDS-ALL-FORMATS OF XXD0010DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0010".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  INXX.
        05 IN70                           PIC 1.
        05 IN71                           PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  XXD0010DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0010DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  POS                           PIC 9(4).
       01  ERG-POS                       LIKE POS.

       01  BIN-VALUE                     LIKE BINERG OF FORMAT1.
       01  BIN-REST                      PIC X(1).
       01  DEZ-VALUE                     LIKE DEZERG OF FORMAT1.
       01  POTENZ                        PIC 9(9).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0010" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0010DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

       ANF010.
           PERFORM  EINGABE-FMT.
           IF       F3  = "1"
            OR      F12 = "1"
                    GO TO ENDE
           END-IF.

           GO TO    ANF010.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES FORMATS
     ‚*--------------------------------------------------------------
       EINGABE-FMT SECTION.
       EIN-FMT-00.

           INITIALIZE F3, F12.
           INITIALIZE FORMAT1.
           MOVE     AUS    TO INXX.
           MOVE     ZEROES TO BINVAL1 OF FORMAT1.
           MOVE     ZEROES TO BINVAL2 OF FORMAT1.

       EIN-FMT-10.
           PERFORM  SFL-HEADER.
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
                    GO TO EIN-FMT-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT1-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO EIN-FMT-90
           END-IF.

     ‚* PRÜFE EINGABE 1
           MOVE     ZEROES TO POS.
           PERFORM  LENGTH OF BINVAL1 OF FORMAT1 TIMES
                    ADD 1 TO POS
                    IF   BINVAL1 OF FORMAT1(POS:1) NOT = "1"
                     AND BINVAL1 OF FORMAT1(POS:1) NOT = "0"
                         MOVE AN TO IN70 OF INXX
                         GO TO EIN-FMT-10
                    END-IF
           END-PERFORM.
     ‚* PRÜFE EINGABE 2
           MOVE     ZEROES TO POS.
           PERFORM  LENGTH OF BINVAL2 OF FORMAT1 TIMES
                    ADD 1 TO POS
                    IF   BINVAL2 OF FORMAT1(POS:1) NOT = "1"
                     AND BINVAL2 OF FORMAT1(POS:1) NOT = "0"
                         MOVE AN TO IN71 OF INXX
                         GO TO EIN-FMT-10
                    END-IF
           END-PERFORM.

     ‚* JETZT BINVAL1 UMRECHNEN
           MOVE     "0"                TO BIN-VALUE(1:1).
           MOVE     BINVAL1 OF FORMAT1 TO BIN-VALUE(2:).
           PERFORM  UMRECHNEN-BINARY.
           MOVE     DEZ-VALUE          TO DEZVAL1 OF FORMAT1.

     ‚* JETZT BINVAL2 UMRECHNEN
           MOVE     "0"                TO BIN-VALUE(1:1).
           MOVE     BINVAL2 OF FORMAT1 TO BIN-VALUE(2:).
           PERFORM  UMRECHNEN-BINARY.
           MOVE     DEZ-VALUE          TO DEZVAL2 OF FORMAT1.

     ‚* JETZT BEIDE ZAHLEN MITEINANDER "ADDIERERN"
           PERFORM  ADD-BINARY.

     ‚* ERGEBNIS DEZIMAL UMRECHNEN
           MOVE     BINERG  OF FORMAT1 TO BIN-VALUE.
           PERFORM  UMRECHNEN-BINARY.
           MOVE     DEZ-VALUE          TO DEZERG OF FORMAT1.

           GO TO    EIN-FMT-10.

       EIN-FMT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INS DEZIMALE UMRECHNEN
     ‚*--------------------------------------------------------------
       UMRECHNEN-BINARY SECTION.
       UMR-BIN-00.

           INITIALIZE DEZ-VALUE.
     ‚* JETZT DIE UMRECHNUNG STARTEN
           MOVE     ZEROES TO POS.
           PERFORM  LENGTH OF BIN-VALUE TIMES
                    ADD 1 TO POS
                    IF   BIN-VALUE(POS:1) = "1"
     ‚*                  POTENZ ERRECHNEN, DA WIR VORNE STARTEN WÄRE
     ‚*                  DIE ERSTE POTENZ 7 UND DIE LETZTE 0
                         COMPUTE POTENZ = LENGTH OF BIN-VALUE - POS
                         END-COMPUTE
     ‚*                  JETZT BERECHNEN
                         COMPUTE DEZ-VALUE =   DEZ-VALUE
                                             + (2 ** POTENZ)
                         END-COMPUTE
                    END-IF
           END-PERFORM.

       UMR-BIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZWEI BINÄRZAHLEN ADDIEREN
     ‚*--------------------------------------------------------------
       ADD-BINARY SECTION.
       ADD-BIN-00.

           INITIALIZE BINERG OF FORMAT1.
           MOVE     "0" TO BIN-REST.
     ‚* JETZT VON HINTEN ANFANGEN ZU ADDIEREN
           MOVE     LENGTH OF BINVAL1 OF FORMAT1 TO POS.
           MOVE     LENGTH OF BINERG  OF FORMAT1 TO ERG-POS.
           PERFORM  WITH TEST BEFORE UNTIL POS = ZEROES
                    EVALUATE BINVAL1 OF FORMAT1(POS:1) ALSO
                             BINVAL2 OF FORMAT1(POS:1) ALSO
                             BIN-REST
     ‚*                      ALLE AN
                             WHEN "1" ALSO "1" ALSO "1"
                                  MOVE "1" TO BINERG OF
                                              FORMAT1(ERG-POS:1)
                                  MOVE "1" TO BIN-REST
     ‚*                      ZWEI AN
                             WHEN "1" ALSO "1" ALSO "0"
                             WHEN "1" ALSO "0" ALSO "1"
                             WHEN "0" ALSO "1" ALSO "1"
                                  MOVE "0" TO BINERG OF
                                              FORMAT1(ERG-POS:1)
                                  MOVE "1" TO BIN-REST
     ‚*                      EINS AN
                             WHEN "1" ALSO "0" ALSO "0"
                             WHEN "0" ALSO "1" ALSO "0"
                             WHEN "0" ALSO "0" ALSO "1"
                                  MOVE "1" TO BINERG OF
                                              FORMAT1(ERG-POS:1)
                                  MOVE "0" TO BIN-REST
     ‚*                      KEINS AN
                             WHEN "0" ALSO "0" ALSO "0"
                                  MOVE "0" TO BINERG OF
                                              FORMAT1(ERG-POS:1)
                                  MOVE "0" TO BIN-REST
                    END-EVALUATE
                    SUBTRACT 1 FROM ERG-POS
                    SUBTRACT 1 FROM POS
           END-PERFORM.
           IF       BIN-REST = "1"
                    MOVE "1" TO BINERG OF FORMAT1(ERG-POS:1)
           END-IF.

       ADD-BIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE-ÜBERSCHRIFTEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0010"         TO MAINUEB OF FORMAT1.
           MOVE     "XXP0010*1"       TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
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
