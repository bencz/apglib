       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0051.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* BEFEHLSEINGABE                                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  07.07.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BEFEHL EINGEBEN                              *
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
                  ASSIGN       TO  WORKSTATION-CFD0051DE
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
           COPY DDS-ALL-FORMATS OF CFD0051DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CFD0051DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CFD0051DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0051".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  PCFGCMD-WRK.
           COPY DDS-ALL-FORMATS OF PCFGCMD.

       01  NAMESPACE-WRK                 LIKE CMNSPC OF PCFGCMD-WRK.
       01  COMMAND-WRK                   PIC X(1024).
       01  POINTER-WRK                   USAGE POINTER.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0051" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0051DE" TO DISPLAY-WRK.
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
     ‚* FORMAT ANZEIGEN
           PERFORM  BEFEHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* POINTER ANZEIGEN
           CALL     "CFP0050" USING POINTER-WRK
           END-CALL.

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
       BEFEHL SECTION.
       BEF-EHL-00.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT1.
           MOVE     "CFP0051*1"       TO SUBUEB  OF FORMAT1.

       BEF-EHL-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.

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
                    GO TO BEF-EHL-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO BEF-EHL-90
           END-IF.

     ‚* BEFEHL PARSEN
           MOVE     NAMESPACE OF FORMAT1 TO NAMESPACE-WRK.
           MOVE     TXT       OF FORMAT1 TO COMMAND-WRK.
           CALL     "CFP0011" USING NAMESPACE-WRK
                                    COMMAND-WRK
                                    POINTER-WRK
                                    ERRFLD-WRK
           END-CALL.
           IF       ERRFLD-WRK NOT = SPACES
                    GO TO BEF-EHL-10
           END-IF.

       BEF-EHL-90.
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
