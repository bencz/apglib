       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0041.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* EINGABE EINER SPOOL-DATEI FÜR MAPDTA                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.01.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  EINGABE EINER SPOOLDATEI                     *
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
                  ASSIGN       TO  WORKSTATION-CRD0041DE
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
            COPY DDS-ALL-FORMATS OF CRD0041DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0041".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CRD0041DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF CRD0041DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-SPOOL-TYPE                  PIC X(10).
       01  X-SHOW-00                     PIC 9(1).
       01  X-SHOW-10                     PIC 9(1).
       01  X-SHOW-20                     PIC 9(1).
       01  X-SHOW-30                     PIC 9(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-SPOOL-TYPE
                                X-SHOW-00
                                X-SHOW-10
                                X-SHOW-20
                                X-SHOW-30
                                X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0041" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CRD0041DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE X-F12.
           INITIALIZE FORMAT1.
           MOVE     X-SRCMBR  TO SPLNAM OF FORMAT1.
           MOVE     X-SRCMBR  TO JOBNAM OF FORMAT1.
           MOVE     X-SHOW-00 TO FKST00 OF FORMAT1.
           MOVE     X-SHOW-10 TO FKST10 OF FORMAT1.
           MOVE     X-SHOW-20 TO FKST20 OF FORMAT1.
           MOVE     X-SHOW-30 TO FKST30 OF FORMAT1.

     ‚* ÜBERSCHRIFT
           MOVE     "CRP0041*1"       TO MAINUEB OF FORMAT1.

       ANF010.
           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚*€   MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
     ‚*€   MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

     ‚* JETZT NOCH SPOOLDATEI KOPIEREN, SOFERN VORHANDEN
           PERFORM  COPY-SPOOLFILE.
           IF       RET-CODE NOT = SPACES
                    GO TO ANF010
           END-IF.

     ‚* STUFEN ZURÜCKGEBEN
           MOVE     FKST00 OF FORMAT1 TO X-SHOW-00.
           MOVE     FKST10 OF FORMAT1 TO X-SHOW-10.
           MOVE     FKST20 OF FORMAT1 TO X-SHOW-20.
           MOVE     FKST30 OF FORMAT1 TO X-SHOW-30.

     ‚* IST DAS GGF. CHKSRC ODER PRELOG?
           IF       SPLNAM OF FORMAT1 = "CHKSRC"
            OR      SPLNAM OF FORMAT1 = "DMF1051"
                    MOVE "CHKSRC" TO X-SPOOL-TYPE
           END-IF.
           IF       SPLNAM OF FORMAT1 = "QPRELOG"
                    MOVE "PRELOG" TO X-SPOOL-TYPE
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SPOOLFILE KOPIEREN
     ‚*--------------------------------------------------------------
       COPY-SPOOLFILE SECTION.
       CPY-SPL-00.

           INITIALIZE RET-CODE.
     ‚* DAS PROGRAMM ÜBERNIMMT DAS KOPIEREN
           CALL     "CRP0037" USING SPLNAM OF FORMAT1
                                    JOBNAM OF FORMAT1
                                    RET-CODE
           END-CALL.

       CPY-SPL-90.
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
