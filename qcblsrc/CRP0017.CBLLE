       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0017.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE WINDOW ZUR EINGABE EINES DATUMS                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  21.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  EINGABE EINES SOURCEDATUMS                   *
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
                  ASSIGN       TO  WORKSTATION-CRD0017DE
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
            COPY DDS-ALL-FORMATS OF CRD0017DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT      OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0017".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CRD0017DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF CRD0017DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-SRCSEQ                      LIKE SRCSEQ OF QSETSRC-WRK.
       01  X-SRCDAT                      LIKE SRCDAT OF QSETSRC-WRK.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-SRCSEQ
                                X-SRCDAT
                                X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0017" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.
           INITIALIZE X-F12.

     ‚* ÜBERSCHRIFT
           MOVE     "CRP0017*1"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT1.
           MOVE     X-SRCLIB          TO SRCLIB OF FORMAT1.
           MOVE     X-SRCRCN          TO SRCRCN OF FORMAT1.
           MOVE     X-SRCMBR          TO SRCMBR OF FORMAT1.
           MOVE     X-SRCSEQ          TO SRCSEQ OF FORMAT1.
           MOVE     X-SRCDAT          TO DT_SRC OF FORMAT1.

       ANF010.
     ‚*€   MOVE     CORR INXX         TO FMT01-O-INDIC.
           MOVE     CORR FORMAT1      TO FMT01-O.
           WRITE    WS-REC     FORMAT IS "FMT02".
           WRITE    WS-REC     FORMAT IS "FMT01".
           READ     WS-DISPLAY FORMAT IS "FMT01".
           MOVE     CORR FMT01-I      TO FORMAT1.
     ‚*€   MOVE     AUS TO INXX.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FMT01-I-INDIC  = AN
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

           MOVE     DT_SRC OF FORMAT1 TO X-SRCDAT.

       ENDE.
           CLOSE                 WS-DISPLAY.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
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
