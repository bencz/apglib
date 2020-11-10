       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0006.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE WINDOW ZUR EINGABE VON NOTIZEN                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.12.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  EINGABE VON NOTIZEN                          *
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
                  ASSIGN       TO  WORKSTATION-CRD0006DE
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
            COPY DDS-ALL-FORMATS OF CRD0006DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0006".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CRD0006DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF CRD0006DE
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
       01  X-NOTES-PTR                   USAGE POINTER.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-SRCSEQ
                                X-NOTES-PTR
                                X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0006" TO PGM-WRK.
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

     ‚* WENN DER POINTER NICHT NULL IST, DANN KÖNNTE ES SCHON
     ‚* EINEN TEXT FÜR DIESE ZEILE GEBEN
           IF       X-NOTES-PTR NOT = NULL
                    SET     MAP-PTR OF MAPDTA-REC TO X-NOTES-PTR
                    MOVE    X-SRCLIB TO MAP-KEY OF MAPDTA-REC(1:10)
                    MOVE    X-SRCRCN TO MAP-KEY OF MAPDTA-REC(11:10)
                    MOVE    X-SRCMBR TO MAP-KEY OF MAPDTA-REC(21:10)
                    MOVE    X-SRCSEQ TO MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE    MAP-READ TO MAP-ACT OF MAPDTA-REC
                    PERFORM COPY-CALL-MAPDTA
                    IF   RET-CODE = SPACES
                         MOVE MAP-REC OF MAPDTA-REC(1:20)  TO
                              NOTE1 OF FORMAT1
                         MOVE MAP-REC OF MAPDTA-REC(21:20) TO
                              NOTE2 OF FORMAT1
                    END-IF
           END-IF.

     ‚* ÜBERSCHRIFT
           MOVE     "CRP0006*1"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT1.
           MOVE     X-SRCLIB          TO SRCLIB OF FORMAT1.
           MOVE     X-SRCRCN          TO SRCRCN OF FORMAT1.
           MOVE     X-SRCMBR          TO SRCMBR OF FORMAT1.
           MOVE     X-SRCSEQ          TO SRCSEQ OF FORMAT1.

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
                    GO TO ENDE
           END-IF.

     ‚* JETZT DIE NOTIZ SCHREIBEN
           INITIALIZE MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-NOTES-PTR.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     X-SRCSEQ           TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     NOTE1 OF FORMAT1   TO MAP-REC OF MAPDTA-REC(1:20).
           MOVE     NOTE2 OF FORMAT1   TO MAP-REC OF MAPDTA-REC(21:20).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-NOTES-PTR        TO MAP-PTR OF MAPDTA-REC.

       ENDE.
           CLOSE                 WS-DISPLAY.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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
