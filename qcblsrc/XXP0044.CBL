       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0044.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* STARTZEIT NOTIEREN                                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  STARTZEIT FESTHALTEN                         *
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
                  ASSIGN       TO  WORKSTATION-XXD0044DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PGABEL: GBUSER
     ‚*-------------------------------------------------------------
           SELECT PGABEL-DP
                  ASSIGN       TO  DATABASE-PGABEL
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

     ‚*-----BILDSCHIRMDATEI -------------------------
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF XXD0044DE.
      /
     ‚*- PGABEL: GBUSER
       FD  PGABEL-DP
           LABEL RECORDS ARE STANDARD.
       01  PGABEL-P.
           COPY DDS-ALL-FORMATS OF PGABEL.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0044".
       01  PGM-TYP                       PIC X(8)  VALUE "FUN    ".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0044DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0044DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  NEUANLAGE                     PIC 9(1).
       01  ONLY-1-TIME-PER-CALL          PIC 9(1).

     ‚* SONSTIGE VARIABLEN
       01  COMPLETE-SECS                 LIKE DIFF-SECS.
       01  SEC-WITH-COMMA                PIC S9(9)V9(3) IS TYPEDEF.
       01  GBSEC-COMMA                   TYPE SEC-WITH-COMMA.
       01  ALL-COMMA                     TYPE SEC-WITH-COMMA.

     ‚* ZEITPARAMETER
       01  START-DATE                    LIKE DATE-8.
       01  START-TIME                    LIKE TIME-6.

     ‚* PARAMETER FÜR DAS CFP8008
       01  AKTION-WRK                    PIC X(20).

     ‚* PARAMETER FÜR DAS CFP0025
       01  ZITAT-PTR                     USAGE POINTER.
       01  LEN                           PIC 9(4).
       01  RESULT-TAB                    TYPE HLP0021-TAB.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0044" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0044DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           INITIALIZE ONLY-1-TIME-PER-CALL.
     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PGABEL-DP.

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

     ‚* ÜBERSCHRIFT
       ANF010.
           INITIALIZE PGABELF OF PGABEL-P.
           MOVE     T-USER TO GBUSER OF PGABEL-P.
           READ     PGABEL-DP WITH NO LOCK.
           IF       FILE-STATUS = ZEROES
                    MOVE ZEROES TO NEUANLAGE
     ‚*             GLEICHNAMIGE FELDER FÜLLEN
                    MOVE CORR PGABELF OF PGABEL-P TO FORMAT1
     ‚*             SEKUNDEN IN MINUTEN UND SEKUNDEN ANZEIGEN
                    DIVIDE   GBSEC  OF PGABEL-P BY 60
                             GIVING    MINS OF FORMAT1
                             REMAINDER SECS OF FORMAT1
                    END-DIVIDE
           ELSE
                    MOVE 1      TO NEUANLAGE
           END-IF.

           MOVE     "XXP0044*1"     TO MAINUEB OF FORMAT1.
           MOVE     DATE-8          TO START-DATE.
           MOVE     TIME-6          TO START-TIME.
           PERFORM  GET-ZITAT.

     ‚* GESAMTZEIT ERMITTELN
           PERFORM  GET-COMPLETE.
           IF       COMPLETE-SECS > ZEROES
     ‚*             SEKUNDEN IN MINUTEN UND SEKUNDEN ANZEIGEN
                    DIVIDE   COMPLETE-SECS BY 60
                             GIVING    ALLMIN OF FORMAT1
                             REMAINDER ALLSEC OF FORMAT1
                    END-DIVIDE
     ‚*             ANTEIL BERECHNEN
                    MOVE GBSEC OF FORMAT1 TO GBSEC-COMMA
                    MOVE COMPLETE-SECS    TO ALL-COMMA
                    COMPUTE  USRPRZ OF FORMAT1 =  (   GBSEC-COMMA
                                                    / ALL-COMMA )
                                                  * 100,00
                    END-COMPUTE
           END-IF.

           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    PERFORM  SAVE-RECORD
                    GO TO ENDE
           END-IF.

     ‚* F5 GEDRÜCKT ?
           IF       IN05 OF FORMAT1-I-INDIC  = AN
                    PERFORM  SAVE-RECORD
     ‚*             DATUM/ZEIT AKTUALISIEREN
                    PERFORM  COPY-GET-TIME
                    GO TO ANF010
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    PERFORM  SAVE-RECORD
                    GO TO ENDE
           END-IF.

     ‚* RECORD SPEICHERN
           PERFORM  SAVE-RECORD.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PGABEL-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DATENSATZ SPEICHERN
     ‚*--------------------------------------------------------------
       SAVE-RECORD SECTION.
       SAV-REC-00.

           IF       NEUANLAGE = 1
                    PERFORM  WRITE-RECORD
                    MOVE     ZEROES TO NEUANLAGE
           ELSE
                    PERFORM  UPDATE-RECORD
           END-IF.

       SAV-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BESTEHENDEN RECORD UPDATEN
     ‚*--------------------------------------------------------------
       UPDATE-RECORD SECTION.
       UPD-REC-00.

           INITIALIZE PGABELF OF PGABEL-P.
           MOVE     T-USER TO GBUSER OF PGABEL-P.
           READ     PGABEL-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             ERMITTELN ENDDATUM/ZEIT
                    PERFORM  COPY-GET-TIME
                    PERFORM  GET-MINUTES
                    ADD      DIFF-SECS         TO GBSEC  OF PGABEL-P
                    IF   ONLY-1-TIME-PER-CALL = ZEROES
                         MOVE     1            TO ONLY-1-TIME-PER-CALL
                         ADD      1            TO GBCALL OF PGABEL-P
                    END-IF
     ‚*             MODIFIKATIONSDATEN FÜLLEN
                    MOVE     T-USER            TO GBMSER OF PGABEL-P
                    MOVE     T-TERM            TO GBMBS  OF PGABEL-P
                    MOVE     PGM-WRK           TO GBMPGM OF PGABEL-P
                    MOVE     DATE-8            TO GBMDTA OF PGABEL-P
                    MOVE     TIME-6            TO GBMTIA OF PGABEL-P
                    REWRITE PGABEL-P
           END-IF.

       UPD-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUEN RECORD ANLEGEN
     ‚*--------------------------------------------------------------
       WRITE-RECORD SECTION.
       WRI-REC-00.

           INITIALIZE PGABELF OF PGABEL-P.
     ‚* USER
           MOVE     T-USER            TO GBUSER OF PGABEL-P.

     ‚* ERMITTELN ENDDATUM/ZEIT
           PERFORM  COPY-GET-TIME.

           PERFORM  GET-MINUTES.
           MOVE     DIFF-SECS         TO GBSEC  OF PGABEL-P.
           MOVE     1                 TO GBCALL OF PGABEL-P.

     ‚* ANLAGEDATEN FÜLLEN
           MOVE     T-USER            TO GBAUSR OF PGABEL-P.
           MOVE     T-TERM            TO GBABS  OF PGABEL-P.
           MOVE     PGM-WRK           TO GBAPGM OF PGABEL-P.
           MOVE     DATE-8            TO GBADTA OF PGABEL-P.
           MOVE     TIME-6            TO GBATIA OF PGABEL-P.

           WRITE    PGABEL-P.

       WRI-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEITUNTERSCHIED IN MINUTEN ERMITTELN
     ‚*--------------------------------------------------------------
       GET-MINUTES SECTION.
       GET-MIN-00.

           INITIALIZE DIFF-SECS.
           MOVE     "SEC" TO AKTION-WRK.
           CALL     "CFP8008" USING START-DATE
                                    START-TIME
                                    DATE-8
                                    TIME-6
                                    AKTION-WRK
                                    DIFF-SECS
           END-CALL.

       GET-MIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GESAMTZEIT ERMITTELN
     ‚*--------------------------------------------------------------
       GET-COMPLETE SECTION.
       GET-COM-00.

           INITIALIZE COMPLETE-SECS.
     ‚* GANZE PGABEL SUMMIEREN
           INITIALIZE PGABELF OF PGABEL-P.
           MOVE     SPACES TO GBUSER OF PGABEL-P.
           START    PGABEL-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-COM-90
           END-IF.

       GET-COM-20.
           READ     PGABEL-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-COM-90
           END-IF.

           ADD      GBSEC  OF PGABEL-P TO COMPLETE-SECS.
           GO TO    GET-COM-20.

       GET-COM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZITAT ERMITTELN
     ‚*--------------------------------------------------------------
       GET-ZITAT SECTION.
       GET-ZIT-00.

           INITIALIZE RESULT-TAB.
           MOVE     LENGTH OF ZIT1 OF FORMAT1 TO LEN.
           CALL     "CFP0025" USING ZITAT-PTR
                                    LEN
                                    RESULT-TAB
                                    PGM-WRK
           END-CALL.
           IF       TAB-MAX OF RESULT-TAB >= 1
                    MOVE ZEILE OF RESULT-TAB(1) TO ZIT1 OF FORMAT1
           END-IF.
           IF       TAB-MAX OF RESULT-TAB >= 2
                    MOVE ZEILE OF RESULT-TAB(2) TO ZIT2 OF FORMAT1
           END-IF.
           IF       TAB-MAX OF RESULT-TAB >= 3
                    MOVE ZEILE OF RESULT-TAB(3) TO ZIT3 OF FORMAT1
           END-IF.

       GET-ZIT-90.
           EXIT.
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
