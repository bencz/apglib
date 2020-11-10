       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9102.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANEIGE BLUESCREEN                                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BLUESCREEN ANZEIGEN                          *
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
                  ASSIGN       TO  WORKSTATION-CFD9102DE
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
            COPY DDS-ALL-FORMATS OF CFD9102DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9102".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CFD9102DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF CFD9102DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  ZEILE-WRK                     PIC 9(5).
       01  POS-WRK                       PIC 9(5).
       01  STRING-WRK                    PIC X(80).

       01  INSERT-POS                    PIC 9(5).
       01  TEXT-ID                       LIKE CPY-TXID.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-PGM                         PIC X(10).
       01  X-TYP                         PIC X(10).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-PGM
                                X-TYP.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9102 " TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.

     ‚* "A PROBLEM HAS BEEN DETECTED ..."
           MOVE     3                 TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0002"         TO TEXT-ID.
           PERFORM  ADD-TEXT.

     ‚* TEXT HINZUFÜGEN
           MOVE     5                 TO ZEILE-WRK.
           MOVE     6                 TO POS-WRK.
     ‚* QSYS.LIB/<LIB>.LIB/<PGM>.PGM/<TYP>.EXE
           STRING   "QSYS.LIB"        DELIMITED BY SPACES
                    "/"               DELIMITED BY SIZE
                    CONST-LIB         DELIMITED BY SPACES
                    ".LIB"            DELIMITED BY SIZE
                    "/"               DELIMITED BY SIZE
                    X-PGM             DELIMITED BY SPACES
                    ".PGM"            DELIMITED BY SIZE
                    "/"               DELIMITED BY SIZE
                    X-TYP             DELIMITED BY SPACES
                    ".DLL"            DELIMITED BY SIZE
           INTO     STRING-WRK.
           PERFORM  ADD-TEXT.

     ‚* "IF THIS IS THE FIRST ..."
           MOVE     7                 TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0003"         TO TEXT-ID.
           PERFORM  ADD-TEXT.
           MOVE     8                 TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0004"         TO TEXT-ID.
           PERFORM  ADD-TEXT.

     ‚* "IF THIS PROBLEM CONTINUES,..."
           MOVE     10                TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0006"         TO TEXT-ID.
           PERFORM  ADD-TEXT.
           MOVE     11                TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0007"         TO TEXT-ID.
           PERFORM  ADD-TEXT.
           MOVE     12                TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0008"         TO TEXT-ID.
           PERFORM  ADD-TEXT.
           MOVE     13                TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0009"         TO TEXT-ID.
           PERFORM  ADD-TEXT.

     ‚* "TECHNICAL INFORMATION:"
           MOVE     16                TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0010"         TO TEXT-ID.
           PERFORM  ADD-TEXT.

     ‚* "*** STOP: OXOOOOOO..."
           MOVE     18                TO ZEILE-WRK.
           MOVE     4                 TO POS-WRK.
           MOVE     "PGM0005"         TO TEXT-ID.
           PERFORM  ADD-TEXT.

       ANF010.
           MOVE     CORR FORMAT1      TO FMT01-O.
           WRITE    WS-REC     FORMAT IS "FMT02".
           WRITE    WS-REC     FORMAT IS "FMT01".
           READ     WS-DISPLAY FORMAT IS "FMT01".
           MOVE     CORR FMT01-I      TO FORMAT1.

     ‚* F24 GEDRÜCKT ?
           IF       IN24 OF FMT01-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

           GO TO    ANF010.

       ENDE.
           CLOSE                 WS-DISPLAY.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES TEXTES
     ‚*--------------------------------------------------------------
       ADD-TEXT SECTION.
       ADD-TXT-00.

     ‚* BERECHNEN DER POSITION
           COMPUTE  INSERT-POS = (ZEILE-WRK * 80) + POS-WRK - 80.

     ‚* IST DER TEXT EIN PGM-TEXT?
           IF       TEXT-ID    NOT = SPACES
     ‚*             TEXT LESEN
                    MOVE     TEXT-ID           TO CPY-TXID
                    MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    PERFORM  COPY-PGM-TXT
                    MOVE     CPY-TXTEXT        TO STRING-WRK
           END-IF.

     ‚* TEXT HINZUFÜGEN
           IF       INSERT-POS NOT = ZEROES
            AND     STRING-WRK NOT = SPACES
                    MOVE STRING-WRK TO TEXTST OF FORMAT1(INSERT-POS:)
           END-IF.

     ‚* WERTE NUN INITIALISIEREN
           INITIALIZE STRING-WRK.
           INITIALIZE TEXT-ID.

       ADD-TXT-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
