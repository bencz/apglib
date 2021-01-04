       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0051.
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
                  ASSIGN       TO  WORKSTATION-XXD0051DE
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
            COPY DDS-ALL-FORMATS OF XXD0051DE.
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

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0051".
       01  PGM-TYP                       PIC X(8)  VALUE "FUN    ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0051DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0051DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  NEUANLAGE                     PIC 9(1).
       01  ONLY-1-TIME-PER-CALL          PIC 9(1).

     ‚* PARAMETER FÜR DAS CFP8008
       01  AKTION-WRK                    PIC X(20).

     ‚* PARAMETER FÜR DAS CFP0025
       01  FOOD-PTR                      USAGE POINTER.
       01  READ-PTR                      USAGE POINTER.

     ‚* PARAMETER FÜR DAS CFP0008
       01  CFID-WRK                      LIKE CFID   OF CFG-CPY.
       01  CFKEY-WRK                     LIKE CFKEY  OF CFG-CPY.
       01  CFKEY2-WRK                    LIKE CFKEY2 OF CFG-CPY.
       01  CFKEY3-WRK                    LIKE CFKEY3 OF CFG-CPY.
       01  CFKEY4-WRK                    LIKE CFKEY4 OF CFG-CPY.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0051" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0051DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.
           MOVE     "*USER"         TO  MODUS OF FORMAT1.
           MOVE     "Piccolo Mondo" TO  RESTR OF FORMAT1.
           SET      FOOD-PTR        TO NULL.

           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* ÜBERSCHRIFT
       ANF010.
           MOVE     "XXP0051*1"     TO MAINUEB OF FORMAT1.
           PERFORM  GET-FOOD.

           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

     ‚* F5 GEDRÜCKT ?
           IF       IN05 OF FORMAT1-I-INDIC  = AN
                    GO TO ANF010
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR DIENST
           IF       RESTR  OF FORMAT1(1:1) = "?"
                    MOVE "K201" TO CFID-WRK
                    MOVE SPACES TO CFKEY-WRK
                    MOVE SPACES TO CFKEY2-WRK
                    MOVE SPACES TO CFKEY3-WRK
                    MOVE SPACES TO CFKEY4-WRK
                    MOVE SPACES TO F12
                    CALL "CFP0008" USING CFID-WRK
                                         CFKEY-WRK
                                         CFKEY2-WRK
                                         CFKEY3-WRK
                                         CFKEY4-WRK
                                         F12
                    END-CALL
                    INITIALIZE F12
                    MOVE CFKEY-WRK TO RESTR  OF FORMAT1
                    MOVE "RESTR" TO DSP-CUR-FLD
                    GO TO ANF010
           END-IF.

           GO TO    ANF010.

       ENDE.
           IF       FOOD-PTR NOT = NULL
                    SET      PTR-MAIN OF PTRMAP-REC TO FOOD-PTR
                    SET      PTR-CLEAR OF PTRMAP-REC TO TRUE
                    PERFORM  COPY-PTR-MAPPING
           END-IF.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ZITAT ERMITTELN
     ‚*--------------------------------------------------------------
       GET-FOOD SECTION.
       GET-FOD-00.

           CALL     "XXP0043" USING MODUS  OF FORMAT1
                                    RESTR  OF FORMAT1
                                    FOOD-PTR
           END-CALL.
           IF       FOOD-PTR NOT = NULL
     ‚*             LESEN DES KATEGORIE-POINTER
                    SET      PTR-MAIN OF PTRMAP-REC TO FOOD-PTR
                    SET      PTR-READ OF PTRMAP-REC TO TRUE
                    MOVE     "FOODLIST" TO PTR-KEY OF PTRMAP-REC
                    PERFORM  COPY-PTR-MAPPING
                    SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC
                    IF   READ-PTR NOT = NULL
     ‚*                  EINTRAG LESEN
                         MOVE   "ENTRY"   TO MAP-KEY OF MAPDTA-REC
                         MOVE   MAP-READ  TO MAP-ACT OF MAPDTA-REC
                         SET    MAP-PTR OF MAPDTA-REC TO READ-PTR
                         PERFORM  COPY-CALL-MAPDTA
                         IF   RET-CODE = SPACES
                              MOVE MAP-REC OF MAPDTA-REC TO
                                   RNDFOD OF FORMAT1
                         END-IF
                    END-IF
           END-IF.

       GET-FOD-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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
