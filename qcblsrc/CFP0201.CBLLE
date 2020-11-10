       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0201.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ÄNDERN EINES TAGS                                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.05.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE/ÄNDERN EINES TAGS                    *
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
                  ASSIGN       TO  WORKSTATION-CFD0201DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TEXTE
     ‚*-------------------------------------------------------------
           SELECT PCFGTXT-DP
                  ASSIGN       TO  DATABASE-PCFGTXT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF CFD0201DE.
      /
     ‚*--- TEXTE
       FD  PCFGTXT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGTXT-P.
           COPY DDS-ALL-FORMATS OF PCFGTXT.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN43                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  CFD0201DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CFD0201DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0201".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* SONSTIGE VARIABLEN
       01  NEUANLAGE                     PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TXSPR                       LIKE TXSPR  OF PCFGTXT-P.
       01  X-TXID                        LIKE TXID   OF PCFGTXT-P.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-TXSPR
                                         X-TXID
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0201" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0201DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN I-O   PCFGTXT-DP
           END-IF.

           INITIALIZE F3, F12.
           INITIALIZE X-F3, X-F12.
           INITIALIZE NEUANLAGE.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     AUS TO INXX.

       ANF020.
     ‚* PCFGTXT LESEN
           IF       X-TXSPR  NOT = SPACES
            AND     X-TXID   NOT = SPACES
                    INITIALIZE PCFGTXTF OF PCFGTXT-P
                    MOVE X-TXSPR             TO TXSPR  OF PCFGTXT-P
                    MOVE X-TXID              TO TXID   OF PCFGTXT-P
                    READ PCFGTXT-DP WITH NO LOCK
                    IF   FILE-STATUS NOT = ZEROES
                         INITIALIZE PCFGTXTF OF PCFGTXT-P
                         MOVE 1      TO NEUANLAGE
                    ELSE
                         MOVE ZEROES TO NEUANLAGE
                    END-IF
           ELSE
                    MOVE 1      TO NEUANLAGE
           END-IF.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-DATEN.
           IF       F3 = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-DATEN SECTION.
       ANZ-DAT-00.

           INITIALIZE FORMAT1.
           MOVE     AUS    TO INXX.

       ANZ-DAT-20.
     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE AN  TO IN43 OF INXX
                    MOVE CORR PCFGTXTF OF PCFGTXT-P TO FORMAT1
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    MOVE AUS TO IN43 OF INXX
                    MOVE X-TXSPR                TO TXSPR  OF FORMAT1
                    MOVE X-TXID                 TO TXID   OF FORMAT1
           END-IF.

     ‚* ANZEIGE FORMAT
           MOVE     CORR   INXX     TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           MOVE     AUS  TO INXX
           INITIALIZE DSP-ATTR.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT1-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT1-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* SPRACHE MUSS GEFÜLTL SEIN
           IF       TXSPR  OF FORMAT1 = SPACES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* ID MUSS GEFÜLTL SEIN
           IF       TXID   OF FORMAT1 = SPACES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* NEUANLAGE/ÄNDERN
           IF       NEUANLAGE NOT = ZEROES
                    PERFORM ADD-ENTRY
           ELSE
                    PERFORM CHANGE-ENTRY
           END-IF.

       ANZ-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE-ÜBERSCHRIFTEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

           MOVE     "CFP0000"         TO MAINUEB OF FORMAT1.
           MOVE     "CFP0201*1"       TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

           INITIALIZE PCFGTXTF OF PCFGTXT-P.
     ‚* FUNKTIONSCODES .. ETC
           MOVE     CORR FORMAT1 TO PCFGTXTF OF PCFGTXT-P.
     ‚* KEY NOCHMAL FÜLLEN
           MOVE     TXSPR  OF FORMAT1 TO TXSPR  OF PCFGTXT-P.
           MOVE     TXID   OF FORMAT1 TO TXID   OF PCFGTXT-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO TXAUSR OF PCFGTXT-P.
           MOVE     T-TERM            TO TXABS  OF PCFGTXT-P.
           MOVE     DATE-8            TO TXADTA OF PCFGTXT-P.
           MOVE     TIME-6            TO TXATIA OF PCFGTXT-P.
           MOVE     PGM-WRK           TO TXAPGM OF PCFGTXT-P.

           WRITE    PCFGTXT-P.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       CHANGE-ENTRY SECTION.
       CHG-ENT-00.

           INITIALIZE PCFGTXTF OF PCFGTXT-P.
           MOVE     TXSPR  OF FORMAT1 TO TXSPR  OF PCFGTXT-P.
           MOVE     TXID   OF FORMAT1 TO TXID   OF PCFGTXT-P.
           READ     PCFGTXT-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-ENT-90
           END-IF.

     ‚* DATEN FÜLLEN
           MOVE     TXTEXT OF FORMAT1 TO TXTEXT OF PCFGTXT-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO TXMSER OF PCFGTXT-P.
           MOVE     T-TERM            TO TXMBS  OF PCFGTXT-P.
           MOVE     DATE-8            TO TXMDTA OF PCFGTXT-P.
           MOVE     TIME-6            TO TXMTIA OF PCFGTXT-P.
           MOVE     PGM-WRK           TO TXMPGM OF PCFGTXT-P.

           REWRITE  PCFGTXT-P.

       CHG-ENT-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
