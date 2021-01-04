       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0015.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANLAGE EINES HINWEISTEXTES                                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  18.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANLAGE PHINTXT-TEXT                          *
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
                  ASSIGN       TO  WORKSTATION-CFD0015DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHINTXT: HWPGM. HWUSER, HWLFPO
     ‚*-------------------------------------------------------------
           SELECT PHINTXT-DP
                  ASSIGN       TO  DATABASE-PHINTXT
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
           COPY DDS-ALL-FORMATS OF CFD0015DE.
      /
     ‚*--- PHINTXT: HWPGM. HWUSER, HWLFPO
       FD  PHINTXT-DP
           LABEL RECORDS ARE STANDARD.
       01  PHINTXT-P.
           COPY DDS-ALL-FORMATS OF PHINTXT.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN43                          PIC 1.
        05 IN44                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  CFD0015DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CFD0015DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0015".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* PARAMETER FÜR CFP0008
       01   CFID-WRK                     LIKE CFID   OF CFG-CPY.
       01   CFKEY-WRK                    LIKE CFKEY  OF CFG-CPY.
       01   CFKEY2-WRK                   LIKE CFKEY2 OF CFG-CPY.
       01   CFKEY3-WRK                   LIKE CFKEY3 OF CFG-CPY.
       01   CFKEY4-WRK                   LIKE CFKEY4 OF CFG-CPY.

     ‚* SONSTIGE VARIABLEN
       01  LFPO-WRK                      LIKE HWLFPO OF PHINTXT-P.
       01  NEUANLAGE                     PIC 9(1).
       01  FORMAT1-SAVE                  LIKE FORMAT1.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-F3
                                         X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0015" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0015DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PHINTXT-DP.

           INITIALIZE F3, F12.
           INITIALIZE X-F3, X-F12.
           MOVE     1 TO NEUANLAGE.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     AUS TO INXX.

       ANF020.
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

           CLOSE                 PHINTXT-DP.

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
           MOVE     PGM-TYP                    TO PGMTYP OF FORMAT1.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.
           MOVE     AN  TO IN41 OF INXX.
           MOVE     AUS TO IN42 OF INXX.

     ‚* ANZEIGE FORMAT
           MOVE     CORR   INXX     TO  FORMAT1-O-INDIC.
           MOVE     FORMAT1         TO  FORMAT1-SAVE.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           MOVE     AUS  TO INXX

     ‚* F3 ENDE
           IF       IN03 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* HAT SICH WAS GEÄNDERT? WENN NICHT EINFACH RAUS.
           IF       FORMAT1 = FORMAT1-SAVE
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR TYP
           IF       HWPGM  OF FORMAT1(1:1) = "?"
                    MOVE "CP"        TO CFID-WRK
                    MOVE CONST-LIB   TO CFKEY-WRK
                    MOVE "QCBLSRC"   TO CFKEY2-WRK
                    MOVE SPACES      TO CFKEY3-WRK
                    MOVE SPACES      TO CFKEY4-WRK
                    MOVE SPACES      TO F12
                    CALL "CFP0008" USING CFID-WRK
                                         CFKEY-WRK
                                         CFKEY2-WRK
                                         CFKEY3-WRK
                                         CFKEY4-WRK
                                         F12
                    END-CALL
                    IF   CFKEY3-WRK NOT = SPACES
                         MOVE CFKEY3-WRK TO HWPGM  OF FORMAT1
                    ELSE
                         MOVE SPACES     TO HWPGM  OF FORMAT1
                    END-IF
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* PROGRAMM GEFÜLLT?
           IF       HWPGM  OF FORMAT1 = SPACES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* IST DER EINTRAG AUCH ERLAUBT?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "CP"              TO CFID   OF CFG-CPY.
           MOVE     CONST-LIB         TO CFKEY  OF CFG-CPY.
           MOVE     "QCBLSRC"         TO CFKEY2 OF CFG-CPY.
           MOVE     HWPGM  OF FORMAT1 TO CFKEY3 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* FEHLT DER TEXT?
           IF       TEXT1  OF FORMAT1 = SPACES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* NEUANLAGE/ÄNDERN
           IF       NEUANLAGE NOT = ZEROES
                    PERFORM ADD-ENTRY
           END-IF.

       ANZ-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE-ÜBERSCHRIFTEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CFP0015"         TO MAINUEB OF FORMAT1.
           MOVE     "CFP0015*1"       TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

     ‚* POSITION ERMITTELN
           PERFORM  GET-NEXT-POS.
           ADD      1 TO LFPO-WRK.

           INITIALIZE PHINTXTF OF PHINTXT-P.
     ‚* FUNKTIONSCODES .. ETC
           MOVE     CORR FORMAT1 TO PHINTXTF OF PHINTXT-P.
     ‚* KEY FÜLLEN
           MOVE     HWPGM  OF FORMAT1     TO HWPGM  OF PHINTXT-P.
           MOVE     SPACES                TO HWUSER OF PHINTXT-P.
           MOVE     LFPO-WRK              TO HWLFPO OF PHINTXT-P.
     ‚* DATEN FÜLLEN
           MOVE     TEXT1  OF FORMAT1     TO HWTEXT OF PHINTXT-P(1:40).
           MOVE     TEXT2  OF FORMAT1     TO HWTEXT OF PHINTXT-P(41: ).

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     DATE-8            TO HWADTA OF PHINTXT-P.
           MOVE     TIME-6            TO HWATIA OF PHINTXT-P.
           MOVE     T-USER            TO HWAUSR OF PHINTXT-P.
           MOVE     T-TERM            TO HWABS  OF PHINTXT-P.
           MOVE     PGM-WRK           TO HWAPGM OF PHINTXT-P.

           WRITE    PHINTXT-P.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES SCHON EINEN EINTRAG GIBT
     ‚*--------------------------------------------------------------
       GET-NEXT-POS SECTION.
       GET-POS-00.

           INITIALIZE LFPO-WRK.
     ‚* ERMITTELN DES ERSTEN TAGES
           INITIALIZE PHINTXTF OF PHINTXT-P.
           MOVE     HWPGM  OF FORMAT1   TO HWPGM  OF PHINTXT-P.
           MOVE     SPACES              TO HWUSER OF PHINTXT-P.
           MOVE     ZEROES              TO HWLFPO OF PHINTXT-P.
           START    PHINTXT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-POS-90
           END-IF.

       GET-POS-20.
           READ     PHINTXT-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-POS-90
           END-IF.

           IF       HWPGM  OF PHINTXT-P NOT = HWPGM  OF FORMAT1
            OR      HWUSER OF PHINTXT-P NOT = SPACES
                    GO TO GET-POS-90
           END-IF.

     ‚* POSITION MERKEN
           MOVE     HWLFPO OF PHINTXT-P TO LFPO-WRK.

           GO TO    GET-POS-20.

       GET-POS-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
