       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0209.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANLAGE EINER BESTELLUNG                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.03.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BESTELLUNG ANLEGEN/ÄNDERN                    *
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
                  ASSIGN       TO  WORKSTATION-XXD0209DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI BESTELLUNG
     ‚*-------------------------------------------------------------
           SELECT PKDBST1-DP
                  ASSIGN       TO  DATABASE-PKDBST1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI BESTELLEINGÄNGE
     ‚*-------------------------------------------------------------
           SELECT PKDBEIN-DP
                  ASSIGN       TO  DATABASE-PKDBEIN
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
           COPY DDS-ALL-FORMATS OF XXD0209DE.
      /
     ‚*--- PKDBST1: KBLFDN
       FD  PKDBST1-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDBST1-P.
           COPY DDS-ALL-FORMATS OF PKDBST1.
      /
     ‚*--- PKDBEIN: BELFDN
       FD  PKDBEIN-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDBEIN-P.
           COPY DDS-ALL-FORMATS OF PKDBEIN.
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
           COPY  DDS-FMT01-O OF  XXD0209DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0209DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0209".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

     ‚* PARAMETER FÜR CFP0008
       01   CFID-WRK                     LIKE CFID   OF CFG-CPY.
       01   CFKEY-WRK                    LIKE CFKEY  OF CFG-CPY.
       01   CFKEY2-WRK                   LIKE CFKEY2 OF CFG-CPY.
       01   CFKEY3-WRK                   LIKE CFKEY3 OF CFG-CPY.
       01   CFKEY4-WRK                   LIKE CFKEY4 OF CFG-CPY.

     ‚* SONSTIGE VARIABLEN
       01  NEUANLAGE                     PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-KBLFDN                      LIKE KBLFDN OF PKDBST1-P.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-KBLFDN
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0209" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0209DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PKDBST1-DP
                          PKDBEIN-DP.

           INITIALIZE F3, F12.
           INITIALIZE X-F3, X-F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     AUS TO INXX.

       ANF020.
     ‚* PKDBST1 LESEN
           INITIALIZE PKDBST1F OF PKDBST1-P.
           MOVE     X-KBLFDN            TO KBLFDN OF PKDBST1-P.
           READ     PKDBST1-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
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

           CLOSE                 PKDBST1-DP
                                 PKDBEIN-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-DATEN SECTION.
       ANZ-DAT-00.

           INITIALIZE FORMAT1.
     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

           MOVE     AUS    TO INXX.
           MOVE     DATE-8 TO DT_A OF FORMAT1.
           MOVE     CORR PKDBST1F OF PKDBST1-P TO FORMAT1.

       ANZ-DAT-20.
           MOVE     PGM-TYP                    TO PGMTYP OF FORMAT1.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* BEZEICHNUNGEN ERMITTELN
           IF       KBTYP  OF FORMAT1 NOT = SPACES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE     "K120"            TO CFID   OF CFG-CPY
                    MOVE     KBTYP  OF FORMAT1 TO CFKEY  OF CFG-CPY
                    PERFORM  COPY-CFG-CPY
                    IF   CFG-DA OF CFG-CPY NOT = ZEROES
                         MOVE CFTXT1 OF CFG-CPY TO TYPBEZ OF FORMAT1
                    END-IF
           END-IF.
           IF       KBIDEN OF FORMAT1 NOT = SPACES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE     "K130"            TO CFID   OF CFG-CPY
                    MOVE     KBIDEN OF FORMAT1 TO CFKEY  OF CFG-CPY
                    PERFORM  COPY-CFG-CPY
                    IF   CFG-DA OF CFG-CPY NOT = ZEROES
                         MOVE CFTXT1 OF CFG-CPY TO ARTBEZ OF FORMAT1
                    END-IF
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

     ‚* FEHLT DIE MENGE?
           IF       KBLMEN OF FORMAT1 = ZEROES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* BUCHEN DER MENGE
           PERFORM  ADD-ENTRY.

       ANZ-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE-ÜBERSCHRIFTEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0200"         TO MAINUEB OF FORMAT1.
           MOVE     "XXP0209*1"       TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

           INITIALIZE PKDBEINF OF PKDBEIN-P.
     ‚* BESTELLNUMMER UND LAUFENDE NUMMER ERMITTELN
           MOVE     "PKDBEIN" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK               TO BELFDN OF PKDBEIN-P.
           MOVE     "E"                    TO BEWENR OF PKDBEIN-P(1:1).
           MOVE     LFDN-WRK               TO BEWENR OF PKDBEIN-P(2:9).

     ‚* LIEFERMENGE ÜBERNEHMEN
           MOVE     KBLMEN OF FORMAT1      TO BELMEN OF PKDBEIN-P.

     ‚* DATEN AUS DER PKDBST1 ÜBERNEHMEN
           MOVE     KBTYP  OF PKDBST1-P    TO BETYP  OF PKDBEIN-P.
           MOVE     KBBST1 OF PKDBST1-P    TO BEBST1 OF PKDBEIN-P.
           MOVE     KBIDEN OF PKDBST1-P    TO BEIDEN OF PKDBEIN-P.
           MOVE     "9"                    TO BEVSTS OF PKDBEIN-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     DATE-8                 TO BEADTA OF PKDBEIN-P.
           MOVE     TIME-6                 TO BEATIA OF PKDBEIN-P.
           MOVE     T-USER                 TO BEAUSR OF PKDBEIN-P.
           MOVE     T-TERM                 TO BEABS  OF PKDBEIN-P.
           MOVE     PGM-WRK                TO BEAPGM OF PKDBEIN-P.

           WRITE    PKDBEIN-P.
           IF       FILE-STATUS = ZEROES
                    PERFORM UPDATE-RECORD
           END-IF.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DES DATENSATZES
     ‚*--------------------------------------------------------------
       UPDATE-RECORD SECTION.
       UPD-REC-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* DATENSATZ LÖSCHEN
           INITIALIZE PKDBST1F OF PKDBST1-P.
           MOVE     X-KBLFDN            TO KBLFDN OF PKDBST1-P.
           READ     PKDBST1-DP.
           IF       FILE-STATUS = ZEROES
                    IF   KBVSTS OF PKDBST1-P NOT = "4"
                     AND KBVSTS OF PKDBST1-P NOT = "9"
                         MOVE    T-USER      TO KBMSER OF PKDBST1-P
                         MOVE    T-TERM      TO KBMBS  OF PKDBST1-P
                         MOVE    PGM-WRK     TO KBMPGM OF PKDBST1-P
                         MOVE    DATE-8      TO KBMDTA OF PKDBST1-P
                         MOVE    TIME-6      TO KBMTIA OF PKDBST1-P
                         MOVE    "2"         TO KBVSTS OF PKDBST1-P
                         ADD     KBLMEN OF FORMAT1 TO
                                 KBLMEN OF PKDBST1-P
                    END-IF
                    REWRITE PKDBST1-P
           END-IF.

       UPD-REC-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CHECK-DATE: DATUMSFELDER PRÜFEN
     ‚*---------------------------------------------------------------

           COPY     CHKDATE    OF APG-QCPYSRC.
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
