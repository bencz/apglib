       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0207.
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
                  ASSIGN       TO  WORKSTATION-XXD0207DE
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
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF XXD0207DE.
      /
     ‚*--- PKDBST1: KBLFDN
       FD  PKDBST1-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDBST1-P.
           COPY DDS-ALL-FORMATS OF PKDBST1.
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
           COPY  DDS-FMT01-O OF  XXD0207DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0207DE
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP0207".
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
           MOVE     "XXP0207" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0207DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PKDBST1-DP.

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
     ‚* PKDBST1 LESEN
           IF       X-KBLFDN NOT = ZEROES
                    INITIALIZE PKDBST1F OF PKDBST1-P
                    MOVE X-KBLFDN            TO KBLFDN OF PKDBST1-P
                    READ PKDBST1-DP WITH NO LOCK
                    IF   FILE-STATUS NOT = ZEROES
                         INITIALIZE PKDBST1F OF PKDBST1-P
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

           CLOSE                 PKDBST1-DP.

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
     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PKDBST1F OF PKDBST1-P TO FORMAT1
                    MOVE KBADTA OF PKDBST1-P  TO DT_ANL OF FORMAT1
                    MOVE KBMDTA OF PKDBST1-P  TO DT_MOD OF FORMAT1
                    MOVE KBATIA OF PKDBST1-P  TO TI_ANL OF FORMAT1
                    MOVE KBMTIA OF PKDBST1-P  TO TI_MOD OF FORMAT1
           END-IF.

       ANZ-DAT-20.
           MOVE     PGM-TYP                    TO PGMTYP OF FORMAT1.
           MOVE     "KD"                       TO KBTYP  OF FORMAT1.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE AUS TO IN43 OF INXX
                    IF   KBVSTS OF PKDBST1-P = "4"
                     OR  KBVSTS OF PKDBST1-P = "9"
                         MOVE AN TO IN43 OF INXX
                    END-IF
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    MOVE AUS TO IN43 OF INXX
           END-IF.

     ‚* STATUS ANZEIGEN
           EVALUATE KBVSTS OF FORMAT1
                    WHEN "1"
                         MOVE     "PGM0023"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN "2"
                         MOVE     "PGM0024"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN "4"
                         MOVE     "PGM0025"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN "9"
                         MOVE     "PGM0026"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
           END-EVALUATE.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT   TO STSBEZ OF FORMAT1.

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

     ‚* DATUM GEFÜLLT?
           IF       DT_A   OF FORMAT1(1:8) = ZEROES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* SOLL UPDATE ERFOLGEN?
           IF       KBVSTS OF PKDBST1-P = "4"
            OR      KBVSTS OF PKDBST1-P = "9"
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* DATUM AUF GÜLTIGKEIT PRÜFEN
           MOVE     DT_A   OF FORMAT1(1:8) TO DATE-DB.
           PERFORM  COPY-CHECK-DATE.
           IF       DATUM-RETCODE NOT = SPACES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR ARTIKEL
           IF       KBTYP  OF FORMAT1(1:1) = "?"
                    MOVE "K120" TO CFID-WRK
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
                    IF   CFKEY-WRK NOT = SPACES
                         MOVE CFKEY-WRK TO KBTYP  OF FORMAT1
                    END-IF
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR ARTIKEL
           IF       KBIDEN OF FORMAT1(1:1) = "?"
                    MOVE "K130" TO CFID-WRK
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
                    IF   CFKEY-WRK NOT = SPACES
                         MOVE CFKEY-WRK TO KBIDEN OF FORMAT1
                    END-IF
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* IST DER EINTRAG AUCH ERLAUBT?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "K120"            TO CFID   OF CFG-CPY.
           MOVE     KBTYP  OF FORMAT1 TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* FEHLT DIE ARTIKELNUMMER?
           IF       KBIDEN OF FORMAT1 = SPACES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* IST DER EINTRAG AUCH ERLAUBT?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "K130"            TO CFID   OF CFG-CPY.
           MOVE     KBIDEN OF FORMAT1 TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* FEHLT DIE MENGE?
           IF       KBBMEN OF FORMAT1 = ZEROES
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

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0200"         TO MAINUEB OF FORMAT1.
           MOVE     "XXP0207*1"       TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

           INITIALIZE PKDBST1F OF PKDBST1-P.
           MOVE     CORR FORMAT1 TO PKDBST1F OF PKDBST1-P.
     ‚* BESTELLNUMMER UND LAUFENDE NUMMER EMRITTELN
           MOVE     "PKDBST1" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK               TO KBLFDN OF PKDBST1-P.

     ‚* BESTELLNUMMER
           MOVE     "K"                    TO KBBST1 OF PKDBST1-P(1:1).
           MOVE     LFDN-WRK               TO KBBST1 OF PKDBST1-P(2:9).

           MOVE     DT_A   OF FORMAT1(1:8) TO KBDTB  OF PKDBST1-P.
           MOVE     KBTYP  OF FORMAT1      TO KBTYP  OF PKDBST1-P.
           MOVE     "1"                    TO KBVSTS OF PKDBST1-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* BESTELLUNGSANLAGE FÜLLEN
           MOVE     DATE-8            TO KBDTB  OF PKDBST1-P.
           MOVE     TIME-6            TO KBTIB  OF PKDBST1-P.
           MOVE     T-USER            TO KBBUSR OF PKDBST1-P.
           MOVE     T-TERM            TO KBBBS  OF PKDBST1-P.
           MOVE     PGM-WRK           TO KBBPGM OF PKDBST1-P.

     ‚* USER FÜLLEN
           MOVE     DATE-8            TO KBADTA OF PKDBST1-P.
           MOVE     TIME-6            TO KBATIA OF PKDBST1-P.
           MOVE     T-USER            TO KBAUSR OF PKDBST1-P.
           MOVE     T-TERM            TO KBABS  OF PKDBST1-P.
           MOVE     PGM-WRK           TO KBAPGM OF PKDBST1-P.

           WRITE    PKDBST1-P.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       CHANGE-ENTRY SECTION.
       CHG-ENT-00.

           INITIALIZE PKDBST1F OF PKDBST1-P.
           MOVE     X-KBLFDN          TO KBLFDN OF PKDBST1-P.
           READ     PKDBST1-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-ENT-90
           END-IF.

           MOVE     CORR FORMAT1 TO PKDBST1F OF PKDBST1-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO KBMSER OF PKDBST1-P.
           MOVE     T-TERM            TO KBMBS  OF PKDBST1-P.
           MOVE     PGM-WRK           TO KBMPGM OF PKDBST1-P.
           MOVE     DATE-8            TO KBMDTA OF PKDBST1-P.
           MOVE     TIME-6            TO KBMTIA OF PKDBST1-P.

           REWRITE  PKDBST1-P.

       CHG-ENT-90.
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
