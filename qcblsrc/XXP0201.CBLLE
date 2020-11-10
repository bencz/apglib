       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0201.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ÄNDERN EINES KD-SATZES                                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE/ÄNDERN KÜCHENDIENST                  *
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
                  ASSIGN       TO  WORKSTATION-XXD0201DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI KÜCHENDIENST
     ‚*-------------------------------------------------------------
           SELECT PKDPLAN-DP
                  ASSIGN       TO  DATABASE-PKDPLAN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI DATUM
     ‚*-------------------------------------------------------------
           SELECT PCFGDAT-DP
                  ASSIGN       TO  DATABASE-PCFGDAT
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
           COPY DDS-ALL-FORMATS OF XXD0201DE.
      /
     ‚*--- KÜCHENDIENST
       FD  PKDPLAN-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDPLAN-P.
           COPY DDS-ALL-FORMATS OF PKDPLAN.
      /
     ‚*--- DATUMTABELLE
       FD  PCFGDAT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGDAT-P.
           COPY DDS-ALL-FORMATS OF PCFGDAT.
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
           COPY  DDS-FMT01-O OF  XXD0201DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0201DE
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP0201".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

     ‚* SONSTIGE VARIABLEN
       01  NEUANLAGE                     PIC 9(1).
       01  FORMAT1-SAVE                  LIKE FORMAT1.

     ‚* PARAMETER FÜR CFP0008
       01  CFID-WRK                      LIKE CFID   OF CFG-CPY.
       01  CFKEY-WRK                     LIKE CFKEY  OF CFG-CPY.
       01  CFKEY2-WRK                    LIKE CFKEY2 OF CFG-CPY.
       01  CFKEY3-WRK                    LIKE CFKEY3 OF CFG-CPY.
       01  CFKEY4-WRK                    LIKE CFKEY4 OF CFG-CPY.

     ‚* PARAMETER FÜR DAS CFP0027
       01  FILE-WRK                      PIC X(10).
       01  FORMAT-DUMMY                  PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-KDDATE                      LIKE KDDATE OF PKDPLAN-P.
       01  X-KDTYP                       LIKE KDTYP  OF PKDPLAN-P.
       01  X-KDUSER                      LIKE KDUSER OF PKDPLAN-P.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-KDDATE
                                         X-KDTYP
                                         X-KDUSER
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0201" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0201DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PKDPLAN-DP.
           OPEN     INPUT PCFGDAT-DP.

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
     ‚* PKDPLAN LESEN
           IF       X-KDDATE NOT = ZEROES
            AND     X-KDTYP  NOT = SPACES
            AND     X-KDUSER NOT = SPACES
                    INITIALIZE PKDPLANF OF PKDPLAN-P
                    MOVE X-KDDATE            TO KDDATE OF PKDPLAN-P
                    MOVE X-KDTYP             TO KDTYP  OF PKDPLAN-P
                    MOVE X-KDUSER            TO KDUSER OF PKDPLAN-P
                    READ PKDPLAN-DP WITH NO LOCK
                    IF   FILE-STATUS NOT = ZEROES
                         INITIALIZE PKDPLANF OF PKDPLAN-P
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

           CLOSE                 PCFGDAT-DP
                                 PKDPLAN-DP.

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

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE CORR PKDPLANF OF PKDPLAN-P TO FORMAT1
                    MOVE KDDATE OF PKDPLAN-P  TO DT_KD  OF FORMAT1
                    IF   KDMSER OF PKDPLAN-P NOT = SPACES
                         MOVE KDMSER OF PKDPLAN-P TO KDMSER  OF FORMAT1
                         MOVE KDMDTA OF PKDPLAN-P TO DT_MOD  OF FORMAT1
                         MOVE KDMTIA OF PKDPLAN-P TO TI_MOD  OF FORMAT1
                         MOVE "*"                 TO CHGWRK1 OF FORMAT1
                         MOVE "*"                 TO CHGWRK2 OF FORMAT1
                         MOVE "*"                 TO CHGWRK3 OF FORMAT1
                    END-IF
     ‚*             ANLAGEDATUM IST JETZT AUCH IMMER VORHANDEN
                    MOVE KDAUSR OF PKDPLAN-P TO KDAUSR  OF FORMAT1
                    MOVE KDADTA OF PKDPLAN-P TO DT_CRT  OF FORMAT1
                    MOVE KDATIA OF PKDPLAN-P TO TI_CRT  OF FORMAT1
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
           END-IF.

     ‚* ANZEIGE FORMAT
           MOVE     SYS-COMPIL      TO  SYS_CMP OF FORMAT1.
           MOVE     CORR   INXX     TO  FORMAT1-O-INDIC.
           MOVE     FORMAT1         TO  FORMAT1-SAVE.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           MOVE     AUS  TO INXX
           INITIALIZE DSP-ATTR.

     ‚* FEHLER AUSSCHALTEN
           INITIALIZE A_KDTYP  OF FORMAT1.
           INITIALIZE A_DATWRK OF FORMAT1.

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

           IF       IN20 OF FORMAT1-I-INDIC = AN
                    IF   NEUANLAGE = ZEROES
                         MOVE "PKDPLAN" TO FILE-WRK
                         MOVE SPACES    TO FORMAT-DUMMY
                         MOVE PKDPLAN-P TO RECORD-WRK
                         CALL "CFP0027" USING RECORD-WRK
                                              CONST-LIB
                                              FILE-WRK
                                              FORMAT-DUMMY
                                              F3
                         END-CALL
                         IF   F3 = "1"
                              GO TO ANZ-DAT-90
                         END-IF
                    END-IF
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* HAT SICH WAS GEÄNDERT? WENN NICHT EINFACH RAUS.
           IF       FORMAT1 = FORMAT1-SAVE
                    GO TO ANZ-DAT-90
           END-IF.

     ‚* DATUM GEFÜLLT?
           IF       DT_KD  OF FORMAT1(1:8) = ZEROES
                    MOVE    COL-GRN-RI TO A_DATWRK OF FORMAT1
                    MOVE    "ERR0004"  TO DSP-ERR-ID
                    MOVE    "DT_KD"    TO DSP-ERR-FLD
                    PERFORM COPY-ADD-ERR-TAB
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* DATUM AUF GÜLTIGKEIT PRÜFEN
           MOVE     DT_KD  OF FORMAT1(1:8) TO DATE-DB.
           PERFORM  COPY-CHECK-DATE.
           IF       DATUM-RETCODE NOT = SPACES
                    MOVE    COL-GRN-RI TO A_DATWRK OF FORMAT1
                    MOVE    "ERR0004"  TO DSP-ERR-ID
                    MOVE    "DT_KD"    TO DSP-ERR-FLD
                    PERFORM COPY-ADD-ERR-TAB
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR TYP
           IF       KDTYP  OF FORMAT1(1:1) = "?"
                    MOVE "K110" TO CFID-WRK
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
                         MOVE CFKEY-WRK TO KDTYP  OF FORMAT1
                    END-IF
                    MOVE "KDTYP" TO DSP-CUR-FLD
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* IST DER EINTRAG AUCH ERLAUBT?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "K110"            TO CFID   OF CFG-CPY.
           MOVE     KDTYP  OF FORMAT1 TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    MOVE    COL-GRN-RI TO A_KDTYP  OF FORMAT1
                    MOVE    "ERR0006"  TO DSP-ERR-ID
                    MOVE    "KDTYP"    TO DSP-ERR-FLD
                    PERFORM COPY-ADD-ERR-TAB
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* FEHLT DER USER?
           IF       KDUSER OF FORMAT1 = SPACES
                    GO TO ANZ-DAT-20
           END-IF.

     ‚* DA DIE DATEI AUF NICHT UNIQUE GEÄNDERT WURDE - KANN
     ‚* JETZT IM EINTRAG GESAGT WERDEN OB DER EINTRAG DATUM/TYP
     ‚* EINDEUTIG SEIN MUSS (Z.B. KÜCHENDIENST)
           IF       CFFK02 OF CFG-CPY NOT = ZEROES
            AND     NEUANLAGE         NOT = ZEROES
                    PERFORM CHECK-EINTRAG
                    IF   RET-CODE NOT = SPACES
                         MOVE COL-GRN-RI TO A_DATWRK OF FORMAT1
                         MOVE COL-GRN-RI TO A_KDTYP  OF FORMAT1
                         MOVE "ERR0005"  TO DSP-ERR-ID
                         MOVE "KDTYP"    TO DSP-ERR-FLD
                         PERFORM COPY-ADD-ERR-TAB
                         GO TO ANZ-DAT-20
                    END-IF
           END-IF.

     ‚* NEUANLAGE/ÄNDERN
           IF       NEUANLAGE NOT = ZEROES
                    PERFORM ADD-ENTRY
           ELSE
                    PERFORM CHANGE-ENTRY
           END-IF.

     ‚* F4 DURCHFÜHREN + NÄCHSTER TAG
           IF       IN04 OF FORMAT1-I-INDIC = AN
                    MOVE 1 TO NEUANLAGE
                    PERFORM GET-NEXT-DAY
     ‚*             DATUM FÜLLEN
                    MOVE     DTLFDN OF PCFGDAT-P  TO DT_KD  OF FORMAT1
                    MOVE     SPACES               TO KDUSER OF FORMAT1
                    MOVE     AN TO IN44 OF INXX
                    GO TO ANZ-DAT-20
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
           MOVE     "XXP0201*1"       TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

           INITIALIZE PKDPLANF OF PKDPLAN-P.
     ‚* FUNKTIONSCODES .. ETC
           MOVE     CORR FORMAT1 TO PKDPLANF OF PKDPLAN-P.
     ‚* KEY FÜLLEN
           MOVE     DT_KD  OF FORMAT1(1:8) TO KDDATE OF PKDPLAN-P.
           MOVE     KDTYP  OF FORMAT1      TO KDTYP  OF PKDPLAN-P.

     ‚* WOCHENTAG FÜLLEN
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-DB           TO DTLFDN OF PCFGDAT-P.
           READ     PCFGDAT-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE DTWOTA OF PCFGDAT-P TO KDWOTA OF PKDPLAN-P
                    MOVE DTKW   OF PCFGDAT-P TO KDKW   OF PKDPLAN-P
           END-IF.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     DATE-8            TO KDADTA OF PKDPLAN-P.
           MOVE     TIME-6            TO KDATIA OF PKDPLAN-P.
           MOVE     T-USER            TO KDAUSR OF PKDPLAN-P.
           MOVE     T-TERM            TO KDABS  OF PKDPLAN-P.
           MOVE     PGM-WRK           TO KDAPGM OF PKDPLAN-P.

           WRITE    PKDPLAN-P.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       CHANGE-ENTRY SECTION.
       CHG-ENT-00.

           INITIALIZE PKDPLANF OF PKDPLAN-P.
           MOVE     X-KDDATE          TO KDDATE OF PKDPLAN-P.
           MOVE     X-KDTYP           TO KDTYP  OF PKDPLAN-P.
           MOVE     X-KDUSER          TO KDUSER OF PKDPLAN-P.
           READ     PKDPLAN-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-ENT-90
           END-IF.

     ‚* FUNKTIONSCODES .. ETC
           MOVE     CORR FORMAT1 TO PKDPLANF OF PKDPLAN-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO KDMSER OF PKDPLAN-P.
           MOVE     T-TERM            TO KDMBS  OF PKDPLAN-P.
           MOVE     PGM-WRK           TO KDMPGM OF PKDPLAN-P.
           MOVE     DATE-8            TO KDMDTA OF PKDPLAN-P.
           MOVE     TIME-6            TO KDMTIA OF PKDPLAN-P.

           REWRITE  PKDPLAN-P.

       CHG-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES NÄCHSTEN TAGES
     ‚*--------------------------------------------------------------
       GET-NEXT-DAY SECTION.
       GET-NDY-00.

           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-DB           TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGDATF OF PCFGDAT-P
                    GO TO GET-NDY-90
           END-IF.

       GET-NDY-20.
           READ     PCFGDAT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGDATF OF PCFGDAT-P
                    GO TO GET-NDY-90
           END-IF.

     ‚* DEN ERMITTELTEN TAG ÜBERLESEN
           IF       DTLFDN OF PCFGDAT-P = DATE-DB
                    GO TO GET-NDY-20
           END-IF.

     ‚* KEINE SAMSTAGE ODER SONNTAGE
           IF       DTWOTA OF PCFGDAT-P = 6
            OR      DTWOTA OF PCFGDAT-P = 7
                    GO TO GET-NDY-20
           END-IF.

       GET-NDY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES SCHON EINEN EINTRAG GIBT
     ‚*--------------------------------------------------------------
       CHECK-EINTRAG SECTION.
       CHK-EIN-00.

           INITIALIZE RET-CODE.
     ‚* ERMITTELN DES ERSTEN TAGES
           INITIALIZE PKDPLANF OF PKDPLAN-P.
           MOVE     DATE-DB             TO KDDATE OF PKDPLAN-P.
           MOVE     KDTYP  OF FORMAT1   TO KDTYP  OF PKDPLAN-P.
           MOVE     SPACES              TO KDUSER OF PKDPLAN-P.
           START    PKDPLAN-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-EIN-90
           END-IF.

       CHK-EIN-20.
           READ     PKDPLAN-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-EIN-90
           END-IF.

           IF       KDDATE OF PKDPLAN-P NOT = DATE-DB
            OR      KDTYP  OF PKDPLAN-P NOT = KDTYP  OF FORMAT1
                    GO TO CHK-EIN-90
           END-IF.

     ‚* EINTRAG VORHANDEN
           MOVE     "1" TO RET-CODE.

       CHK-EIN-90.
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
