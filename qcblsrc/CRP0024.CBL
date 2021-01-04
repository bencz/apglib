       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0024.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SEU-OPTIONEN ÄNDERN                                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  23.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SEU-OPTIONEN                                 *
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
                  ASSIGN       TO  WORKSTATION-CRD0024DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
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
           COPY DDS-ALL-FORMATS OF CRD0024DE.
      /
     ‚*--- PROGRAMME
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN03                          PIC 1.
     ‚* INDIK. FÜR ANZEIGE OB INFOS VORHANDEN SIND
        05 IN40                          PIC 1.
     ‚* INDIK. FÜR ANZEIGE OB DATEI GEÄNDERT
        05 IN41                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  CRD0024DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CRD0024DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0024".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.

     ‚* WORK-RECORDS
       01  PCHKHGH-WRK.
           COPY DDS-ALL-FORMATS OF PCHKHGH.

     ‚* SONSTIGE VARIABLEN
       01  SYTYP-WRK                     LIKE SYTYP  OF PCHKHGH-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-F3
                                X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0024"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CRD0024DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PCONFIG-DP.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

           INITIALIZE X-F3, X-F12.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT IN CFG-CPY ÜBERNEHMEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H101"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
     ‚*             SYNTAX-HIGHLIGHTING UND ANDERE DEFAULTS
                    MOVE  1           TO CFFK01 OF CFG-CPY
                    MOVE  1           TO CFFK03 OF CFG-CPY
                    MOVE  ZEROES      TO CFFK04 OF CFG-CPY
                    MOVE  1           TO CFFK13 OF CFG-CPY
                    MOVE  "QTMPSRC"   TO CFDAT2 OF CFG-CPY
                    MOVE  "APG"       TO CFDAT3 OF CFG-CPY
                    MOVE  "QTEMP"     TO CFDAT4 OF CFG-CPY
                    MOVE  "*SAME"     TO CFDAT5 OF CFG-CPY
                    MOVE 0,01         TO CFNK01 OF CFG-CPY
           END-IF.

     ‚* ERMITTELN DES DATEITYPEN
           INITIALIZE RTVMBRD-REC.
           MOVE     X-SRCLIB          TO OBJLIB   OF RTVMBRD-REC.
           MOVE     X-SRCRCN          TO FILENAME OF RTVMBRD-REC.
           MOVE     X-SRCMBR          TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    INITIALIZE RTVMBRD-REC
           END-IF.

     ‚* EINSTELLUNGEN AUFRUFEN
           PERFORM  CHANGE-SETTINGS.
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

           CLOSE                 PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* EINSTELLUNGEN ÄNDERN
     ‚*--------------------------------------------------------------
       CHANGE-SETTINGS SECTION.
       CHG-SET-00.

           INITIALIZE FORMAT1.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CRP0024"         TO MAINUEB OF FORMAT1.
           MOVE     "CRP0024*1"       TO SUBUEB  OF FORMAT1.

     ‚* DATEN AUS CFG-CPY FÜLLEN
           MOVE     CORR PCONFIGF OF CFG-CPY TO FORMAT1.
     ‚* DATUM KONVERTIEREN
           IF       CFDAT1 OF CFG-CPY(1:8) IS NUMERIC
                    MOVE    CFDAT1 OF CFG-CPY(1:8) TO DT_VGL OF FORMAT1
           ELSE
                    MOVE    DATE-8                 TO DT_VGL OF FORMAT1
           END-IF.

     ‚* LETZTE SICHEURNGSNUMMER ERMITTELN
           MOVE     "SEUHST"          TO LFDN-NAME.
           MOVE     T-USER            TO LFDN-ID.
           CALL     "CFP0022"   USING LFDN-NAME
                                      LFDN-ID
                                      LFDN-WRK
           END-CALL.
           IF       LFDN-WRK > ZEROES
                    MOVE "H"          TO SEUHST OF FORMAT1(1:1)
                    MOVE LFDN-WRK     TO SEUHST OF FORMAT1(2:9)
           END-IF.

       CHG-SET-10.
     ‚* ZU VERWENDENDEN SOURCE-TYP ANZEIGEN
           CALL     "CRP0020" USING T-USER
                                    SRCTYPE OF RTVMBRD-REC
                                    SYTYP   OF FORMAT1
           END-CALL.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO CHG-SET-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO CHG-SET-90
           END-IF.

     ‚* F18 PFLEGE SOURCE-HIGHLIGHTING
           IF       IN18 OF FORMAT1-I-INDIC  = AN
                    INITIALIZE F3, F12
                    CALL "CRP0021" USING F3
                                         F12
                    END-CALL
                    IF   F3 = "1"
                         GO TO CHG-SET-90
                    END-IF
                    INITIALIZE F12
                    GO TO CHG-SET-10
           END-IF.

     ‚* F19 PFLEGE SOURCE-HIGHLIGHTING SOURCETYPEN
           IF       IN19 OF FORMAT1-I-INDIC  = AN
                    INITIALIZE F3, F12
                    CALL "CRP0022" USING F3
                                         F12
                    END-CALL
                    IF   F3 = "1"
                         GO TO CHG-SET-90
                    END-IF
                    INITIALIZE F12
                    GO TO CHG-SET-10
           END-IF.

     ‚* F20 INFORMATIONEN
           IF       IN20 OF FORMAT1-I-INDIC  = AN
                    PERFORM SHOW-MEMBER-INFOS
                    IF   F3 = "1"
                         GO TO CHG-SET-90
                    END-IF
                    MOVE SPACES TO F12
                    GO TO CHG-SET-10
           END-IF.

     ‚* FRAGEZEICHEN FÜR CFFK01 - CFFK05
           MOVE     SPACES TO SYTYP-WRK.
           IF       CFFK01 OF FORMAT1 = "?"
                    CALL "CRP0020" USING T-USER
                                         SRCTYPE OF RTVMBRD-REC
                                         SYTYP-WRK
                    END-CALL
                    PERFORM SHOW-SYNTAX-COMMAND
                    MOVE CFFK01 OF CFG-CPY TO CFFK01 OF FORMAT1
                    GO TO CHG-SET-10
           END-IF.
           IF       CFFK03 OF FORMAT1 = "?"
                    MOVE "E+P"   TO SYTYP-WRK
                    PERFORM SHOW-SYNTAX-COMMAND
                    MOVE CFFK03 OF CFG-CPY TO CFFK03 OF FORMAT1
                    GO TO CHG-SET-10
           END-IF.
           IF       CFFK05 OF FORMAT1 = "?"
                    MOVE "OTHER" TO SYTYP-WRK
                    PERFORM SHOW-SYNTAX-COMMAND
                    MOVE CFFK05 OF CFG-CPY TO CFFK05 OF FORMAT1
                    GO TO CHG-SET-10
           END-IF.

     ‚* SICHERUNG OHNE LIB?
           IF       CFFK15 OF FORMAT1 NOT = ZEROES
                    IF   CFDAT4 OF FORMAT1 = SPACES
                         GO TO CHG-SET-10
                    END-IF
                    IF   CFDAT5 OF FORMAT1 = SPACES
                         GO TO CHG-SET-10
                    END-IF
           END-IF.

     ‚* VORSICHTSHALBER ABFANGEN - WEGEN FRAGEZEICHEN ALPHANUMERISCH
           IF       CFFK01 OF FORMAT1 IS NOT NUMERIC
                    MOVE CFFK01 OF CFG-CPY TO CFFK01 OF FORMAT1
           END-IF.
           IF       CFFK03 OF FORMAT1 IS NOT NUMERIC
                    MOVE CFFK03 OF CFG-CPY TO CFFK03 OF FORMAT1
           END-IF.
           IF       CFFK05 OF FORMAT1 IS NOT NUMERIC
                    MOVE CFFK05 OF CFG-CPY TO CFFK05 OF FORMAT1
           END-IF.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* EINSTELLUNGEN JETZT ÄNDERN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "H101"                TO CFID   OF PCONFIG-P.
           MOVE     T-USER                TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN FÜLLEN
                    MOVE CORR FORMAT1 TO PCONFIGF OF PCONFIG-P
                    MOVE DT_VGL OF FORMAT1 TO CFDAT1 OF PCONFIG-P
     ‚*             USER/DATUM/UHRZEIT FÜLLEN
                    MOVE T-USER            TO CFMSER OF PCONFIG-P
                    MOVE T-TERM            TO CFMBS  OF PCONFIG-P
                    MOVE PGM-WRK           TO CFMPGM OF PCONFIG-P
                    MOVE DATE-8            TO CFMDTA OF PCONFIG-P
                    MOVE TIME-6            TO CFMTIA OF PCONFIG-P
     ‚*             AKTUALISIEREN
                    REWRITE PCONFIG-P
           ELSE
     ‚*             DATEN FÜLLEN
                    MOVE CORR FORMAT1 TO PCONFIGF OF PCONFIG-P
                    MOVE DT_VGL OF FORMAT1 TO CFDAT1 OF PCONFIG-P
     ‚*             USER/DATUM/UHRZEIT FÜLLEN
                    MOVE T-USER            TO CFAUSR OF PCONFIG-P
                    MOVE T-TERM            TO CFABS  OF PCONFIG-P
                    MOVE PGM-WRK           TO CFAPGM OF PCONFIG-P
                    MOVE DATE-8            TO CFADTA OF PCONFIG-P
                    MOVE TIME-6            TO CFATIA OF PCONFIG-P
     ‚*             KEY FÜLLEN
                    MOVE "H101"            TO CFID   OF PCONFIG-P
                    MOVE T-USER            TO CFKEY  OF PCONFIG-P
                    MOVE SPACES            TO CFKEY2 OF PCONFIG-P
                    MOVE SPACES            TO CFKEY3 OF PCONFIG-P
                    MOVE SPACES            TO CFKEY4 OF PCONFIG-P
     ‚*             SCHREIBEN DES SATZES
                    WRITE   PCONFIG-P
           END-IF.

     ‚* JETZT IN CFG-CPY ÜBERNEHMEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF CFG-CPY.

       CHG-SET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WINDOW ZUR PFLEGE DES SYNTAXHIGHLIGHTING
     ‚*--------------------------------------------------------------
       SHOW-SYNTAX-COMMAND SECTION.
       SHW-SCM-00.

           INITIALIZE F12.
     ‚* ALLE SYSNTAX-EINTRÄGE ANZEIGEN
           CALL     "CRP0004" USING SYTYP-WRK
                                    F12
           END-CALL.
           INITIALIZE F12.

       SHW-SCM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WINDOW ZUR PFLEGE DES SYNTAXHIGHLIGHTING
     ‚*--------------------------------------------------------------
       SHOW-MEMBER-INFOS SECTION.
       SHW-INF-00.

           INITIALIZE F3, F12.
     ‚* ALLE SYSNTAX-EINTRÄGE ANZEIGEN
           CALL     "CRP0027" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    F3
                                    F12
           END-CALL.
           INITIALIZE F12.

       SHW-INF-90.
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
