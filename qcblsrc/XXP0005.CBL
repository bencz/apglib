       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0005.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* RAUMPLANUNG                                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  24.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  RAUMPLANUNG                                  *
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
                  ASSIGN       TO  WORKSTATION-XXD0005DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI PCFGFFD NACH LIB/FILE
     ‚*-------------------------------------------------------------
           SELECT PCFGFFD-DL
                  ASSIGN       TO  DATABASE-PCFGFFDL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF XXD0005DE.
      /
     ‚*--- PCFGFFD NACH LIB/FILE
       FD  PCFGFFD-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-L.
           COPY DDS-ALL-FORMATS OF PCFGFFDL1.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN03                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  XXD0005DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0005DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0005DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0005DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0005DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0005DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  FORMAT4X.
           COPY  DDS-FMT04-O OF  XXD0005DE
           REPLACING FMT04-O BY FORMAT4
                     FMT04-O-INDIC BY FORMAT4-O-INDIC.

       01  FORMAT4Y.
           COPY  DDS-FMT04-I OF  XXD0005DE
           REPLACING FMT04-I BY FORMAT4-I
                     FMT04-I-INDIC BY FORMAT4-I-INDIC.

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

       01  PGM-WRK                       PIC X(10) VALUE "XXP0005".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  ANZEIGE-MODUS                 PIC 9(1).

       01  LEN                           PIC 9(4).
       01  POS                           PIC 9(4).
       01  USER-ROOM                     LIKE CFIN05 OF CFG-CPY.
       01  RAUM-NAME                     LIKE CFIN01 OF CFG-CPY.
       01  RAUM-WRK                      LIKE WHFLDE OF PCFGFFD-L.

       01  RET-ROOM                      LIKE CFIN05 OF CFG-CPY.
       01  MITARB-WRK                    LIKE MITARB OF FORMAT1.

       01  WAHL-SAV                      PIC 9(1).
       01  USER-SAV                      PIC X(10).

     ‚* PARAMETER FÜR DAS CFP0008
       01  CFID-WRK                      LIKE CFID   OF CFG-CPY.
       01  CFKEY-WRK                     LIKE CFKEY  OF CFG-CPY.
       01  CFKEY2-WRK                    LIKE CFKEY2 OF CFG-CPY.
       01  CFKEY3-WRK                    LIKE CFKEY3 OF CFG-CPY.
       01  CFKEY4-WRK                    LIKE CFKEY4 OF CFG-CPY.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        LIKE T-USER.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-USER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0005"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     INPUT PCFGFFD-DL.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0005DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* EINSTELLUNGEN AUFRUFEN
       ANF010.
           PERFORM  RAUMANZEIGE.
           IF       F3 = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

       ENDE.
           CLOSE                 PCFGFFD-DL.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* EINSTELLUNGEN ÄNDERN
     ‚*--------------------------------------------------------------
       RAUMANZEIGE SECTION.
       RAU-ZEI-00.

           INITIALIZE FORMAT1.
           INITIALIZE WAHL-SAV.
           MOVE     X-USER            TO MITARB  OF FORMAT1.
           MOVE     X-USER            TO USER-SAV.
           MOVE     ZEROES TO ANZEIGE-MODUS.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "XXP0005"         TO MAINUEB OF FORMAT1.
           MOVE     "XXP0005*1"       TO SUBUEB  OF FORMAT1.
     ‚* WURDE EIN MITARBEITER SELEKTIERT?
           IF       MITARB OF FORMAT1 NOT = SPACES
                    MOVE    MITARB OF FORMAT1 TO MITARB-WRK
                    PERFORM GET-ROOM-BY-USER
                    MOVE    RET-ROOM          TO USER-ROOM
                    IF   RET-ROOM(2:1) IS NUMERIC
                         MOVE RET-ROOM(2:1) TO WAHL OF FORMAT1
                         ADD   1      TO WAHL    OF FORMAT1
                    ELSE
                         MOVE  2      TO WAHL    OF FORMAT1
                    END-IF
           ELSE
                    MOVE  2           TO WAHL    OF FORMAT1
           END-IF.

     ‚* RÄUME ANZEIGEN
       RAU-ZEI-10.
     ‚* AUFBEREITUNG STARTEN
           EVALUATE WAHL OF FORMAT1
                    WHEN 1 PERFORM ETAGE-1
                    WHEN 2 PERFORM ETAGE-2
                    WHEN 3 PERFORM ETAGE-3
           END-EVALUATE.

     ‚* JETZT NOCH DIE FUNKTIONSTASTEN
           MOVE     "FMT05"  TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.

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
                    GO TO RAU-ZEI-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO RAU-ZEI-90
           END-IF.

     ‚* WECHSEL DES NAMEN
           IF       IN10 OF FORMAT1-I-INDIC = AN
                    EVALUATE ANZEIGE-MODUS
                             WHEN ZEROES MOVE 1      TO ANZEIGE-MODUS
                             WHEN 1      MOVE ZEROES TO ANZEIGE-MODUS
                    END-EVALUATE
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR USER
           IF       MITARB OF FORMAT1(1:1) = "?"
                    MOVE "H050" TO CFID-WRK
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
                    MOVE CFKEY-WRK TO MITARB OF FORMAT1
           END-IF.

     ‚* IST ES EIN ANDERER USER, DANN BEIM ERSTEN MAL ETAGE WECHSELN
           IF       MITARB OF FORMAT1 NOT = USER-SAV
                    MOVE    MITARB OF FORMAT1 TO MITARB-WRK
                    PERFORM GET-ROOM-BY-USER
                    MOVE    RET-ROOM          TO USER-ROOM
           END-IF.

     ‚* USER SICHERN
           MOVE     MITARB OF FORMAT1    TO USER-SAV.

           GO TO    RAU-ZEI-10.

       RAU-ZEI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DES "KELLERS"
     ‚*--------------------------------------------------------------
       ETAGE-1 SECTION.
       ETA-GE1-00.

           INITIALIZE FORMAT2.
           MOVE     AN TO IN02 OF INXX.
           MOVE     CORR INXX       TO  FORMAT2-O-INDIC.
           MOVE     "FMT02"         TO  FMT-WRK.
           MOVE     FORMAT2         TO  WSREC-WRK.
     ‚* DA ES SOVIELE RÄUME SIND, WERDEN WIR HIER DIE INDIKATOREN
     ‚* ÜBER DIE PCFGFFD SETZEN
           PERFORM  AUFBEREITEN-WSREC.
     ‚* ... UND HIER ERST DAS DISPLAY SCHREIBEN
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     AUS TO INXX.

       ETA-GE1-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES ERDGESCHOSSES
     ‚*--------------------------------------------------------------
       ETAGE-2 SECTION.
       ETA-GE2-00.

           INITIALIZE FORMAT3.
           MOVE     AN TO IN02 OF INXX.
           MOVE     CORR INXX       TO  FORMAT3-O-INDIC.
           MOVE     "FMT03"         TO  FMT-WRK.
           MOVE     FORMAT3         TO  WSREC-WRK.
     ‚* DA ES SOVIELE RÄUME SIND, WERDEN WIR HIER DIE INDIKATOREN
     ‚* ÜBER DIE PCFGFFD SETZEN
           PERFORM  AUFBEREITEN-WSREC.
     ‚* ... UND HIER ERST DAS DISPLAY SCHREIBEN
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     AUS TO INXX.

       ETA-GE2-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DES DACHGESCHOSSES
     ‚*--------------------------------------------------------------
       ETAGE-3 SECTION.
       ETA-GE3-00.

           INITIALIZE FORMAT4.
           MOVE     AN TO IN02 OF INXX.
           MOVE     CORR INXX       TO  FORMAT4-O-INDIC.
           MOVE     "FMT04"         TO  FMT-WRK.
           MOVE     FORMAT4         TO  WSREC-WRK.
     ‚* DA ES SOVIELE RÄUME SIND, WERDEN WIR HIER DIE INDIKATOREN
     ‚* ÜBER DIE PCFGFFD SETZEN
           PERFORM  AUFBEREITEN-WSREC.
     ‚* ... UND HIER ERST DAS DISPLAY SCHREIBEN
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     AUS TO INXX.

       ETA-GE3-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DES RECORDS
     ‚*--------------------------------------------------------------
       AUFBEREITEN-WSREC SECTION.
       AUF-WSR-00.

           INITIALIZE USER-ROOM.
           INITIALIZE CFKEY-WRK.
     ‚* WURDE EIN MITARBEITER SELEKTIERT?
           IF       MITARB OF FORMAT1 NOT = SPACES
                    MOVE    MITARB OF FORMAT1 TO MITARB-WRK
                    PERFORM GET-ROOM-BY-USER
                    MOVE    RET-ROOM          TO USER-ROOM
           END-IF.

           INITIALIZE POS, LEN.
     ‚* JETZT ALLE FELDER DURCHSUCHEN UND AUFBEREITEN
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB           TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK         TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK             TO WHNAME OF PCFGFFD-L.
           MOVE     SPACES              TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO AUF-WSR-90
           END-IF.

       AUF-WSR-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO AUF-WSR-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L NOT = FMT-WRK
                    GO TO AUF-WSR-90
           END-IF.

     ‚* POSITION/LÄNGE SICHERN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

     ‚* ERMITTELN DES RAUMNAMEN
           IF       WHFLDE OF PCFGFFD-L(1:2) = "A_"
                    MOVE WHFLDE OF PCFGFFD-L(3:4) TO RAUM-WRK
           ELSE
                    MOVE WHFLDE OF PCFGFFD-L(1:4) TO RAUM-WRK
           END-IF.
           PERFORM  GET-RAUMNAME.

     ‚* IST DER USER IM DEM RAUM?
           IF       WHFLDE OF PCFGFFD-L(1:2) = "A_"
            AND     WHFLDE OF PCFGFFD-L(3:4) = USER-ROOM
                    MOVE COL-GRN-RI TO WSREC-WRK(POS:LEN)
           END-IF.

     ‚* IST DAS TEAM IM DEM RAUM?
           IF       WHFLDE OF PCFGFFD-L(1:2) = "A_"
            AND     RAUM-NAME                = TEAM OF FORMAT1
            AND     RAUM-NAME            NOT = SPACES
                    IF   WSREC-WRK(POS:LEN) = COL-GRN-RI
                         MOVE COL-YLW-RI TO WSREC-WRK(POS:LEN)
                    ELSE
                         MOVE COL-RED-RI TO WSREC-WRK(POS:LEN)
                    END-IF
           END-IF.

           IF       WHFLDE OF PCFGFFD-L(1:1) = "R"
                    EVALUATE ANZEIGE-MODUS
     ‚*                      FELDNAME
                             WHEN ZEROES
                                  MOVE WHFLDE OF PCFGFFD-L(2:3) TO
                                       WSREC-WRK(POS:LEN)
     ‚*                      TEAM WELCHES HIER SITZT
                             WHEN 1
                                  IF   RAUM-NAME NOT = SPACES
                                       MOVE RAUM-NAME TO
                                            WSREC-WRK(POS:LEN)
                                  END-IF
                    END-EVALUATE
           END-IF.

           GO TO    AUF-WSR-20.

       AUF-WSR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES RAUMNAMEN
     ‚*--------------------------------------------------------------
       GET-RAUMNAME SECTION.
       GET-RMN-00.

           INITIALIZE RAUM-NAME.
     ‚* LESEN DER EINSTELLUNGEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H052"            TO CFID   OF CFG-CPY
           MOVE     RAUM-WRK          TO CFKEY  OF CFG-CPY
           PERFORM  COPY-CFG-CPY
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

           MOVE     CFIN01 OF CFG-CPY TO RAUM-NAME.

       GET-RMN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES RAUMNAMEN
     ‚*--------------------------------------------------------------
       GET-ROOM-BY-USER SECTION.
       GET-ROU-00.

           INITIALIZE RET-ROOM.
     ‚* WURDE EIN MITARBEITER SELEKTIERT?
           IF       MITARB-WRK NOT = SPACES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE    "H050"            TO CFID   OF CFG-CPY
                    MOVE    MITARB-WRK        TO CFKEY  OF CFG-CPY
                    PERFORM COPY-CFG-CPY
                    IF   CFG-DA OF CFG-CPY = ZEROES
                         INITIALIZE PCONFIGF OF CFG-CPY
                    ELSE
                         MOVE CFIN01 OF CFG-CPY TO CFKEY-WRK
     ‚*                  RAUM DES USERS SPEICHERN
                         MOVE CFIN26 OF CFG-CPY TO RET-ROOM
                    END-IF
           END-IF.

       GET-ROU-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
