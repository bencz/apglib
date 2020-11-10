       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              AZP0100.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AZUBI-VERWALTUNG                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.08.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ÜBERSICHT BERUFSSCHULTAGE                    *
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
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                         ATTRIBUTE-DATA IS BS-ATRIBUTE,
                         I-O-FEEDBACK   IS FILE-IO-FEEDBACK.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚*-   TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
     ‚*-------------------------------------------------------------
           SELECT BSSORT-DAT
                  ASSIGN       TO  DISK-BSSORT.

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-AZD0100DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
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
     ‚*- TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
       SD  BSSORT-DAT.
       01  BSSORT-REC.
           COPY  DDS-FMT02-O OF AZD0100DE
           REPLACING FMT02-O BY SORT2-O
                     FMT02-O-INDIC BY SORT2-O-INDIC.
      /
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF AZD0100DE.
      /
     ‚*-- PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  AZD0100DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  AZD0100DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  AZD0100DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  AZD0100DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  AZD0100DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  AZD0100DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

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

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CMDEXC-REC                    TYPE CMDEXC-TYPE.

       01  PTXTFIT-LIKE.
           COPY DDS-ALL-FORMATS OF PTXTFIT.

       01  PGM-WRK                       PIC X(10) VALUE "AZP0100".
       01  PGM-TYP                       PIC X(8)  VALUE "AZ:VERW".

       01  AUSWAHL                       PIC X(2).
       01  POS                           PIC 9(4).
       01  HEX-WRK                       PIC X(1).
       01  SORT-KZ                       PIC X(2) VALUE "11".
       01  TRENNLINIE                    PIC 9(1).

     ‚* KALENDERWOCHEN-VARIABLEN
       01  KW-TAB                        TYPE AZP0100-TAB.
       01  AKT-KW                        PIC 9(2).
       01  START-KW                      LIKE AKT-KW.

     ‚* DATUMSVARIABLEN
       01  DATE-JAHRANFANG               LIKE DATE-8.
       01  DATE-JAHRENDE                 LIKE DATE-8.

     ‚* GRUPPIERUNGSVARIABLEN
       01  CFIN01-GROUP                  LIKE CFIN01 OF CFG-CPY.
       01  CFIN02-GROUP                  LIKE CFIN02 OF CFG-CPY.

     ‚* PARAMETER FÜR DAS AZP0101
       01  USER-WRK                      LIKE T-USER.
       01  KW-WRK                        LIKE AKT-KW.
       01  MODE-WRK                      PIC X(20).
       01  KW-STRING                     PIC X(10).

     ‚* PARAMETER FÜR DAS CFP0008
       01  CFID-WRK                      LIKE CFID   OF PCONFIG-P.
       01  CFKEY-WRK                     LIKE CFKEY  OF PCONFIG-P.
       01  CFKEY2-WRK                    LIKE CFKEY2 OF PCONFIG-P.
       01  CFKEY3-WRK                    LIKE CFKEY3 OF PCONFIG-P.
       01  CFKEY4-WRK                    LIKE CFKEY4 OF PCONFIG-P.

     ‚* PARAMETER FÜR DAS CFP0023
       01  FTFILE-WRK                    LIKE FTFILE OF PTXTFIT-LIKE.
       01  FTKEY-WRK                     LIKE FTKEY  OF PTXTFIT-LIKE.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AZP0100" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "AZD0100DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     INPUT PCONFIG-DP
     ‚*             USER-KONFIGURATION LESEN
                    PERFORM GET-CONFIG
           END-IF.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.
           INITIALIZE TAB-MAX OF KW-TAB.
     ‚* ERMITTELN DATUM/ZEIT
           PERFORM  COPY-GET-TIME.
     ‚* ERMITTELN KALENDERWOCHE
           INITIALIZE KW-WRK.
           CALL     "CFP8007" USING DATE-8
                                    KW-WRK
           END-CALL.
           MOVE     KW-WRK TO AKT-KW.
           MOVE     KW-WRK TO START-KW.
           PERFORM  FILL-KW-TAB.
           IF       T-USER = "DSM"
                    MOVE 1      TO TRENNLINIE
           ELSE
                    MOVE ZEROES TO TRENNLINIE
           END-IF.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.

     ‚* VORAUSWAHL ANZEIGEN
       ANF010.
           PERFORM  VORAUSWAHL.
           IF       F3 = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* ANZEIGEN SUBFILE
       ANF020.
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3 = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    INITIALIZE F12
                    GO TO ANF010
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SELEKTIEREN ALLER NÖTIGEN DATEN
     ‚*--------------------------------------------------------------
       VORAUSWAHL SECTION.
       VOR-AUS-00.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "AZP0100"         TO MAINUEB OF FORMAT1.
           MOVE     "AZP0100*1"       TO SUBUEB  OF FORMAT1.

       VOR-AUS-10.
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
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR SCHLÜSSEL?
           IF       CFIN01 OF FORMAT1(1:1) = "?"
                    MOVE    "V110" TO CFID-WRK
                    PERFORM SHOW-KEYS
                    IF   CFKEY-WRK NOT = SPACES
                         MOVE CFKEY-WRK TO CFIN01 OF FORMAT1
                    END-IF
                    MOVE "CFIN01" TO DSP-CUR-FLD
                    GO TO VOR-AUS-10
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR SCHLÜSSEL?
           IF       CFIN02 OF FORMAT1(1:1) = "?"
                    MOVE    "V111" TO CFID-WRK
                    PERFORM SHOW-KEYS
                    IF   CFKEY-WRK NOT = SPACES
                         MOVE CFKEY-WRK TO CFIN02 OF FORMAT1
                    END-IF
                    MOVE "CFIN02" TO DSP-CUR-FLD
                    GO TO VOR-AUS-10
           END-IF.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SORTFILE EINLESEN UND AUSGEBEN
     ‚*--------------------------------------------------------------
       LESE-DATEN-SORTFILE SECTION.
       LES-DAS-00.

           INITIALIZE DATEN.
     ‚* AUSGABE SORTIEREN
           EVALUATE SORT-KZ
                    WHEN "11"
                         SORT     BSSORT-DAT ON ASCENDING KEY
                                                  CFIN01 OF BSSORT-REC,
                                                  CFIN02 OF BSSORT-REC,
                                                  CFFK01 OF BSSORT-REC,
                                                  CFFK02 OF BSSORT-REC,
                                                  CFFK03 OF BSSORT-REC,
                                                  CFFK04 OF BSSORT-REC,
                                                  CFFK05 OF BSSORT-REC
                                   WITH DUPLICATES
                                  INPUT  PROCEDURE LESE-DATEN
                                  OUTPUT PROCEDURE SUBFILE-SORT-OUTPUT
           END-EVALUATE.

       LES-DAS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

     ‚* PCONFIG LESEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "V100"              TO CFID   OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = "V100"
                    GO TO LES-DAT-90
           END-IF.

     ‚* KOPFSATZ NICHT
           IF       CFKEY  OF PCONFIG-P     = SPACES
                    GO TO LES-DAT-20
           END-IF.

     ‚* BERUFSSCHULE?
           IF       CFIN01 OF FORMAT1   NOT = SPACES
            AND     CFIN01 OF PCONFIG-P NOT = CFIN01 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* KLASSE?
           IF       CFIN02 OF FORMAT1   NOT = SPACES
            AND     CFIN02 OF PCONFIG-P NOT = CFIN02 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* BERUF?
           IF       CFIN03 OF FORMAT1   NOT = SPACES
            AND     CFIN03 OF PCONFIG-P NOT = CFIN03 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* JAHR?
           IF       CFNM04 OF FORMAT1   NOT = ZEROES
            AND     CFNM04 OF PCONFIG-P NOT = CFNM04 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* USER?
           IF       CFKEY  OF FORMAT1   NOT = SPACES
            AND     CFKEY  OF PCONFIG-P NOT = CFKEY  OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

           INITIALIZE BSSORT-REC.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                    SORT2-O OF BSSORT-REC.

           RELEASE  BSSORT-REC.
           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DES SUBFILES INKL. GRUPPIERUNG
     ‚*--------------------------------------------------------------
       SUBFILE-SORT-OUTPUT SECTION.
       SUB-SOU-00.

           INITIALIZE ANZREC-WRK.
           INITIALIZE CFIN01-GROUP.
           INITIALIZE CFIN02-GROUP.

     ‚* SORTFILE AUSLESEN
       SUB-SOU-20.
           RETURN   BSSORT-DAT AT END
                    GO TO SUB-SOU-90
           END-RETURN.


     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* GRUPPIERUNG BEIM ERSTEN MAL SICHERN
           IF       CFIN01-GROUP = SPACES
                    MOVE CFIN01 OF BSSORT-REC TO CFIN01-GROUP
           END-IF.

     ‚* DSM WILL EINE TRENNLINIE BEI WECHSEL SCHULE/KLASSE HABEN
           IF       TRENNLINIE = 1
                    IF   CFIN01 OF BSSORT-REC NOT = CFIN01-GROUP
                     OR  CFIN02 OF BSSORT-REC NOT = CFIN02-GROUP
                         PERFORM WRITE-TRENNLINIE
                         MOVE CFIN01 OF BSSORT-REC TO CFIN01-GROUP
                         MOVE CFIN02 OF BSSORT-REC TO CFIN02-GROUP
                    END-IF
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     BSSORT-REC        TO FORMAT2.
           MOVE     CFKEY  OF FORMAT2 TO USER-WRK.

     ‚* IN EINER ZEILE PASSEN 15 KW
           MOVE     ZEROES TO POS.
           PERFORM  TAB-MAX OF KW-TAB TIMES
                    ADD     1 TO POS
                    MOVE    KW OF KW-TAB(POS) TO KW-WRK
                    PERFORM GET-KW-STRING
                    PERFORM SET-KW-STRING
           END-PERFORM.

           PERFORM  SET-COLOR-FMT02.
           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           ADD      1            TO ANZREC-WRK.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

           IF       SUBFILE-RECORD-NUMBER < 9999
                    GO TO  SUB-SOU-20
           END-IF.

       SUB-SOU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TRENNLINIE AUSGEBEN
     ‚*--------------------------------------------------------------
       WRITE-TRENNLINIE SECTION.
       WRT-TRN-00.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     ALL "-" TO CFKEY  OF FORMAT2.
           MOVE     ALL "-" TO KW01   OF FORMAT2.
           MOVE     ALL "-" TO KW02   OF FORMAT2.
           MOVE     ALL "-" TO KW03   OF FORMAT2.
           MOVE     ALL "-" TO KW04   OF FORMAT2.
           MOVE     ALL "-" TO KW05   OF FORMAT2.
           MOVE     ALL "-" TO KW06   OF FORMAT2.
           MOVE     ALL "-" TO KW07   OF FORMAT2.

           PERFORM  SET-COLOR-FMT02.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           ADD      1            TO ANZREC-WRK.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

       WRT-TRN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
           MOVE     1          TO SFL-ZEILE.

       ANZ-SFL-10.
     ‚* SELEKTION ANZEIGEN
           MOVE     AUS    TO INXX.
           PERFORM  LESE-DATEN-SORTFILE.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES          TO WSREC-WRK.
           MOVE     "FMT04"         TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
           EVALUATE SFL-ZEILE
                    WHEN 1
                         MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                         MOVE 1 TO SUBFILE-RECORD-NUMBER
                    WHEN OTHER
                         IF   SFL-ZEILE <= ANZREC-WRK
                              MOVE SFL-ZEILE  TO SUBFILE-RECORD-NUMBER
                         ELSE
                              MOVE ANZREC-WRK TO SUBFILE-RECORD-NUMBER
                         END-IF
           END-EVALUATE.

           IF       DATEN = ZEROES
     ‚*             AUSGABE FEHLERMELDUNG
                    MOVE     "FMT05"         TO  FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE     AUS    TO IN91  IN  INXX
           ELSE
                    MOVE     AN     TO IN91  IN  INXX
           END-IF.
           MOVE     AN TO IN02 OF INXX.

           MOVE     START-KW              TO SHWKW   OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F19 LINKS (EINE KW NIEDRIGER)
           IF       IN19 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    IF   START-KW > 1
                         SUBTRACT 1 FROM START-KW
                    END-IF
                    PERFORM FILL-KW-TAB
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* F20 RECHTS (EINE KW HÖHER)
           IF       IN20 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    IF   START-KW < 57
                         ADD 1 TO START-KW
                    END-IF
                    PERFORM FILL-KW-TAB
                    GO TO ANZ-SFL-10
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    GO TO ANZ-SFL-10
           END-IF.

           MOVE     WSREC-WRK      TO FORMAT2-I.
           MOVE     CORR FORMAT2-I TO FORMAT2.

           IF       AUSW OF FORMAT2 = SPACES
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* RECORD-NUMMER SICHERN, DAMIT BEI FEHLERMELDUNG DER CURSOR
     ‚* AN DER RICHTIGEN POSITION STEHT
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.

     ‚* BERECHTIGUNG FÜR DIESE FUNKTION ÜBERPRÜFEN
           IF       AUSW OF FORMAT2 NOT = SPACES
                    MOVE  SPACES          TO RET-CODE
                    MOVE  AUSW OF FORMAT2 TO AUSWAHL
                    CALL "CFP9002" USING T-USER
                                         AUSWAHL
                                         PGM-WRK
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         MOVE     SPACES TO AUSW OF FORMAT2
                         MOVE     "FMT02" TO FMT-WRK
                         MOVE     FORMAT2 TO WSREC-WRK
                         PERFORM  COPY-DISPLAY-SFLREWRITE
                         GO TO ANZ-SFL-50
                    END-IF
           END-IF.

     ‚* MEHR TEXT ZU DEM EINTRAG ERFASSEN
           IF       AUSW OF FORMAT2 = "8"
                    MOVE "AZ:VERW"         TO FTFILE-WRK
                    MOVE CFKEY  OF FORMAT2 TO FTKEY-WRK
                    MOVE "2"               TO AUSWAHL
                    CALL "CFP0023" USING FTFILE-WRK
                                         FTKEY-WRK
                                         AUSWAHL
                                         PGM-WRK
                                         F3
                                         F12
                    END-CALL
                    IF   F3 = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    INITIALIZE F12
           END-IF.

     ‚* URLAUB ANZEIGEN
           IF       AUSW OF FORMAT2 = "9"
                    CALL "PROPGM612" USING CFKEY  OF FORMAT2(1:3)
                                           DATE-JAHRANFANG
                                           DATE-JAHRENDE
                                           F3
                    END-CALL
                    IF   F3 = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    INITIALIZE F12
           END-IF.

     ‚* GB MIT USERNAME AUFRUFEN
           IF       AUSW OF FORMAT2 = "T"
                    PERFORM MAT-GB
           END-IF.

           PERFORM  SET-COLOR-FMT02.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.
           MOVE     AUS TO INXX.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "AZP0100"         TO MAINUEB OF FORMAT3.
           MOVE     "AZP0100*3"       TO SUBUEB  OF FORMAT3.

     ‚* IN EINER ZEILE PASSEN 15 KW
           MOVE     ZEROES TO POS.
           PERFORM  TAB-MAX OF KW-TAB TIMES
                    ADD     1 TO POS
                    MOVE    KW OF KW-TAB(POS) TO KW-WRK
                    PERFORM SET-KW-HED-STRING
           END-PERFORM.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOMMANDOZEILE AUFRUFEN
     ‚*--------------------------------------------------------------
       KOMMANDOZEILE SECTION.
       CMD-LIN-00.

           CALL     "XXP2001"
           END-CALL.

       CMD-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN VON INFORMATIONSTEXTEN
     ‚*--------------------------------------------------------------
       ANZEIGE-PGM-INFOS SECTION.
       ANZ-PGM-00.

           INITIALIZE F12.
     ‚* INFORMATIONEN ANZEIGEN
           CALL     "CFP0009" USING PGM-WRK
                                    F12
           END-CALL.
           INITIALIZE F12.

       ANZ-PGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GB STARTEN
     ‚*--------------------------------------------------------------
       MAT-GB SECTION.
       MAT-GBS-00.

     ‚* COMMAND AUFRUFEN
           INITIALIZE CMDEXC-REC.
           STRING   "QGPL/GB "             DELIMITED BY SIZE
                    "MAKU("                DELIMITED BY SIZE
                    CFKEY  OF FORMAT2      DELIMITED BY SPACES
                    ")"                    DELIMITED BY SIZE
               INTO COMMAND OF CMDEXC-REC
           END-STRING.
           CALL     "CLP0057" USING CMDEXC-REC
                                    RET-CODE
           END-CALL.

       MAT-GBS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER INTERNEN TABELLE
     ‚*--------------------------------------------------------------
       FILL-KW-TAB SECTION.
       FIL-KWT-00.

     ‚* DA ES SONST NIRGENDS PASST, HIER AUCH EBEN DEN DATUMSBERICH
     ‚* FÜR DIE URLAUBSABFRAGE FESTLEGEN. WIR GEHEN ERSTMAL ÜBER
     ‚* DAS GANZE JAHR. GGF. SPÄTER AUCH KW BEZOGEN
           MOVE     DATE-8 TO DATE-JAHRANFANG.
           MOVE     DATE-8 TO DATE-JAHRENDE.
           MOVE     0101   TO DATE-JAHRANFANG(5:4).
           MOVE     1231   TO DATE-JAHRENDE(5:4).

     ‚* IMMER BEI DER START-KW ANFANGEN
           MOVE     START-KW    TO KW-WRK.

           MOVE     ZEROES TO TAB-MAX OF KW-TAB.
     ‚* TABELLE FÜLLEN (KW-UMBRUCH ERFOLGT SPÄTER)
           PERFORM  7 TIMES
                    ADD  1      TO TAB-MAX OF KW-TAB
                    MOVE KW-WRK TO KW   OF KW-TAB(TAB-MAX OF KW-TAB)
                    ADD  1      TO KW-WRK
           END-PERFORM.

       FIL-KWT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFRUF CFP0008 ZUR ANZEIGE DER KEYS
     ‚*--------------------------------------------------------------
       SHOW-KEYS SECTION.
       SHW-KEY-00.

           INITIALIZE F12.
     ‚* ANZEIGEN DER SCHLÜSSEL
           MOVE     SPACES TO CFKEY-WRK.
           MOVE     SPACES TO CFKEY2-WRK.
           MOVE     SPACES TO CFKEY3-WRK.
           MOVE     SPACES TO CFKEY4-WRK.
           MOVE     SPACES TO F12.
           CALL     "CFP0008" USING CFID-WRK
                                    CFKEY-WRK
                                    CFKEY2-WRK
                                    CFKEY3-WRK
                                    CFKEY4-WRK
                                    F12
           END-CALL.
           IF       F12 = "1"
                    MOVE SPACES TO CFKEY-WRK
           END-IF.

       SHW-KEY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBE FÜR FMT02 SETZEN
     ‚*--------------------------------------------------------------
       SET-COLOR-FMT02 SECTION.
       SET-COL-00.

           IF       CFKEY  OF FORMAT2 NOT = "----------"
                    CALL     "AZP0102" USING USER-WRK
                                             HEX-WRK
                    END-CALL
                    MOVE HEX-WRK   TO A_CFKEY  OF FORMAT2
                    MOVE HEX-WRK   TO A_CFFK01 OF FORMAT2
                    MOVE HEX-WRK   TO A_CFFK02 OF FORMAT2
                    MOVE HEX-WRK   TO A_CFFK03 OF FORMAT2
                    MOVE HEX-WRK   TO A_CFFK04 OF FORMAT2
                    MOVE HEX-WRK   TO A_CFFK05 OF FORMAT2
           ELSE
                    MOVE COL-BLU   TO A_CFKEY  OF FORMAT2
                    MOVE COL-BLU   TO A_KW01   OF FORMAT2
                    MOVE COL-BLU   TO A_KW02   OF FORMAT2
                    MOVE COL-BLU   TO A_KW03   OF FORMAT2
                    MOVE COL-BLU   TO A_KW04   OF FORMAT2
                    MOVE COL-BLU   TO A_KW05   OF FORMAT2
                    MOVE COL-BLU   TO A_KW06   OF FORMAT2
                    MOVE COL-BLU   TO A_KW07   OF FORMAT2
                    MOVE ATR-PR-ND TO A_AUSW   OF FORMAT2
           END-IF.

     ‚* INDIKATOR ANSCHALTEN
           MOVE     AN TO IN02 OF INXX.

       SET-COL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SETZEN DES KW-STRINGS (HEADER)
     ‚*--------------------------------------------------------------
       SET-KW-HED-STRING SECTION.
       SET-KHS-00.

     ‚* AKTUELLE KW HIGHLIGHTEN
           IF       KW-WRK = AKT-KW
                    MOVE ATR-PR-HI-RI TO HEX-WRK
           ELSE
                    MOVE ATR-PR-HI    TO HEX-WRK
           END-IF.

           EVALUATE POS
                    WHEN  1 MOVE KW-WRK    TO HED001   OF FORMAT3(2:)
                            MOVE HEX-WRK   TO A_HED001 OF FORMAT3
                    WHEN  2 MOVE KW-WRK    TO HED002   OF FORMAT3(2:)
                            MOVE HEX-WRK   TO A_HED002 OF FORMAT3
                    WHEN  3 MOVE KW-WRK    TO HED003   OF FORMAT3(2:)
                            MOVE HEX-WRK   TO A_HED003 OF FORMAT3
                    WHEN  4 MOVE KW-WRK    TO HED004   OF FORMAT3(2:)
                            MOVE HEX-WRK   TO A_HED004 OF FORMAT3
                    WHEN  5 MOVE KW-WRK    TO HED005   OF FORMAT3(2:)
                            MOVE HEX-WRK   TO A_HED005 OF FORMAT3
                    WHEN  6 MOVE KW-WRK    TO HED006   OF FORMAT3(2:)
                            MOVE HEX-WRK   TO A_HED006 OF FORMAT3
                    WHEN  7 MOVE KW-WRK    TO HED007   OF FORMAT3(2:)
                            MOVE HEX-WRK   TO A_HED007 OF FORMAT3
           END-EVALUATE.

       SET-KHS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SETZEN DES KW-STRINGS
     ‚*--------------------------------------------------------------
       SET-KW-STRING SECTION.
       SET-KWS-00.

           EVALUATE POS
                    WHEN  1 MOVE KW-STRING TO KW01   OF FORMAT2
                    WHEN  2 MOVE KW-STRING TO KW02   OF FORMAT2
                    WHEN  3 MOVE KW-STRING TO KW03   OF FORMAT2
                    WHEN  4 MOVE KW-STRING TO KW04   OF FORMAT2
                    WHEN  5 MOVE KW-STRING TO KW05   OF FORMAT2
                    WHEN  6 MOVE KW-STRING TO KW06   OF FORMAT2
                    WHEN  7 MOVE KW-STRING TO KW07   OF FORMAT2
           END-EVALUATE.

       SET-KWS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES KW-STRINGS
     ‚*--------------------------------------------------------------
       GET-KW-STRING SECTION.
       GET-KWS-00.

           INITIALIZE KW-STRING.
     ‚* DAS PROGRAMM ERMITTELT EINEN STRING FÜR DIE AUSGABE DER KW
           CALL     "AZP0101" USING USER-WRK
                                    KW-WRK
                                    MODE-WRK
                                    KW-STRING
           END-CALL.

       GET-KWS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* USERKONFIGURATION
     ‚*--------------------------------------------------------------
       GET-CONFIG SECTION.
       GET-CFG-00.

           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H103"            TO CFID   OF CFG-CPY.
           MOVE     T-USER            TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    MOVE "DAY" TO CFIN01 OF CFG-CPY
           END-IF.

     ‚* EINSTELLUNGEN ÜBERNEHMEN
           MOVE     CFIN01 OF CFG-CPY TO MODE-WRK.

       GET-CFG-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN KONFIGURATIONSDATEI
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
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
