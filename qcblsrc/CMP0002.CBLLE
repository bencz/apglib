       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP0002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* COMPILEREINSTELLUNGEN PFLEGEN                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSWAHL + ANZEIGE COMPILEREINSTELLUNG        *
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
                  ASSIGN       TO  WORKSTATION-CMD0002DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI EINSTELLUNGEN
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
           COPY DDS-ALL-FORMATS OF CMD0002DE.
      /
     ‚*--- KONFIGURATION
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CMD0002DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CMD0002DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  CMD0002DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CMD0002DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CMD0002DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CMD0002DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  FORMAT6X.
           COPY  DDS-FMT06-O OF  CMD0002DE
           REPLACING FMT06-O BY FORMAT6
                     FMT06-O-INDIC BY FORMAT6-O-INDIC.

       01  FORMAT6Y.
           COPY  DDS-FMT06-I OF  CMD0002DE
           REPLACING FMT06-I BY FORMAT6-I
                     FMT06-I-INDIC BY FORMAT6-I-INDIC.

       01  FORMAT7X.
           COPY  DDS-FMT07-O OF  CMD0002DE
           REPLACING FMT07-O BY FORMAT7
                     FMT07-O-INDIC BY FORMAT7-O-INDIC.

       01  FORMAT7Y.
           COPY  DDS-FMT07-I OF  CMD0002DE
           REPLACING FMT07-I BY FORMAT7-I
                     FMT07-I-INDIC BY FORMAT7-I-INDIC.


       01  FORMAT8X.
           COPY  DDS-FMT08-O OF  CMD0002DE
           REPLACING FMT08-O BY FORMAT8
                     FMT08-O-INDIC BY FORMAT8-O-INDIC.

       01  FORMAT8Y.
           COPY  DDS-FMT08-I OF  CMD0002DE
           REPLACING FMT08-I BY FORMAT8-I
                     FMT08-I-INDIC BY FORMAT8-I-INDIC.

       01  FORMAT9X.
           COPY  DDS-FMT09-O OF  CMD0002DE
           REPLACING FMT09-O BY FORMAT9
                     FMT09-O-INDIC BY FORMAT9-O-INDIC.

       01  FORMAT9Y.
           COPY  DDS-FMT09-I OF  CMD0002DE
           REPLACING FMT09-I BY FORMAT9-I
                     FMT09-I-INDIC BY FORMAT9-I-INDIC.

       01  FORMAT10X.
           COPY  DDS-FMT10-O OF  CMD0002DE
           REPLACING FMT10-O BY FORMAT10
                     FMT10-O-INDIC BY FORMAT10-O-INDIC.

       01  FORMAT10Y.
           COPY  DDS-FMT10-I OF  CMD0002DE
           REPLACING FMT10-I BY FORMAT10-I
                     FMT10-I-INDIC BY FORMAT10-I-INDIC.

       01  FORMAT11X.
           COPY  DDS-FMT11-O OF  CMD0002DE
           REPLACING FMT11-O BY FORMAT11
                     FMT11-O-INDIC BY FORMAT11-O-INDIC.

       01  FORMAT11Y.
           COPY  DDS-FMT11-I OF  CMD0002DE
           REPLACING FMT11-I BY FORMAT11-I
                     FMT11-I-INDIC BY FORMAT11-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP0002".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  RTVOBJD-REC                   TYPE RTVOBJD-TYPE.

       01  LEN-L                         PIC 9(4).
       01  LEN-F                         PIC 9(4).
       01  LEN-M                         PIC 9(4).
       01  CFKEY-WRK                     LIKE CFKEY  OF PCONFIG-P.
       01  CFKEY2-WRK                    LIKE CFKEY2 OF PCONFIG-P.
       01  CFKEY3-WRK                    LIKE CFKEY2 OF PCONFIG-P.
       01  AUSWAHL                       PIC X(2).

     ‚* PARAMETER FÜR DAS CLP0008
       01  LIB-WRK                       PIC X(10).
       01  FILE-WRK                      PIC X(10).
       01  MBR-WRK                       PIC X(10).
       01  ERROR-TEXT-SHORT              PIC X(255).
       01  ERROR-TEXT-LONG               PIC X(3000).
       01  OPTION-WRK                    PIC X(2).

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  HSLIB-WRK                     LIKE HSLIB  OF PARCHIV-WRK.
       01  HSFILE-WRK                    LIKE HSFILE OF PARCHIV-WRK.

     ‚* SOURCE ZUM ARBEITEN
       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.
       01  SRCSEQ-WRK                    LIKE SRCSEQ OF QSETSRC-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CMP0002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CMD0002DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PCONFIG-DP.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.
           PERFORM  GET-VORBELEGUNG.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DER TAGS BZW. PROGRAMMNAME
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN ANHAND DER TAG-DATEI
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                         GO TO ANF010
                    END-IF
                    GO TO ANF020
           END-IF.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3 = "1"
                    GO TO ENDE
           END-IF.
           IF       F5 = "1"
                    INITIALIZE F5
                    GO TO ANF020
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

           CLOSE                 PCONFIG-DP.

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
           MOVE     "CMP0000"         TO MAINUEB OF FORMAT1.
           MOVE     "CMP0002*1"       TO SUBUEB  OF FORMAT1.

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
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F9 KOMMANDOZEILE AUFRUFEN
           IF       IN09 OF FORMAT1-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO VOR-AUS-10
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK, LEN-L, LEN-F, LEN-M.
     ‚* WURDE EINE LIB MIT STERN SELEKTIERT?
           IF       CFKEY  OF FORMAT1      NOT = SPACES
            AND     CFKEY  OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN-L
                    INSPECT  CFKEY  OF FORMAT1
                             TALLYING LEN-L FOR CHARACTERS
                             BEFORE INITIAL "*"
                    MOVE     CFKEY  OF FORMAT1(1:LEN-L) TO CFKEY-WRK
           ELSE
                    MOVE     SPACES                     TO CFKEY-WRK
           END-IF.

     ‚* WURDE EINE DATEI MIT STERN SELEKTIERT?
           IF       CFKEY2 OF FORMAT1      NOT = SPACES
            AND     CFKEY2 OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN-F
                    INSPECT  CFKEY2 OF FORMAT1
                             TALLYING LEN-F FOR CHARACTERS
                             BEFORE INITIAL "*"
                    MOVE     CFKEY2 OF FORMAT1(1:LEN-F) TO CFKEY2-WRK
           ELSE
                    MOVE     SPACES                     TO CFKEY2-WRK
           END-IF.

     ‚* WURDE EINE TEILDATEI MIT STERN SELEKTIERT?
           IF       CFKEY3 OF FORMAT1      NOT = SPACES
            AND     CFKEY3 OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN-M
                    INSPECT  CFKEY3 OF FORMAT1
                             TALLYING LEN-M FOR CHARACTERS
                             BEFORE INITIAL "*"
                    MOVE     CFKEY3 OF FORMAT1(1:LEN-M) TO CFKEY3-WRK
           ELSE
                    MOVE     SPACES                     TO CFKEY3-WRK
           END-IF.

     ‚* PCONFIG LESEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "CP"                TO CFID   OF PCONFIG-P.
           MOVE     CFKEY-WRK           TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2-WRK          TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3-WRK          TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCONFIG-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = "CP"
                    GO TO LES-DAT-90
           END-IF.

           IF       LEN-L > ZEROES
            AND     CFKEY  OF PCONFIG-P(1:LEN-L) NOT =
                    CFKEY  OF FORMAT1(1:LEN-L)
                    GO TO LES-DAT-90
           END-IF.

           IF       LEN-F > ZEROES
            AND     CFKEY2 OF PCONFIG-P(1:LEN-F) NOT =
                    CFKEY2 OF FORMAT1(1:LEN-F)
                    GO TO LES-DAT-20
           END-IF.

           IF       LEN-M > ZEROES
            AND     CFKEY3 OF PCONFIG-P(1:LEN-M) NOT =
                    CFKEY3 OF FORMAT1(1:LEN-M)
                    GO TO LES-DAT-20
           END-IF.

     ‚* NUR BESTIMMTE OBJEKTART ANZEIGEN?
           IF       CFIN01 OF FORMAT1   NOT = SPACES
            AND     CFIN01 OF PCONFIG-P NOT = CFIN01 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* NACH TEXT SELEKTIEREN?
           IF       TXTSEL OF FORMAT1   NOT = SPACES
                    MOVE PCONFIG-P           TO RECORD-WRK
                    MOVE LENGTH OF PCONFIG-P TO RECORD-LEN
                    MOVE TXTSEL OF FORMAT1   TO SELEKTION-WRK
                    CALL PROCEDURE "HLP0001" USING RECORD-WRK
                                                   RECORD-LEN
                                                   SELEKTION-WRK
                                                   RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         GO TO LES-DAT-20
                    END-IF
           END-IF.

     ‚* AUSGABE EINER SUBFILE-ZEILE
           PERFORM  AUSGABE-SFL.
           IF       SUBFILE-RECORD-NUMBER < 9999
                    GO TO LES-DAT-20
           END-IF.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER SFL-ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-SFL SECTION.
       AUS-SFL-00.

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

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO FORMAT2.

     ‚* KEYS SIND 20-STELLIG UND ALS HIDDEN-FELDER DEFINIERT, JETZT
     ‚* DIE FELDER FÜR DIE ANZEIGE FÜLLEN
           MOVE     CFKEY  OF PCONFIG-P(1:10) TO LIB       OF FORMAT2.
           MOVE     CFKEY2 OF PCONFIG-P(1:10) TO FILENM    OF FORMAT2.
           MOVE     CFKEY3 OF PCONFIG-P(1:10) TO MBR       OF FORMAT2.
           PERFORM  GET-OBJD.
           MOVE     CRTUSER OF RTVOBJD-REC    TO GEWANDELT OF FORMAT2.
           IF       GEWANDELT OF FORMAT2 NOT = "APG"
            AND     GEWANDELT OF FORMAT2 NOT = SPACES
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    MOVE AUS TO IN41 OF INXX
           END-IF.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

       AUS-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
           MOVE     AUS    TO INXX.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
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

           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AN     TO IN91  IN  INXX.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.

     ‚* BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
           MOVE     9999 TO SFL-ZEILE.

     ‚* F3 ENDE
           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN08 OF FORMAT3-I-INDIC = AN
                    PERFORM NEUANLAGE
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                         GO TO ANZ-SFL-20
                    END-IF
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F9 KOMMANDOZEILE AUFRUFEN
           IF       IN09 OF FORMAT3-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
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

     ‚* ÄNDERN DES DATENSATZES
           IF       AUSW OF FORMAT2 = "1"
                    PERFORM BEARBEITEN-DATEN
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                    END-IF
           END-IF.

     ‚* LÖSCHEN DES SATZES
           IF       AUSW OF FORMAT2 = "4"
                    PERFORM LOESCHEN-EINTRAG
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
           END-IF.

     ‚* DETAILANZEIGE DES DATENSATZES
           IF       AUSW OF FORMAT2 = "5"
                    PERFORM START-SEU
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FEHLERMELDUNG WENN KEINE SFL-ZEILEN VORHANDEN
     ‚*--------------------------------------------------------------
       FEHLERMELDUNG SECTION.
       FEH-MEL-00.

           INITIALIZE F3, F12.
           INITIALIZE FORMAT3.

     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX    TO  FORMAT3-O-INDIC.
           MOVE     "FMT03"        TO FMT-WRK.
           MOVE     FORMAT3        TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.

           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO FEH-MEL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN08 OF FORMAT3-I-INDIC = AN
                    PERFORM NEUANLAGE
                    IF   F3  = "1"
                         GO TO FEH-MEL-90
                    END-IF
                    INITIALIZE F12
                    GO TO FEH-MEL-90
           END-IF.

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CMP0000"         TO MAINUEB OF FORMAT3.
           MOVE     "CMP0002*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUANLAGE EINES PROGRAMMS
     ‚*--------------------------------------------------------------
       NEUANLAGE SECTION.
       NEU-ANL-00.

           INITIALIZE F3, F12.
           INITIALIZE LIB-WRK, FILE-WRK

       NEU-ANL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEARBEITEN DER DATEN
     ‚*--------------------------------------------------------------
       BEARBEITEN-DATEN SECTION.
       BEA-DAT-00.

           INITIALIZE FORMAT6.
     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO BEA-DAT-90
           END-IF.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CMP0000"         TO MAINUEB OF FORMAT6.
           MOVE     "CMP0002*6"       TO SUBUEB  OF FORMAT6.

           MOVE     CORR PCONFIGF OF PCONFIG-P   TO FORMAT6.

       BEA-DAT-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT6.

           MOVE     CORR INXX       TO  FORMAT6-O-INDIC.
           MOVE     "FMT06"         TO  FMT-WRK.
           MOVE     FORMAT6         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT6-I.
           MOVE     CORR FORMAT6-I  TO FORMAT6.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT6-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO BEA-DAT-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT6-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO BEA-DAT-90
           END-IF.

     ‚* PLAUSIBILITÄTSPRÜFUNG
           PERFORM  CHECK-EINGABE.
           IF       RET-CODE NOT = SPACES
                    GO TO BEA-DAT-10
           END-IF.

     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN BEFÜLLEN
                    MOVE CORR FORMAT6 TO PCONFIGF OF PCONFIG-P
                    PERFORM COPY-GET-TIME
     ‚*             DATUM/UHRZEIT/TERMINAL/USER
                    MOVE    T-USER      TO CFMSER OF PCONFIG-P
                    MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                    MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                    MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                    MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    REWRITE PCONFIG-P
           END-IF.

     ‚* NACHDEM DER SATZ GESCHRIEBEN WURDE, IN DIE FOLGEFORMATE
     ‚* VERZWEIGEN, SOFERN ES EINS FÜR DIESEN TYP GIBT
           EVALUATE CFIN01 OF FORMAT6
                    WHEN "CBL"
                    WHEN "CBLLE"
                         PERFORM CHANGE-CBLLE
                    WHEN "CLP"
                    WHEN "CLLE"
                         PERFORM CHANGE-CLLE
                    WHEN "CMD"
                         PERFORM CHANGE-CMD
                    WHEN "DSPF"
                         PERFORM CHANGE-DSPF
                    WHEN "C"
                         PERFORM CHANGE-C
           END-EVALUATE.
           IF       F12 = "1"
                    INITIALIZE F12
                    GO TO BEA-DAT-00
           END-IF.

       BEA-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER COBOL-SPEZIFISCHEN DATEN
     ‚*--------------------------------------------------------------
       CHANGE-CBLLE SECTION.
       CHG-CBL-00.

           INITIALIZE FORMAT7.
     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-CBL-90
           END-IF.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CMP0000"         TO MAINUEB OF FORMAT7.
           MOVE     "CMP0002*7"       TO SUBUEB  OF FORMAT7.
           MOVE     CORR PCONFIGF OF PCONFIG-P   TO FORMAT7.

       CHG-CBL-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT7.

           MOVE     CORR INXX       TO  FORMAT7-O-INDIC.
           MOVE     "FMT07"         TO  FMT-WRK.
           MOVE     FORMAT7         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT7-I.
           MOVE     CORR FORMAT7-I  TO FORMAT7.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT7-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO CHG-CBL-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT7-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO CHG-CBL-90
           END-IF.

     ‚* PLAUSIBILITÄTSPRÜFUNG
           PERFORM  CHECK-EINGABE.
           IF       RET-CODE NOT = SPACES
                    GO TO CHG-CBL-10
           END-IF.

     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN BEFÜLLEN
                    MOVE CORR FORMAT7 TO PCONFIGF OF PCONFIG-P
                    PERFORM COPY-GET-TIME
     ‚*             DATUM/UHRZEIT/TERMINAL/USER
                    MOVE    T-USER      TO CFMSER OF PCONFIG-P
                    MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                    MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                    MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                    MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    REWRITE PCONFIG-P
           END-IF.

       CHG-CBL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER CMD-SPEZIFISCHEN DATEN
     ‚*--------------------------------------------------------------
       CHANGE-CMD SECTION.
       CHG-CMD-00.

           INITIALIZE FORMAT8.
     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-CMD-90
           END-IF.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CMP0000"         TO MAINUEB OF FORMAT8.
           MOVE     "CMP0002*8"       TO SUBUEB  OF FORMAT8.
           MOVE     CORR PCONFIGF OF PCONFIG-P   TO FORMAT8.

       CHG-CMD-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT8.

           MOVE     CORR INXX       TO  FORMAT8-O-INDIC.
           MOVE     "FMT08"         TO  FMT-WRK.
           MOVE     FORMAT8         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT8-I.
           MOVE     CORR FORMAT8-I  TO FORMAT8.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT8-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO CHG-CMD-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT8-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO CHG-CMD-90
           END-IF.

     ‚* PLAUSIBILITÄTSPRÜFUNG
           PERFORM  CHECK-EINGABE.
           IF       RET-CODE NOT = SPACES
                    GO TO CHG-CMD-10
           END-IF.

     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN BEFÜLLEN
                    MOVE CORR FORMAT8 TO PCONFIGF OF PCONFIG-P
                    PERFORM COPY-GET-TIME
     ‚*             DATUM/UHRZEIT/TERMINAL/USER
                    MOVE    T-USER      TO CFMSER OF PCONFIG-P
                    MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                    MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                    MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                    MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    REWRITE PCONFIG-P
           END-IF.

       CHG-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER DSPF-SPEZIFISCHEN DATEN
     ‚*--------------------------------------------------------------
       CHANGE-DSPF SECTION.
       CHG-DSP-00.

           INITIALIZE FORMAT9.
     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-DSP-90
           END-IF.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CMP0000"         TO MAINUEB OF FORMAT9.
           MOVE     "CMP0002*9"       TO SUBUEB  OF FORMAT9.
           MOVE     CORR PCONFIGF OF PCONFIG-P   TO FORMAT9.

       CHG-DSP-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT9.

           MOVE     CORR INXX       TO  FORMAT9-O-INDIC.
           MOVE     "FMT09"         TO  FMT-WRK.
           MOVE     FORMAT9         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT9-I.
           MOVE     CORR FORMAT9-I  TO FORMAT9.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT9-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO CHG-DSP-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT9-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO CHG-DSP-90
           END-IF.

     ‚* PLAUSIBILITÄTSPRÜFUNG
           PERFORM  CHECK-EINGABE.
           IF       RET-CODE NOT = SPACES
                    GO TO CHG-DSP-10
           END-IF.

     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN BEFÜLLEN
                    MOVE CORR FORMAT9 TO PCONFIGF OF PCONFIG-P
                    PERFORM COPY-GET-TIME
     ‚*             DATUM/UHRZEIT/TERMINAL/USER
                    MOVE    T-USER      TO CFMSER OF PCONFIG-P
                    MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                    MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                    MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                    MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    REWRITE PCONFIG-P
           END-IF.

       CHG-DSP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER COBOL-SPEZIFISCHEN DATEN
     ‚*--------------------------------------------------------------
       CHANGE-CLLE SECTION.
       CHG-CLL-00.

           INITIALIZE FORMAT10.
     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-CLL-90
           END-IF.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CMP0000"         TO MAINUEB OF FORMAT10.
           MOVE     "CMP0002*10"      TO SUBUEB  OF FORMAT10.
           MOVE     CORR PCONFIGF OF PCONFIG-P   TO FORMAT10.

       CHG-CLL-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT10.

           MOVE     CORR INXX       TO  FORMAT10-O-INDIC.
           MOVE     "FMT10"         TO  FMT-WRK.
           MOVE     FORMAT10        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT10-I.
           MOVE     CORR FORMAT10-I TO FORMAT10.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT10-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO CHG-CLL-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT10-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO CHG-CLL-90
           END-IF.

     ‚* PLAUSIBILITÄTSPRÜFUNG
           PERFORM  CHECK-EINGABE.
           IF       RET-CODE NOT = SPACES
                    GO TO CHG-CLL-10
           END-IF.

     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN BEFÜLLEN
                    MOVE CORR FORMAT10 TO PCONFIGF OF PCONFIG-P
                    PERFORM COPY-GET-TIME
     ‚*             DATUM/UHRZEIT/TERMINAL/USER
                    MOVE    T-USER      TO CFMSER OF PCONFIG-P
                    MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                    MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                    MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                    MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    REWRITE PCONFIG-P
           END-IF.

       CHG-CLL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER C-SPEZIFISCHEN DATEN
     ‚*--------------------------------------------------------------
       CHANGE-C SECTION.
       CHG-CMO-00.

           INITIALIZE FORMAT11.
     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-CMO-90
           END-IF.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CMP0000"         TO MAINUEB OF FORMAT11.
           MOVE     "CMP0002*11"      TO SUBUEB  OF FORMAT11.
           MOVE     CORR PCONFIGF OF PCONFIG-P   TO FORMAT11.

       CHG-CMO-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT11.

           MOVE     CORR INXX       TO  FORMAT11-O-INDIC.
           MOVE     "FMT11"         TO  FMT-WRK.
           MOVE     FORMAT11        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT11-I.
           MOVE     CORR FORMAT11-I TO FORMAT11.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT11-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO CHG-CMO-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT11-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO CHG-CMO-90
           END-IF.

     ‚* PLAUSIBILITÄTSPRÜFUNG
           PERFORM  CHECK-EINGABE.
           IF       RET-CODE NOT = SPACES
                    GO TO CHG-CMO-10
           END-IF.

     ‚* REWRITE DER DATEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN BEFÜLLEN
                    MOVE CORR FORMAT11 TO PCONFIGF OF PCONFIG-P
                    PERFORM COPY-GET-TIME
     ‚*             DATUM/UHRZEIT/TERMINAL/USER
                    MOVE    T-USER      TO CFMSER OF PCONFIG-P
                    MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                    MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                    MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                    MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    REWRITE PCONFIG-P
           END-IF.

       CHG-CMO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DES EINTRAGS
     ‚*--------------------------------------------------------------
       LOESCHEN-EINTRAG SECTION.
       LOE-EIN-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PCONFIG-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO HSLIB-WRK
                    MOVE "PCONFIG"   TO HSFILE-WRK
                    MOVE PCONFIG-P   TO RECORD-WRK
                    CALL "CFP8100" USING HSLIB-WRK
                                         HSFILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       LOE-EIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STARTEN DES SEU ÜBER EIN CL
     ‚*--------------------------------------------------------------
       START-SEU SECTION.
       STR-SEU-00.

           INITIALIZE RET-CODE.
           INITIALIZE FILE-WRK.
           INITIALIZE LIB-WRK.
           INITIALIZE MBR-WRK.
           INITIALIZE OPTION-WRK.

     ‚* HIER KOMMT MAN EH NUR MIT "5" REIN
           MOVE     AUSW OF FORMAT2     TO OPTION-WRK.
     ‚* DATEI UMSETZEN
           MOVE     CFKEY  OF FORMAT2   TO LIB-WRK.
           MOVE     CFKEY2 OF FORMAT2   TO FILE-WRK.
           MOVE     CFKEY3 OF FORMAT2   TO MBR-WRK.

     ‚* SEU STARTEN
           CALL     "CRP0036" USING LIB-WRK
                                    FILE-WRK
                                    MBR-WRK
                                    SRCSEQ-WRK
                                    OPTION-WRK
                                    F3
                                    F12
           END-CALL.

       STR-SEU-90.
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
     ‚* ERMITTELN DER VORBELUNG
     ‚*--------------------------------------------------------------
       GET-VORBELEGUNG SECTION.
       GET-VOB-00.

     ‚* ERSTMAL DIE STANDARDWERTE NEHMEN
           MOVE     "APG"        TO CFKEY  OF FORMAT1.
           MOVE     "*"          TO CFKEY2 OF FORMAT1.
           MOVE     "*"          TO CFKEY3 OF FORMAT1.

       GET-VOB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER OBJEKTBESCHREIBUNG
     ‚*--------------------------------------------------------------
       GET-OBJD SECTION.
       GET-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* ERMITTELN DES PROGRAMMTEXTES
           INITIALIZE RTVOBJD-REC.
           MOVE     LIB    OF FORMAT2   TO OBJLIB   OF RTVOBJD-REC.
           MOVE     MBR    OF FORMAT2   TO OBJ      OF RTVOBJD-REC.
           CALL     "HLP0010" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.

       GET-OBJ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* <bitte Kommentar eingeben>
     ‚*--------------------------------------------------------------
       CHECK-EINGABE SECTION.
       CHK-EIN-00.

           INITIALIZE RET-CODE.

       CHK-EIN-90.
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
