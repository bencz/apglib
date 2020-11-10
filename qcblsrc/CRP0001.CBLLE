       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* STARTPROGRAMM FÜR CODEREVIEW                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSWAHL + START CODEREVIEW                   *
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
                  ASSIGN       TO  WORKSTATION-CRD0001DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI CODEREVIEW
     ‚*-------------------------------------------------------------
           SELECT PCHKSRC-DP
                  ASSIGN       TO  DATABASE-PCHKSRC
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI CHKSRC SORTIERT NACH VORGANG
     ‚*-------------------------------------------------------------
           SELECT PCHKSRC-DL
                  ASSIGN       TO  DATABASE-PCHKSRCL1
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
           COPY DDS-ALL-FORMATS OF CRD0001DE.
      /
     ‚*--- CODEREVIEW
       FD  PCHKSRC-DP
           LABEL RECORDS ARE STANDARD.
       01  PCHKSRC-P.
           COPY DDS-ALL-FORMATS OF PCHKSRC.
     ‚*--- CODEREVIEW NACH VORGANG
       FD  PCHKSRC-DL
           LABEL RECORDS ARE STANDARD.
       01  PCHKSRC-L.
           COPY DDS-ALL-FORMATS OF PCHKSRCL1.
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
           COPY  DDS-FMT01-O OF CRD0001DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CRD0001DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  CRD0001DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CRD0001DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CRD0001DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CRD0001DE
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

       01  PGM-WRK                       PIC X(10) VALUE "CRP0001".
       01  PGM-TYP                       PIC X(8)  VALUE "CHKSRC".

       01  LEN                           PIC 9(4).
       01  AUSWAHL                       PIC X(2).
       01  CRVORG-WRK                    LIKE CRVORG OF PCHKSRC-P.
       01  CRLFDN-WRK                    LIKE CRLFDN OF PCHKSRC-P.

     ‚* VARIABLEN FÜR OBJEKTPRÜFUNG
       01  LIB-WRK                       LIKE CRLIB  OF PCHKSRC-P.
       01  FILE-WRK                      LIKE CRFILE OF PCHKSRC-P.
       01  MBR-WRK                       LIKE CRMBR  OF PCHKSRC-P.

     ‚* PARAMETER FÜR DAS CLP0008
       01  SEUOPT-WRK                    PIC X(2).

     ‚* PARAMETER FÜR DAS CLP0013
       01  ERROR-TEXT-SHORT              PIC X(255).
       01  ERROR-TEXT-LONG               PIC X(3000).
       01  OPTION-WRK                    PIC X(10).
       01  SUCHE-WRK                     PIC X(40).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.
           OPEN     I-O   PCHKSRC-DP.
           OPEN     INPUT PCHKSRC-DL.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.
           PERFORM  GET-VORBELEGUNG.
           PERFORM  INIT-VAR.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DER TAGS BZW. PROGRAMMNAME
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* JETZT DEN KOPFSATZ ERSTELLEN
           PERFORM  SCHREIBE-VORGANG.

     ‚* OBJEKT IST VORHANDEN, JETZT DAS CR-PROGRAMM AUFRUFEN
           PERFORM  CALL-CODEREVIEW.

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
           CLOSE                 WS-DISPLAY
                                 PCHKSRC-DP
                                 PCHKSRC-DL.

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
           MOVE     "CRP0000"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT1.
     ‚* LESEN ZWEITE ÜBERSCHRIFT
           MOVE     "CRP0001*1"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT1.

       VOR-AUS-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.

           MOVE     CORR INXX         TO FMT01-O-INDIC.
           MOVE     CORR FORMAT1      TO FMT01-O.
           WRITE    WS-REC     FORMAT IS "FMT01".
           READ     WS-DISPLAY FORMAT IS "FMT01".
           MOVE     CORR FMT01-I      TO FORMAT1.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FMT01-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F9 KOMMANDOZEILE AUFRUFEN
           IF       IN09 IN FMT01-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO VOR-AUS-10
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FMT01-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

     ‚* PRÜFEN OB ES DIE TEILDATEI GIBT
           MOVE     CRLIB  OF FORMAT1 TO LIB-WRK.
           MOVE     CRFILE OF FORMAT1 TO FILE-WRK.
           MOVE     CRMBR  OF FORMAT1 TO MBR-WRK.
           PERFORM  CHECK-OBJECT.
           IF       RET-CODE NOT = SPACES
                    GO TO VOR-AUS-10
           END-IF.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK.
     ‚* PCHKSRC ÜBER VORGANG LESEN
           INITIALIZE PCHKSRCF OF PCHKSRC-L.
           MOVE     CRVORG-WRK          TO CRVORG OF PCHKSRC-L.
           MOVE     ZEROES              TO CRSECN OF PCHKSRC-L.
           MOVE     SPACES              TO CRFID  OF PCHKSRC-L.
           START    PCHKSRC-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCHKSRC-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       CRVORG OF PCHKSRC-L NOT = CRVORG-WRK
                    GO TO LES-DAT-90
           END-IF.

     ‚* EINTRÄGE OHNE FEHLER IGNORIEREN
           IF       CRFID  OF PCHKSRC-L     = SPACES
                    GO TO LES-DAT-20
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
                    MOVE    CORR INXX  TO FMT03-O-INDIC
                    WRITE   SUBFILE WS-REC FORMAT IS "FMT03"
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PCHKSRCF OF PCHKSRC-L TO FORMAT2.

     ‚* DATUM DER SOURCE-ZEILE AUFBEREITEN
           INITIALIZE DATE-DB, DATE-DB-6.
           MOVE     CRSDTA OF PCHKSRC-L TO DATE-DB-6.
           MOVE     "TO-DSP"            TO DATUM-CONVERT.
           PERFORM  COPY-CONVERT-DATE.
           MOVE     DATUM-ALPHA-10      TO DT_SRC  OF FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           MOVE     CORR FORMAT2 TO FMT02-O.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT02".
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
  ******   MOVE     1      TO  SUBFILE-RECORD-NUMBER.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           WRITE    WS-REC FORMAT IS "FMT04".

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
           IF       SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                    MOVE SFL-ZEILE TO  SUBFILE-RECORD-NUMBER
           END-IF.
           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AN     TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FMT03-O-INDIC.
           MOVE     CORR   FORMAT3  TO  FMT03-O.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT03".
           MOVE     1      TO  SUBFILE-RECORD-NUMBER.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           READ     WS-DISPLAY  FORMAT IS "FMT03".
           MOVE     AUS  TO INXX

     ‚* BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
           MOVE     9999 TO SFL-ZEILE.

     ‚* F3 ENDE
           IF       IN03 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F8 SEU-STARTEN
           IF       IN08 IN FMT03-I-INDIC = AN
                    PERFORM START-SEU
                    GO TO ANZ-SFL-20
           END-IF.

     ‚* F9 KOMMANDOZEILE AUFRUFEN
           IF       IN09 IN FMT03-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           READ     SUBFILE WS-DISPLAY NEXT MODIFIED RECORD
                    FORMAT IS "FMT02".
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

           MOVE     CORR FMT02-I TO FORMAT2.

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
                         MOVE     CORR FORMAT2 TO FMT02-O
                         REWRITE  SUBFILE WS-REC FORMAT IS "FMT02"
                         GO TO ANZ-SFL-50
                    END-IF
           END-IF.

     ‚* ÄNDERN DES DATENSATZES
           IF       AUSW OF FORMAT2 = "1"
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                    END-IF
           END-IF.

     ‚* DETAILANZEIGE DES DATENSATZES
           IF       AUSW OF FORMAT2 = "2"
            OR      AUSW OF FORMAT2 = "5"
                    PERFORM START-FNDSTRPDM
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2
           MOVE     CORR FORMAT2 TO FMT02-O

           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           REWRITE  SUBFILE WS-REC FORMAT IS "FMT02".

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
           WRITE    WS-REC FORMAT IS "FMT04".
           WRITE    WS-REC FORMAT IS "FMT05".
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FMT03-O-INDIC.
           MOVE     CORR   FORMAT3  TO  FMT03-O.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT03".
           MOVE     1      TO  SUBFILE-RECORD-NUMBER.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           READ     WS-DISPLAY  FORMAT IS "FMT03".

           IF       IN03 OF FMT03-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO FEH-MEL-90
           END-IF.

           IF       IN12 OF FMT03-I-INDIC = AN
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
           MOVE     "CRP0000"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT3.

     ‚* ZWEITE ÜBERSCHRIFT
           MOVE     "CRP0001*3"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT3.

     ‚* ZEILE
           MOVE     "SFT0007"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0007 OF FORMAT3.

     ‚* ID
           MOVE     "SFT0005"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0005 OF FORMAT3.

     ‚* BESCHREIBUNG
           MOVE     "SFT0003"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0003 OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STARTEN DES SEU ÜBER EIN CL
     ‚*--------------------------------------------------------------
       START-SEU SECTION.
       STR-SEU-00.

           INITIALIZE RET-CODE.
           INITIALIZE OPTION-WRK.

     ‚* HIER KOMMT MAN EH NUR MIT "5" REIN
           MOVE     "2" TO SEUOPT-WRK.

     ‚* SEU STARTEN
           CALL     "CLP0008"  USING CRLIB  OF FORMAT1
                                     CRFILE OF FORMAT1
                                     CRMBR  OF FORMAT1
                                     SEUOPT-WRK
                                     ERROR-TEXT-SHORT
                                     ERROR-TEXT-LONG
                                     RET-CODE
           END-CALL.

       STR-SEU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STARTEN DES SEU ÜBER EIN CL
     ‚*--------------------------------------------------------------
       START-FNDSTRPDM SECTION.
       STR-FSP-00.

           INITIALIZE RET-CODE.
           INITIALIZE LEN.
           INITIALIZE OPTION-WRK.

     ‚* HIER KOMMT MAN EH NUR MIT "5" REIN
           IF       AUSW OF FORMAT2 = "2"
                    MOVE     "*EDIT"           TO OPTION-WRK
           ELSE
                    MOVE     "*DSP"            TO OPTION-WRK
           END-IF.
           INSPECT  CRLINE OF FORMAT2 TALLYING LEN FOR LEADING SPACES.
           IF       LEN     > ZEROES
            AND     LEN NOT = LENGTH OF CRLINE OF FORMAT2
                    ADD  1                       TO LEN
                    MOVE CRLINE OF FORMAT2(LEN:) TO SUCHE-WRK
           ELSE
                    MOVE CRLINE OF FORMAT2       TO SUCHE-WRK
           END-IF.

     ‚* SEU STARTEN
           CALL     "CLP0013"  USING SUCHE-WRK
                                     CRLIB  OF FORMAT2
                                     CRFILE OF FORMAT2
                                     CRMBR  OF FORMAT2
                                     OPTION-WRK
                                     ERROR-TEXT-SHORT
                                     ERROR-TEXT-LONG
                                     RET-CODE
           END-CALL.

       STR-FSP-90.
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
     ‚* SCHREIBEN KOPFSATZ DES VORGANGS
     ‚*--------------------------------------------------------------
       SCHREIBE-VORGANG SECTION.
       SCR-VOR-00.

           INITIALIZE PCHKSRCF OF PCHKSRC-P.
     ‚* NÄCHSTE LAUFENDE NUMMER ERMITTELN
           MOVE     "PCHKSRC" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.

     ‚* SCHLÜSSEL FÜLLEN
           MOVE     LFDN-WRK               TO CRLFDN OF PCHKSRC-P.
           MOVE     LFDN-WRK               TO CRLFDN-WRK.
     ‚* DATEN AUS DEM FORMAT KOPIEREN
     ‚*
     ‚* SOURCE
           MOVE     CRLIB  OF FORMAT1      TO CRLIB  OF PCHKSRC-P.
           MOVE     CRFILE OF FORMAT1      TO CRFILE OF PCHKSRC-P.
           MOVE     CRMBR  OF FORMAT1      TO CRMBR  OF PCHKSRC-P.
     ‚* PROGRAMMIERER
           MOVE     CRCUSR OF FORMAT1      TO CRCUSR OF PCHKSRC-P.
           MOVE     CRPGMA OF FORMAT1      TO CRPGMA OF PCHKSRC-P.
     ‚* DATUM AUFBEREITEN
           MOVE     CRCDTA OF FORMAT1      TO DATUM-ALPHA-10.
           MOVE     "TO-DB"                TO DATUM-CONVERT.
           PERFORM  COPY-CONVERT-DATE.
           MOVE     DATE-DB                TO CRCDTA OF PCHKSRC-P.
     ‚* KOPFSATZ HAT KEINE FEHLERMELDUNG
           MOVE     ZEROES                 TO CRSECN OF PCHKSRC-P.
           MOVE     ZEROES                 TO CRSDTA OF PCHKSRC-P.
           MOVE     SPACES                 TO CRFID  OF PCHKSRC-P.
           MOVE     ZEROES                 TO CRFTYP OF PCHKSRC-P.
           MOVE     SPACES                 TO CRFTXT OF PCHKSRC-P.
     ‚* NÄCHSTE LAUFENDE NUMMER ERMITTELN
           MOVE     "CHK-VORG" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK               TO CRVORG OF PCHKSRC-P.
           MOVE     LFDN-WRK               TO CRVORG-WRK.
     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                 TO CRAUSR OF PCHKSRC-P.
           MOVE     T-TERM                 TO CRABS  OF PCHKSRC-P.
           MOVE     PGM-WRK                TO CRAPGM OF PCHKSRC-P.
           MOVE     DATE-8                 TO CRADTA OF PCHKSRC-P.
           MOVE     TIME-6                 TO CRATIA OF PCHKSRC-P.

           WRITE    PCHKSRC-P.

       SCR-VOR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFRUFEN DES CODEREVIEW-PROGRAMM
     ‚*--------------------------------------------------------------
       CALL-CODEREVIEW SECTION.
       CAL-COD-00.

     ‚* AUFRUF DES PROGRAMMS, HIER WERDEN DIE PCHKSRC-EINTRÄGE
     ‚* GESCHRIEBEN DIE SPÄTER ANGEZEIGT WERDEN
           CALL     "CRP1050" USING CRLFDN-WRK
                                    CRLIB  OF FORMAT1
                                    CRFILE OF FORMAT1
                                    CRMBR  OF FORMAT1
           END-CALL.

       CAL-COD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER VORBELUNG
     ‚*--------------------------------------------------------------
       GET-VORBELEGUNG SECTION.
       GET-VOB-00.

           MOVE     "QCBLSRC" TO CRFILE OF FORMAT1.

       GET-VOB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INITIALISIEREN ALLER VARIABLEN
     ‚*--------------------------------------------------------------
       INIT-VAR SECTION.
       INI-VAR-00.

     ‚* INITIALISIEREN ALLER VARIABLEN
           INITIALIZE LIB-WRK.
           INITIALIZE FILE-WRK.
           INITIALIZE MBR-WRK.
           INITIALIZE CRVORG-WRK.

       INI-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DAS OBJEKT EXISTIERT
     ‚*--------------------------------------------------------------
       CHECK-OBJECT SECTION.
       CHK-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* MBR MUSS GEFÜLLT SEIN!
           IF       MBR-WRK = SPACES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-OBJ-90
           END-IF.

     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     FILE-WRK     TO OBJ     OF CHKOBJ-REC.
           MOVE     MBR-WRK      TO MBR     OF CHKOBJ-REC.
           MOVE     LIB-WRK      TO LIB     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "2" TO RET-CODE
                    GO TO CHK-OBJ-90
           END-IF.

       CHK-OBJ-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CONVERT-DATE: DATUMSFELDER AUFBEREITEN/UMSETZEN
     ‚*---------------------------------------------------------------

           COPY     CNVDATE    OF APG-QCPYSRC.
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
