       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0200.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUSWAHLPROGRAMM FÜR HERMES (STARTPROGRAMM)                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSWAHL + ANZEIGE PROGRAMME                  *
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
                  ASSIGN       TO  WORKSTATION-XXD0200DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI KÜCHENPLAN
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
           COPY DDS-ALL-FORMATS OF XXD0200DE.
      /
     ‚*--- KÜCHENPLAN
       FD  PKDPLAN-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDPLAN-P.
           COPY DDS-ALL-FORMATS OF PKDPLAN.
      /
     ‚*--- DATUMS-TABELLE
       FD  PCFGDAT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGDAT-P.
           COPY DDS-ALL-FORMATS OF PCFGDAT.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN40                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN43                          PIC 1.
        05 IN44                          PIC 1.
        05 IN45                          PIC 1.
        05 IN46                          PIC 1.
        05 IN47                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0200DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD0200DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0200DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0200DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0200DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0200DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0200".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  AUSWAHL                       PIC X(2).
       01  COUNTER                       PIC 9(4).
       01  WOTA-WRK                      LIKE DTWOTA OF PCFGDAT-P.
       01  FIRST-DAY                     LIKE DTLFDN OF PCFGDAT-P.
       01  KDTYP-WRK                     LIKE KDTYP  OF PKDPLAN-P.

       01  KDDATE-DUMMY                  LIKE KDDATE OF PKDPLAN-P.
       01  KDTYP-DUMMY                   LIKE KDTYP  OF PKDPLAN-P.
       01  KDUSER-DUMMY                  LIKE KDUSER OF PKDPLAN-P.

       01  KD-PLAN-TAB.
        05 TAB-POS                       PIC 9(4).
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC OCCURS 500 TIMES.
         06 KDDATE                       LIKE KDDATE OF PKDPLAN-P.
         06 KDEUP                        LIKE KDUSER OF PKDPLAN-P.
         06 KDIAW                        LIKE KDUSER OF PKDPLAN-P.
         06 KDLG1                        LIKE KDUSER OF PKDPLAN-P.
         06 KDLG2                        LIKE KDUSER OF PKDPLAN-P.
         06 KDES41                       LIKE KDUSER OF PKDPLAN-P.
         06 KDES42                       LIKE KDUSER OF PKDPLAN-P.
         06 KDML1                        LIKE KDUSER OF PKDPLAN-P.
         06 KDML2                        LIKE KDUSER OF PKDPLAN-P.
     ‚* LAGERDIENST: PUTZEN JA/NEIN
         06 KDLGW1                       LIKE KDFK01 OF PKDPLAN-P.
         06 KDLGW2                       LIKE KDFK01 OF PKDPLAN-P.

       01   CFID-WRK                     LIKE CFID   OF CFG-CPY.
       01   CFKEY-WRK                    LIKE CFKEY  OF CFG-CPY.
       01   CFKEY2-WRK                   LIKE CFKEY2 OF CFG-CPY.
       01   CFKEY3-WRK                   LIKE CFKEY3 OF CFG-CPY.
       01   CFKEY4-WRK                   LIKE CFKEY4 OF CFG-CPY.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0200" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.
           OPEN     INPUT PKDPLAN-DP
                          PCFGDAT-DP.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.

       ANF010.
     ‚* ERMITTELN DES ERSTEN TAGES IN DIESER WOCHE
           PERFORM  GET-FIRST-DAY-IN-WEEK.
           PERFORM  GET-VORBELEGUNG.

     ‚* VORAUSWAHL: SELEKTIEREN DES DATUMS UND ANZEIGE
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
           CLOSE                 WS-DISPLAY
                                 PKDPLAN-DP
                                 PCFGDAT-DP.

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
           MOVE     "XXP0200"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT1.
     ‚* LESEN ZWEITE ÜBERSCHRIFT
           MOVE     "XXP0200*1"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT1.

       VOR-AUS-10.
           MOVE     AUS TO INXX.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.
     ‚* SIND INFORMATIONEN VORHANDEN?
           CALL     "CFP0010" USING PGM-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    MOVE AN  TO IN40 OF INXX
           ELSE
                    MOVE AUS TO IN40 OF INXX
           END-IF.

     ‚* DEN AKTUELLEN TAG MARKIEREN
           EVALUATE WOTA-WRK
                    WHEN 1     MOVE AN   TO IN41 OF INXX
                    WHEN 2     MOVE AN   TO IN42 OF INXX
                    WHEN 3     MOVE AN   TO IN43 OF INXX
                    WHEN 4     MOVE AN   TO IN44 OF INXX
                    WHEN 5     MOVE AN   TO IN45 OF INXX
                               MOVE AN   TO IN47 OF INXX
           END-EVALUATE.

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

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FMT01-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F12 WOCHENPLAN
           IF       IN14 IN FMT01-I-INDIC = AN
                    MOVE SPACES TO F3
                    CALL "XXP0203" USING F3
                    END-CALL
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    GO TO VOR-AUS-10
           END-IF.

     ‚* F20 PROGRAMMINFORMATIONEN
           IF       IN20 IN FMT01-I-INDIC = AN
                    PERFORM ANZEIGE-PGM-INFOS
                    GO TO VOR-AUS-10
           END-IF.

     ‚* F21 KOMMANDOZEILE AUFRUFEN
           IF       IN21 IN FMT01-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO VOR-AUS-10
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR DIENST
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
                    MOVE CFKEY-WRK TO KDTYP  OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

     ‚* DATUM AUF GÜLTIGKEIT PRÜFEN
           MOVE     VONDAT OF FORMAT1 TO DATUM-ALPHA-10.
           MOVE     "TO-DB"           TO DATUM-CONVERT.
           PERFORM  COPY-CONVERT-DATE.
           PERFORM  COPY-CHECK-DATE.
           IF       DATUM-RETCODE NOT = SPACES
                    GO TO VOR-AUS-10
           END-IF.

     ‚* WURDE EIN DIENST SELEKTIERT, MUSS DIESER AUCH STIMMEN
           IF       KDTYP  OF FORMAT1 NOT = SPACES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE     "K110"            TO CFID   OF CFG-CPY
                    MOVE     KDTYP  OF FORMAT1 TO CFKEY  OF CFG-CPY
                    PERFORM  COPY-CFG-CPY
                    IF   CFG-DA OF CFG-CPY = ZEROES
                         GO TO VOR-AUS-10
                    END-IF
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
     ‚* DAS DATUM AUS DER VORAUSWAHL KONVERIEREN
           MOVE     VONDAT OF FORMAT1 TO DATUM-ALPHA-10.
           MOVE     "TO-DB"           TO DATUM-CONVERT.
           PERFORM  COPY-CONVERT-DATE.

     ‚* PKDPLAN LESEN
           INITIALIZE PKDPLANF OF PKDPLAN-P.
           MOVE     DATE-DB             TO KDDATE OF PKDPLAN-P.
           MOVE     SPACES              TO KDTYP  OF PKDPLAN-P.
           MOVE     SPACES              TO KDUSER OF PKDPLAN-P.
           START    PKDPLAN-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PKDPLAN-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* USER SELEKTIERT?
           IF       KDUSER OF FORMAT1   NOT = SPACES
     ‚*             VERTETUNGSUSER PRÜFEN WENN GEFÜLLT...
                    IF   KDGUSR OF PKDPLAN-P NOT = SPACES
                     AND KDGUSR OF PKDPLAN-P NOT =
                         KDUSER OF FORMAT1
                         GO TO LES-DAT-20
                    END-IF
     ‚*             ... ANSONSTEN DEN RICHTIGEN USER
                    IF   KDGUSR OF PKDPLAN-P     = SPACES
                     AND KDUSER OF PKDPLAN-P NOT =
                         KDUSER OF FORMAT1
                         GO TO LES-DAT-20
                    END-IF
           END-IF.

     ‚* NACH DIENST SELEKTIERT
           IF       KDTYP  OF FORMAT1 NOT = SPACES
            AND     KDTYP  OF PKDPLAN-P NOT = KDTYP  OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* NACH TEXT SELEKTIEREN?
           IF       TXTSEL OF FORMAT1   NOT = SPACES
                    MOVE PKDPLAN-P           TO RECORD-WRK
                    MOVE LENGTH OF PKDPLAN-P TO RECORD-LEN
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
                    MOVE    CORR INXX  TO FMT03-O-INDIC
                    WRITE   SUBFILE WS-REC FORMAT IS "FMT03"
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PKDPLANF OF PKDPLAN-P TO FORMAT2.
     ‚* DEN KEY INS HIDDEN-FELD SICHERN
           MOVE     KDUSER OF PKDPLAN-P TO KDHSER OF FORMAT2.
     ‚* WENN DER USER GETAUCHT HAT, DANN DEN ANDEREN USER ANZEIGEN
     ‚* ABER AUCH DIE ZEILE FARBLICH MARKIEREN
           IF       KDGUSR OF PKDPLAN-P NOT = SPACES
                    MOVE KDGUSR OF PKDPLAN-P TO
                         KDUSER OF FORMAT2
                    MOVE AN  TO IN46 OF INXX
           ELSE
                    MOVE AUS TO IN46 OF INXX
           END-IF.
     ‚* KOMMENTAR AUFRÜCKEN WENN Z.B. NUR DER ZWEITE GEFÜLLT IST
           IF       KDKOM1 OF FORMAT2 = SPACES
                    IF   KDKOM2 OF PKDPLAN-P NOT = SPACES
                         MOVE KDKOM2 OF PKDPLAN-P TO KDKOM1 OF FORMAT2
                    ELSE
     ‚*                  KOMMENTAR 3 AUFRÜCKEN BZW. TEILDATEIENTEXT
                         IF   KDKOM3 OF PKDPLAN-P NOT = SPACES
                              MOVE KDKOM3 OF PKDPLAN-P TO
                                   KDKOM1 OF FORMAT2
                         END-IF
                    END-IF
           END-IF.

     ‚* WOCHENTAG FÜLLEN
           EVALUATE KDWOTA OF FORMAT2
                    WHEN 1
                         MOVE     "PGM0011"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN 2
                         MOVE     "PGM0012"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN 3
                         MOVE     "PGM0013"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN 4
                         MOVE     "PGM0014"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN 5
                         MOVE     "PGM0015"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN 6
                         MOVE     "PGM0016"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN 7
                         MOVE     "PGM0017"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    WHEN OTHER
                         MOVE     "SFTLEER"         TO CPY-TXID
                         MOVE     SPRACHE-WRK       TO CPY-TXSPR
           END-EVALUATE.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT   TO WOTA    OF FORMAT2.

     ‚* DATUM FÜLLEN
           MOVE     KDDATE OF FORMAT2         TO DATE-DB.
           MOVE     "TO-DSP"                  TO DATUM-CONVERT.
           PERFORM  COPY-CONVERT-DATE.
           MOVE     DATUM-ALPHA-10            TO DATWRK OF FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FMT02-O-INDIC.
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
           MOVE     AUS  TO INXX.

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

     ‚* F8 NEUANLAGE
           IF       IN08 IN FMT03-I-INDIC = AN
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
                    CALL "XXP0201" USING KDDATE OF FORMAT2
                                         KDTYP  OF FORMAT2
                                         KDHSER OF FORMAT2
                                         F3
                                         F12
                    END-CALL
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                    END-IF
           END-IF.

     ‚* LÖSCHEN DES SATZES
           IF       AUSW OF FORMAT2 = "4"
                    CALL "XXP0202" USING KDDATE OF FORMAT2
                                         KDTYP  OF FORMAT2
                                         KDHSER OF FORMAT2
                    END-CALL
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     CORR FORMAT2 TO FMT02-O.

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

     ‚* F8 NEUANLAGE
           IF       IN08 IN FMT03-I-INDIC = AN
                    PERFORM NEUANLAGE
                    IF   F3  = "1"
                         GO TO FEH-MEL-90
                    END-IF
                    INITIALIZE F12
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
           MOVE     "XXP0200"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT3.

     ‚* ZWEITE ÜBERSCHRIFT
           MOVE     "XXP0200*3"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT3.

     ‚* DATUM
           MOVE     "SFT0011"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0011 OF FORMAT3.

     ‚* TAG
           MOVE     "SFT0010"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0010 OF FORMAT3.

     ‚* DIENST
           MOVE     "SFT0008"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0008 OF FORMAT3.

     ‚* USER
           MOVE     "SFT0009"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0009 OF FORMAT3.

     ‚* BESCHREIBUNG
           MOVE     "SFT0003"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0003 OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUANLAGE EINES PROGRAMMS
     ‚*--------------------------------------------------------------
       NEUANLAGE SECTION.
       NEU-ANL-00.

           INITIALIZE F3, F12.
           INITIALIZE KDDATE-DUMMY, KDTYP-DUMMY, KDUSER-DUMMY.
           CALL     "XXP0201" USING KDDATE-DUMMY
                                    KDTYP-DUMMY
                                    KDUSER-DUMMY
                                    F3
                                    F12
           END-CALL.
           IF       F12 = "1"
                    INITIALIZE F12
                    GO TO NEU-ANL-90
           END-IF.

       NEU-ANL-90.
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
     ‚* WOCHENTAGE RUNTERRECHNEN
     ‚*--------------------------------------------------------------
       WOCHENTAG-RUNTERRECHNEN SECTION.
       WOT-REC-00.

           INITIALIZE COUNTER.
     ‚* READ PRIOR
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-8      TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WOT-REC-90
           END-IF.

       WOT-REC-20.
           READ     PCFGDAT-DP PRIOR RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WOT-REC-90
           END-IF.

     ‚* MONTAG - FREITAG FÜLLEN, NICHT MEHR
           ADD      1 TO COUNTER.
           IF       COUNTER >= WOTA-WRK
                    GO TO WOT-REC-90
           END-IF.

           MOVE     DTLFDN OF PCFGDAT-P TO FIRST-DAY.
           GO TO    WOT-REC-20.

       WOT-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER VORBELUNG
     ‚*--------------------------------------------------------------
       GET-VORBELEGUNG SECTION.
       GET-VOB-00.

     ‚* ERSTMAL DIE STANDARDWERTE NEHMEN
           MOVE     FIRST-DAY                 TO DATE-DB.
           MOVE     "TO-DSP"                  TO DATUM-CONVERT.
           PERFORM  COPY-CONVERT-DATE.
           MOVE     DATUM-ALPHA-10            TO VONDAT OF FORMAT1.

     ‚* FÜLLEN DES KÜCHENPLANS FÜR DIESE WOCHE
           IF       COUNTER >= 1
                    MOVE KDEUP  OF TAB-REC(1) TO KDMONE OF FORMAT1
                    MOVE KDIAW  OF TAB-REC(1) TO KDMONI OF FORMAT1
                    MOVE KDML1  OF TAB-REC(1) TO MLMON1 OF FORMAT1
                    MOVE KDML2  OF TAB-REC(1) TO MLMON2 OF FORMAT1
           END-IF.
           IF       COUNTER >= 2
                    MOVE KDEUP  OF TAB-REC(2) TO KDDIEE OF FORMAT1
                    MOVE KDIAW  OF TAB-REC(2) TO KDDIEI OF FORMAT1
                    MOVE KDML1  OF TAB-REC(2) TO MLDIE1 OF FORMAT1
                    MOVE KDML2  OF TAB-REC(2) TO MLDIE2 OF FORMAT1
           END-IF.
           IF       COUNTER >= 3
                    MOVE KDEUP  OF TAB-REC(3) TO KDMITE OF FORMAT1
                    MOVE KDIAW  OF TAB-REC(3) TO KDMITI OF FORMAT1
                    MOVE KDML1  OF TAB-REC(3) TO MLMIT1 OF FORMAT1
                    MOVE KDML2  OF TAB-REC(3) TO MLMIT2 OF FORMAT1
           END-IF.
           IF       COUNTER >= 4
                    MOVE KDEUP  OF TAB-REC(4) TO KDDONE OF FORMAT1
                    MOVE KDIAW  OF TAB-REC(4) TO KDDONI OF FORMAT1
                    MOVE KDML1  OF TAB-REC(4) TO MLDON1 OF FORMAT1
                    MOVE KDML2  OF TAB-REC(4) TO MLDON2 OF FORMAT1
           END-IF.
           IF       COUNTER >= 5
                    MOVE KDEUP  OF TAB-REC(5) TO KDFREE OF FORMAT1
                    MOVE KDIAW  OF TAB-REC(5) TO KDFREI OF FORMAT1
                    MOVE KDML1  OF TAB-REC(5) TO MLFRE1 OF FORMAT1
                    MOVE KDML2  OF TAB-REC(5) TO MLFRE2 OF FORMAT1
                    MOVE KDES41 OF TAB-REC(5) TO KDES41 OF FORMAT1
                    MOVE KDES42 OF TAB-REC(5) TO KDES42 OF FORMAT1
                    MOVE KDLG1  OF TAB-REC(5) TO KDLG1  OF FORMAT1
                    MOVE KDLG2  OF TAB-REC(5) TO KDLG2  OF FORMAT1
                    MOVE KDLGW1 OF TAB-REC(5) TO KDLGW1 OF FORMAT1
                    MOVE KDLGW2 OF TAB-REC(5) TO KDLGW2 OF FORMAT1
           END-IF.

       GET-VOB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES ERSTEN TAGES IN DER WOCHE
     ‚*--------------------------------------------------------------
       GET-FIRST-DAY-IN-WEEK SECTION.
       GET-FWD-00.

           INITIALIZE FIRST-DAY, WOTA-WRK.
     ‚* ERMITTELN DATUM/ZEIT
           PERFORM  COPY-GET-TIME.

     ‚* ERMITTELN DES AKTUELLEN TAGES
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-8    TO DTLFDN OF PCFGDAT-P.
           READ     PCFGDAT-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

     ‚* ERSTEN TAG BERECHNEN
           MOVE     DTWOTA OF PCFGDAT-P TO WOTA-WRK.
           MOVE     DTLFDN OF PCFGDAT-P TO FIRST-DAY.
           PERFORM  WOCHENTAG-RUNTERRECHNEN.

           INITIALIZE COUNTER.
     ‚* JETZT VOM ERSTEN TAG AN, DIE GANZE WOCHE LESEN
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     FIRST-DAY TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

       GET-FWD-20.
           READ     PCFGDAT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

     ‚* MONTAG - FREITAG FÜLLEN, NICHT MEHR
           ADD      1 TO COUNTER.
           IF       COUNTER > 5
                    GO TO GET-FWD-90
           END-IF.

     ‚* FÜLLEN INTERNE TABELLE
           MOVE     DTLFDN OF PCFGDAT-P TO KDDATE OF TAB-REC(COUNTER).

     ‚* ERMITTELN EUP-KÜCHENDIENST
           MOVE     "KD-EUP" TO KDTYP-WRK.
           PERFORM  GET-KDPLAN-TAG.
           MOVE     KDUSER OF PKDPLAN-P TO KDEUP  OF TAB-REC(COUNTER).

     ‚* ERMITTELN IAW-KÜCHENDIENST
           MOVE     "KD-IAW" TO KDTYP-WRK.
           PERFORM  GET-KDPLAN-TAG.
           MOVE     KDUSER OF PKDPLAN-P TO KDIAW  OF TAB-REC(COUNTER).

     ‚* ERMITTELN LAGERDIENST
           MOVE     "LAGER1" TO KDTYP-WRK.
           PERFORM  GET-KDPLAN-TAG.
           MOVE     KDUSER OF PKDPLAN-P TO KDLG1  OF TAB-REC(COUNTER).
           MOVE     KDFK01 OF PKDPLAN-P TO KDLGW1 OF TAB-REC(COUNTER).
           MOVE     "LAGER2" TO KDTYP-WRK.
           PERFORM  GET-KDPLAN-TAG.
           MOVE     KDUSER OF PKDPLAN-P TO KDLG2  OF TAB-REC(COUNTER).
           MOVE     KDFK01 OF PKDPLAN-P TO KDLGW2 OF TAB-REC(COUNTER).

     ‚* ERMITTELN MELDUNG
           MOVE     "MELD-1" TO KDTYP-WRK.
           PERFORM  GET-KDPLAN-TAG.
           MOVE     KDUSER OF PKDPLAN-P TO KDML1  OF TAB-REC(COUNTER).
           MOVE     "MELD-2" TO KDTYP-WRK.
           PERFORM  GET-KDPLAN-TAG.
           MOVE     KDUSER OF PKDPLAN-P TO KDML2  OF TAB-REC(COUNTER).

     ‚* ERMITTELN ESSEN400
           MOVE     "ESSEN400-1" TO KDTYP-WRK.
           PERFORM  GET-KDPLAN-TAG.
           MOVE     KDUSER OF PKDPLAN-P TO KDES41 OF TAB-REC(COUNTER).
           MOVE     "ESSEN400-2" TO KDTYP-WRK.
           PERFORM  GET-KDPLAN-TAG.
           MOVE     KDUSER OF PKDPLAN-P TO KDES42 OF TAB-REC(COUNTER).

           GO TO    GET-FWD-20.

       GET-FWD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KÜCHENDIENSTPLAN LESEN
     ‚*--------------------------------------------------------------
       GET-KDPLAN-TAG SECTION.
       GET-KDP-00.

     ‚* ERMITTELN DES ERSTEN TAGES
           INITIALIZE PKDPLANF OF PKDPLAN-P.
           MOVE     DTLFDN OF PCFGDAT-P TO KDDATE OF PKDPLAN-P.
           MOVE     KDTYP-WRK           TO KDTYP  OF PKDPLAN-P.
           MOVE     SPACES              TO KDUSER OF PKDPLAN-P.
           START    PKDPLAN-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PKDPLANF OF PKDPLAN-P
                    GO TO GET-KDP-90
           END-IF.

       GET-KDP-20.
           READ     PKDPLAN-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PKDPLANF OF PKDPLAN-P
                    GO TO GET-KDP-90
           END-IF.

     ‚* KEY ABFRAGEN
           IF       KDDATE OF PKDPLAN-P NOT = DTLFDN OF PCFGDAT-P
            OR      KDTYP  OF PKDPLAN-P NOT = KDTYP-WRK
                    INITIALIZE PKDPLANF OF PKDPLAN-P
                    GO TO GET-KDP-90
           END-IF.

     ‚* WENN GETAUSCHT WURDE, DANN DEN ANDEREN USER EINTRAGEN
           IF       KDGUSR OF PKDPLAN-P NOT = SPACES
                    MOVE KDGUSR OF PKDPLAN-P TO
                         KDUSER OF PKDPLAN-P
           END-IF.

       GET-KDP-90.
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
