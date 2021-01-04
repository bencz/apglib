       IDENTIFICATION DIVISION.
     ‚*
##CR ˆ* FALSCHE PROGRM-ID
         PROGRAM-ID.              XXP9999A.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM FÜR CR                                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  CR-TESTPROGRAMM (FEHLERHAFT!!!)              *
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
                  ASSIGN       TO  WORKSTATION-XXD9999DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PPGMLIB-DP
                  ASSIGN       TO  DATABASE-PPGMLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
##CR ˆ* FALSCHE WITH DUPLICATES
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI TAGS SORTIERT NACH TAG
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL2
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
           COPY DDS-ALL-FORMATS OF XXD9999DE.
      /
     ‚*--- PROGRAMME
       FD  PPGMLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLIB-P.
           COPY DDS-ALL-FORMATS OF PPGMLIB.
     ‚*--- TAGS
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL2.
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
           COPY  DDS-FMT01-O OF XXD9999DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  XXD9999DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD9999DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD9999DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD9999DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD9999DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9999".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

     ‚* INTERNE TABELLE
       01  SELTAG-TAB                    TYPE SELTAG-TYPE.

       01  LEN-L                         PIC 9(4).
       01  LEN-F                         PIC 9(4).
       01  PLLIB-WRK                     LIKE PLLIB  OF PPGMLIB-P.
       01  PLFILE-WRK                    LIKE PLFILE OF PPGMLIB-P.
       01  AUSWAHL                       PIC X(2).

       01  TAG-WRK                       LIKE PTTAG  OF PTAGLIB-L.
       01  TAG-VORHANDEN                 PIC 9(4).
       01  TAG-SELEKTIERT                PIC 9(4).

     ‚* PARAMETER FÜR DAS HMP0004
       01  LIB-WRK                       LIKE PLLIB  OF PPGMLIB-P.
       01  LIB-SAV                       LIKE PLLIB  OF PPGMLIB-P.
       01  FILE-WRK                      LIKE PLFILE OF PPGMLIB-P.

     ‚* PARAMETER FÜR DAS CLP0008
       01  ERROR-TEXT-SHORT              PIC X(255).
       01  ERROR-TEXT-LONG               PIC X(3000).
       01  OPTION-WRK                    PIC X(2).
##CR ˆ* NICHT VERWENDETE VARIABLE
       01  NICHT-VERWENDET               PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9999" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

##CR ˆ* FALSCHES FIRST-CALL
           IF       FIRST-CALL NOT = "1"
                    INITIALIZE RET-CODE
           END-IF.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.
           OPEN     INPUT PPGMLIB-DP
                          PTAGLIB-DL.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

##CR ˆ* NICHT VERWENDETE SPRUNGMARKE
       ANF001.
           INITIALIZE FORMAT1.
           PERFORM  GET-VORBELEGUNG.
           PERFORM  INIT-VAR.

##CR ˆ* WEIL ER SOWEIT HINAUS GEHT, GILT HIER AUCH: KEIN PUNKT AM ENDE
           MOVE     ZEROES                                        TO LEN-L.
##CR ˆ* BEFEHL DER ÜBER 80 HINAUSGEHT
           MOVE     ZEROES                                     TO LEN-L.
           MOVE     ZEROES                                    TO LEN-L.

##CR ˆ* BEFEHL DER NICHT BEI 29 WEITERGEFÜHRT WIRD
           MOVE  ZEROES TO LEN-L.

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

##CR ˆ* BEFEHL OHNE PUNKT DER ÜBER MEHRERE ZEILEN GEHT
           MOVE
                    ZEROES
                           TO
                              LEN-L

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
                                 PPGMLIB-DP
                                 PTAGLIB-DL.

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
           MOVE     "HMP0000"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT1.
     ‚* LESEN ZWEITE ÜBERSCHRIFT
           MOVE     "XXP9999*1"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SUBUEB  OF FORMAT1.

     ‚* INTERNE TABELLE LÖSCHEN
           PERFORM  DEL-SELTAB.
           INITIALIZE LIB-SAV.

##CR ˆ* SPRUNGMARKEN-NAME ABWEICHEND
       VOR-SEL-10.
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

     ‚* F18 ABBRECHEN ?
           IF       IN18 OF FMT01-I-INDIC  = AN
                    CALL "HMP1000" USING F3
                    END-CALL
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    GO TO VOR-AUS-00
           END-IF.

     ‚* FRAGEZEICHEN-FUNKTION FÜR EIN TAG
     ‚*
     ‚* TAG1
           IF       TAG1   OF FORMAT1(1:1) = "?"
                    PERFORM TAG-SELEKTION
                    IF   F3 = "1"
                         GO TO VOR-AUS-90
                    END-IF
                    MOVE TAG-WRK              TO TAG1    OF FORMAT1
                    GO TO VOR-AUS-10
           END-IF.

     ‚* BERECHNEN ANZAHL DER SELEKTIERTEN TAGS
           INITIALIZE TAG-SELEKTIERT.
           IF       TAG1   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG1   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.
           IF       TAG2   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG2   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.
           IF       TAG3   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG3   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.
           IF       TAG4   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG4   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.
           IF       TAG5   OF FORMAT1 NOT = SPACES
                    ADD 1 TO TAG-SELEKTIERT
                    MOVE    TAG5   OF FORMAT1 TO TAG-WRK
                    PERFORM ADD-SELTAB
           END-IF.

     ‚* WEITERE TAGS PFLEGEN
           IF       WEITERE OF FORMAT1 = "+"
                    PERFORM AENDERN-TAGSELEKTION
                    MOVE SPACES TO WEITERE OF FORMAT1
           END-IF.

       VOR-AUS-90.
           EXIT.
##CR ˆ* DOPPELTES EXIT
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE PLFILE-WRK.
           INITIALIZE DATEN, ANZREC-WRK, LEN-L, LEN-F.
     ‚* WURDE EIN PGM MIT STERN SELEKTIERT?
           IF       PLFILE OF FORMAT1      NOT = SPACES
            AND     PLFILE OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN-F
                    INSPECT  PLFILE OF FORMAT1
                             TALLYING LEN-F FOR CHARACTERS
                             BEFORE INITIAL "*"
                    MOVE     PLFILE OF FORMAT1(1:LEN-F) TO PLFILE-WRK
           ELSE
                    MOVE     SPACES                     TO PLFILE-WRK
           END-IF.

     ‚* WURDE EINE LIB MIT STERN SELEKTIERT?
           IF       PLLIB  OF FORMAT1      NOT = SPACES
            AND     PLLIB  OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN-L
                    INSPECT  PLLIB  OF FORMAT1
                             TALLYING LEN-L FOR CHARACTERS
                             BEFORE INITIAL "*"
                    MOVE     PLLIB  OF FORMAT1(1:LEN-L) TO PLLIB-WRK
           ELSE
                    MOVE     SPACES                     TO PLLIB-WRK
           END-IF.

     ‚* PPGMLIB LESEN
##CR ˆ* NICHT VERWENDETE SPRUNGMARKE
       LES-DAT-10.
           INITIALIZE PPGMLIBF OF PPGMLIB-P.
           MOVE     PLLIB-WRK           TO PLLIB  OF PPGMLIB-P.
           MOVE     PLFILE-WRK          TO PLFILE OF PPGMLIB-P.
           START    PPGMLIB-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PPGMLIB-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       LEN-L > ZEROES
            AND     PLLIB  OF PPGMLIB-P(1:LEN-L) NOT =
                    PLLIB  OF FORMAT1(1:LEN-L)
                    GO TO LES-DAT-90
           END-IF.

           IF       LEN-F > ZEROES
            AND     PLFILE OF PPGMLIB-P(1:LEN-F) NOT =
                    PLFILE OF FORMAT1(1:LEN-F)
                    GO TO LES-DAT-20
           END-IF.

     ‚* NUR BESTIMMTE OBJEKTART ANZEIGEN?
           IF       PLSEU2 OF FORMAT1   NOT = SPACES
            AND     PLSEU2 OF PPGMLIB-P NOT = PLSEU2 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* NACH TEXT SELEKTIEREN?
           IF       TXTSEL OF FORMAT1   NOT = SPACES
                    MOVE PPGMLIB-P           TO RECORD-WRK
                    MOVE LENGTH OF PPGMLIB-P TO RECORD-LEN
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

     ‚* PRÜFEN ALLE SELEKTIERTEN TAGS VORHANDEN IST
##CR ˆ* FEHLENDER PUNKT AM ENDE
           PERFORM  CHECK-TAG-VORHANDEN
           IF       RET-CODE = SPACES
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
##CR ˆ* FEHLENDES END-IF
     ‚*€   END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PPGMLIBF OF PPGMLIB-P TO FORMAT2.
     ‚* KOMMENTAR AUFRÜCKEN WENN Z.B. NUR DER ZWEITE GEFÜLLT IST
           IF       PLKOM1 OF FORMAT2 = SPACES
                    IF   PLKOM2 OF PPGMLIB-P NOT = SPACES
                         MOVE PLKOM2 OF PPGMLIB-P TO PLKOM1 OF FORMAT2
                    ELSE
     ‚*                  KOMMENTAR 3 AUFRÜCKEN BZW. TEILDATEIENTEXT
                         IF   PLKOM3 OF PPGMLIB-P NOT = SPACES
                              MOVE PLKOM3 OF PPGMLIB-P TO
                                   PLKOM1 OF FORMAT2
                         ELSE
                              MOVE PLMTXT OF PPGMLIB-P TO
                                   PLKOM1 OF FORMAT2
                         END-IF
                    END-IF
           END-IF.

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

     ‚* F13 WEITERE SELEKTION
           IF       IN13 IN FMT03-I-INDIC = AN
                    PERFORM AENDERN-TAGSELEKTION
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
                    CALL "HMP0003" USING PLLIB  OF FORMAT2
                                         PLFILE OF FORMAT2
                                         PGM-WRK
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
                    CALL "HMP0008" USING PLLIB  OF FORMAT2
                                         PLFILE OF FORMAT2
                    END-CALL
           END-IF.

     ‚* DETAILANZEIGE DES DATENSATZES
           IF       AUSW OF FORMAT2 = "5"
                    PERFORM START-SEU
           END-IF.

     ‚* BESCHREIBUNG ANZEIGEN
           IF       AUSW OF FORMAT2 = "6"
                    INITIALIZE F3, F12
                    MOVE "5" TO AUSWAHL
                    CALL "HMP0006" USING PLLIB  OF FORMAT2
                                         PLFILE OF FORMAT2
                                         AUSWAHL
                                         PGM-WRK
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

     ‚* AUSWAHL S STORNO TRANSPORTAUFTRÄGE
           IF       AUSW OF FORMAT2 = "S"
                    IF   RET-CODE = "4"
                         MOVE     AN TO IN01 OF INXX
                         MOVE     CORR INXX TO FMT02-O-INDIC
                         MOVE     CORR FORMAT2 TO FMT02-O
                         REWRITE  SUBFILE WS-REC FORMAT IS "FMT02"
                         GO TO ANZ-SFL-20
                    END-IF
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     CORR FORMAT2 TO FMT02-O.

           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           REWRITE  SUBFILE WS-REC FORMAT IS "FMT02".

##CR ˆ* FALSCHER GOTO-BEFEHL
           GO TO    FEH-MEL-90.

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

     ‚* F13 WEITERE SELEKTION
           IF       IN13 IN FMT03-I-INDIC = AN
                    PERFORM AENDERN-TAGSELEKTION
                    GO TO FEH-MEL-90
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUE SECTION
     ‚*--------------------------------------------------------------
##CR ˆ* SECTION WIRD NICHT VERWENDET
       TEST-SECTION SECTION.
##CR ˆ* ERSTE SPRUNGMARKE BEGINNT NICHT MIT -00
##CR ˆ* ... UND DAHER WIRD DIESE SPRUNGMARKE AUCH NICHT VERWENDET
       TST-SEC-01.

           INITIALIZE RET-CODE.

     ‚* DIE EINZIGE RICHTIGES GO TO-VARIANTE
           GO TO    TST-SEC-90.

     ‚* FALSCHE GO TO-VARIANTEN
           GO       TO TST-SEC-90.
           GO TO TST-SEC-90.

       TST-SEC-90.
           EXIT.
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
