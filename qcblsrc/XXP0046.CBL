       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0046.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* VERGLEICH ZWEIER DB-DATEIEN                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  10.08.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  VERGLEICH VON 2 DB-DATEIEN                   *
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
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-XXD0046DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PCFGFFDL1: WHLIB, WHFILE, WHIBO
     ‚*-------------------------------------------------------------
           SELECT PCFGFFD-DL
                  ASSIGN       TO  DATABASE-PCFGFFDL2
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
           COPY DDS-ALL-FORMATS OF XXD0046DE.
      /
     ‚*- PCFGFFDL1: WHLIB, WHFILE, WHNAME, WHFLDE
       FD  PCFGFFD-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-L.
           COPY DDS-ALL-FORMATS OF PCFGFFDL2.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN03                          PIC 1.
        05 IN40                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0046DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0046DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF XXD0046DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF XXD0046DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF XXD0046DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF XXD0046DE
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP0046".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST   ".

     ‚* ALLE FLEDER
       01  FLD-TAB1.
        02 TMAX1                         PIC 9(5) COMP-3.
        02 FLD-REC1 OCCURS 0 TO 9999 TIMES DEPENDING ON TMAX1
                                           INDEXED   BY TIDX1.
         04 FLD-REC11.
          07 WHFLDE                      LIKE WHFLDE  OF PCFGFFD-L.
          07 WHFLDT                      LIKE WHFLDT  OF PCFGFFD-L.
          07 WHFLDB                      LIKE WHFLDB  OF PCFGFFD-L.
          07 WHFLDD                      LIKE WHFLDD  OF PCFGFFD-L.
          07 WHFLDP                      LIKE WHFLDP  OF PCFGFFD-L.

       01  FLD-TAB2.
        02 TMAX2                         PIC 9(5) COMP-3.
        02 FLD-REC2 OCCURS 0 TO 9999 TIMES DEPENDING ON TMAX2
                                           INDEXED   BY TIDX2.
         04 FLD-REC21.
          07 WHFLDE                      LIKE WHFLDE  OF PCFGFFD-L.
          07 WHFLDT                      LIKE WHFLDT  OF PCFGFFD-L.
          07 WHFLDB                      LIKE WHFLDB  OF PCFGFFD-L.
          07 WHFLDD                      LIKE WHFLDD  OF PCFGFFD-L.
          07 WHFLDP                      LIKE WHFLDP  OF PCFGFFD-L.

     ‚* ALLE FLEDER
       01  FLD-TAB3.
        02 TMAX3                         PIC 9(5) COMP-3.
        02 FLD-REC3 OCCURS 0 TO 9999 TIMES DEPENDING ON TMAX3
                                           INDEXED   BY TIDX3.
         04 FLD-REC31.
          07 WHFLDE1                     LIKE WHFLDE  OF PCFGFFD-L.
          07 WHFLDT1                     LIKE WHFLDT  OF PCFGFFD-L.
          07 WHFLDB1                     LIKE WHFLDB  OF PCFGFFD-L.
          07 WHFLDD1                     LIKE WHFLDD  OF PCFGFFD-L.
          07 WHFLDP1                     LIKE WHFLDP  OF PCFGFFD-L.
          07 WHFLDE2                     LIKE WHFLDE  OF PCFGFFD-L.
          07 WHFLDT2                     LIKE WHFLDT  OF PCFGFFD-L.
          07 WHFLDB2                     LIKE WHFLDB  OF PCFGFFD-L.
          07 WHFLDD2                     LIKE WHFLDD  OF PCFGFFD-L.
          07 WHFLDP2                     LIKE WHFLDP  OF PCFGFFD-L.

       01  AUSWAHL                       PIC X(2).

     ‚* SCHALTER
       01  TAB-WRK                       PIC 9(1).

     ‚* SUCHFELDER
       01  SEARCH-FLD                    LIKE WHFLDE  OF PCFGFFD-L.
       01  INDEX-FLD                     PIC 9(4).
       01  TIDX-SAV                      PIC 9(4).

     ‚* PARAMETER FÜR DAS CMP0005
       01  LIB-WRK                       PIC X(10).
       01  FILE-WRK                      PIC X(10).

     ‚* PARAMETER FÜR DAS XXP0045
       01  0045-AKTION                   PIC X(20).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0046" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0046DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN INPUT PCFGFFD-DL
           END-IF.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DER DATEIEN
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

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
           MOVE     "XXP0046"         TO MAINUEB OF FORMAT1.
           MOVE     "XXP0046*1"       TO SUBUEB  OF FORMAT1.

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
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F20 PROGRAMMINFORMATIONEN
           IF       IN20 IN FORMAT1-I-INDIC = AN
                    PERFORM ANZEIGE-PGM-INFOS
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

           INITIALIZE DATEN.
     ‚* ERSTE DATEI EINLESEN
           MOVE     1                 TO TAB-WRK.
           MOVE     LIB1   OF FORMAT1 TO LIB-WRK.
           MOVE     FILE1  OF FORMAT1 TO FILE-WRK.
           PERFORM  FILL-INT-TAB.
     ‚* ZWEITE DATEI EINLESEN
           MOVE     2                 TO TAB-WRK.
           MOVE     LIB2   OF FORMAT1 TO LIB-WRK.
           MOVE     FILE2  OF FORMAT1 TO FILE-WRK.
           PERFORM  FILL-INT-TAB.

     ‚* 1. ERST DIE LINKE SEITE FÜLLEN
           PERFORM  FILL-CTABLE-FROM-LEFT.

     ‚* 2. DANN MIT DER ZWEITEN DATEI ALLE FELDER FÜLLEN DIE ES BEI
     ‚*    BEIDEN GIBT
           PERFORM  FILL-CTABLE-FROM-RIGHT.

     ˆ* 3. DANACH IN DER ZWEITEN DATEI PRÜFEN WELCHE FELDER ES NICHT
     ˆ*    GIBT UND DIESE DANN AN DER STELLE HINZUFÜGEN UND ALLE
     ˆ*    ANDEREN NACH UNTEN VERSCHIEBEN

     ‚* JETZT DIE VOLLSTÄNDIG GEFÜLLTE TABELLE DURCHLESEN
           SET      TIDX3 TO ZEROES.

       LES-DAT-20.
           SET      TIDX3 UP BY 1.
           IF       TIDX3 > TMAX3 OF FLD-TAB3
                    GO TO LES-DAT-90
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
                    MOVE    ZEROES  TO ANZREC-WRK
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

           INITIALIZE FORMAT2.
           MOVE     CORR FLD-REC31 OF FLD-TAB3(TIDX3) TO FORMAT2.

     ‚* FARBATTRIBUTE SETZEN
           PERFORM  SET-COLOR-FMT02.

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

       ANZ-SFL-10.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN
           PERFORM  LESE-DATEN.

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
           IF       DATEN = ZEROES
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT05" TO FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE  AUS TO IN91 OF INXX
           ELSE
                    MOVE  AN  TO IN91 OF INXX
           END-IF.

           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
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

           MOVE     SPACES TO SFL-CHG.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    IF   SFL-CHG = "1"
                         GO TO ANZ-SFL-10
                    ELSE
                         GO TO ANZ-SFL-20
                    END-IF
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

           PERFORM  SET-COLOR-FMT02.
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
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0046"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0046*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
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
     ‚* FARBATTRIBUTE SETZEN
     ‚*--------------------------------------------------------------
       SET-COLOR-FMT02 SECTION.
       SET-COL-00.

           INITIALIZE RET-CODE.

       SET-COL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TABELLE MIT ALLEN FELDERN AUS DER LINKEN DATEI FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-CTABLE-FROM-LEFT SECTION.
       FIL-CFL-00.

     ‚* GESAMTGRÖßE KANN HIER DIREKT GESETZT WERDEN
           MOVE     TMAX1 OF FLD-TAB1 TO TMAX3 OF FLD-TAB3.

     ‚* DIE EINZELNEN FELDER IN EINER SCHLEIFE DURCHGEHEN
           SET      TIDX1 TO ZEROES.
           PERFORM  TMAX1 OF FLD-TAB1 TIMES
                    SET TIDX1 UP BY 1
                    SET TIDX3 TO TIDX1
     ‚*             ALLE FELDER RÜBERKOPIEREN
                    MOVE WHFLDE  OF FLD-TAB1(TIDX1) TO
                         WHFLDE1 OF FLD-TAB3(TIDX3)
                    MOVE WHFLDT  OF FLD-TAB1(TIDX1) TO
                         WHFLDT1 OF FLD-TAB3(TIDX3)
                    MOVE WHFLDB  OF FLD-TAB1(TIDX1) TO
                         WHFLDB1 OF FLD-TAB3(TIDX3)
                    MOVE WHFLDD  OF FLD-TAB1(TIDX1) TO
                         WHFLDD1 OF FLD-TAB3(TIDX3)
                    MOVE WHFLDP  OF FLD-TAB1(TIDX1) TO
                         WHFLDP1 OF FLD-TAB3(TIDX3)
           END-PERFORM.

       FIL-CFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE GLEICHLAUTENDEN FELDER SUCHEN
     ‚*--------------------------------------------------------------
       FILL-CTABLE-FROM-RIGHT SECTION.
       FIL-CFR-00.

     ‚* DIE EINZELNEN FELDER IN EINER SCHLEIFE DURCHGEHEN
           SET      TIDX2 TO ZEROES.
           PERFORM  TMAX2 OF FLD-TAB2 TIMES
                    SET TIDX2 UP BY 1
     ‚*             JETZT DAS FELD IN DER LINKEN TABELLE SUCHEN
                    MOVE WHFLDE  OF FLD-TAB2(TIDX2) TO SEARCH-FLD
                    MOVE 1                          TO TAB-WRK
                    PERFORM CHECK-FIELD-IN-TABLE
                    IF   INDEX-FLD NOT = ZEROES
     ‚*                  ALLE FELDER RÜBERKOPIEREN
                         MOVE WHFLDE  OF FLD-TAB2(TIDX2) TO
                              WHFLDE2 OF FLD-TAB3(TIDX3)
                         MOVE WHFLDT  OF FLD-TAB2(TIDX2) TO
                              WHFLDT2 OF FLD-TAB3(TIDX3)
                         MOVE WHFLDB  OF FLD-TAB2(TIDX2) TO
                              WHFLDB2 OF FLD-TAB3(TIDX3)
                         MOVE WHFLDD  OF FLD-TAB2(TIDX2) TO
                              WHFLDD2 OF FLD-TAB3(TIDX3)
                         MOVE WHFLDP  OF FLD-TAB2(TIDX2) TO
                              WHFLDP2 OF FLD-TAB3(TIDX3)
                    END-IF
           END-PERFORM.

       FIL-CFR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATEI IN INT. TABELLE EINLESEN
     ‚*--------------------------------------------------------------
       FILL-INT-TAB SECTION.
       FIL-TAB-00.

     ‚* DATEI BEFÜLLEN
           CALL     "CMP0005" USING LIB-WRK
                                    FILE-WRK
           END-CALL.

           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     LIB-WRK           TO WHLIB  OF PCFGFFD-L.
           MOVE     FILE-WRK          TO WHFILE OF PCFGFFD-L.
           MOVE     SPACES            TO WHNAME OF PCFGFFD-L.
           MOVE     SPACES            TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.

       FIL-TAB-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO FIL-TAB-90
           END-IF.

     ‚* KEY-ABFRAGE
           IF       WHLIB  OF PCFGFFD-L  NOT = LIB-WRK
            OR      WHFILE OF PCFGFFD-L  NOT = FILE-WRK
                    GO TO FIL-TAB-90
           END-IF.

           EVALUATE TAB-WRK
                    WHEN ZEROES PERFORM FILL-LEFT
                    WHEN 1      PERFORM FILL-RIGHT
           END-EVALUATE.

           GO TO    FIL-TAB-20.

       FIL-TAB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LINKE TABELLE FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-LEFT SECTION.
       FIL-LFT-00.

           ADD      1 TO TMAX1 OF FLD-TAB1.
           MOVE     WHFLDE OF PCFGFFD-L TO WHFLDE OF FLD-TAB1(TMAX1).
           MOVE     WHFLDT OF PCFGFFD-L TO WHFLDT OF FLD-TAB1(TMAX1).
           MOVE     WHFLDB OF PCFGFFD-L TO WHFLDB OF FLD-TAB1(TMAX1).
           MOVE     WHFLDD OF PCFGFFD-L TO WHFLDD OF FLD-TAB1(TMAX1).
           MOVE     WHFLDP OF PCFGFFD-L TO WHFLDP OF FLD-TAB1(TMAX1).

       FIL-LFT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* RECHTE TABELLE FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-RIGHT SECTION.
       FIL-RGT-00.

           ADD      1 TO TMAX2 OF FLD-TAB2.
           MOVE     WHFLDE OF PCFGFFD-L TO WHFLDE OF FLD-TAB2(TMAX2).
           MOVE     WHFLDT OF PCFGFFD-L TO WHFLDT OF FLD-TAB2(TMAX2).
           MOVE     WHFLDB OF PCFGFFD-L TO WHFLDB OF FLD-TAB2(TMAX2).
           MOVE     WHFLDD OF PCFGFFD-L TO WHFLDD OF FLD-TAB2(TMAX2).
           MOVE     WHFLDP OF PCFGFFD-L TO WHFLDP OF FLD-TAB2(TMAX2).

       FIL-RGT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN FELD IN EINER TABELLE VORKOMMT
     ‚*--------------------------------------------------------------
       CHECK-FIELD-IN-TABLE SECTION.
       CHK-FIT-00.

           INITIALIZE RET-CODE.
           INITIALIZE INDEX-FLD.
     ‚* FELD IN DER PASSENDEN TABELLE SUCHEN
           IF       SEARCH-FLD NOT = SPACES
                    EVALUATE TAB-WRK
     ‚*                      LINKE TABELLE
                             WHEN 1    SET  TIDX-SAV TO TIDX1
                                       PERFORM CHECK-FIELD-IN-TABLE-L
                                       SET  TIDX1 TO TIDX-SAV
     ‚*                      RECHTE TABELLE
                             WHEN 2    SET  TIDX-SAV TO TIDX2
                                       PERFORM CHECK-FIELD-IN-TABLE-R
                                       SET  TIDX2 TO TIDX-SAV
                    END-EVALUATE
           END-IF.

       CHK-FIT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* IN DER LINKEN TABELLE SUCHEN
     ‚*--------------------------------------------------------------
       CHECK-FIELD-IN-TABLE-L SECTION.
       CHK-FIL-00.

           SET      TIDX1 TO 1.
           SEARCH   FLD-REC1
                    AT END MOVE "1" TO RET-CODE
     ‚*             FELD VORHANDEN? INDEX MERKEN!
                    WHEN   WHFLDE OF FLD-REC1(TIDX1) = SEARCH-FLD
                           SET  INDEX-FLD TO TIDX1
           END-SEARCH.

       CHK-FIL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* IN DER RECHTEN TABELLE SUCHEN
     ‚*--------------------------------------------------------------
       CHECK-FIELD-IN-TABLE-R SECTION.
       CHK-FIR-00.

           SET      TIDX2 TO 1.
           SEARCH   FLD-REC2
                    AT END MOVE "1" TO RET-CODE
     ‚*             FELD VORHANDEN? INDEX MERKEN!
                    WHEN   WHFLDE OF FLD-REC2(TIDX2) = SEARCH-FLD
                           SET  INDEX-FLD TO TIDX2
           END-SEARCH.

       CHK-FIR-90.
           EXIT.
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
