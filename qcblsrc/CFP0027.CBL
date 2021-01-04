       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0027.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* WINDOW ZUR ANZEIGE EINES RECORDS (LGPGM106)                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.09.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE RECORDS                              *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚* PCFGFFDL2: WHLIB, WHFILE, WHIBO
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
     ‚*--- PCFGFFDL2: WHLIB, WHFILE, WHIBO
       FD  PCFGFFD-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-L.
           COPY DDS-ALL-FORMATS OF PCFGFFDL2.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0027".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚*----Temporäre Indikator------
       01  INXX.
         02 IN50                          PIC 1.
         02 IN51                          PIC 1.
         02 IN52                          PIC 1.
         02 IN70                          PIC 1.
         02 IN91                          PIC 1.
         02 IN95                          PIC 1.
         02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF CFD0027DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF CFD0027DE
           REPLACING FMT02-I BY FORMAT2-I
                    FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF CFD0027DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF CFD0027DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  AUSWAHL                       PIC X(2).

       01  LEN                           PIC 9(3).
       01  POS                           PIC 9(3).
       01  RECH-FLD-1                    PIC 9(4).
       01  RECH-FLD-2                    PIC 9(1).

       01  ERROR-FLAG                    PIC 9(1).
       01  NUMBER-WRK                    PIC X(31).
       01  NUMBER-PARM                   PIC X(31).
       01  NUMBER-JUST                   PIC 9(1).
       01  COMPUTE-FLD                   PIC 9(3).

     ‚* FELDER FÜR UMSETZUNG GEPACKT<->GEZONT
       01  ZONED-PUFFER                     PIC X(18).
       01  ZONED-01 REDEFINES ZONED-PUFFER  PIC S9(01).
       01  ZONED-02 REDEFINES ZONED-PUFFER  PIC S9(02).
       01  ZONED-03 REDEFINES ZONED-PUFFER  PIC S9(03).
       01  ZONED-04 REDEFINES ZONED-PUFFER  PIC S9(04).
       01  ZONED-05 REDEFINES ZONED-PUFFER  PIC S9(05).
       01  ZONED-06 REDEFINES ZONED-PUFFER  PIC S9(06).
       01  ZONED-07 REDEFINES ZONED-PUFFER  PIC S9(07).
       01  ZONED-08 REDEFINES ZONED-PUFFER  PIC S9(08).
       01  ZONED-09 REDEFINES ZONED-PUFFER  PIC S9(09).
       01  ZONED-10 REDEFINES ZONED-PUFFER  PIC S9(10).
       01  ZONED-11 REDEFINES ZONED-PUFFER  PIC S9(11).
       01  ZONED-12 REDEFINES ZONED-PUFFER  PIC S9(12).
       01  ZONED-13 REDEFINES ZONED-PUFFER  PIC S9(13).
       01  ZONED-14 REDEFINES ZONED-PUFFER  PIC S9(14).
       01  ZONED-15 REDEFINES ZONED-PUFFER  PIC S9(15).
       01  ZONED-16 REDEFINES ZONED-PUFFER  PIC S9(16).
       01  ZONED-17 REDEFINES ZONED-PUFFER  PIC S9(17).
       01  ZONED-18 REDEFINES ZONED-PUFFER  PIC S9(18).

       01  PACKED-PUFFER                      PIC X(10).
       01  PACKED-01 REDEFINES PACKED-PUFFER  PIC S9(01) COMP-3.
       01  PACKED-02 REDEFINES PACKED-PUFFER  PIC S9(02) COMP-3.
       01  PACKED-03 REDEFINES PACKED-PUFFER  PIC S9(03) COMP-3.
       01  PACKED-04 REDEFINES PACKED-PUFFER  PIC S9(04) COMP-3.
       01  PACKED-05 REDEFINES PACKED-PUFFER  PIC S9(05) COMP-3.
       01  PACKED-06 REDEFINES PACKED-PUFFER  PIC S9(06) COMP-3.
       01  PACKED-07 REDEFINES PACKED-PUFFER  PIC S9(07) COMP-3.
       01  PACKED-08 REDEFINES PACKED-PUFFER  PIC S9(08) COMP-3.
       01  PACKED-09 REDEFINES PACKED-PUFFER  PIC S9(09) COMP-3.
       01  PACKED-10 REDEFINES PACKED-PUFFER  PIC S9(10) COMP-3.
       01  PACKED-11 REDEFINES PACKED-PUFFER  PIC S9(11) COMP-3.
       01  PACKED-12 REDEFINES PACKED-PUFFER  PIC S9(12) COMP-3.
       01  PACKED-13 REDEFINES PACKED-PUFFER  PIC S9(13) COMP-3.
       01  PACKED-14 REDEFINES PACKED-PUFFER  PIC S9(14) COMP-3.
       01  PACKED-15 REDEFINES PACKED-PUFFER  PIC S9(15) COMP-3.
       01  PACKED-16 REDEFINES PACKED-PUFFER  PIC S9(16) COMP-3.
       01  PACKED-17 REDEFINES PACKED-PUFFER  PIC S9(17) COMP-3.
       01  PACKED-18 REDEFINES PACKED-PUFFER  PIC S9(18) COMP-3.

       01  NUMBER-ROUNDED                PIC X(1) VALUE "0".
       01  NUMBER-DECFMT                 PIC X(1).
       01  NUMBER-EDTCDE                 PIC X(1) VALUE "L".
       01  NUMBER-DIGITS                 PIC 9(2).
       01  NUMBER-DECIMAL                PIC 9(2).
       01  NUMBER-OUT-LEN                PIC 9(2).
       01  NUMBER-OUT                    PIC X(40).

       01  FLD-DECIMAL                   LIKE WHFLDP OF PCFGFFD-L.
       01  FLD-LEN                       LIKE WHFLDB OF PCFGFFD-L.
       01  FLD-LEN-2                     PIC 9(2).
       01  FLD-LEN-4                     PIC 9(4).
       01  FLD-POS                       LIKE WHFOBO OF PCFGFFD-L.
       01  FLD-POS-UNI                   LIKE WHFOBO OF PCFGFFD-L.
       01  FLD-DIGITS                    LIKE WHFLDD OF PCFGFFD-L.
       01  SELECTION-SAVE                LIKE FLDSEARCH OF FORMAT3.
       01  FLDSTART-SAVE                 LIKE FLDSTART  OF FORMAT3.
       01  FLDSTART-LEN                  PIC 9(3).
       01  FLDSTART-POS-LAST             LIKE SUBFILE-RECORD-NUMBER.
       01  FLDSTART-POS                  LIKE SUBFILE-RECORD-NUMBER.
       01  WEITERSUCHEN                  PIC 9(1).
       01  NUM-NEGATIVE                  PIC 9(1).
       01  CHAR-NUM                      PIC 9(3).
       01  NEGATIV-KZ1                   PIC 9(3).
       01  NEGATIV-KZ2                   PIC 9(3).
       01  RECORD-UNICODE                PIC N(8192) NATIONAL.
     ‚* VARIABLEN FÜR DIE KONVERTIERUNG
       01  CNV-LEN-IN                    PIC S9(9) BINARY.
       01  CNV-LEN-OUT                   PIC S9(9) BINARY.
       01  CCSID-IN                      PIC 9(5).
       01  CODE-PAGE                     PIC 9(5).
       01  FILE-NAME-READ                LIKE WHNAME OF PCFGFFD-L.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-RECORD                      LIKE RECORD-WRK.
       01  X-LIB                         PIC X(10).
       01  X-FILE                        PIC X(10).
       01  X-FORMAT                      LIKE WHNAME OF PCFGFFD-L.
       01  X-F3                          PIC X(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-RECORD
                                X-LIB
                                X-FILE
                                X-FORMAT
                                X-F3.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0027" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0027DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN INPUT PCFGFFD-DL
           END-IF.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* VARIABLEN INITIALISIEREN
           MOVE     1 TO SFL-ZEILE.
           PERFORM  INITIALIZE-VARIABLEN.

       ANF010.
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3 = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER FELDER
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
           INITIALIZE ERROR-FLAG.
     ‚* FELDER AUSLESEN
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     X-LIB               TO WHLIB  OF PCFGFFD-L.
           MOVE     X-FILE              TO WHFILE OF PCFGFFD-L.
           MOVE     ZEROES              TO WHIBO  OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* KEYABFRAGE
           IF       WHLIB  OF PCFGFFD-L NOT = X-LIB
            OR      WHFILE OF PCFGFFD-L NOT = X-FILE
                    GO TO LES-DAT-90
           END-IF.

     ‚* FORMAT ANGEGEBEN?
           IF       X-FORMAT            NOT = SPACES
            AND     WHNAME OF PCFGFFD-L NOT = X-FORMAT
                    GO TO LES-DAT-20
           END-IF.

     ‚* FELDER FÜR SPÄTERE ABFRAGEN SICHERN
           MOVE     WHFLDP OF PCFGFFD-L TO FLD-DECIMAL.
           MOVE     WHFLDB OF PCFGFFD-L TO FLD-LEN.
           MOVE     WHFOBO OF PCFGFFD-L TO FLD-POS.
           MOVE     WHFLDD OF PCFGFFD-L TO FLD-DIGITS.

     ‚* FELDLÄNGE HINZUADIEREN
     ‚* ABHÄNGIG DAVON, OB ES SICH UM EINE UNICODEFELD                 HT
     ‚* HANDELT ODER NICHT                                             HT
           IF       WHFLDT OF PCFGFFD-L = "G"
                    ADD WHFLDG OF PCFGFFD-L TO FLD-POS-UNI
           ELSE
                    ADD WHFLDB OF PCFGFFD-L TO FLD-POS-UNI
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PCFGFFDF OF PCFGFFD-L TO FORMAT2.

           MOVE     WHFLDE OF PCFGFFD-L TO FLDNAME OF FORMAT2.
           MOVE     WHFTXT OF PCFGFFD-L TO FLDDESC OF FORMAT2

           EVALUATE WHFLDT OF PCFGFFD-L
     ‚*             ALPHANUMERISCH
                    WHEN "A" PERFORM FILL-ALPHA
     ‚*             GEZONETE ZAHLEN
                    WHEN "S" PERFORM FILL-NUMERIC
     ‚*             GEPACKTE ZAHLEN
                    WHEN "P" PERFORM FILL-NUMERIC
     ‚*             UNICODE WERTE
                    WHEN "G" PERFORM FILL-UNICODE
     ‚*             REST
                    WHEN OTHER
                         MOVE WHFLDT OF PCFGFFD-L TO FLDTYP OF FORMAT2
                         PERFORM FILL-ALPHA
           END-EVALUATE.
           MOVE     FUNCTION DISPLAY-OF(FLDCONTENT OF FORMAT2) TO
                    HIDCONTENT OF FORMAT2.

     ‚* SELEKTION VORGESEHEN?
           IF       SELECTION-SAVE NOT = SPACES
                    MOVE SELECTION-SAVE         TO FLDSEARCH OF FORMAT3
                    MOVE HIDCONTENT OF FORMAT2  TO RECORD-WRK
                    MOVE FLDSEARCH  OF FORMAT3  TO SELEKTION-WRK
                    MOVE LENGTH OF HIDCONTENT OF FORMAT2 TO RECORD-LEN
                    CALL PROCEDURE "HLP0001" USING RECORD-WRK
                                                   RECORD-LEN
                                                   SELEKTION-WRK
                                                   RET-CODE
                    END-CALL
                    IF   RET-CODE = "1"
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
                    MOVE    ZEROES  TO ANZREC-WRK
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* FELDNAMEN GEFUNDEN ?
           IF       FLDSTART-SAVE NOT = SPACES
                    IF   FLDNAME OF FORMAT2(1:FLDSTART-LEN) =
                         FLDSTART-SAVE(1:FLDSTART-LEN)
     ‚*                  EINTRAG INVERS DARSTELLEN
                         MOVE AN  TO IN52 OF INXX
                    ELSE
                         MOVE AUS TO IN52 OF INXX
                    END-IF
           END-IF.

     ‚* JEDE 2. ZEILE HELL ANZEIGEN
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           PERFORM  SET-COLOR-FMT02.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
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
           PERFORM  LESE-DATEN.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

           IF       DATEN = ZEROES
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT05" TO FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE  AUS TO IN91 OF INXX
           ELSE
                    MOVE  AN  TO IN91 OF INXX
           END-IF.

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
           IF       FLDSTART-POS > ZEROES
                    IF   FLDSTART-POS > ANZREC-WRK
                         MOVE 1 TO FLDSTART-POS
                    END-IF
                    MOVE FLDSTART-POS TO SUBFILE-RECORD-NUMBER
           ELSE
                    IF   SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                         MOVE SFL-ZEILE TO SUBFILE-RECORD-NUMBER
                    END-IF
           END-IF.

     ‚* WENN UNGÜLTIGE DATEN ANGEZEIGT WERDEN, SO WIRD EINE MELDUNG
     ‚* AUSGEGEBEN
           IF       ERROR-FLAG = 1
                    MOVE AN  TO IN70 IN INXX
           ELSE
                    MOVE AUS TO IN70 IN INXX
           END-IF.

     ‚* ANZEIGE SUBFILE
           MOVE     X-FILE                TO ANZEIGETYP OF FORMAT3.

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

     ‚* WURDE EINE BESTIMMT ZEILE SELEKTIERT?
           IF       FLDSTART OF FORMAT3 NOT = SPACES
                    PERFORM ERMITTELN-LISTENANFANG
           ELSE
     ‚*             WURDE DIE FELDAUSWAHL ENTFERNT, DANN SUBFILE NEU
     ‚*             AUSGEBEN.
                    IF   FLDSTART-SAVE NOT = SPACES
                         MOVE "1" TO SFL-CHG
                    END-IF
                    MOVE ZEROES TO FLDSTART-POS
                    MOVE SPACES TO FLDSTART-SAVE
           END-IF.

     ‚* GEGEBENENFALLS SUCHAUSWAHL MERKEN
           IF       FLDSEARCH OF FORMAT3 NOT = SPACES
                    MOVE "1" TO SFL-CHG
                    MOVE FLDSEARCH OF FORMAT3 TO SELECTION-SAVE
           ELSE
                    IF   SELECTION-SAVE NOT = SPACES
                         MOVE "1" TO SFL-CHG
                    END-IF
                    INITIALIZE SELECTION-SAVE
           END-IF.

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

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PPGMTXT FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CFP0027"         TO MAINUEB OF FORMAT3.
           MOVE     "CFP0027*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LISTENANFANG ERMITTELN
     ‚*--------------------------------------------------------------
       ERMITTELN-LISTENANFANG SECTION.
       ERM-LSA-00.

     ‚* FELDAUSWAHL GEÄNDERT -> SUBFILE POSITIONIERUNG ZURÜCKSETZEN
           IF       FLDSTART OF FORMAT3 NOT = FLDSTART-SAVE
                    MOVE ZERO   TO WEITERSUCHEN
                    MOVE ZEROES TO FLDSTART-POS
                    MOVE ZEROES TO FLDSTART-POS-LAST
           ELSE
                    MOVE 1      TO WEITERSUCHEN
           END-IF.

     ‚* FELDAUSWAHL MERKEN
           MOVE     FLDSTART OF FORMAT3 TO FLDSTART-SAVE.

     ‚* EINGABELÄNGE ERMITTELN
           MOVE     ZEROES TO FLDSTART-LEN.
           INSPECT  FLDSTART-SAVE TALLYING FLDSTART-LEN
                    FOR CHARACTERS BEFORE SPACES.

     ‚* ALLE SUBFILEZEILEN DURCHLESEN + TREFFERZEILEN MARKIEREN
           PERFORM  VARYING SUBFILE-RECORD-NUMBER FROM 1 BY 1
             UNTIL  SUBFILE-RECORD-NUMBER > ANZREC-WRK
                OR  SUBFILE-RECORD-NUMBER = ZEROES
                    MOVE "FMT02"  TO FMT-WRK
                    MOVE FORMAT2  TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLREADREC
                    MOVE WSREC-WRK      TO FORMAT2-I
                    MOVE CORR FORMAT2-I TO FORMAT2
     ‚*             FELDNAMEN GEFUNDEN ?
                    IF   FLDNAME OF FORMAT2(1:FLDSTART-LEN) =
                         FLDSTART-SAVE(1:FLDSTART-LEN)
                         IF   FLDSTART-POS = ZEROES
     ‚*                       ERSTER TREFFER ?
                              MOVE ZERO TO WEITERSUCHEN
                              MOVE SUBFILE-RECORD-NUMBER TO
                                   FLDSTART-POS
                              MOVE SUBFILE-RECORD-NUMBER TO
                                   FLDSTART-POS-LAST
                         ELSE
     ‚*                       NACH DEM NÄCHSTEN TREFFER SUCHEN UND
     ‚*                       SUBFILE POSITIONIEREN.
                              IF   WEITERSUCHEN = 1
                               AND SUBFILE-RECORD-NUMBER > FLDSTART-POS
                                   MOVE ZERO TO WEITERSUCHEN
                                   MOVE SUBFILE-RECORD-NUMBER TO
                                        FLDSTART-POS
                                   MOVE SUBFILE-RECORD-NUMBER TO
                                        FLDSTART-POS-LAST
                              END-IF
                         END-IF
     ‚*                  EINTRAG INVERS DARSTELLEN
                         MOVE AN  TO IN52 OF INXX
                    ELSE
                         MOVE AUS TO IN52 OF INXX
                    END-IF
     ‚*             SATZ ZURÜCKSCHREIBEN
                    PERFORM SET-COLOR-FMT02
                    MOVE CORR INXX TO FORMAT2-O-INDIC
                    MOVE "FMT02"   TO FMT-WRK
                    MOVE FORMAT2   TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLREWRITE
                    MOVE AUS TO INXX
           END-PERFORM.

       ERM-LSA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER RECORDDATEN BEI ALPHA-FELDER
     ‚*--------------------------------------------------------------
       FILL-ALPHA SECTION.
       FIL-ALP-00.

           MOVE     AUS TO IN51 OF INXX.

     ‚* FELD TYPEN FÜLLEN
           MOVE     FLD-LEN TO FLD-LEN-4.
           MOVE     FLD-LEN-4 TO FLDTYP OF FORMAT2(1:4).
           INSPECT  FLDTYP OF FORMAT2 REPLACING LEADING
                    ZEROES BY SPACES.
           MOVE     "A" TO FLDTYP OF FORMAT2(5:1).

     ‚* RECORD IN UNICODE KONVERTIEREN
           MOVE     ZERO                TO CCSID-IN.
           MOVE     13488               TO CODE-PAGE.
           MOVE     FLD-LEN             TO CNV-LEN-IN.
           COMPUTE  CNV-LEN-OUT = FLD-LEN * 2
           END-COMPUTE.
           CALL     PROCEDURE  "CCP0818" USING
                                           X-RECORD(FLD-POS:FLD-LEN)
                                           RECORD-UNICODE(FLD-POS-UNI:)
                                           CNV-LEN-IN
                                           CNV-LEN-OUT
                                           CCSID-IN
                                           CODE-PAGE
                                           RET-CODE
           END-CALL.
     ‚* IN EINER SCHLEIFE DIE ZEICHEN ÜBERNEHMEN
           PERFORM  WITH TEST BEFORE VARYING POS FROM 1 BY 1 UNTIL
     ‚*             WEGEN UNICODE FELDLÄNGE DURCH 2 TEILEN!
                    POS > ( LENGTH  OF FLDCONTENT OF FORMAT2 / 2) OR
                    POS > FLD-LEN
     ‚*             UNGÜLTIGE HEX-ZEICHEN WERDEN NICHT ANGEZEIGT ...
                    IF   RECORD-UNICODE(FLD-POS-UNI + POS - 1:1)
                         < SPACES
                         MOVE AN TO IN51 OF INXX
                         MOVE 1 TO ERROR-FLAG
     ‚*             ... SONDERN DURCH "@" ERSETZT
                         MOVE "@" TO FLDCONTENT OF FORMAT2(POS:1)
                    ELSE
                         MOVE RECORD-UNICODE(FLD-POS-UNI + POS - 1:1) TO
                              FLDCONTENT OF FORMAT2(POS:1)
                    END-IF
           END-PERFORM.

       FIL-ALP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER RECORDDATEN BEI NUMERISCHEN-FELDER
     ‚*--------------------------------------------------------------
       FILL-NUMERIC SECTION.
       FIL-NUM-00.

           MOVE     AUS TO IN51 OF INXX.

     ‚* FELD TYPEN FÜLLEN
           MOVE     FLD-DIGITS  TO FLD-LEN-2.
           MOVE     FLD-LEN-2 TO FLDTYP OF FORMAT2(3:2).
           INSPECT  FLDTYP OF FORMAT2(3:2) REPLACING
                    LEADING ZEROES BY SPACES.
           MOVE     WHFLDT OF PCFGFFD-L    TO FLDTYP OF FORMAT2(5:1).
           MOVE     FLD-DECIMAL TO FLD-LEN-2.
           MOVE     FLD-LEN-2 TO FLDTYP OF FORMAT2(7:2).
           IF       FLDTYP OF FORMAT2(7:1) = "0"
                    MOVE FLDTYP OF FORMAT2(8:1) TO
                         FLDTYP OF FORMAT2(7:1)
                    MOVE SPACE TO FLDTYP OF FORMAT2(8:1)
           END-IF.

     ‚* IN EINER SCHLEIFE DIE ZEICHEN ÜBERNEHMEN
           PERFORM  WITH TEST BEFORE VARYING POS FROM 1 BY 1 UNTIL
                    POS > LENGTH  OF FLDCONTENT OF FORMAT2 OR
                    POS > FLD-LEN
     ‚*             UNGÜLTIGE HEX-ZEICHEN WERDEN NICHT ANGEZEIGT ...
                    IF   X-RECORD(FLD-POS + POS - 1:1) < SPACES
                         MOVE AN TO IN51 OF INXX
                         MOVE 1 TO ERROR-FLAG
     ‚*             ... SONDERN DURCH "@" ERSETZT
                         MOVE ALL "@" TO
                              FLDCONTENT OF FORMAT2(1:FLD-LEN)
                         GO TO FIL-NUM-90
                    ELSE
                         MOVE X-RECORD(FLD-POS + POS - 1:1) TO
                              NUMBER-WRK(POS:1)
                    END-IF
           END-PERFORM.

     ‚* UMSETZUNG GEPACKT<->GEZONT BEI BEDARF
           IF       WHFLDT OF PCFGFFD-L    = "P"
                    PERFORM PACKED-ZONED
           END-IF.

     ‚* WERT ZU GROß ZUM FORMATIEREN?
           IF       FLD-DIGITS > 21
                    MOVE NUMBER-WRK TO FLDCONTENT OF FORMAT2
                    GO TO FIL-NUM-90
           END-IF.

     ‚* PRÜFEN OB DIE ZAHL NEGATIV IST UND UMSETZEN IN EINE
     ‚* NICHT NEGATIVE ZAHL
           MOVE     NUMBER-WRK(FLD-DIGITS:1) TO NEGATIVE-WRK.
           PERFORM  COPY-CHECK-NEGATIVITY.
           MOVE     NEGATIVE-FLAG TO NUM-NEGATIVE.
           MOVE     NEGATIVE-WRK  TO NUMBER-WRK(FLD-DIGITS:1).

           MOVE     ZEROES TO NUMBER-PARM.
           COMPUTE  COMPUTE-FLD =
                    21 - (FLD-DIGITS - FLD-DECIMAL) + 1
           END-COMPUTE.
           MOVE     NUMBER-WRK TO NUMBERORIG OF FORMAT2.
           MOVE     NUMBER-WRK TO
                    NUMBER-PARM(COMPUTE-FLD:FLD-DIGITS).

     ‚*    WENN DIE ZAHL NEGATIV WAR, DIE NEUE ZAHL NUN EBENFALLS
     ‚*    NEGATIV SETZEN
           IF       NUM-NEGATIVE = 1
                    MOVE    NUMBER-PARM(31:1) TO NEGATIVE-WRK
                    PERFORM COPY-CONVERT-NEGATIVE
                    MOVE    NEGATIVE-WRK TO NUMBER-PARM(31:1)
           END-IF.

     ‚* AUFRUF PROGRAMM ZUR FORMATIERUNG DER ZAHLEN
           MOVE     ZEROES TO NUMBER-OUT-LEN.
           MOVE     FLD-DIGITS  TO NUMBER-DIGITS.
           MOVE     FLD-DECIMAL TO NUMBER-DECIMAL.
           MOVE     ZEROES      TO NUMBER-JUST.
           CALL     PROCEDURE "HLP0018" USING NUMBER-PARM
                                              NUMBER-OUT
                                              NUMBER-OUT-LEN
                                              NUMBER-DIGITS
                                              NUMBER-DECIMAL
                                              NUMBER-EDTCDE
                                              NUMBER-ROUNDED
                                              NUMBER-DECFMT
                                              NUMBER-JUST
           END-CALL.
           MOVE     NUMBER-OUT TO FLDCONTENT OF FORMAT2.

       FIL-NUM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER RECORDDATEN BEI ALPHA-FELDER
     ‚*--------------------------------------------------------------
       FILL-UNICODE SECTION.
       FIL-UNI-00.

     ‚* IN UNICODE CONVERTIEREN
           MOVE     13488    TO CCSID-IN.
           MOVE     13488    TO CODE-PAGE.
           MOVE     FLD-LEN                  TO CNV-LEN-IN.
           COMPUTE  CNV-LEN-OUT = FLD-LEN * 2
           END-COMPUTE.
           CALL     PROCEDURE  "CCP0818" USING
                                           X-RECORD(FLD-POS:FLD-LEN)
                                           RECORD-UNICODE(FLD-POS-UNI:)
                                           CNV-LEN-IN
                                           CNV-LEN-OUT
                                           CCSID-IN
                                           CODE-PAGE
                                           RET-CODE
           END-CALL.

           MOVE     AUS TO IN51 OF INXX.

     ‚* FELD TYPEN FÜLLEN
           MOVE     FLD-LEN TO FLD-LEN-4.
           MOVE     FLD-LEN-4 TO FLDTYP OF FORMAT2(1:4).
           INSPECT  FLDTYP OF FORMAT2 REPLACING LEADING
                    ZEROES BY SPACES.
           MOVE     "A" TO FLDTYP OF FORMAT2(5:1).

     ‚* IN EINER SCHLEIFE DIE ZEICHEN ÜBERNEHMEN
           PERFORM  WITH TEST BEFORE VARYING POS FROM 1 BY 1 UNTIL
                    POS > (LENGTH  OF FLDCONTENT OF FORMAT2 / 2) OR
                    POS > FLD-LEN
     ‚*             UNGÜLTIGE HEX-ZEICHEN WERDEN NICHT ANGEZEIGT ...
                    IF   RECORD-UNICODE(FLD-POS-UNI + POS - 1:1)
                         < SPACES
                         MOVE AN TO IN51 OF INXX
                         MOVE 1 TO ERROR-FLAG
     ‚*             ... SONDERN DURCH "@" ERSETZT
                         MOVE "@" TO FLDCONTENT OF FORMAT2(POS:1)
                    ELSE
                         MOVE RECORD-UNICODE(FLD-POS-UNI + POS - 1:1) TO
                              FLDCONTENT OF FORMAT2(POS:1)
                    END-IF
           END-PERFORM.

       FIL-UNI-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* UMSETZUNG GEPACKTE FELDER IN GEZONTE FELDER
     ‚*---------------------------------------------------------------
       PACKED-ZONED SECTION.
       PAC-ZON-00.

           MOVE     NUMBER-WRK TO PACKED-PUFFER.

     ‚* JE NACH FELDLÄNGE IN EINEM SPEZIELLEN MOVE AUS DEN GEPACKTEN
     ‚* FELDERN EIN GEZONTES MACHEN
           EVALUATE (FLD-DIGITS - FLD-DECIMAL)
                    WHEN 1
                      MOVE PACKED-01 TO ZONED-01
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 2
                      MOVE PACKED-02 TO ZONED-02
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 3
                      MOVE PACKED-03 TO ZONED-03
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 4
                      MOVE PACKED-04 TO ZONED-04
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 5
                      MOVE PACKED-05 TO ZONED-05
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 6
                      MOVE PACKED-06 TO ZONED-06
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 7
                      MOVE PACKED-07 TO ZONED-07
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 8
                      MOVE PACKED-08 TO ZONED-08
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 9
                      MOVE PACKED-09 TO ZONED-09
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 10
                      MOVE PACKED-10 TO ZONED-10
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 11
                      MOVE PACKED-11 TO ZONED-11
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 12
                      MOVE PACKED-12 TO ZONED-12
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 13
                      MOVE PACKED-13 TO ZONED-13
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 14
                      MOVE PACKED-14 TO ZONED-14
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 15
                      MOVE PACKED-15 TO ZONED-15
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 16
                      MOVE PACKED-16 TO ZONED-16
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 17
                      MOVE PACKED-17 TO ZONED-17
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
                    WHEN 18
                      MOVE PACKED-18 TO ZONED-18
                      MOVE ZONED-PUFFER(1:FLD-DIGITS - FLD-DECIMAL) TO
                           NUMBER-WRK
           END-EVALUATE.

       PAC-ZON-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VARIABLEN INITIALISIEREN
     ‚*--------------------------------------------------------------
       INITIALIZE-VARIABLEN SECTION.
       INZ-VAR-00.

           IF       BETRIEBSSYSTEM > ZEROES
                    MOVE 86  TO NEGATIV-KZ1
                    MOVE 41  TO NEGATIV-KZ2
           ELSE
                    MOVE 240 TO NEGATIV-KZ1
                    MOVE 32  TO NEGATIV-KZ2
           END-IF.
           INITIALIZE NUMBER-WRK.

     ‚* INITIALISIEREN DIVERSER FELDER
           INITIALIZE SELECTION-SAVE.
           INITIALIZE FLDSTART-SAVE.
           INITIALIZE FLDSTART-POS.
           INITIALIZE FLDSTART-POS-LAST.
           INITIALIZE WEITERSUCHEN.
           INITIALIZE F3, F12.
           INITIALIZE X-F3.
           INITIALIZE FORMAT3.

           MOVE     "J" TO NUMBER-DECFMT.
     ‚*€   MOVE     "I" TO NUMBER-DECFMT.

       INZ-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBE FÜR SUBFILEZEILE SETZEN. JEDE 2. ZEILE HELL ANZEIGEN
     ‚*--------------------------------------------------------------
       SET-COLOR-FMT02 SECTION.
       SET-COL-00.

     ‚* FELDNAMEN SELEKTIERT ?
           IF       FLDSTART-SAVE NOT = SPACES
                    IF   FLDNAME OF FORMAT2(1:FLDSTART-LEN) =
                         FLDSTART-SAVE(1:FLDSTART-LEN)
     ‚*                  EINTRAG INVERS DARSTELLEN
                         MOVE AN  TO IN52 OF INXX
                    END-IF
           END-IF.

     ‚* JEDE 2. ZEILE HELL ANZEIGEN
           DIVIDE   SUBFILE-RECORD-NUMBER BY 2
                    GIVING    RECH-FLD-1
                    REMAINDER RECH-FLD-2.
           IF       RECH-FLD-2 = ZERO
                    MOVE  AN  TO IN50 OF INXX
           ELSE
                    MOVE  AUS TO IN50 OF INXX
           END-IF.

       SET-COL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* HANDLING VON NEGATIVEN GEZONTEN ZAHLEN
     ‚*---------------------------------------------------------------

           COPY     NEGNUM     OF APG-QCPYSRC.
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
