       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0014.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SUCHEN UND ANZEIGEN INDIKATOREN EINER DISPLAY-SOURCE          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.09.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  INDIKATOREN SUCHEN + ANZEIGEN                *
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
                  ASSIGN       TO  WORKSTATION-XXD0014DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚*-   TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
     ‚*-------------------------------------------------------------
           SELECT BSSORT-DAT
                  ASSIGN       TO  DISK-BSSORT.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF XXD0014DE.
      /
     ‚*- TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
       SD  BSSORT-DAT.
       01  BSSORT-REC.
            COPY  DDS-FMT02-O OF XXD0014DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN91                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0014DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0014DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0014DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0014DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0014".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

       01  AUSWAHL                       PIC X(2).
       01  START-ERFOLGT                 PIC 9(1).
       01  ACTFMT-WRK                    LIKE FMT-WRK.
       01  AUSGABE-WRK                   LIKE INDIK OF FORMAT2.
       01  LAST-AUSGABE                  LIKE INDIK OF FORMAT2.
       01  INDIK-WRK                     PIC X(9).
       01  ONLY-INDIC                    PIC 9(1).

     ‚* PARAMETER FÜR DAS CRP0025
       01  DUMMY-LIB                     PIC X(10).
       01  DUMMY-RCN                     PIC X(10).
       01  DUMMY-MBR                     PIC X(10).
       01  DUMMY-SRCSEQ                  LIKE SRCSEQ OF QSETSRC-WRK.
       01  ANZAHL-WRK                    PIC 9(9).
       01  FIRST-SEQ                     LIKE SRCSEQ OF QSETSRC-WRK.
       01  LAST-SEQ                      LIKE SRCSEQ OF QSETSRC-WRK.
       01  SOURCE-PTR                    USAGE POINTER.
       01  SECTION-PTR                   USAGE POINTER.

     ‚* PARAMETER FÜR DAS HLP0011
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).
       01  HEX-WRK                       PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-LIB                         PIC X(10).
       01  X-RCN                         PIC X(10).
       01  X-MBR                         PIC X(10).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-LIB
                                X-RCN
                                X-MBR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0014" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0014DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.
           MOVE     1 TO SFL-ZEILE.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     ZEROES TO ONLY-INDIC.
     ‚* SOURCE IN MAPDTA EINLESEN
           PERFORM  SRC-EINLESEN.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN ANHAND DER TAG-DATEI
           PERFORM  LESE-DATEN-SORTFILE.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         GO TO ENDE
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
                    GO TO ENDE
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
           IF       SOURCE-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           IF       SECTION-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SECTION-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      SOURCE-PTR  TO NULL.
           SET      SECTION-PTR TO NULL.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* JETZT EINLESEN DER SOURCE IN MAPDTA
     ‚*--------------------------------------------------------------
       SRC-EINLESEN SECTION.
       SRC-EIN-00.

           INITIALIZE RET-CODE.
     ‚* HILFSPARAMETER LEEREN
           INITIALIZE DUMMY-LIB, DUMMY-RCN, DUMMY-MBR, DUMMY-SRCSEQ.
           INITIALIZE ANZAHL-WRK, FIRST-SEQ, LAST-SEQ.
           IF       SOURCE-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           IF       SECTION-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SECTION-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      SOURCE-PTR  TO NULL.
           SET      SECTION-PTR TO NULL.

     ‚* LESEPORGRAMM AUFRUFEN
           CALL     "CRP0025" USING X-LIB
                                    X-RCN
                                    X-MBR
                                    DUMMY-LIB
                                    DUMMY-RCN
                                    DUMMY-MBR
                                    DUMMY-SRCSEQ
                                    ANZAHL-WRK
                                    FIRST-SEQ
                                    LAST-SEQ
                                    SOURCE-PTR
                                    SECTION-PTR
                                    RET-CODE
           END-CALL.

       SRC-EIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SORTFILE EINLESEN UND AUSGEBEN
     ‚*--------------------------------------------------------------
       LESE-DATEN-SORTFILE SECTION.
       LES-DAS-00.

           INITIALIZE DATEN.
           SORT     BSSORT-DAT ON ASCENDING KEY SORTKZ OF BSSORT-REC
                     WITH DUPLICATES
                     INPUT  PROCEDURE LESE-DATEN
                     OUTPUT PROCEDURE SUBFILE-SORT-OUTPUT.

       LES-DAS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE START-ERFOLGT, ACTFMT-WRK.
           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.

       LES-DAT-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE     1             TO START-ERFOLGT
                    INITIALIZE MAPDTA-REC
                    MOVE     ZEROES        TO MAP-LFDN OF MAPDTA-REC
                    MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO LES-DAT-90
                    END-IF
           ELSE
                    MOVE     MAP-READ-NEXT TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO LES-DAT-90
                    END-IF
           END-IF.

     ‚* SETTINGS NICHT LESEN
           IF       MAP-KEY OF MAPDTA-REC(1:1) = "*"
                    GO TO LES-DAT-20
           END-IF.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-REC OF MAPDTA-REC(10:)  TO QSETSRC-WRK.

     ‚* KOMMENTARE IGNORIEREN
           IF       SRCDTA OF QSETSRC-WRK(7:1) = "*"
                    GO TO LES-DAT-20
           END-IF.

           INITIALIZE AUSGABE-WRK, INDIK-WRK.
     ‚* NUR FORMATANGABEN UND INDIKATOREN BEACHTEN
           IF       SRCDTA OF QSETSRC-WRK(17:1) = "R"
                    MOVE SRCDTA OF QSETSRC-WRK(19:10) TO AUSGABE-WRK
                    MOVE SRCDTA OF QSETSRC-WRK(19:19) TO ACTFMT-WRK
           END-IF.

     ‚* INDIKATOREN SICHERN
           MOVE     SRCDTA OF QSETSRC-WRK(8:9) TO INDIK-WRK.

           INITIALIZE FORMAT2.
     ‚* AUSGABE EINER SUBFILE-ZEILE
           IF       INDIK-WRK     = SPACES
            OR      INDIK-WRK(2:) = "*DS3"
            OR      INDIK-WRK(2:) = "*DS4"
                    IF   AUSGABE-WRK NOT = SPACES
                         MOVE    ACTFMT-WRK  TO SORTKZ OF FORMAT2
                         MOVE    AUSGABE-WRK TO INDIK  OF FORMAT2
                         MOVE    FORMAT2 TO BSSORT-REC
                         PERFORM RELEASE-SORTFILE
                    END-IF
           ELSE
     ‚*             INDIKATOREN EINZELND HINZUFÜGEN
                    IF   INDIK-WRK(2:2) NOT = SPACES
     ‚*                  SORTIERUNG FÜLLEN
                         MOVE    ACTFMT-WRK TO SORTKZ OF FORMAT2(1:10)
                         MOVE    "2"        TO SORTKZ OF FORMAT2(11:1)
                         MOVE    INDIK-WRK(2:2) TO
                                 SORTKZ OF FORMAT2(12:2)
     ‚*                  ANZEIGE FÜLLEN
                         MOVE    "IN"           TO INDIK OF FORMAT2(2:)
                         MOVE    INDIK-WRK(2:2) TO INDIK OF FORMAT2(4:)
                         MOVE    FORMAT2 TO BSSORT-REC
                         PERFORM RELEASE-SORTFILE
                    END-IF
                    IF   INDIK-WRK(5:2) NOT = SPACES
     ‚*                  SORTIERUNG FÜLLEN
                         MOVE    ACTFMT-WRK TO SORTKZ OF FORMAT2(1:10)
                         MOVE    "2"        TO SORTKZ OF FORMAT2(11:1)
                         MOVE    INDIK-WRK(5:2) TO
                                 SORTKZ OF FORMAT2(12:2)
     ‚*                  ANZEIGE FÜLLEN
                         MOVE    "IN"           TO INDIK OF FORMAT2(2:)
                         MOVE    INDIK-WRK(5:2) TO INDIK OF FORMAT2(4:)
                         MOVE    FORMAT2 TO BSSORT-REC
                         PERFORM RELEASE-SORTFILE
                    END-IF
                    IF   INDIK-WRK(8:2) NOT = SPACES
     ‚*                  SORTIERUNG FÜLLEN
                         MOVE    ACTFMT-WRK TO SORTKZ OF FORMAT2(1:10)
                         MOVE    "2"        TO SORTKZ OF FORMAT2(11:1)
                         MOVE    INDIK-WRK(8:2) TO
                                 SORTKZ OF FORMAT2(12:2)
     ‚*                  ANZEIGE FÜLLEN
                         MOVE    "IN"           TO INDIK OF FORMAT2(2:)
                         MOVE    INDIK-WRK(8:2) TO INDIK OF FORMAT2(4:)
                         MOVE    FORMAT2 TO BSSORT-REC
                         PERFORM RELEASE-SORTFILE
                    END-IF
           END-IF.

           INITIALIZE FORMAT2.
     ‚* FUNKTIONSTASTEN AUSGEBEN
           IF       (    SRCDTA OF QSETSRC-WRK(45:2) = "CA"
                     OR  SRCDTA OF QSETSRC-WRK(45:2) = "CF")
            AND     SRCDTA OF QSETSRC-WRK(49:1) = "("
     ‚*             SORTIERUNG FÜLLEN
                    IF   ACTFMT-WRK = SPACES
                         MOVE    SPACES     TO SORTKZ OF FORMAT2(1:10)
                         MOVE    "1"        TO SORTKZ OF FORMAT2(11:1)
                    ELSE
                         MOVE    ACTFMT-WRK TO SORTKZ OF FORMAT2(1:10)
                         MOVE    "1"        TO SORTKZ OF FORMAT2(11:1)
                    END-IF
                    MOVE SRCDTA OF QSETSRC-WRK(45:4) TO
                         SORTKZ OF FORMAT2(12:4)
     ‚*             DISPLAY FÜLLEN
                    MOVE    COL-BLU TO INDIK OF FORMAT2(1:1)
                    MOVE    SRCDTA OF QSETSRC-WRK(45:4) TO
                            INDIK OF FORMAT2(2:)
                    MOVE    FORMAT2 TO BSSORT-REC
                    PERFORM RELEASE-SORTFILE
           END-IF.

           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SORTFILEZEILE SCHREIBEN
     ‚*--------------------------------------------------------------
       RELEASE-SORTFILE SECTION.
       RLS-SRT-00.

     ‚* NUR INDIKATOREN ANZEIGEN !!
           IF       ONLY-INDIC = 1
                    IF   SORTKZ OF BSSORT-REC(11:1) = "2"
                         MOVE SPACES TO SORTKZ OF BSSORT-REC(1:10)
                         RELEASE  BSSORT-REC
                    END-IF
           ELSE
                    RELEASE  BSSORT-REC
           END-IF.

       RLS-SRT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DES SUBFILES INKL. GRUPPIERUNG
     ‚*--------------------------------------------------------------
       SUBFILE-SORT-OUTPUT SECTION.
       SUB-SOU-00.

           INITIALIZE FORMAT2, ANZREC-WRK.
           INITIALIZE LAST-AUSGABE.

     ‚* SORTFILE AUSLESEN
       SUB-SOU-20.
           RETURN   BSSORT-DAT AT END
                    GO TO SUB-SOU-90
           END-RETURN.

           IF       LAST-AUSGABE = INDIK OF BSSORT-REC
                    GO TO SUB-SOU-20
           END-IF.

           MOVE     INDIK OF BSSORT-REC TO LAST-AUSGABE.

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
           MOVE     BSSORT-REC   TO FORMAT2.

     ‚* JE NACH TYP PASSENDEN TEXT ANZEIGEN
           EVALUATE SORTKZ OF FORMAT2(11:1)
                    WHEN SPACES
                         MOVE     "PGM0031"  TO CPY-TXID
                    WHEN "1"
                         IF   SORTKZ OF FORMAT2(1:10) = SPACES
                              MOVE "PGM0033" TO CPY-TXID
                         ELSE
                              MOVE "PGM0034" TO CPY-TXID
                         END-IF
                    WHEN "2"
                         MOVE     "PGM0032"  TO CPY-TXID
                    WHEN OTHER
                         MOVE     "PGM0035"  TO CPY-TXID
           END-EVALUATE.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO EXPLAIN OF FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
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
     ‚* AUSGABE DER SFL-ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-SFL SECTION.
       AUS-SFL-00.

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
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

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
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F14 ABBRECHEN
           IF       IN14 IN FORMAT3-I-INDIC = AN
                    EVALUATE ONLY-INDIC
                             WHEN ZEROES MOVE 1      TO ONLY-INDIC
                             WHEN 1      MOVE ZEROES TO ONLY-INDIC
                    END-EVALUATE
                    MOVE "1" TO F5
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

     ‚* AUSGABE INFOZEILE DASS DISPLAY LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
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
           MOVE     "XXP0014"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0014*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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
