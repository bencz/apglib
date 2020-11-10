       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0052.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* COBOL START/READ-NEXT EXAMPLE                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.05.2013                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  CODE-EXAMPLE FÜR START/READ-NEXT             *
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
                  ASSIGN       TO  WORKSTATION-XXD0052DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚*-   TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
     ‚*-------------------------------------------------------------
           SELECT BSSORT-DAT
                  ASSIGN       TO  DISK-SORT.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF XXD0052DE.
      /
     ‚*- TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
       SD  BSSORT-DAT.
       01  BSSORT-REC.
           COPY DDS-FMT02-O OF XXD0052DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN40                          PIC 1.
        05 IN41                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0052DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0052DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0052DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0052DE
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

       01  PGM-WRK                        PIC X(10) VALUE "XXP0052".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

       01  AUSWAHL                        PIC X(2).
       01  ACTION-WRK                     PIC X(20).
       01  KEYVAL-WRK                LIKE KEYVAL OF FORMAT2.
       01  OPTVAL-WRK                LIKE OPTVAL OF FORMAT2.
       01  MAP-LFDN-WRK              LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* SCHALTER
       01  IN-LIST                        PIC 9(1).
       01  IN-READ                        PIC 9(1).
       01  KEY-CHECK                      PIC 9(1).
       01  KEYOPT-MODE                    PIC 9(1).
       01  KEYCHK-SAV                     PIC 9(1).
       01  OPTCHK-SAV                     PIC 9(1).
       01  SHOW-DEFAULT                   PIC 9(1).
       01  DEFVAL-WRK                     PIC 9(1).

     ‚* LÄNGENVARIABLEN
       01  KLEN                      LIKE KEYLEN OF FORMAT3.
       01  OLEN                      LIKE OPTLEN OF FORMAT3.

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0052" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0052DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           INITIALIZE FORMAT3.
           INITIALIZE F3, F12.
           INITIALIZE KEYOPT-MODE.
           MOVE     1 TO SFL-ZEILE.
           MOVE     1 TO SHOW-DEFAULT.
           MOVE     AUS TO INXX.
           SET      PTR-MAIN OF PTRMAP-REC TO NULL.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
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
                    GO TO ENDE
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
           SET      PTR-CLEAR OF PTRMAP-REC TO TRUE.
           PERFORM  COPY-PTR-MAPPING.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE ZEILEN SCHREIBEN
     ‚*--------------------------------------------------------------
       SUBFILE-SORT-INPUT SECTION.
       SFL-INP-00.

           INITIALIZE KEY-CHECK.
           INITIALIZE DATEN, ANZREC-WRK.
           MOVE     1                    TO DEFVAL-WRK.

     ‚* WERTE INS SFL ÜBERNEHMEN
           INITIALIZE BSSORT-REC.
           MOVE     "ABC-123"            TO KEYVAL-WRK.
           MOVE     "TX-5"               TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "AXZ-556"            TO KEYVAL-WRK.
           MOVE     "TX-3"               TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "AXZ-557"            TO KEYVAL-WRK.
           MOVE     "SR-2"               TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "AZZ-123"            TO KEYVAL-WRK.
           MOVE     "TR-7"               TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "AZZZ-12"            TO KEYVAL-WRK.
           MOVE     SPACES               TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "B00-ALPHA"          TO KEYVAL-WRK.
           MOVE     "JJ-10"              TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "B00-ALPHA"          TO KEYVAL-WRK.
           MOVE     "JJ-11"              TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "B00-ALPHA"          TO KEYVAL-WRK.
           MOVE     "JJ-22"              TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "DHL-1"              TO KEYVAL-WRK.
           MOVE     "TOUR"               TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "DHL-1"              TO KEYVAL-WRK.
           MOVE     "ROUTE"              TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "DHL-2"              TO KEYVAL-WRK.
           MOVE     "ROUTE"              TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           MOVE     "DHL-3"              TO KEYVAL-WRK.
           MOVE     "ROUTE"              TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

     ‚* NUN ALLE WERTE AUS DER INTERNEN TABELLE BEARBEITEN
           MOVE     ZEROES               TO DEFVAL-WRK.
           PERFORM  SUBFILE-FROM-MAPDTA.

       SFL-INP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE SELBST ERSTELLTEN DATEN EINTRAGEN
     ‚*--------------------------------------------------------------
       SUBFILE-FROM-MAPDTA SECTION.
       SFL-FMD-00.

           SET      PTR-READ   TO TRUE.
           MOVE     "VALUES"   TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           MOVE     -1         TO MAP-LFDN-WRK.

       SFL-FMD-20.
           ADD      1          TO MAP-LFDN-WRK.
           MOVE     MAP-LFDN-WRK  TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO SFL-FMD-90
           END-IF.

           MOVE     MAP-KEY OF MAPDTA-REC TO KEYVAL-WRK.
           MOVE     MAP-REC OF MAPDTA-REC TO OPTVAL-WRK.
           PERFORM  SUBFILE-SORT-WRITE.

           GO TO    SFL-FMD-20.

       SFL-FMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN DER SORT-FILE ZEILE
     ‚*--------------------------------------------------------------
       SUBFILE-SORT-WRITE SECTION.
       SFL-WRT-00.

     ‚* DEFAULTWERTE AUSBLENDEN?
           IF       SHOW-DEFAULT = ZEROES
            AND     DEFVAL-WRK   = 1
                    GO TO SFL-WRT-90
           END-IF.

           EVALUATE KEYOPT-MODE
               WHEN ZEROES
                    MOVE KEYVAL-WRK TO KEYVAL OF BSSORT-REC
                    MOVE OPTVAL-WRK TO OPTVAL OF BSSORT-REC
                    RELEASE BSSORT-REC
               WHEN 1
                    MOVE KEYVAL-WRK TO OPTVAL OF BSSORT-REC
                    MOVE OPTVAL-WRK TO KEYVAL OF BSSORT-REC
                    RELEASE BSSORT-REC
           END-EVALUATE.

       SFL-WRT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEZEILEN AUSGEBEN
     ‚*--------------------------------------------------------------
       SUBFILE-SORT-OUTPUT SECTION.
       SUB-OUT-00.

           RETURN   BSSORT-DAT AT END
                    GO TO SUB-OUT-90
           END-RETURN.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
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
           MOVE     BSSORT-REC TO FORMAT2.
     ‚* MÜSSTE DER SATZ GELESEN WERDEN?
           PERFORM  CHECK-IN-READ.
           IF       IN-READ = 1
                    MOVE AUS TO IN41 OF INXX
           ELSE
                    MOVE AN  TO IN41 OF INXX
           END-IF.
     ‚* MÜSSTE DER SATZ VERARBEITET WERDEN?
           PERFORM  CHECK-IN-LIST.
           IF       IN-LIST = 1
                    MOVE AN  TO IN40 OF INXX
           ELSE
                    MOVE AUS TO IN40 OF INXX
           END-IF.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

           GO TO    SUB-OUT-00.

       SUB-OUT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           MOVE     AUS    TO INXX.

       ANZ-SFL-10.
           SORT     BSSORT-DAT ON ASCENDING KEY KEYVAL OF BSSORT-REC
                    WITH DUPLICATES
                    INPUT  PROCEDURE SUBFILE-SORT-INPUT
                    OUTPUT PROCEDURE SUBFILE-SORT-OUTPUT.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

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
           IF       DATEN = ZEROES
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT05" TO FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE     AUS    TO IN91  IN  INXX
           ELSE
                    MOVE     AN     TO IN91  IN  INXX
           END-IF.
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

     ‚* F8 EIGENE WERTE DER TABELLE HINZUFÜGEN
           IF       IN08 IN FORMAT3-I-INDIC = AN
                    CALL "XXP0053" USING PTR-MAIN OF PTRMAP-REC
                                         F12
                    END-CALL
                    INITIALIZE F12
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F20 CODE IN EINEM WINDOW ANZEIGEN
           IF       IN20 IN FORMAT3-I-INDIC = AN
                    EVALUATE CDEOUT OF FORMAT3
                        WHEN ZEROES MOVE "PLAIN"    TO ACTION-WRK
                        WHEN 1      MOVE "DBMAIN"   TO ACTION-WRK
                        WHEN 2      MOVE "EXTENDED" TO ACTION-WRK
                    END-EVALUATE
                    CALL "XXP0054" USING ACTION-WRK
                                         KEYVAL OF FORMAT3
                                         KEYCHK OF FORMAT3
                                         KEYLEN OF FORMAT3
                                         OPTVAL OF FORMAT3
                                         OPTCHK OF FORMAT3
                                         OPTLEN OF FORMAT3
                                         F12
                    END-CALL
                    INITIALIZE F12
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* KEY<->OPT WERTE TAUSCHEN
           IF       IN21 IN FORMAT3-I-INDIC = AN
     ‚*             ALLE EINGABEN IN FMT03 TAUSCHEN
     ‚*
     ‚*             STRINGEINGABE
                    MOVE KEYVAL OF FORMAT3 TO KEYVAL-WRK
                    MOVE OPTVAL OF FORMAT3 TO OPTVAL-WRK
                    MOVE OPTVAL-WRK        TO KEYVAL OF FORMAT3
                    MOVE KEYVAL-WRK        TO OPTVAL OF FORMAT3
     ‚*             LÄNGE
                    MOVE KEYLEN OF FORMAT3 TO KLEN
                    MOVE OPTLEN OF FORMAT3 TO OLEN
                    MOVE KLEN              TO OPTLEN OF FORMAT3
                    MOVE OLEN              TO KEYLEN OF FORMAT3
     ‚*             ABFRAGE JA/NEIN
                    MOVE KEYCHK OF FORMAT3 TO KEYCHK-SAV
                    MOVE OPTCHK OF FORMAT3 TO OPTCHK-SAV
                    MOVE KEYCHK-SAV        TO OPTCHK OF FORMAT3
                    MOVE OPTCHK-SAV        TO KEYCHK OF FORMAT3
     ‚*             JETZT DEN MODUS TAUSCHEN
                    EVALUATE KEYOPT-MODE
                        WHEN ZEROES MOVE 1          TO KEYOPT-MODE
                        WHEN 1      MOVE ZEROES     TO KEYOPT-MODE
                    END-EVALUATE
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* STANDARDWERTE AUSBLENDEN
           IF       IN24 OF FORMAT3-I-INDIC = AN
                    EVALUATE SHOW-DEFAULT
                        WHEN ZEROES MOVE 1      TO SHOW-DEFAULT
                        WHEN 1      MOVE ZEROES TO SHOW-DEFAULT
                    END-EVALUATE
                    GO TO ANZ-SFL-10
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
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0052"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0052*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EINTRAG IN DER LISTE WÄRE
     ‚*--------------------------------------------------------------
       CHECK-IN-LIST SECTION.
       CHK-INL-00.

           INITIALIZE IN-LIST.

     ‚* WENN DIE KEYABFRAGE EINMAL FEHLGESCHLAGEN IST, IST ALLES DANACH
     ‚* NICHT MEHR IN DER LISTE
           IF       KEY-CHECK = 1
                    GO TO CHK-INL-90
           END-IF.

     ‚* LÄNGEN ÜBERNEHMEN
           MOVE     KEYLEN OF FORMAT3 TO KLEN.
           MOVE     OPTLEN OF FORMAT3 TO OLEN.
           IF       KLEN = ZEROES
            OR      KLEN > LENGTH OF KEYVAL OF FORMAT3
                    MOVE LENGTH OF KEYVAL OF FORMAT3 TO KLEN
           END-IF.
           IF       OLEN = ZEROES
            OR      OLEN > LENGTH OF OPTVAL OF FORMAT3
                    MOVE LENGTH OF OPTVAL OF FORMAT3 TO OLEN
           END-IF.

     ‚* IST DER WERT NOCH VOR DEM STARTWERT?
           IF       KEYVAL OF FORMAT3 NOT = SPACES
            AND     KEYVAL OF FORMAT2     < KEYVAL OF FORMAT3
                    GO TO CHK-INL-90
           END-IF.

     ‚* SOLL EINE KEYABFRAGE DURCHGEFÜHRT WERDEN?
           IF       KEYCHK OF FORMAT3             = 1
            AND     KEYVAL OF FORMAT3(1:KLEN) NOT =
                    KEYVAL OF FORMAT2(1:KLEN)
                    MOVE 1 TO KEY-CHECK
                    GO TO CHK-INL-90
           END-IF.

     ‚* SOLL EINE OPTINALE ABFRAGE DURCHGEFÜHRT WERDEN?
           IF       OPTCHK OF FORMAT3             = 1
            AND     OPTVAL OF FORMAT3(1:OLEN) NOT =
                    OPTVAL OF FORMAT2(1:OLEN)
                    GO TO CHK-INL-90
           END-IF.

     ‚* WERT KANN ANGEZEIGT WERDEN
           MOVE     1 TO IN-LIST.

       CHK-INL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EINTRAG GELESEN WIRD
     ‚*--------------------------------------------------------------
       CHECK-IN-READ SECTION.
       CHK-INR-00.

           MOVE     1 TO IN-READ.

     ‚* WENN DIE KEYABFRAGE EINMAL FEHLGESCHLAGEN IST, IST ALLES DANACH
     ‚* NICHT MEHR IN DER LISTE
           IF       KEY-CHECK = 1
                    INITIALIZE IN-READ
                    GO TO CHK-INR-90
           END-IF.

     ‚* IST DER WERT NOCH VOR DEM STARTWERT?
           IF       KEYVAL OF FORMAT3 NOT = SPACES
            AND     KEYVAL OF FORMAT2     < KEYVAL OF FORMAT3
                    INITIALIZE IN-READ
                    GO TO CHK-INR-90
           END-IF.

       CHK-INR-90.
           EXIT.
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
