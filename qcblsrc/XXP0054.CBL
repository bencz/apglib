       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0054.
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
                  ASSIGN       TO  WORKSTATION-XXD0054DE
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
           COPY DDS-ALL-FORMATS OF XXD0054DE.
      /
     ‚*- TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
       SD  BSSORT-DAT.
       01  BSSORT-REC.
           COPY DDS-FMT02-O OF XXD0054DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN40                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0054DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0054DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0054DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0054DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP0054".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST   ".

       01  LN                        LIKE COBCDE OF FORMAT2.
       01  ACTION-WRK                     PIC X(20).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-ACTION                       PIC X(20).
       01  X-KEYVAL                       PIC X(10).
       01  X-KEYCHK                       PIC 9(1).
       01  X-KEYLEN                       PIC 9(2).
       01  X-OPTVAL                       PIC X(10).
       01  X-OPTCHK                       PIC 9(1).
       01  X-OPTLEN                       PIC 9(2).
       01  X-F12                     LIKE F12.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-ACTION
                                X-KEYVAL
                                X-KEYCHK
                                X-KEYLEN
                                X-OPTVAL
                                X-OPTCHK
                                X-OPTLEN
                                X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0054" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0054DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           INITIALIZE FORMAT3.
           INITIALIZE F3, F12.
           MOVE     1        TO SFL-ZEILE.
           MOVE     AUS      TO INXX.
           MOVE     X-ACTION TO ACTION-WRK.

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
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           MOVE     AUS    TO INXX.

       ANZ-SFL-10.
           PERFORM  WRITE-SFL-LINES.

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

     ‚*€   MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
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

     ‚* F20 ANSICHT WECHSELN
           IF       IN20 IN FORMAT3-I-INDIC = AN
                    MOVE     1 TO SFL-ZEILE
                    EVALUATE ACTION-WRK
                        WHEN "PLAIN"    MOVE "DBMAIN"   TO ACTION-WRK
                        WHEN "DBMAIN"   MOVE "EXTENDED" TO ACTION-WRK
                        WHEN "EXTENDED" MOVE "PLAIN"    TO ACTION-WRK
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
           MOVE     "XXP0054"         TO MAINUEB OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN DER ZEILE JE NACH ANFORDERUNG
     ‚*--------------------------------------------------------------
       WRITE-SFL-LINES SECTION.
       WRT-SFL-00.

           INITIALIZE DATEN.
           INITIALIZE ANZREC-WRK.

           EVALUATE ACTION-WRK
               WHEN "PLAIN"     PERFORM WRITE-SFL-PLAIN
               WHEN "DBMAIN"    PERFORM WRITE-SFL-DBMAIN
               WHEN "EXTENDED"  PERFORM WRITE-SFL-EXTENDED
           END-EVALUATE.

       WRT-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN FÜR NORMAL COBOL
     ‚*--------------------------------------------------------------
       WRITE-SFL-PLAIN SECTION.
       WRK-SFP-00.

     ‚* INITIALIZE
           INITIALIZE LN.
           STRING   "     INITIALIZE PTESTDBF OF PTESTDB-RECP."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* MOVE "<KEYVAL>" TO KEYFLD OF PTESTDB-RECP
           INITIALIZE LN.
           IF       X-KEYLEN = ZEROES
            OR      X-KEYLEN = LENGTH OF X-KEYVAL
     ‚*             OHNE SUBSTRING
                    STRING   "     MOVE     " '"' X-KEYVAL
                             '" ' "TO KEYFLD OF PTESTDB-RECP."
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
           ELSE
     ‚*             MIT SUBSTRING
                    STRING   "     MOVE     " '"' X-KEYVAL '" '
                             "TO KEYFLD OF PTESTDB-RECP"
                             "(1:" X-KEYLEN ")."
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
           END-IF.
           PERFORM  WRITE-SFL-LINE.

     ‚* START FILENAME
           INITIALIZE LN.
           STRING   "     START    PTESTDB-DATP KEY NOT < "
                    "EXTERNALLY-DESCRIBED-KEY."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* FILE-STATUS
           PERFORM  WRITE-SFL-FILESTATUS.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* -20 LABEL
           MOVE     " STR-NXT-20." TO LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* READ NEXT RECORD
           INITIALIZE LN.
           STRING   "     READ     PTESTDB-DATP NEXT RECORD."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* FILE-STATUS
           PERFORM  WRITE-SFL-FILESTATUS.

     ‚* KEYCHECK
           IF       X-KEYCHK = 1
     ‚*             <EMPTY LINE>
                    INITIALIZE LN
                    PERFORM  WRITE-SFL-LINE
     ‚*             ABFRAGE DES KEYS
                    INITIALIZE LN
                    IF   X-KEYLEN = ZEROES
                     OR  X-KEYLEN = LENGTH OF X-KEYVAL
     ‚*                  OHNE SUBSTRING
                         STRING  "     IF       KEYFLD OF PTESTDB-RECP"
                                 " NOT = " '"' X-KEYVAL '"'
                                 DELIMITED BY SIZE
                           INTO  LN
                         END-STRING
                    ELSE
     ‚*                  MIT SUBSTRING
                         STRING  "     IF       KEYFLD OF PTESTDB-RECP"
                                 "(1:" X-KEYLEN ") NOT = "
                                 '"' X-KEYVAL '"'
                                 DELIMITED BY SIZE
                           INTO  LN
                         END-STRING
                    END-IF
                    PERFORM  WRITE-SFL-LINE
     ‚*             GO TO
                    INITIALIZE LN
                    STRING   "              GO TO STR-NXT-90"
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
                    PERFORM  WRITE-SFL-LINE
     ‚*             END-IF.
                    INITIALIZE LN
                    STRING   "     END-IF."
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
                    PERFORM  WRITE-SFL-LINE
           END-IF.

     ‚* OPTIONALER CHECK
           IF       X-OPTCHK = 1
     ‚*             <EMPTY LINE>
                    INITIALIZE LN
                    PERFORM  WRITE-SFL-LINE
     ‚*             ABFRAGE DES KEYS
                    INITIALIZE LN
                    IF   X-OPTLEN = ZEROES
                     OR  X-OPTLEN = LENGTH OF X-OPTVAL
     ‚*                  OHNE SUBSTRING
                         STRING  "     IF       OPTFLD OF PTESTDB-RECP"
                                 " NOT = " '"' X-OPTVAL '"'
                                 DELIMITED BY SIZE
                           INTO  LN
                         END-STRING
                    ELSE
     ‚*                  MIT SUBSTRING
                         STRING  "     IF       OPTFLD OF PTESTDB-RECP"
                                 "(1:" X-OPTLEN ") NOT = "
                                 '"' X-OPTVAL '"'
                                 DELIMITED BY SIZE
                           INTO  LN
                         END-STRING
                    END-IF
                    PERFORM  WRITE-SFL-LINE
     ‚*             GO TO
                    INITIALIZE LN
                    STRING   "              GO TO STR-NXT-20"
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
                    PERFORM  WRITE-SFL-LINE
     ‚*             END-IF.
                    INITIALIZE LN
                    STRING   "     END-IF."
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
                    PERFORM  WRITE-SFL-LINE
           END-IF.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* <DO SOMETHING>
           MOVE     "     <DO SOMETHING>" TO LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* GO TO -20
           INITIALIZE LN.
           STRING   "     GO TO    STR-NXT-20."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

       WRT-SFP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* START/READ-NEXT MIT DBMAIN
     ‚*--------------------------------------------------------------
       WRITE-SFL-DBMAIN SECTION.
       WRT-SFD-00.

     ‚* INITIALIZE
           INITIALIZE LN.
           STRING   "     INITIALIZE PTESTDBF OF PTESTDB-P."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* MOVE "<KEYVAL>" TO KEYFLD OF PTESTDB-RECP
           INITIALIZE LN.
           IF       X-KEYLEN = ZEROES
            OR      X-KEYLEN = LENGTH OF X-KEYVAL
     ‚*             OHNE SUBSTRING
                    STRING   "     MOVE     " '"' X-KEYVAL
                             '" ' "TO KEYFLD OF PTESTDB-P."
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
           ELSE
     ‚*             MIT SUBSTRING
                    STRING   "     MOVE     " '"' X-KEYVAL '" '
                             "TO KEYFLD OF PTESTDB-P"
                             "(1:" X-KEYLEN ")."
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
           END-IF.
           PERFORM  WRITE-SFL-LINE.

     ‚* START FILENAME
           INITIALIZE LN.
           STRING   "     SET      DB-PTR TO PTESTDB-DP."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

           INITIALIZE LN.
           STRING   "     PERFORM  DB-START-NL."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* FILE-STATUS
           PERFORM  WRITE-SFL-FILESTATUS.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* -20 LABEL
           MOVE     " STR-NXT-20." TO LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* READ NEXT RECORD
           INITIALIZE LN.
           STRING   "     SET      DB-PTR TO PTESTDB-DP."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.
           INITIALIZE LN.
           STRING   "     PERFORM  DB-READ-NEXT-NL."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* FILE-STATUS
           PERFORM  WRITE-SFL-FILESTATUS.

     ‚* KEYCHECK
           IF       X-KEYCHK = 1
     ‚*             <EMPTY LINE>
                    INITIALIZE LN
                    PERFORM  WRITE-SFL-LINE
     ‚*             ABFRAGE DES KEYS
                    INITIALIZE LN
                    IF   X-KEYLEN = ZEROES
                     OR  X-KEYLEN = LENGTH OF X-KEYVAL
     ‚*                  OHNE SUBSTRING
                         STRING  "     IF       KEYFLD OF PTESTDB-P"
                                 " NOT = " '"' X-KEYVAL '"'
                                 DELIMITED BY SIZE
                           INTO  LN
                         END-STRING
                    ELSE
     ‚*                  MIT SUBSTRING
                         STRING  "     IF       KEYFLD OF PTESTDB-P"
                                 "(1:" X-KEYLEN ") NOT = "
                                 '"' X-KEYVAL '"'
                                 DELIMITED BY SIZE
                           INTO  LN
                         END-STRING
                    END-IF
                    PERFORM  WRITE-SFL-LINE
     ‚*             GO TO
                    INITIALIZE LN
                    STRING   "              GO TO STR-NXT-90"
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
                    PERFORM  WRITE-SFL-LINE
     ‚*             END-IF.
                    INITIALIZE LN
                    STRING   "     END-IF."
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
                    PERFORM  WRITE-SFL-LINE
           END-IF.

     ‚* OPTIONALER CHECK
           IF       X-OPTCHK = 1
     ‚*             <EMPTY LINE>
                    INITIALIZE LN
                    PERFORM  WRITE-SFL-LINE
     ‚*             ABFRAGE DES KEYS
                    INITIALIZE LN
                    IF   X-OPTLEN = ZEROES
                     OR  X-OPTLEN = LENGTH OF X-OPTVAL
     ‚*                  OHNE SUBSTRING
                         STRING  "     IF       OPTFLD OF PTESTDB-P"
                                 " NOT = " '"' X-OPTVAL '"'
                                 DELIMITED BY SIZE
                           INTO  LN
                         END-STRING
                    ELSE
     ‚*                  MIT SUBSTRING
                         STRING  "     IF       OPTFLD OF PTESTDB-P"
                                 "(1:" X-OPTLEN ") NOT = "
                                 '"' X-OPTVAL '"'
                                 DELIMITED BY SIZE
                           INTO  LN
                         END-STRING
                    END-IF
                    PERFORM  WRITE-SFL-LINE
     ‚*             GO TO
                    INITIALIZE LN
                    STRING   "              GO TO STR-NXT-20"
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
                    PERFORM  WRITE-SFL-LINE
     ‚*             END-IF.
                    INITIALIZE LN
                    STRING   "     END-IF."
                             DELIMITED BY SIZE
                      INTO   LN
                    END-STRING
                    PERFORM  WRITE-SFL-LINE
           END-IF.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* <DO SOMETHING>
           MOVE     "     <DO SOMETHING>" TO LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* GO TO -20
           INITIALIZE LN.
           STRING   "     GO TO    STR-NXT-20."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

       WRT-SFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* START/READ-NEXT IN EXTENDED
     ‚*--------------------------------------------------------------
       WRITE-SFL-EXTENDED SECTION.
       WRT-SFE-00.

     ‚* INITIALIZE
           INITIALIZE LN.
           STRING   "*DB  DB-START PTESTDB-DP"
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* MOVE "<KEYVAL>" TO KEYFLD OF PTESTDB-RECP
           INITIALIZE LN.
           IF       X-KEYCHK = 1
                    IF   X-KEYLEN = ZEROES
                     OR  X-KEYLEN = LENGTH OF X-KEYVAL
     ‚*                  OHNE SUBSTRING
                         STRING   "*DB  K        KEYFLD = "
                                  '"' X-KEYVAL '" '
                                  DELIMITED BY SIZE
                           INTO   LN
                         END-STRING
                    ELSE
     ‚*                  MIT SUBSTRING
                         STRING   "*DB  K        KEYFLD"
                                  "(1:" X-KEYLEN ") "
                                  '= "' X-KEYVAL '" '
                                  DELIMITED BY SIZE
                           INTO   LN
                         END-STRING
                    END-IF
                    PERFORM  WRITE-SFL-LINE
           ELSE
                    IF   X-KEYLEN = ZEROES
                     OR  X-KEYLEN = LENGTH OF X-KEYVAL
     ‚*                  OHNE SUBSTRING
                         STRING   "*DB  K        KEYFLD >= "
                                  '"' X-KEYVAL '" '
                                  DELIMITED BY SIZE
                           INTO   LN
                         END-STRING
                    ELSE
     ‚*                  MIT SUBSTRING
                         STRING   "*DB  K        KEYFLD"
                                  "(1:" X-KEYLEN ") >= "
                                  '"' X-KEYVAL '" '
                                  DELIMITED BY SIZE
                           INTO   LN
                         END-STRING
                    END-IF
                    PERFORM  WRITE-SFL-LINE
           END-IF.

     ‚* OPTIONALER CHECK
           INITIALIZE LN.
           IF       X-OPTCHK = 1
                    IF   X-OPTLEN = ZEROES
                     OR  X-OPTLEN = LENGTH OF X-OPTVAL
     ‚*                  OHNE SUBSTRING
                         STRING   "*DB  S        OPTFLD = "
                                  '"' X-OPTVAL '" '
                                  DELIMITED BY SIZE
                           INTO   LN
                         END-STRING
                    ELSE
     ‚*                  MIT SUBSTRING
                         STRING   "*DB  S        OPTFLD"
                                  "(1:" X-OPTLEN ") = "
                                  '"' X-OPTVAL '" '
                                  DELIMITED BY SIZE
                           INTO   LN
                         END-STRING
                    END-IF
                    PERFORM  WRITE-SFL-LINE
           END-IF.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* -20 LABEL
           MOVE     " STR-NXT-20." TO LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* READ NEXT RECORD
           INITIALIZE LN.
           STRING   "     SET      DB-PTR TO PTESTDB-DP."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

           INITIALIZE LN.
           STRING   "     PERFORM  DB-READ-NEXT-NL."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* FILE-STATUS
           PERFORM  WRITE-SFL-FILESTATUS.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* <DO SOMETHING>
           MOVE     "     <DO SOMETHING>" TO LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* <EMPTY LINE>
           INITIALIZE LN.
           PERFORM  WRITE-SFL-LINE.

     ‚* GO TO -20
           INITIALIZE LN.
           STRING   "     GO TO    STR-NXT-20."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

       WRT-SFE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FILE-STATUS BLOCK SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-SFL-FILESTATUS SECTION.
       WRT-SFS-00.

     ‚* IF FILE-STATUS NOT = ZEROES
           INITIALIZE LN.
           STRING   "     IF       FILE-STATUS NOT = ZEROES"
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* GO TO STR-NXT-90
           INITIALIZE LN.
           STRING   "              GO TO STR-NXT-90"
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

     ‚* END-IF.
           INITIALIZE LN.
           STRING   "     END-IF."
                    DELIMITED BY SIZE
             INTO   LN
           END-STRING.
           PERFORM  WRITE-SFL-LINE.

       WRT-SFS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEZEILEN AUSGEBEN
     ‚*--------------------------------------------------------------
       WRITE-SFL-LINE SECTION.
       WRT-SFL-00.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
     ‚*€            MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

           INITIALIZE FORMAT2.
           MOVE     LN TO COBCDE OF FORMAT2.

           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

       WRT-SFL-90.
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
