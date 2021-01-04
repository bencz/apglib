       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DSPMAIN                                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DSPMAIN FÜR ALLE PROGRAMME                   *
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
                  ASSIGN       TO  WORKSTATION-CFD0000DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI PCFGFFD NACH LIB/FILE
     ‚*-------------------------------------------------------------
           SELECT PCFGFFD-DL
                  ASSIGN       TO  DATABASE-PCFGFFDL1
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
       01  WS-REC                        PIC X(4096).
      /
     ‚*--- PCFGFFD NACH LIB/FILE
       FD  PCFGFFD-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-L.
           COPY DDS-ALL-FORMATS OF PCFGFFDL1.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0000".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  LEN                           PIC 9(4).
       01  POS                           PIC 9(4).

     ‚* SCHALTER
       01  AUFBEREITUNG-ZEIT-AKTIV       PIC 9(1)  VALUE 1.
       01  AUFBEREITUNG-DATUM-AKTIV      PIC 9(1)  VALUE 1.
       01  AUFBEREITUNG-TEXTE-AKTIV      PIC 9(1)  VALUE 1.
       01  AUFBEREITUNG-HEAD-AKTIV       PIC 9(1)  VALUE 1.
       01  EXT-INFO-SETTING              PIC 9(1)  VALUE ZEROES.

       01  WHFLDE-WRK                    LIKE WHFLDE OF PCFGFFD-L.

     ‚* PARAMETER FÜR DAS CFP0024
       01  LIB-WRK                       PIC X(10).
       01  RCN-WRK                       PIC X(10).
       01  0024-AKTION                   PIC X(20).
       01  SYS-CMP-TEXT                  PIC X(21).
       01  SYS-CMP-BEZ                   PIC X(21).

     ‚* PARAMETER FÜR DAS CLP0064
       01  DLY-JOB                       PIC X(4).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-WSREC                       LIKE WSREC-WRK.
       01  X-FMT                         LIKE FMT-WRK.
       01  X-FUNCTION                    PIC X(1).
       01  X-DISPLAY                     LIKE DISPLAY-WRK.
       01  X-PGM                         LIKE PGM-WRK.
       01  X-PGM-TYP                     LIKE PGM-TYP.
       01  X-SPRACHE                     LIKE SPRACHE-WRK.
       01  X-COMPILE-DATE                LIKE SYS-COMPIL.
       01  X-SFLNR                       LIKE SUBFILE-RECORD-NUMBER.
       01  X-BSSTAT                      LIKE BS-STAT.
       01  X-BS-ATR                      LIKE BS-ATR.
       01  X-BS-IO-AREA                  LIKE BS-IO-AREA.
       01  X-DSP-ATTR                    LIKE DSP-ATTR.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-WSREC
                                X-FMT
                                X-FUNCTION
                                X-DISPLAY
                                X-PGM
                                X-PGM-TYP
                                X-SPRACHE
                                X-COMPILE-DATE
                                X-SFLNR
                                X-BSSTAT
                                X-BS-ATR
                                X-BS-IO-AREA
                                X-DSP-ATTR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0000"   TO PGM-WRK.
           MOVE     "CFD0000DE" TO DISPLAY-WRK.

     ‚* FORMAT ÜBERNEHMEN
           MOVE     X-FMT     TO FMT-WRK.
           MOVE     X-DISPLAY TO DISPLAY-WRK.
           MOVE     X-SPRACHE TO SPRACHE-WRK.
           IF       SPRACHE-WRK = SPACES
                    MOVE "DEU" TO SPRACHE-WRK
           END-IF.

     ‚* DATEI ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     INPUT PCFGFFD-DL
     ‚*             ERMITTELN JOB-INFORMATIONEN
                    MOVE     "INIT" TO COPY-AKTION
                    CALL     PROCEDURE "CFP9000" USING COPY-AKTION
                                                       JOBINFO
                                                       PGM-WRK
                    END-CALL
     ‚*             ERMITTELN PGM-ZUSATZDATEN
                    PERFORM  GET-PGMDATA
           END-IF.

     ‚* ATTRIBUTE ÜBERNEHMEN
           MOVE     X-DSP-ATTR TO DSP-ATTR.

     ‚* CURSER AUF EIN FELD SETZEN?
           IF       X-FUNCTION = DSP-WRITE
            OR      X-FUNCTION = DSP-SFLWRITE
                    IF   DSP-CUR-FLD OF DSP-ATTR NOT = SPACES
     ‚*                  FELD ÜBERNEHMEN UND ERMITTELN
                         MOVE DSP-CUR-FLD OF DSP-ATTR TO WHFLDE-WRK
                         PERFORM GET-XMLFFD
     ‚*                  DANN ALS X UND Y ÜBERNEHMEN
                         IF   WHDROW OF PCFGFFD-L NOT = ZEROES
                          AND WHDCOL OF PCFGFFD-L NOT = ZEROES
                              MOVE WHDCOL OF PCFGFFD-L TO
                                   DSP-CURX OF DSP-ATTR
                              MOVE WHDROW OF PCFGFFD-L TO
                                   DSP-CURY OF DSP-ATTR
                         END-IF
                    END-IF
           END-IF.

           EVALUATE X-FUNCTION
     ‚*        ÖFFNEN DES DISPLAYS
               WHEN DSP-OPEN
                    PERFORM DSP-FNC-OPEN
     ‚*        SCHLIEßEN DES DISPLAYS
               WHEN DSP-CLOSE
                    PERFORM DSP-FNC-CLOSE
     ‚*        SCHREIBEN DES DISPLAYS
               WHEN DSP-WRITE
                    PERFORM DSP-FNC-WRITE
     ‚*        SCHREIBEN DES SUBFILES
               WHEN DSP-SFLWRITE
                    PERFORM DSP-FNC-SFLWRITE
     ‚*        ZURÜCKSCHREIBEN DES SUBFILES
               WHEN DSP-SFLREWRITE
                    PERFORM DSP-FNC-SFLREWRITE
     ‚*        LESEN DES DISPLAYS
               WHEN DSP-READ
                    PERFORM DSP-FNC-READ
     ‚*        LESEN DES DISPLAYS OHNE DATEN (AUTO-REFRESH)
               WHEN DSP-READ-NDTA
                    PERFORM DSP-FNC-READ-NDTA
     ‚*        LESEN DES SUBFILES
               WHEN DSP-SFLREAD
                    PERFORM DSP-FNC-SFLREAD
               WHEN DSP-SFLREADREC
                    PERFORM DSP-FNC-SFLREADREC
     ‚*        DISPLAY-CURSER AUSFÜHREN
               WHEN DSP-CURSER
                    PERFORM DSP-FNC-CURSER
           END-EVALUATE.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* STARTEN DES OVERWRITE UND ÖFFNEN DES DISPLAYS
     ‚*--------------------------------------------------------------
       DSP-FNC-OPEN SECTION.
       DSP-OPN-00.

           OPEN     I-O WS-DISPLAY.
           MOVE     BS-STAT TO X-BSSTAT.

       DSP-OPN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHLIEßEN DES DISPLAYS
     ‚*--------------------------------------------------------------
       DSP-FNC-CLOSE SECTION.
       DSP-CLS-00.

           CLOSE    WS-DISPLAY.
           MOVE     BS-STAT TO X-BSSTAT.

       DSP-CLS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN DES DISPLAYS
     ‚*--------------------------------------------------------------
       DSP-FNC-WRITE SECTION.
       DSP-WRT-00.

     ‚* WSREC FÜLLEN UND DISPLAY SCHREIBEN
           MOVE     X-WSREC   TO WSREC-WRK.
           PERFORM  AUFBEREITEN-WSREC-WRITE.

     ‚* RECORD NUN INS DISPLAY SCHREIBEN
           MOVE     WSREC-WRK TO WS-REC.
           WRITE    WS-REC FORMAT IS FMT-WRK.
           MOVE     BS-STAT TO X-BSSTAT.

       DSP-WRT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE-WRITE AUSFÜHREN
     ‚*--------------------------------------------------------------
       DSP-FNC-SFLWRITE SECTION.
       DSP-SFW-00.

     ‚* WSREC FÜLLEN UND DISPLAY SCHREIBEN
           MOVE     X-WSREC   TO WSREC-WRK.
           PERFORM  AUFBEREITEN-WSREC-WRITE.

     ‚* SUBFILERECORDNUMBER ÜBERNEHMEN
           MOVE     X-SFLNR TO SUBFILE-RECORD-NUMBER.

     ‚* RECORD NUN INS DISPLAY SCHREIBEN
           MOVE     WSREC-WRK TO WS-REC.
           WRITE    SUBFILE WS-REC FORMAT IS FMT-WRK.
           MOVE     BS-STAT TO X-BSSTAT.

       DSP-SFW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* SUBFILE ZURÜCKSCHREIBEN
     ‚*-------------------------------------------------------
       DSP-FNC-SFLREWRITE SECTION.
       DSP-RWS-00.

     ‚* WSREC FÜLLEN UND DISPLAY SCHREIBEN
           MOVE     X-WSREC   TO WSREC-WRK.
           PERFORM  AUFBEREITEN-WSREC-WRITE.

     ‚* SUBFILERECORDNUMBER ÜBERNEHMEN
           MOVE     X-SFLNR TO SUBFILE-RECORD-NUMBER.

           MOVE     WSREC-WRK TO WS-REC.
           REWRITE  SUBFILE WS-REC FORMAT IS FMT-WRK.
           MOVE     BS-STAT TO X-BSSTAT.

       DSP-RWS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DER USEREINGABEN
     ‚*--------------------------------------------------------------
       DSP-FNC-READ SECTION.
       DSP-RED-00.

           INITIALIZE X-WSREC.

           READ     WS-DISPLAY FORMAT IS FMT-WRK.
           ACCEPT   BS-IO-AREA FROM FILE-IO-FEEDBACK.

           MOVE     BS-STAT TO X-BSSTAT.
           IF       BS-STAT = ZEROES
     ‚*             WSREC AUFBEREITEN
                    MOVE     WS-REC   TO WSREC-WRK
                    PERFORM  AUFBEREITEN-WSREC-READ
                    MOVE     WSREC-WRK TO X-WSREC
           END-IF.

       DSP-RED-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DER USEREINGABEN (AUTO-REFRESH)
     ‚*--------------------------------------------------------------
       DSP-FNC-READ-NDTA SECTION.
       DSP-RND-00.

           INITIALIZE X-WSREC.

           READ     WS-DISPLAY RECORD NO DATA
     ‚*             JOB EINE SEKUNDE PAUSIEREN
                    MOVE "0001" TO DLY-JOB
                    CALL "CLP0064" USING DLY-JOB
                    END-CALL
     ‚*             JETZT NOCHMAL LESEN
                    READ WS-DISPLAY RECORD NO DATA
     ‚*                  IMMERNOCH NICHTS EINGEGEBEN?
                         MOVE "ND" TO BS-STAT
                         ACCEPT   BS-IO-AREA FROM FILE-IO-FEEDBACK
                         GO TO DSP-RND-90
                    END-READ
           END-READ.
           ACCEPT   BS-IO-AREA FROM FILE-IO-FEEDBACK.

           MOVE     BS-STAT TO X-BSSTAT.
           IF       BS-STAT = ZEROES
     ‚*             WSREC AUFBEREITEN
                    MOVE     WS-REC   TO WSREC-WRK
                    PERFORM  AUFBEREITEN-WSREC-READ
                    MOVE     WSREC-WRK TO X-WSREC
           END-IF.

       DSP-RND-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DES SUBFILESATZES
     ‚*--------------------------------------------------------------
       DSP-FNC-SFLREAD SECTION.
       DSP-RES-00.

           READ     SUBFILE WS-DISPLAY NEXT MODIFIED RECORD
                    FORMAT IS FMT-WRK.
           ACCEPT   BS-IO-AREA FROM FILE-IO-FEEDBACK.

           MOVE     BS-STAT TO X-BSSTAT.
           IF       BS-STAT = ZEROES
     ‚*             WSREC AUFBEREITEN
                    MOVE     WS-REC   TO WSREC-WRK
                    PERFORM  AUFBEREITEN-WSREC-READ
                    MOVE     WSREC-WRK TO X-WSREC
           END-IF.

     ‚* SUBFILERECORDNUMBER ZURÜCKGEBEN
           MOVE     SUBFILE-RECORD-NUMBER TO X-SFLNR.

       DSP-RES-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DES SUBFILESATZES
     ‚*--------------------------------------------------------------
       DSP-FNC-SFLREADREC SECTION.
       DSP-RRC-00.

     ‚* SFL-NUMMER ÜBERNEHMEN UND GEZIELT LESEN
           MOVE     X-SFLNR TO SUBFILE-RECORD-NUMBER.
           READ     SUBFILE WS-DISPLAY FORMAT IS FMT-WRK.

           ACCEPT   BS-IO-AREA FROM FILE-IO-FEEDBACK.

           MOVE     BS-STAT TO X-BSSTAT.
           IF       BS-STAT = ZEROES
     ‚*             WSREC AUFBEREITEN
                    MOVE     WS-REC   TO WSREC-WRK
                    PERFORM  AUFBEREITEN-WSREC-READ
                    MOVE     WSREC-WRK TO X-WSREC
           END-IF.

     ‚* SUBFILERECORDNUMBER ZURÜCKGEBEN
           MOVE     SUBFILE-RECORD-NUMBER TO X-SFLNR.

       DSP-RRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DISPLAY-CURSER AUSFÜHREN
     ‚*--------------------------------------------------------------
       DSP-FNC-CURSER SECTION.
       DSP-CUR-00.

           ACCEPT   BS-ATR              FROM BS-ATRIBUTE.
     ‚* GESICHERTE STRUKTUR VERWENDEN

           IF       ENDIAN-MODE = ZEROES
                    MOVE IOF-DSP-CURSER-LINE OF BS-IO-AREA TO
                         WORK-BIN-CHAR2
                    MOVE IOF-DSP-CURSER-COL OF BS-IO-AREA TO
                         WORK-BIN-CHAR2
                    MOVE WORK-BIN TO CURSER-ZEILE
                    MOVE WORK-BIN TO CURSER-SPALTE
           ELSE
                    MOVE IOF-DSP-CURSER-LINE OF BS-IO-AREA TO
                         WORK-BIN-CHAR1
                    MOVE IOF-DSP-CURSER-COL OF BS-IO-AREA TO
                         WORK-BIN-CHAR1
                    MOVE WORK-BIN TO CURSER-ZEILE
                    MOVE WORK-BIN TO CURSER-SPALTE
           END-IF.

     ‚* BILDSCHIRMZEILE
           MOVE     IOF-SFL-LOW-NBR OF BS-IO-AREA TO SFL-ZEILE.
           IF       SFL-ZEILE = ZEROES
                    MOVE  1 TO SFL-ZEILE
           END-IF.

     ‚* ZURÜCKGEBEN
           MOVE     BS-ATR     TO X-BS-ATR.
           MOVE     BS-IO-AREA TO X-BS-IO-AREA.

     ‚* FÜR DEN DEBUGGER
           IF       1 = 2
                    MOVE IOF-SFL-REC-NBR     OF BS-IO-AREA TO
                         IOF-SFL-REC-NBR     OF BS-IO-AREA
                    MOVE IOF-SFL-REC-COUNTER OF BS-IO-AREA TO
                         IOF-SFL-REC-COUNTER OF BS-IO-AREA
           END-IF.

       DSP-CUR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER DATUMSFELDER
     ‚*--------------------------------------------------------------
       AUFBEREITEN-WSREC-WRITE SECTION.
       AUF-WSR-00.

     ‚* LESEN ALLER DATUMSFELDER
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB       TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK     TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK         TO WHNAME OF PCFGFFD-L.
           MOVE     SPACES          TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO AUF-WSR-90
           END-IF.

       AUF-WSR-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO AUF-WSR-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L      NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L      NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L      NOT = FMT-WRK
                    GO TO AUF-WSR-90
           END-IF.

     ‚* AUSGABEFELD?
           IF       WHFIOB OF PCFGFFD-L      NOT = "B"
            AND     WHFIOB OF PCFGFFD-L      NOT = "O"
                    GO TO AUF-WSR-20
           END-IF.

     ‚* DATUMSAUFBEREITUNG?
           IF       AUFBEREITUNG-DATUM-AKTIV NOT = ZEROES
            AND     WHFLDE OF PCFGFFD-L(1:3)     = "DT_"
            AND     WHFLDB OF PCFGFFD-L         >= 8
                    PERFORM AUFBEREITEN-DATE-WRITE
           END-IF.

     ‚* ZEITAUFBEREITUNG?
           IF       AUFBEREITUNG-ZEIT-AKTIV  NOT = ZEROES
            AND     WHFLDE OF PCFGFFD-L(1:3)     = "TI_"
            AND     WHFLDB OF PCFGFFD-L         >= 5
                    PERFORM AUFBEREITEN-TIME-WRITE
           END-IF.

     ‚* FORMATAUFBEREITUNG?
           IF       AUFBEREITUNG-DATUM-AKTIV NOT = ZEROES
            AND     (   WHFLDE OF PCFGFFD-L(1:3) = "DF_"
                     OR WHFLDE OF PCFGFFD-L(1:3) = "TF_")
                    PERFORM AUFBEREITEN-FORMAT-WRITE
           END-IF.

     ‚* PROGRAMMTEXTE AUFBEREITEN?
           IF       AUFBEREITUNG-TEXTE-AKTIV NOT = ZEROES
            AND     WHFLDE OF PCFGFFD-L(1:3)     = "SFT"
                    PERFORM AUFBEREITEN-SFTTXT-WRITE
           END-IF.

     ‚* SYSTEMFELDER AUFBEREITEN?
           IF       AUFBEREITUNG-TEXTE-AKTIV NOT = ZEROES
            AND     WHFLDE OF PCFGFFD-L(1:4)     = "SYS_"
                    PERFORM AUFBEREITEN-SYSFLD-WRITE
           END-IF.

     ‚* ÜBERSCHRIFTEN AUFBEREITEN?
           IF       AUFBEREITUNG-HEAD-AKTIV  NOT = ZEROES
            AND     (    WHFLDE OF PCFGFFD-L     = "MAINUEB"
                     OR  WHFLDE OF PCFGFFD-L     = "SUBUEB")
                    PERFORM AUFBEREITEN-HEADER-WRITE
           END-IF.

     ‚* SOLL EIN CURSER-AKTIVIERT WERDEN?
           IF       WHFLDE OF PCFGFFD-L(1:4)     = "CURX"
            OR      WHFLDE OF PCFGFFD-L(1:4)     = "CURY"
            OR      WHFLDE OF PCFGFFD-L(1:5)     = "*IN03"
                    PERFORM AUFBEREITEN-CURSER
           END-IF.

     ‚* MUSS EINE FEHLERMELDUNG AKTIVIERT WERDEN?
           IF       WHFLDE OF PCFGFFD-L(1:6)     = "ERRMSG"
            OR      WHFLDE OF PCFGFFD-L(1:5)     = "*IN01"
                    IF   DSP-ERR-ID OF DSP-ATTR NOT = SPACES
                         PERFORM AUFBEREITEN-FEHLERMELDUNG
                    END-IF
           END-IF.

           GO TO    AUF-WSR-20.

       AUF-WSR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER ZEITFELDER
     ‚*--------------------------------------------------------------
       AUFBEREITEN-TIME-WRITE SECTION.
       AUF-TIW-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND GGF. DAS DATUM AUFBEREITEN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

     ‚* BEI SYSTEMWERTEN ZEIT EINTRAGEN
           IF       WHFLDE OF PCFGFFD-L = "TI_SYS"
                    PERFORM COPY-GET-TIME
                    MOVE TIME-6 TO WSREC-WRK(POS:6)
           END-IF.

           IF       WSREC-WRK(POS:6) IS NUMERIC
     ‚*             DATUM JETZT KONVERTIEREN
                    MOVE     WSREC-WRK(POS:6)  TO TIME-DB
                    MOVE     "TO-DSP"          TO TIME-CONVERT
                    PERFORM  COPY-CONVERT-TIME
                    EVALUATE WHFLDB OF PCFGFFD-L
                             WHEN 8
                                  MOVE TIME-ALPHA-8      TO
                                       WSREC-WRK(POS:LEN)
                             WHEN 6
                             WHEN 5
                                  MOVE TIME-ALPHA-8(1:5) TO
                                       WSREC-WRK(POS:LEN)
                    END-EVALUATE
           END-IF.

       AUF-TIW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER DATUMS/ZEITFELDER
     ‚*--------------------------------------------------------------
       AUFBEREITEN-DATE-WRITE SECTION.
       AUF-DTW-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND GGF. DAS DATUM AUFBEREITEN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

     ‚* BEI SYSTEMWERTEN ZEIT EINTRAGEN
           IF       WHFLDE OF PCFGFFD-L = "DT_SYS"
                    PERFORM COPY-GET-TIME
                    MOVE DATE-8 TO WSREC-WRK(POS:8)
           END-IF.

           IF       WSREC-WRK(POS:8) IS NUMERIC
     ‚*             DATUM JETZT KONVERTIEREN
                    MOVE     WSREC-WRK(POS:8)  TO DATE-DB
                    MOVE     "TO-DSP"          TO DATUM-CONVERT
                    PERFORM  COPY-CONVERT-DATE
                    EVALUATE WHFLDB OF PCFGFFD-L
                             WHEN 10
                                  MOVE DATUM-ALPHA-10 TO
                                       WSREC-WRK(POS:LEN)
                             WHEN 8
                                  MOVE DATUM-ALPHA-8  TO
                                       WSREC-WRK(POS:LEN)
                    END-EVALUATE
           ELSE
     ‚*             DATUM JETZT KONVERTIEREN
                    IF   WSREC-WRK(POS:6) IS NUMERIC
                         MOVE     ZEROES            TO DATE-DB
                         MOVE     WSREC-WRK(POS:6)  TO DATE-DB-6
                         MOVE     "TO-DSP"          TO DATUM-CONVERT
                         PERFORM  COPY-CONVERT-DATE
                         EVALUATE WHFLDB OF PCFGFFD-L
                                  WHEN 10
                                       MOVE DATUM-ALPHA-10 TO
                                            WSREC-WRK(POS:LEN)
                                  WHEN 8
                                       MOVE DATUM-ALPHA-8  TO
                                            WSREC-WRK(POS:LEN)
                         END-EVALUATE
                    END-IF
           END-IF.

       AUF-DTW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER SYSTEMFELDER
     ‚*--------------------------------------------------------------
       AUFBEREITEN-FORMAT-WRITE SECTION.
       AUF-FMT-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND GGF. DAS DATUM AUFBEREITEN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

     ‚* SYSTEMFELDER FÜLLEN
           EVALUATE WHFLDE OF PCFGFFD-L(1:3)
                    WHEN "DF_"
                         EVALUATE WHFLDB OF PCFGFFD-L
                             WHEN 10 MOVE "TT.MM.JJJJ" TO
                                          WSREC-WRK(POS:LEN)
                             WHEN  8 MOVE "TT.MM.JJ"   TO
                                          WSREC-WRK(POS:LEN)
                         END-EVALUATE
                    WHEN "TF_"
                         EVALUATE WHFLDB OF PCFGFFD-L
                             WHEN  8 MOVE "HH:MM:SS"   TO
                                          WSREC-WRK(POS:LEN)
                             WHEN  5 MOVE "HH:MM"      TO
                                          WSREC-WRK(POS:LEN)
                         END-EVALUATE
           END-EVALUATE.

       AUF-FMT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER PROGRAMMTEXTE
     ‚*--------------------------------------------------------------
       AUFBEREITEN-SFTTXT-WRITE SECTION.
       AUF-SFT-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND GGF. DAS DATUM AUFBEREITEN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

     ‚* TEXT NUN LESEN
           MOVE     WHFLDE OF PCFGFFD-L(1:7) TO CPY-TXID.
           MOVE     X-SPRACHE                TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT               TO WSREC-WRK(POS:LEN).

       AUF-SFT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER SYSTEMFELDER
     ‚*--------------------------------------------------------------
       AUFBEREITEN-SYSFLD-WRITE SECTION.
       AUF-SYS-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND GGF. DAS DATUM AUFBEREITEN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

     ‚* SYSTEMFELDER FÜLLEN
           EVALUATE WHFLDE OF PCFGFFD-L
                    WHEN "SYS_FMT"
                         MOVE FMT-WRK        TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_DSP"
                         MOVE DISPLAY-WRK    TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_PGM"
                         MOVE X-PGM          TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_PGMTYP"
                         MOVE X-PGM-TYP      TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_USER"
                         MOVE T-USER         TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_TERM"
                         MOVE T-TERM         TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_JBNR"
                         MOVE T-JBNR         TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_LANG"
                         MOVE SPRACHE-WRK    TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_CMP"
                         MOVE SYS-CMP-TEXT   TO WSREC-WRK(POS:LEN)
                    WHEN "SYS_CMT"
                         MOVE SYS-CMP-BEZ    TO WSREC-WRK(POS:LEN)
           END-EVALUATE.

       AUF-SYS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER PROGRAMMTEXTE
     ‚*--------------------------------------------------------------
       AUFBEREITEN-HEADER-WRITE SECTION.
       AUF-HDW-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND GGF. DAS DATUM AUFBEREITEN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

     ‚* TEXT NUN LESEN
           MOVE     WSREC-WRK(POS:LEN)       TO CPY-TXID.
           MOVE     X-SPRACHE                TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT               TO WSREC-WRK(POS:LEN).

       AUF-HDW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* CURSER-POSITION AUFBEREITEN
     ‚*--------------------------------------------------------------
       AUFBEREITEN-CURSER SECTION.
       AUF-CUR-00.

     ‚* CURSER GESETZT?
           IF       DSP-CURX OF DSP-ATTR = ZEROES
            AND     DSP-CURY OF DSP-ATTR = ZEROES
                    GO TO AUF-CUR-90
           END-IF.

     ‚* WERTE SICHERN UND GGF. CURSER SETZEN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

     ‚* JE NACHDEM FELDER AKTIVIEREN
           EVALUATE TRUE
                    WHEN WHFLDE OF PCFGFFD-L(1:4) = "CURX"
                         MOVE DSP-CURX OF DSP-ATTR TO
                              WSREC-WRK(POS:LEN)
                    WHEN WHFLDE OF PCFGFFD-L(1:4) = "CURY"
                         MOVE DSP-CURY OF DSP-ATTR TO
                              WSREC-WRK(POS:LEN)
                    WHEN WHFLDE OF PCFGFFD-L(1:5) = "*IN03"
                         MOVE "1" TO WSREC-WRK(POS:LEN)
           END-EVALUATE.

       AUF-CUR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER FEHLERMELDUNG
     ‚*--------------------------------------------------------------
       AUFBEREITEN-FEHLERMELDUNG SECTION.
       AUF-SFF-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND FEHLERMELDUNG ERMITTELN
           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.

           EVALUATE TRUE
                    WHEN WHFLDE OF PCFGFFD-L(1:6) = "ERRMSG"
     ‚*                  FIXEN TEXT VERWENDEN, ODER ...
                         IF   DSP-ERR-ID OF DSP-ATTR = "*FIX"
                              MOVE SPACES TO CPY-TXTEXT
                         ELSE
     ‚*                       ... TEXT-ID NUN LESEN
                              MOVE  DSP-ERR-ID OF DSP-ATTR TO CPY-TXID
                              MOVE  X-SPRACHE              TO CPY-TXSPR
                              PERFORM  COPY-PGM-TXT
                         END-IF
                         MOVE     CPY-TXTEXT      TO WSREC-WRK(POS:LEN)
                    WHEN WHFLDE OF PCFGFFD-L(1:5) = "*IN01"
     ‚*                  INDIKATOR AKTIVIEREN
                         MOVE "1" TO WSREC-WRK(POS:LEN)
           END-EVALUATE.

       AUF-SFF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER DATUMSFELDER
     ‚*--------------------------------------------------------------
       AUFBEREITEN-WSREC-READ SECTION.
       AUF-WRR-00.

     ‚* LESEN ALLER DATUMSFELDER
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB       TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK     TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK         TO WHNAME OF PCFGFFD-L.
           MOVE     SPACES          TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO AUF-WRR-90
           END-IF.

       AUF-WRR-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO AUF-WRR-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L      NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L      NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L      NOT = FMT-WRK
                    GO TO AUF-WRR-90
           END-IF.

     ‚* EINGABEFELD?
           IF       WHFIOB OF PCFGFFD-L      NOT = "B"
            AND     WHFIOB OF PCFGFFD-L      NOT = "I"
                    GO TO AUF-WRR-20
           END-IF.

     ‚* DATUMSAUFBEREITUNG?
           IF       AUFBEREITUNG-DATUM-AKTIV NOT = ZEROES
            AND     WHFLDE OF PCFGFFD-L(1:3)     = "DT_"
            AND     WHFLDB OF PCFGFFD-L         >= 8
                    PERFORM AUFBEREITEN-DATE-READ
           END-IF.

     ‚* DATUMSAUFBEREITUNG?
           IF       AUFBEREITUNG-ZEIT-AKTIV  NOT = ZEROES
            AND     WHFLDE OF PCFGFFD-L(1:3)     = "TI_"
            AND     WHFLDB OF PCFGFFD-L         >= 5
                    PERFORM AUFBEREITEN-TIME-READ
           END-IF.

           GO TO    AUF-WRR-20.

       AUF-WRR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER ZEITFELDER
     ‚*--------------------------------------------------------------
       AUFBEREITEN-TIME-READ SECTION.
       AUF-TIR-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND GGF. DAS DATUM AUFBEREITEN
           MOVE     WHIBO  OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.
           IF       WSREC-WRK(POS:LEN) NOT = SPACES
                    EVALUATE WHFLDB OF PCFGFFD-L
                             WHEN 8
                                  MOVE TIME-ALPHA-8(POS:LEN) TO
                                       DATUM-ALPHA-8
                             WHEN 6
                             WHEN 5
                                  MOVE TIME-ALPHA-8(POS:LEN) TO
                                       DATUM-ALPHA-8
                    END-EVALUATE
     ‚*             DATUM JETZT KONVERTIEREN
                    MOVE     ZEROES            TO TIME-DB
                    MOVE     "TO-DB"           TO TIME-CONVERT
                    PERFORM  COPY-CONVERT-TIME
                    MOVE     TIME-DB TO WSREC-WRK(POS:LEN)
           ELSE
                    MOVE ZEROES TO DATE-DB(POS:LEN)
           END-IF.

       AUF-TIR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFBEREITEN DER DATUMS/ZEITFELDER
     ‚*--------------------------------------------------------------
       AUFBEREITEN-DATE-READ SECTION.
       AUF-DTR-00.

           INITIALIZE LEN, POS.
     ‚* WERTE SICHERN UND GGF. DAS DATUM AUFBEREITEN
           MOVE     WHIBO  OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.
           IF       WSREC-WRK(POS:LEN) NOT = SPACES
                    EVALUATE WHFLDB OF PCFGFFD-L
                             WHEN 10
                                  MOVE WSREC-WRK(POS:LEN) TO
                                       DATUM-ALPHA-10
                             WHEN 8
                                  MOVE WSREC-WRK(POS:LEN) TO
                                       DATUM-ALPHA-8
                    END-EVALUATE
     ‚*             DATUM JETZT KONVERTIEREN
                    MOVE     ZEROES            TO DATE-DB
                    MOVE     "TO-DB"           TO DATUM-CONVERT
                    PERFORM  COPY-CONVERT-DATE
                    MOVE     DATE-DB TO WSREC-WRK(POS:LEN)
           ELSE
                    MOVE ZEROES TO WSREC-WRK(POS:LEN)
           END-IF.

       AUF-DTR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES FELDES
     ‚*--------------------------------------------------------------
       GET-XMLFFD SECTION.
       GET-XML-00.

     ‚* LESEN ALLER DATUMSFELDER
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB       TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK     TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK         TO WHNAME OF PCFGFFD-L.
           MOVE     WHFLDE-WRK      TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGFFDF OF PCFGFFD-L
                    GO TO GET-XML-90
           END-IF.

       GET-XML-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGFFDF OF PCFGFFD-L
                    GO TO GET-XML-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L      NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L      NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L      NOT = FMT-WRK
            OR      WHFLDE OF PCFGFFD-L      NOT = WHFLDE-WRK
                    INITIALIZE PCFGFFDF OF PCFGFFD-L
                    GO TO GET-XML-90
           END-IF.

       GET-XML-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZUSATZDATEN DIE RECHTS OBEN ANGEZEIGT WERDEN
     ‚*--------------------------------------------------------------
       GET-PGMDATA SECTION.
       GET-PDT-00.

     ‚* EINSTELLUNG LESEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H050"            TO CFID   OF CFG-CPY.
           MOVE     T-USER            TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
                    MOVE CFIN27 OF CFG-CPY TO 0024-AKTION
           END-IF.

     ‚* DATEN ERMITTELN - DATEINAME ERSTMAL FIX
           MOVE     "APG"     TO LIB-WRK.
           MOVE     "QCBLSRC" TO RCN-WRK.
           CALL     "CFP0024" USING LIB-WRK
                                    RCN-WRK
                                    X-PGM
                                    X-PGM-TYP
                                    DISPLAY-WRK
                                    X-COMPILE-DATE
                                    T-USER
                                    SPRACHE-WRK
                                    0024-AKTION
                                    SYS-CMP-TEXT
                                    SYS-CMP-BEZ
           END-CALL.

       GET-PDT-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
     ‚* COPY-CONVERT-DATE: DATUMSFELDER AUFBEREITEN/UMSETZEN
     ‚*---------------------------------------------------------------

           COPY     CNVDATE    OF APG-QCPYSRC.
      /
