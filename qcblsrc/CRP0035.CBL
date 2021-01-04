       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0035.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ZEICHENPROGRAMM                                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  07.07.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ZEICHENPROGRAMM                              *
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
                  ASSIGN       TO  WORKSTATION-CRD0035DE
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
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF CRD0035DE.
      /
     ‚*--- PCFGFFD NACH LIB/FILE
       FD  PCFGFFD-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-L.
           COPY DDS-ALL-FORMATS OF PCFGFFDL1.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN03                          PIC 1.
     ‚* INDIK. FÜR ANZEIGE OB INFOS VORHANDEN SIND
        05 IN40                          PIC 1.
     ‚* INDIK. FÜR ANZEIGE OB DATEI GEÄNDERT
        05 IN41                          PIC 1.
     ‚* INDIK. FÜR DIE SOURCESTATISTIK
        05 IN42                          PIC 1.

       01  OUTXX.
     ‚* INDIK. FÜR DIE FELDÄNDERUNG (IN50 = ZEILE1, IN69 = ZEILE20)
        05 IN50                          PIC 1.
        05 IN51                          PIC 1.
        05 IN52                          PIC 1.
        05 IN53                          PIC 1.
        05 IN54                          PIC 1.
        05 IN55                          PIC 1.
        05 IN56                          PIC 1.
        05 IN57                          PIC 1.
        05 IN58                          PIC 1.
        05 IN59                          PIC 1.
        05 IN60                          PIC 1.
        05 IN61                          PIC 1.
        05 IN62                          PIC 1.
        05 IN63                          PIC 1.
        05 IN64                          PIC 1.
        05 IN65                          PIC 1.
        05 IN66                          PIC 1.
        05 IN67                          PIC 1.
        05 IN68                          PIC 1.
        05 IN69                          PIC 1.
     ‚* ENDE INDIKATOREN FÜR DIE FELDÄNDERUNG

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  CRD0035DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CRD0035DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0035".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.
       01  DSPPFM-REC                    TYPE DSPPFM-TYPE.
       01  WRKOBJLCK-REC                 TYPE WRKOBJLCK-TYPE.
       01  RUNSQL-REC                    TYPE RUNSQL-TYPE.
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  OVRDBF-REC                    TYPE OVRDBF-TYPE.
       01  DELOVR-REC                    TYPE DELOVR-TYPE.

     ‚* VARIABLEN FÜR DIE SEUCMD-SUCHE
       01  SEU-CMD-GEFUNDEN              PIC 9(1).
       01  SEU-CMD-GEFUNDEN-POS          PIC 9(4).
       01  CMD-SUCHE-TAB.
        05  CMD-REC OCCURS 10.
         06  CMD-SUCHE                   LIKE SEUCMD OF FORMAT1.
         06  CMD-LAENGE                  PIC 9(4).

     ‚* LAUFVARIABLEN
       01  TAB-COUNTER                   PIC 9(4).
       01  ZEILE-WRK                     PIC 9(4).
       01  MAX-ZEILEN                    PIC 9(4) VALUE 20.
       01  LFDN-MAP                      LIKE MAP-LFDN OF MAPDTA-REC.
       01  IND                           PIC 9(4).
       01  START-LFDN                    LIKE MAP-LFDN OF MAPDTA-REC.
       01  AKT-LFDN-MIN                  LIKE MAP-LFDN OF MAPDTA-REC.
       01  AKT-LFDN-MAX                  LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* LÄNGEN-VARIABLEN
       01  SRCLEN-WRK                    PIC 9(4).
       01  LEN-TMP                       PIC 9(4).
       01  POS-TMP                       PIC 9(4).
       01  MAXLEN                        PIC 9(4).
       01  REST-LEN                      PIC 9(4).
       01  FIRST-SPACE-POS               PIC 9(4).

     ‚* VARIABLEN FÜR DIE ERMITTLUNG DES LETZTEN KEY
       01  LAST-KEY                      LIKE MAP-KEY  OF MAPDTA-REC.
       01  LAST-LFDN                     LIKE MAP-LFDN OF MAPDTA-REC.
       01  LAST-REC                      LIKE MAP-REC  OF MAPDTA-REC.

     ‚* VARIABLEN FÜR DIE BEARBEITUNG
       01  SRC-WRK                       LIKE MAP-REC  OF MAPDTA-REC.
       01  SOURCE-LFDN                   LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* SCHALTER
       01  START-ERFOLGT                 PIC 9(1).

     ‚* SONSTIGE VARIABLEN
       01  AUSWAHL                       PIC X(2).
       01  ANZREC-SAV                    PIC 9(9).
       01  IMAGE-WRK                     PIC X(30).

       01  AENDERN-WRK                   PIC 9(1).
       01  AENDERN-TMP                   PIC 9(1).
       01  NEUANZEIGE-WRK                PIC 9(1).
       01  LEERZEILEN-WRK                PIC 9(1).
       01  CHANGE-AKTIV                  PIC 9(1).
       01  CHANGE-ZEILE-AKTIV            PIC 9(1).
       01  CHANGE-SRC-AKTIV              PIC 9(1).
       01  CHANGE-USRCMD-AKTIV           PIC 9(1).

       01  RECORD-FLAGS.
        05 CHANGE-FLAG                   PIC X(1).
        05 CHANGE-TYPE-FLAG              PIC X(1).
        05 VIEW-FLAG                     PIC X(1).
        05 FILLER-FLAG                   PIC X(7).

       01  AENXX                         PIC X(1).
       01  AUSWXX                        PIC X(1).

       01  CURX-WRK                      LIKE CURX OF FORMAT1.
       01  CURY-WRK                      LIKE CURY OF FORMAT1.

     ‚* POINTER ZUR ANZEIGE DER SOURCE, ETC.
       01  SOURCE-PTR                    USAGE POINTER.
       01  RESTORE-PTR                   USAGE POINTER.
       01  BEFEHL-PTR                    USAGE POINTER.
       01  NOTIZEN-PTR                   USAGE POINTER.
       01  HIGHLIGHT-PTR                 USAGE POINTER.
       01  SPRUNG-PTR                    USAGE POINTER.

     ‚* VARIABLEN FÜR DIE COMMAND-ERMITTLUNG
       01  SEUCMD-WRK                    LIKE SEUCMD OF FORMAT1.
       01  SEUCMD-NEUSTART               PIC 9(1).

     ‚* VARIABLEN FÜR DIE XML-FELDERMITTLUNG
       01  XML-FELD                      PIC X(10).
       01  XML-FOUND                     PIC 9(1).

     ‚* VARIABLEN FÜR GET-ZEILE
       01  GET-ZEILE-HLP                 PIC 9(1).

     ‚* VARIABLEN FÜR DIE AUSWAHL
       01  AUSW-WRK                      PIC X(1).
       01  AUSW-LFDN                     LIKE SOURCE-LFDN.

     ‚* PARAMETER FÜR DAS HLP0011
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).

     ‚* PARAMETER FÜR DAS CRP0007
       01  MAPKEY-WRK                    LIKE MAP-KEY OF MAPDTA-REC.

     ‚* PARAMETER FÜR DAS CRP0009
       01  MAP-SUCHE-WRK                 LIKE MAP-REC  OF MAPDTA-REC.
       01  MAP-KEY-WRK                   LIKE MAP-KEY  OF MAPDTA-REC.
       01  MAP-LFDN-WRK                  LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* PARAMETER FÜR DAS CRP0034
       01  0034-AKTION                   PIC X(20).
       01  0034-MAP-LFDN                 LIKE MAP-LFDN OF MAPDTA-REC.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-IMAGE                       PIC X(30).
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-IMAGE
                                X-F3
                                X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0035"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CRD0035DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PCFGFFD-DL.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.
           INITIALIZE X-F3, X-F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÜBERGABEPARAMETER ÜBERNEHMEN
           MOVE     X-IMAGE  TO IMAGE-WRK.

       ANF010.
           PERFORM  INIT-VAR.

     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN IN MAPDTA
           IF       IMAGE-WRK NOT = SPACES
                    PERFORM  LESE-DATEN
                    IF   RET-CODE NOT = SPACES
                         GO TO ENDE
                    END-IF
           END-IF.

       ANF020.
     ‚* ENTSCHEIDEN WELCHEN POINTER WIR HIER WOLLEN
           SET      PTR-READ OF PTRMAP-REC TO TRUE.
           MOVE     "SOURCE-0001" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      SOURCE-PTR TO PTR-RETURN OF PTRMAP-REC.

     ‚* JETZT ANFANGEN DIE ZEILEN ZU LESEN UND INS DISPLAY ZU SCHREIBEN
           PERFORM  FILL-FMT01.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-DISPLAY.
           IF       F3 = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F5 = "1"
                    INITIALIZE F5
                    GO TO ANF020
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PCFGFFD-DL.

     ‚* POINTER LEEREN?
           PERFORM  CLEAR-VAR.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER SOURCZEILEN
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

     ‚* EINLESEN DER SOURCE
           CALL     "CRP0036" USING IMAGE-WRK
                                    PTR-MAIN OF PTRMAP-REC
           END-CALL.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DES HEADERS
     ‚*--------------------------------------------------------------
       FILL-FMT01-HEADER SECTION.
       FIL-FH1-00.

     ‚* NAMEN ANZEIGEN
           MOVE     IMAGE-WRK TO LIBFILE OF FORMAT1.

           INITIALIZE RET-CODE.
     ‚* SIND INFORMATIONEN VORHANDEN?
           CALL     "CFP0010" USING PGM-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    INITIALIZE RET-CODE
                    MOVE AN  TO IN40 OF INXX
           ELSE
                    MOVE AUS TO IN40 OF INXX
           END-IF.

     ‚* WURDE DAS BILD GEÄNDERT?
           MOVE     AUS TO IN41 OF INXX.
           IF       CHANGE-SRC-AKTIV NOT = ZEROES
                    MOVE AN      TO IN02 OF INXX
                    MOVE COL-WHT TO A_CHGINF OF FORMAT1
                    MOVE "*"     TO CHGINF   OF FORMAT1
                    MOVE AN      TO IN41 OF INXX
           END-IF.

     ‚* ANZAHL RECORDS
           CALL     "CRP0018" USING SOURCE-PTR
                                    ANZREC-SAV
           END-CALL.

       FIL-FH1-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN ALLER SOURCEZEILEN
     ‚*--------------------------------------------------------------
       FILL-FMT01 SECTION.
       FIL-FT1-00.

           INITIALIZE FORMAT1.
     ‚* LAUFENDE NUMMER INITIALISIEREN
           MOVE     ZEROES  TO START-ERFOLGT.
           MOVE     ZEROES  TO LFDN-MAP.

           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.

       FIL-FT1-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE     1             TO START-ERFOLGT
                    INITIALIZE MAPDTA-REC
                    MOVE     START-LFDN    TO MAP-LFDN OF MAPDTA-REC
                    MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO FIL-FT1-90
                    END-IF
           ELSE
                    MOVE     MAP-READ-NEXT TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO FIL-FT1-90
                    END-IF
           END-IF.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-LFDN OF MAPDTA-REC       TO LFDN-MAP.
           MOVE     MAP-REC  OF MAPDTA-REC(1:10) TO RECORD-FLAGS.
           MOVE     MAP-REC  OF MAPDTA-REC(10:)  TO SRC-WRK.

     ‚* ZEILE GEÄNDERT?
           IF       CHANGE-FLAG OF RECORD-FLAGS NOT = SPACES
                    MOVE 1 TO CHANGE-SRC-AKTIV
           END-IF.

     ‚* JETZT DIE ZEILE FÜLLEN
           PERFORM  AUSGABE-ZEILE.

     ‚* ZEILENNUMMERN MERKEN
           IF       ZEILE-WRK = 1
                    MOVE MAP-LFDN OF MAPDTA-REC TO AKT-LFDN-MIN
           END-IF.
           IF       ZEILE-WRK = MAX-ZEILEN
                    MOVE MAP-LFDN OF MAPDTA-REC TO AKT-LFDN-MAX
           END-IF.

     ‚* NÄCHSTE ZEILE LESEN
           IF       ZEILE-WRK < MAX-ZEILEN
                    ADD   1       TO LFDN-MAP
                    GO TO FIL-FT1-20
           END-IF.

       FIL-FT1-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-ZEILE SECTION.
       AUS-ZLE-00.

     ‚* IST ÄNDERN ERLAUBT?
           IF       AENDERN-WRK = ZEROES
                    MOVE COL-GRN-PR TO AENXX
                    MOVE AN         TO IN02 OF INXX
           END-IF.

           ADD      1                      TO ZEILE-WRK.
     ‚* JE NACHDEM WELCHE ZEILE WIR HABEN JETZT ANDERS FÜLLEN
           EVALUATE ZEILE-WRK
                    WHEN 1
                         MOVE LFDN-MAP        TO SLFDN01H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA01 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA01 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW01   OF FORMAT1
                    WHEN 2
                         MOVE LFDN-MAP        TO SLFDN02H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA02 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA02 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW02   OF FORMAT1
                    WHEN 3
                         MOVE LFDN-MAP        TO SLFDN03H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA03 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA03 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW03   OF FORMAT1
                    WHEN 4
                         MOVE LFDN-MAP        TO SLFDN04H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA04 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA04 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW04   OF FORMAT1
                    WHEN 5
                         MOVE LFDN-MAP        TO SLFDN05H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA05 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA05 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW05   OF FORMAT1
                    WHEN 6
                         MOVE LFDN-MAP        TO SLFDN06H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA06 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA06 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW06   OF FORMAT1
                    WHEN 7
                         MOVE LFDN-MAP        TO SLFDN07H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA07 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA07 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW07   OF FORMAT1
                    WHEN 8
                         MOVE LFDN-MAP        TO SLFDN08H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA08 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA08 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW08   OF FORMAT1
                    WHEN 9
                         MOVE LFDN-MAP        TO SLFDN09H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA09 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA09 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW09   OF FORMAT1
                    WHEN 10
                         MOVE LFDN-MAP        TO SLFDN10H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA10 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA10 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW10   OF FORMAT1
                    WHEN 11
                         MOVE LFDN-MAP        TO SLFDN11H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA11 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA11 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW11   OF FORMAT1
                    WHEN 12
                         MOVE LFDN-MAP        TO SLFDN12H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA12 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA12 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW12   OF FORMAT1
                    WHEN 13
                         MOVE LFDN-MAP        TO SLFDN13H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA13 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA13 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW13   OF FORMAT1
                    WHEN 14
                         MOVE LFDN-MAP        TO SLFDN14H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA14 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA14 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW14   OF FORMAT1
                    WHEN 15
                         MOVE LFDN-MAP        TO SLFDN15H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA15 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA15 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW15   OF FORMAT1
                    WHEN 16
                         MOVE LFDN-MAP        TO SLFDN16H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA16 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA16 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW16   OF FORMAT1
                    WHEN 17
                         MOVE LFDN-MAP        TO SLFDN17H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA17 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA17 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW17   OF FORMAT1
                    WHEN 18
                         MOVE LFDN-MAP        TO SLFDN18H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA18 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA18 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW18   OF FORMAT1
                    WHEN 19
                         MOVE LFDN-MAP        TO SLFDN19H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA19 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA19 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW19   OF FORMAT1
                    WHEN 20
                         MOVE LFDN-MAP        TO SLFDN20H OF FORMAT1
                         MOVE SRC-WRK         TO SRCDTA20 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA20 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW20   OF FORMAT1
           END-EVALUATE.

       AUS-ZLE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES DISPLAYS
     ‚*--------------------------------------------------------------
       ANZEIGE-DISPLAY SECTION.
       ANZ-DSP-00.

       ANZ-DSP-10.
           PERFORM  FILL-FMT01-HEADER.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.
     ‚* CURSER AUF DEN TEXT SETZEN?
           IF       CURX-WRK = ZEROES
            AND     CURY-WRK = ZEROES
     ‚*             ERMITTELN DER CURSER-POSITION
                    MOVE    "FMT01" TO FMT-WRK
                    PERFORM GET-CURSER-POS
     ‚*             CURSER SETZEN
                    IF   CURX OF FORMAT1 NOT = ZEROES
                     AND CURY OF FORMAT1 NOT = ZEROES
                         MOVE AN  TO IN03 OF INXX
                    END-IF
           ELSE
                    MOVE CURX-WRK TO CURX OF FORMAT1
                    MOVE CURY-WRK TO CURY OF FORMAT1
                    MOVE AN       TO IN03 OF INXX
     ‚*             JETZT WIEDER ZURÜCKSETZEN
                    MOVE ZEROES TO CURX-WRK
                    MOVE ZEROES TO CURY-WRK
           END-IF.

     ‚* ANZEIGE SUBFILE
           MOVE     ZEROES TO NEUANZEIGE-WRK.
           MOVE     ANZREC-SAV      TO  ANZREC OF FORMAT1.
           MOVE     CORR   INXX     TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I-INDIC TO  OUTXX.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           MOVE     AUS  TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 ENDE
           IF       IN03 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* F9 BEFEHL WIEDERHOLEN
           IF       IN09 OF FORMAT1-I-INDIC = AN
                    PERFORM GET-LAST-COMMAND
                    MOVE SEUCMD-WRK TO SEUCMD OF FORMAT1
                    MOVE SPACES     TO SEUCMD-WRK
                    GO TO ANZ-DSP-10
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* PRÜFEN OB EINE ÄNDERUNG VORLIEGT
           PERFORM  CHECK-CHANGE.
           IF       CHANGE-AKTIV NOT = ZEROES
                    MOVE    1 TO CHANGE-SRC-AKTIV
                    PERFORM SOURCE-CHANGE
           END-IF.

     ‚* F13 EINSTELLUNGEN
           IF       IN13 OF FORMAT1-I-INDIC = AN
                    PERFORM CHANGE-SETTINGS
                    IF   F3 = "1"
                         GO TO ANZ-DSP-90
                    END-IF
                    INITIALIZE F12
                    MOVE "1" TO F5
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* F20 INFOS ANZEIGEN
           IF       IN20 OF FORMAT1-I-INDIC = AN
                    PERFORM ANZEIGE-PGM-INFOS
                    GO TO ANZ-DSP-10
           END-IF.

     ‚* F21 KOMMANDOZEILE AUFRUFEN
           IF       IN21 OF FORMAT1-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO ANZ-DSP-10
           END-IF.

     ‚* BILD AUF / BILD AB
           IF       IN98 OF FORMAT1-I-INDIC = AN
                    PERFORM SCROLL-DOWN
                    GO TO ANZ-DSP-90
           END-IF.

           IF       IN99 OF FORMAT1-I-INDIC = AN
                    PERFORM SCROLL-UP
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* JETZT DIE VERSCHIEDENEN ZEILENBEFEHLE AUSFÜHREN
           IF       SEUCMD OF FORMAT1 NOT = SPACES
                    MOVE 1 TO NEUANZEIGE-WRK
                    MOVE SEUCMD OF FORMAT1 TO SEUCMD-WRK
                    PERFORM SEU-COMMAND
                    IF   F3 = "1"
                         GO TO ANZ-DSP-90
                    END-IF
                    IF   RET-CODE NOT = SPACES
                         GO TO ANZ-DSP-10
                    END-IF
                    IF   SEUCMD-NEUSTART NOT = ZEROES
                         GO TO ANZ-DSP-90
                    END-IF
           END-IF.

     ‚* PRÜFEN OB EINE AUSWAHL DURCHGEFÜHRT WERDEN SOLL
           PERFORM  CHECK-AUSWAHL.
           IF       NEUANZEIGE-WRK NOT = ZEROES
            AND     AUSW-WRK       NOT = SPACES
                    GO TO ANZ-DSP-90
           END-IF.

       ANZ-DSP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CRP0035"         TO MAINUEB OF FORMAT1.
           MOVE     "CRP0035*1"       TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINSTELLUNGEN ÄNDERN
     ‚*--------------------------------------------------------------
       CHANGE-SETTINGS SECTION.
       CHG-SET-00.

     ‚* JETZT IN CFG-CPY ÜBERNEHMEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H101"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE  1           TO CFFK01 OF CFG-CPY
                    MOVE  1           TO CFFK03 OF CFG-CPY
                    MOVE  ZEROES      TO CFFK04 OF CFG-CPY
                    MOVE  1           TO CFFK13 OF CFG-CPY
                    MOVE  "QTMPSRC"   TO CFDAT2 OF CFG-CPY
                    MOVE  "APG"       TO CFDAT3 OF CFG-CPY
                    MOVE  "QTEMP"     TO CFDAT4 OF CFG-CPY
                    MOVE  "*SAME"     TO CFDAT5 OF CFG-CPY
           END-IF.

       CHG-SET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* JETZT PRÜFEN OB NOTIZEN VORHANDEN SIND
     ‚*--------------------------------------------------------------
       NOTIZ-HIGHLIGHT SECTION.
       NOT-HGH-00.

     ‚* DATUM ZUM VERGLEICH AUFBEREITEN
           PERFORM  CHECK-NOTIZ-VORHANDEN.
           IF       RET-CODE = SPACES
                    MOVE AN         TO IN02 OF INXX
     ‚*€            MOVE COL-PNK    TO DATINXX
           END-IF.

       NOT-HGH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN DER SEU-COMMAND (Z.B. BOTTOM, TOP, ETC...)
     ‚*--------------------------------------------------------------
       SEU-COMMAND SECTION.
       SEU-CMD-00.

     ˆ* TODO: UMBAUEN! STRING ZEICHENORIENTIERT ANALYSIEREN UND
     ˆ* ANSCHLIEßEND FUNKTION AUSFÜHREN

           INITIALIZE F3.
           INITIALIZE RET-CODE.
           INITIALIZE SEUCMD-NEUSTART.
     ‚* WILL DER USER NACH UNTEN SPRINGEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "B"       TO CMD-SUCHE(1).
           MOVE     2         TO CMD-LAENGE(1).
           MOVE     "BOT"     TO CMD-SUCHE(2).
           MOVE     4         TO CMD-LAENGE(2).
           MOVE     "BOTTOM"  TO CMD-SUCHE(3).
           MOVE     7         TO CMD-LAENGE(3).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    COMPUTE START-LFDN = ANZREC-SAV - MAX-ZEILEN
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER NACH OBEN SPRINGEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "T"       TO CMD-SUCHE(1).
           MOVE     2         TO CMD-LAENGE(1).
           MOVE     "TOP"     TO CMD-SUCHE(2).
           MOVE     4         TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    MOVE ZEROES TO START-LFDN
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER DIE DATEI SICHERN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "SAVE"    TO CMD-SUCHE(1).
           MOVE     5         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SOURCE-SAVE
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WOLLEN WIR EINE ANDERE TEILDATEI ÖFFNEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "OPEN"    TO CMD-SUCHE(1).
           MOVE     5         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-OPEN
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER ALLE NOTIZEN SEHEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "NOTES"   TO CMD-SUCHE(1).
           MOVE     6         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-NOTES
                    GO TO SEU-CMD-90
           END-IF.

     ‚* OPTIONEN ANZEIGEN? WIE F13
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "OPTIONS"  TO CMD-SUCHE(1).
           MOVE     8          TO CMD-LAENGE(1).
           MOVE     "SETTINGS" TO CMD-SUCHE(2).
           MOVE     9          TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM CHANGE-SETTINGS
                    IF   F3 = "1"
                         GO TO SEU-CMD-90
                    END-IF
                    INITIALIZE F12
                    GO TO SEU-CMD-90
           END-IF.

     ‚* GESICHERTEN POINTER ZURÜCKSPIELEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "RESTORE" TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    CALL "CRP0016" USING RESTORE-PTR
                                         SOURCE-PTR
                    END-CALL
                    GO TO SEU-CMD-90
           END-IF.

     ‚* DATEI SCHLIEßEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "CLOSE"   TO CMD-SUCHE(1).
           MOVE     6         TO CMD-LAENGE(1).
           MOVE     "CLS"     TO CMD-SUCHE(2).
           MOVE     4         TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE "1" TO F3
                    GO TO SEU-CMD-90
           END-IF.

     ‚* POINTER ANZEIGEN (AUS POINTERVERWALTUNG)
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "SHOWPTR" TO CMD-SUCHE(1).
           MOVE     8         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-SHOWPTR
                    GO TO SEU-CMD-90
           END-IF.

       SEU-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHEN DES SEU-COMMANDS
     ‚*--------------------------------------------------------------
       SEU-COMMAND-SUCHE-BEFEHL SECTION.
       SEU-SUC-00.

           INITIALIZE IND.
           INITIALIZE SEU-CMD-GEFUNDEN.
           INITIALIZE SEU-CMD-GEFUNDEN-POS.

       SEU-SUC-20.
           ADD      1 TO IND.
           IF       CMD-SUCHE OF CMD-SUCHE-TAB(IND) = SPACES
                    GO TO SEU-SUC-90
           END-IF.

     ‚* SUCHEN DES TEXTES
           MOVE     ZEROES TO LEN-TMP.
           INSPECT  SEUCMD-WRK TALLYING LEN-TMP FOR CHARACTERS BEFORE
                    INITIAL CMD-SUCHE OF CMD-SUCHE-TAB(IND)(1:
                    CMD-LAENGE OF CMD-SUCHE-TAB(IND)).
           IF       LEN-TMP = LENGTH OF SEUCMD-WRK
                    GO TO SEU-SUC-20
           END-IF.
           IF       LEN-TMP > 1
                    IF   SEUCMD-WRK(LEN-TMP - 1:1) NOT = SPACES
                         GO TO SEU-SUC-20
                    END-IF
                    IF   SEUCMD-WRK(LEN-TMP:1)     NOT = SPACES
                         GO TO SEU-SUC-20
                    END-IF
           END-IF.

           MOVE     1 TO SEU-CMD-GEFUNDEN.
           COMPUTE  SEU-CMD-GEFUNDEN-POS = LEN-TMP + 1.
     ‚* BEFEHL HINZUFÜGEN
           PERFORM  ADD-TO-BEFEHL-PTR.

       SEU-SUC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÖFFNEN EINES NEUEN BILDES
     ‚*--------------------------------------------------------------
       SEU-COMMAND-OPEN SECTION.
       SEU-COP-00.

           INITIALIZE RET-CODE.

       SEU-COP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE NOTIZEN IM SUBFILE ANZEIGEN
     ‚*--------------------------------------------------------------
       SEU-COMMAND-NOTES SECTION.
       SCM-NOT-00.

       SCM-NOT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER ZEILENNUMMER AUS DER EINGABE
     ‚*--------------------------------------------------------------
       SEU-COMMAND-SHOWPTR SECTION.
       SCM-SPT-00.

     ‚* NACH SHOWPTR STEHT DER POINTERNAME
           MOVE     ZEROES TO FIRST-SPACE-POS.
           INSPECT  SEUCMD-WRK(SEU-CMD-GEFUNDEN-POS:)
                    TALLYING FIRST-SPACE-POS FOR CHARACTERS
                    BEFORE INITIAL SPACES.
           ADD      SEU-CMD-GEFUNDEN-POS TO FIRST-SPACE-POS.
           IF       FIRST-SPACE-POS = ZEROES
            OR      FIRST-SPACE-POS = LENGTH OF SEUCMD-WRK
                    MOVE "1" TO RET-CODE
                    GO TO SCM-SPT-90
           END-IF.

     ‚* JETZT DEN NAMEN ERMITTELN
           MOVE     ZEROES TO IND.
           COMPUTE  IND = FIRST-SPACE-POS + 1
           END-COMPUTE.

     ‚* POINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     SEUCMD-WRK(IND:) TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO SCM-SPT-90
           END-IF.

     ‚* JETZT DEN POINTER ANZEIGEN
           CALL     "CFP0050" USING PTR-RETURN OF PTRMAP-REC
           END-CALL.

       SCM-SPT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINER NOTIZ ZU EINER ZEILE
     ‚*--------------------------------------------------------------
       AUSW-N-WRITENOTE SECTION.
       ASN-WNT-00.


       ASN-WNT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DIE SOURCE AUS MAPDTA WIEDER IN DIE SOURCE ZURÜCKSCHREIBEN
     ‚*--------------------------------------------------------------
       SOURCE-SAVE SECTION.
       SRC-SAV-00.

           INITIALIZE RET-CODE.
     ‚* SOURCE SICHERN
           IF       AENDERN-WRK NOT = ZEROES
                    CALL     "CRP0032" USING IMAGE-WRK
                                             CHANGE-SRC-AKTIV
                                             SOURCE-PTR
                                             RET-CODE
                    END-CALL
                    IF   RET-CODE = SPACES
                         MOVE ZEROES TO CHANGE-SRC-AKTIV
                    END-IF
           END-IF.

       SRC-SAV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE JETZT ÄNDERN
     ‚*--------------------------------------------------------------
       SOURCE-CHANGE SECTION.
       SRC-CHG-00.

     ‚* JETZT DIE 01. ZEILE AKTUALISIEREN
           IF       IN50 OF OUTXX = AN
                    MOVE    SLFDN01H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA01  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 02. ZEILE AKTUALISIEREN
           IF       IN51 OF OUTXX = AN
                    MOVE    SLFDN02H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA02  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 03. ZEILE AKTUALISIEREN
           IF       IN52 OF OUTXX = AN
                    MOVE    SLFDN03H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA03  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 04. ZEILE AKTUALISIEREN
           IF       IN53 OF OUTXX = AN
                    MOVE    SLFDN04H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA04  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 05. ZEILE AKTUALISIEREN
           IF       IN54 OF OUTXX = AN
                    MOVE    SLFDN05H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA05  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 06. ZEILE AKTUALISIEREN
           IF       IN55 OF OUTXX = AN
                    MOVE    SLFDN06H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA06  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 07. ZEILE AKTUALISIEREN
           IF       IN56 OF OUTXX = AN
                    MOVE    SLFDN07H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA07  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 08. ZEILE AKTUALISIEREN
           IF       IN57 OF OUTXX = AN
                    MOVE    SLFDN08H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA08  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 09. ZEILE AKTUALISIEREN
           IF       IN58 OF OUTXX = AN
                    MOVE    SLFDN09H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA09  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 10. ZEILE AKTUALISIEREN
           IF       IN59 OF OUTXX = AN
                    MOVE    SLFDN10H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA10  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 11. ZEILE AKTUALISIEREN
           IF       IN60 OF OUTXX = AN
                    MOVE    SLFDN11H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA11  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 12. ZEILE AKTUALISIEREN
           IF       IN61 OF OUTXX = AN
                    MOVE    SLFDN12H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA12  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 13. ZEILE AKTUALISIEREN
           IF       IN62 OF OUTXX = AN
                    MOVE    SLFDN13H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA13  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 14. ZEILE AKTUALISIEREN
           IF       IN63 OF OUTXX = AN
                    MOVE    SLFDN14H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA14  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 15. ZEILE AKTUALISIEREN
           IF       IN64 OF OUTXX = AN
                    MOVE    SLFDN15H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA15  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 16. ZEILE AKTUALISIEREN
           IF       IN65 OF OUTXX = AN
                    MOVE    SLFDN16H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA16  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 17. ZEILE AKTUALISIEREN
           IF       IN66 OF OUTXX = AN
                    MOVE    SLFDN17H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA17  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 18. ZEILE AKTUALISIEREN
           IF       IN67 OF OUTXX = AN
                    MOVE    SLFDN18H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA18  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 19. ZEILE AKTUALISIEREN
           IF       IN68 OF OUTXX = AN
                    MOVE    SLFDN19H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA19  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.
     ‚* JETZT DIE 20. ZEILE AKTUALISIEREN
           IF       IN69 OF OUTXX = AN
                    MOVE    SLFDN20H  OF FORMAT1 TO SOURCE-LFDN
                    MOVE    SRCDTA20  OF FORMAT1 TO SRC-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
           END-IF.

       SRC-CHG-90.
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
     ‚* INITIALISIEREN ALLER VARIABLEN
     ‚*--------------------------------------------------------------
       INIT-VAR SECTION.
       INI-VAR-00.

     ‚* POINTER WERDEN DURCH PTRMAP VERWALTET
           SET      SOURCE-PTR     TO NULL.
           SET      RESTORE-PTR    TO NULL.

           IF       NOTIZEN-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO NOTIZEN-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      NOTIZEN-PTR TO NULL.

           IF       SPRUNG-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SPRUNG-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      SPRUNG-PTR TO NULL.

     ‚* VARIABLEN INITIALISIEREN
           MOVE     AUS TO INXX.
           INITIALIZE AKT-LFDN-MIN, AKT-LFDN-MAX.
           INITIALIZE CHANGE-SRC-AKTIV.

       INI-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE VARIABLEN FÜRS BEENDEN LÖSCHEN
     ‚*--------------------------------------------------------------
       CLEAR-VAR SECTION.
       CLR-VAR-00.

     ‚* POINTER JETZT WIEDER FREIGEBEN
           IF       NOTIZEN-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO NOTIZEN-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
           IF       SPRUNG-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO SPRUNG-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
     ‚* ZENTRALE POINTERVERWATLUNG LEEREN
           SET      PTR-CLEAR OF PTRMAP-REC TO TRUE.
           PERFORM  COPY-PTR-MAPPING.

           SET      SOURCE-PTR      TO NULL.
           SET      RESTORE-PTR     TO NULL.
           SET      NOTIZEN-PTR     TO NULL.
           SET      SPRUNG-PTR      TO NULL.

       CLR-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN EINER BESTIMMTEN ZEILE
     ‚*--------------------------------------------------------------
       GET-ZEILE SECTION.
       GET-ZEI-00.

           INITIALIZE SRC-WRK.
     ‚* LESEN MAPDTA UM DIE ZEILE ZU LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     SOURCE-LFDN        TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           IF       GET-ZEILE-HLP NOT = ZEROES
                    SET MAP-PTR OF MAPDTA-REC TO RESTORE-PTR
           ELSE
                    SET MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
           END-IF.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    MOVE MAP-REC OF MAPDTA-REC(10:) TO SRC-WRK
           END-IF.
           MOVE     ZEROES      TO GET-ZEILE-HLP.

       GET-ZEI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER CURSER-POSITIONIERUNG IM BILDSCHIRM
     ‚*--------------------------------------------------------------
       GET-CURSER-POS SECTION.
       GET-CUR-00.

           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB           TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK         TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK             TO WHNAME OF PCFGFFD-L.
           MOVE     CURFLD OF FORMAT1-I TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-CUR-90
           END-IF.

       GET-CUR-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-CUR-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L NOT = FMT-WRK
            OR      WHFLDE OF PCFGFFD-L NOT = CURFLD OF FORMAT1-I
                    GO TO GET-CUR-90
           END-IF.

     ‚* KOORDINATEN ERMITTELN
           MOVE     WHDROW OF PCFGFFD-L TO CURY OF FORMAT1.
           COMPUTE  CURX OF FORMAT1 =   WHDCOL OF PCFGFFD-L
                                      + CURPOS OF FORMAT1-I
                                      - 1
           END-COMPUTE.

           GO TO    GET-CUR-20.

       GET-CUR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER CURSER-POSITION FÜR INSERT
     ‚*--------------------------------------------------------------
       GET-CURSER-POS-X SECTION.
       GET-CUX-00.

           INITIALIZE RET-CODE.

       GET-CUX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER CURSER-POSITIONIERUNG IM BILDSCHIRM
     ‚*--------------------------------------------------------------
       GET-XMLFFD SECTION.
       GET-XML-00.

           MOVE     ZEROES TO XML-FOUND.
     ‚* ERMITTELN DER XMLFFD-DATEN ZU EINEM FELD
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB           TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK         TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK             TO WHNAME OF PCFGFFD-L.
           MOVE     XML-FELD            TO WHFLDE OF PCFGFFD-L.
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

           IF       WHLIB  OF PCFGFFD-L NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L NOT = FMT-WRK
            OR      WHFLDE OF PCFGFFD-L NOT = XML-FELD
                    INITIALIZE PCFGFFDF OF PCFGFFD-L
                    GO TO GET-XML-90
           END-IF.

     ‚* FELD GEFUNDEN
           MOVE     1 TO XML-FOUND.

       GET-XML-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN EINER SOURCE-ZEILE
     ‚*--------------------------------------------------------------
       SET-ZEILE SECTION.
       SET-ZEI-00.

     ‚* LESEN MAPDTA UM DIE ZEILE ZU SCHREIBEN/AKTUALISIEREN
           INITIALIZE MAPDTA-REC.
           MOVE     SOURCE-LFDN        TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
     ‚* JETZT DIE SRC-WRK IN DEN RECORD SCHIEBEN
           MOVE     SRC-WRK            TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "C"                TO MAP-REC OF MAPDTA-REC(1:1).
           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.
           PERFORM  COPY-CALL-MAPDTA.

     ‚* KENNZEICHEN DAS SOURCE GEÄNDERT WURDE
           INITIALIZE MAPDTA-REC.
           MOVE     "*CHANGE"          TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           MOVE     "*YES"             TO MAP-REC OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.
           PERFORM  COPY-CALL-MAPDTA.

       SET-ZEI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHLS-PTR AUFBAUEN
     ‚*--------------------------------------------------------------
       ADD-TO-BEFEHL-PTR SECTION.
       ADD-TBP-00.

           MOVE     "ADD"          TO 0034-AKTION.
           MOVE     "BEFEHL-0001"  TO PTR-KEY OF PTRMAP-REC.
           CALL     "CRP0034" USING PTR-MAIN OF PTRMAP-REC
                                    PTR-KEY  OF PTRMAP-REC
                                    0034-MAP-LFDN
                                    0034-AKTION
                                    SEUCMD-WRK
           END-CALL.

       ADD-TBP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* RUNTERSCROLLEN
     ‚*--------------------------------------------------------------
       SCROLL-DOWN SECTION.
       SCR-DWN-00.

     ‚* STANDARDSCROLLEN
           MOVE     1               TO NEUANZEIGE-WRK.
           MOVE     AKT-LFDN-MAX    TO START-LFDN.
           ADD      1               TO START-LFDN.

       SCR-DWN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HOCHSCROLLEN
     ‚*--------------------------------------------------------------
       SCROLL-UP SECTION.
       SCR-LUP-00.

     ‚* STANDARDSCROLLEN
           MOVE     1               TO NEUANZEIGE-WRK.
           SUBTRACT MAX-ZEILEN FROM START-LFDN.

       SCR-LUP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL AUSFÜHREN
     ‚*--------------------------------------------------------------
       AUSW-EXECUTE SECTION.
       AUS-EXC-00.

           INITIALIZE RET-CODE.

       AUS-EXC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LETZTEN BEFEHL ERMITTELN
     ‚*--------------------------------------------------------------
       GET-LAST-COMMAND SECTION.
       GET-LCD-00.

           MOVE     "GET"          TO 0034-AKTION.
           MOVE     "BEFEHL-0001"  TO PTR-KEY OF PTRMAP-REC.
           CALL     "CRP0034" USING PTR-MAIN OF PTRMAP-REC
                                    PTR-KEY  OF PTRMAP-REC
                                    0034-MAP-LFDN
                                    0034-AKTION
                                    SEUCMD-WRK
           END-CALL.

       GET-LCD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB IN EINER ZEILE EINE AUSWAHL DURCHGEFÜHRT WERDEN SOLL
     ‚*--------------------------------------------------------------
       CHECK-AUSWAHL SECTION.
       CHK-AUS-00.

           INITIALIZE AUSW-WRK.
     ‚* JEDES FELD PRÜFEN UND AUSWAHL SICHERN
           IF       AUSW01 OF FORMAT1 NOT = SPACES
                    MOVE AUSW01    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN01H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW02 OF FORMAT1 NOT = SPACES
                    MOVE AUSW02    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN02H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW03 OF FORMAT1 NOT = SPACES
                    MOVE AUSW03    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN03H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW04 OF FORMAT1 NOT = SPACES
                    MOVE AUSW04    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN04H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW05 OF FORMAT1 NOT = SPACES
                    MOVE AUSW05    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN05H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW06 OF FORMAT1 NOT = SPACES
                    MOVE AUSW06    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN06H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW07 OF FORMAT1 NOT = SPACES
                    MOVE AUSW07    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN07H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW08 OF FORMAT1 NOT = SPACES
                    MOVE AUSW08    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN08H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW09 OF FORMAT1 NOT = SPACES
                    MOVE AUSW09    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN09H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW10 OF FORMAT1 NOT = SPACES
                    MOVE AUSW10    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN10H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW11 OF FORMAT1 NOT = SPACES
                    MOVE AUSW11    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN11H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW12 OF FORMAT1 NOT = SPACES
                    MOVE AUSW12    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN12H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW13 OF FORMAT1 NOT = SPACES
                    MOVE AUSW13    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN13H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW14 OF FORMAT1 NOT = SPACES
                    MOVE AUSW14    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN14H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW15 OF FORMAT1 NOT = SPACES
                    MOVE AUSW15    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN15H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW16 OF FORMAT1 NOT = SPACES
                    MOVE AUSW16    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN16H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW17 OF FORMAT1 NOT = SPACES
                    MOVE AUSW17    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN17H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW18 OF FORMAT1 NOT = SPACES
                    MOVE AUSW18    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN18H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW19 OF FORMAT1 NOT = SPACES
                    MOVE AUSW19    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN19H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW20 OF FORMAT1 NOT = SPACES
                    MOVE AUSW20    OF FORMAT1 TO AUSW-WRK
                    MOVE SLFDN20H  OF FORMAT1 TO AUSW-LFDN
                    PERFORM AUSW-EXECUTE
           END-IF.

       CHK-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EINE NOTIZ VORHANDEN IST
     ‚*--------------------------------------------------------------
       CHECK-NOTIZ-VORHANDEN SECTION.
       CHK-NVO-00.

           INITIALIZE RET-CODE.

       CHK-NVO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB AN DER SOURCE ETWAS GEÄNDERT WURDE
     ‚*--------------------------------------------------------------
       CHECK-CHANGE SECTION.
       CHK-CHG-00.

           INITIALIZE CHANGE-AKTIV.
     ‚* WURDE EINE EINGABE IN DEN ZEILEN GEMACHT?
           IF       IN50 OF OUTXX = AN
            OR      IN51 OF OUTXX = AN
            OR      IN52 OF OUTXX = AN
            OR      IN53 OF OUTXX = AN
            OR      IN54 OF OUTXX = AN
            OR      IN55 OF OUTXX = AN
            OR      IN56 OF OUTXX = AN
            OR      IN57 OF OUTXX = AN
            OR      IN58 OF OUTXX = AN
            OR      IN59 OF OUTXX = AN
            OR      IN60 OF OUTXX = AN
            OR      IN61 OF OUTXX = AN
            OR      IN62 OF OUTXX = AN
            OR      IN63 OF OUTXX = AN
            OR      IN64 OF OUTXX = AN
            OR      IN65 OF OUTXX = AN
            OR      IN66 OF OUTXX = AN
            OR      IN67 OF OUTXX = AN
            OR      IN68 OF OUTXX = AN
            OR      IN69 OF OUTXX = AN
                    MOVE 1 TO CHANGE-AKTIV
           END-IF.

       CHK-CHG-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
