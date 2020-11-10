       IDENTIFICATION DIVISION.
     ‚*
      *    UNDO FUNKTION MIT CHANGELOG
      *    KLEINBUCHSTABEN
      *    SYNTAX
      *    GÜLTIGKEITSPRÜFUNG EINGABEN
     ‚*
         PROGRAM-ID.              DMP0020.
     ‚*
            COPY COPYRIGHTC OF QCPYSRC.
     ‚*
     ‚*****************************************************************
     ‚* LAGERFÜHRUNGSSYSTEM  LFS 400                                  *
     ‚*---------------------------------------------------------------*
     ‚* COPYRIGHT BY  :  EHRHARDT + PARTNER GMBH & CO. KG             *
     ‚*                  SOFTWARE-SYSTEME FÜR WAREHOUSE-LOGISTIK      *
     ‚*                  56154 BOPPARD-BUCHHOLZ                       *
     ‚*                  +49 (0) 67 42 / 87 27 0                      *
     ‚*                  INFO@EHRHARDT-PARTNER.COM                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :           B. GERLICH/A. MUTTSCHELLER          *
     ‚*                                                               *
     ‚* ERSTELLT AM   :           AUGUST 2006                         *
     ‚*                                                               *
     ‚* PROBLEM       :           ERSATZ FÜR DEN IBM SEU              *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON            GRUND DER ÄNDERUNG                  *
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
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-DMD0020DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY
            LABEL RECORDS ARE OMITTED.
       01   WS-REC.
            COPY   DDS-ALL-FORMATS   OF DMD0020DE.
      /
       WORKING-STORAGE SECTION.
     ‚*-   SOURCENDATEI
       01 QSETSRC-P-X.
         02 DB-INFO-DESC          PIC X(20) VALUE "QSETSRC-DP".
         02 FILLER                PIC X(12).
         02 QSETSRC-DP            USAGE POINTER.
     ‚* QDBCPY INHALT
         02 DB-INFO-FILE          PIC X(10).
         02 DB-INFO-PGM           PIC X(20).
         02 DB-INFO-POS           PIC S9(4) BINARY VALUE -1.
         02 DB-INFO-MODE          PIC X(1).
         02 DB-INFO-OPEN          PIC X(1).
         02 DB-INFO-LVLCHK        PIC X(13).
         02 DB-INFO-UNIQUE        PIC 9(1) VALUE 0.

         02 QSETSRC-P.
          05 SRCSEQ               PIC 9(4)V9(2).
          05 SRCDAT               PIC 9(6).
          05 SRCDTA               PIC X(150).

     ‚*-   PXMLFFD
       01 PXMLFFD-P-X.
         02 DB-INFO-DESC          PIC X(20) VALUE "PXMLFFD-DP".
         02 FILLER                PIC X(12).
         02 PXMLFFD-DP            USAGE POINTER.
            COPY PXMLFFD    OF QDBCPY.
         02 PXMLFFD-P.
            COPY DDS-ALL-FORMATS OF PXMLFFD.

     ‚*-   PSETLOG - LOGDATEI FÜR UNDO/REDO
       01 PSETLOG-P-X.
         02 DB-INFO-DESC          PIC X(20) VALUE "PSETLOG-DP".
         02 FILLER                PIC X(12).
         02 PSETLOG-DP            USAGE POINTER.
            COPY PSETLOG    OF QDBCPY.
         02 PSETLOG-P.
            COPY DDS-ALL-FORMATS OF PSETLOG.

       01  PGM-WRK                        PIC X(10) VALUE "DMP0020   ".

           COPY COPYRIGHT OF QCPYSRC.

     ‚*  WORKBEREICH STANDARDFELDER DIALOGPROGRAMME
           COPY WRKSTDBS OF QCPYSRC.

     ‚* WORKBEREICH HANDLING DISPLAY
           COPY WRKDISPLAY OF QCPYSRC.

     ‚*  WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF QCPYSRC.

     ‚*-----Temporäre Indikator------
       01  INXX.
         02 IN51                          PIC 1.

       01  INXX-SEQ.
         02 IN30                          PIC 1.
         02 IN31                          PIC 1.
         02 IN32                          PIC 1.
         02 IN33                          PIC 1.
         02 IN34                          PIC 1.
         02 IN35                          PIC 1.
         02 IN36                          PIC 1.
         02 IN37                          PIC 1.
         02 IN38                          PIC 1.
         02 IN39                          PIC 1.
         02 IN40                          PIC 1.
         02 IN41                          PIC 1.
         02 IN42                          PIC 1.
         02 IN43                          PIC 1.
         02 IN44                          PIC 1.
         02 IN45                          PIC 1.
         02 IN46                          PIC 1.
         02 IN47                          PIC 1.
         02 IN48                          PIC 1.
         02 IN49                          PIC 1.
         02 IN50                          PIC 1.

       01  INXX-DTA.
         02 IN60                          PIC 1.
         02 IN61                          PIC 1.
         02 IN62                          PIC 1.
         02 IN63                          PIC 1.
         02 IN64                          PIC 1.
         02 IN65                          PIC 1.
         02 IN66                          PIC 1.
         02 IN67                          PIC 1.
         02 IN68                          PIC 1.
         02 IN69                          PIC 1.
         02 IN70                          PIC 1.
         02 IN71                          PIC 1.
         02 IN72                          PIC 1.
         02 IN73                          PIC 1.
         02 IN74                          PIC 1.
         02 IN75                          PIC 1.
         02 IN76                          PIC 1.
         02 IN77                          PIC 1.
         02 IN78                          PIC 1.
         02 IN79                          PIC 1.
         02 IN80                          PIC 1.

     ‚*  WORKBEREICH STANDARDFELDER ALLE PROGRAMME
           COPY WRKSTDFLD OF QCPYSRC.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  DMD0020DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  DMD0020DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  DMD0020DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  DMD0020DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT6X.
           COPY  DDS-FMT06-O OF  DMD0020DE
           REPLACING FMT06-O BY FORMAT6
                     FMT06-O-INDIC BY FORMAT6-O-INDIC.

       01  FORMAT6Y.
           COPY  DDS-FMT06-I OF  DMD0020DE
           REPLACING FMT06-I BY FORMAT6-I
                     FMT06-I-INDIC BY FORMAT6-I-INDIC.

       01  VON-ERSETZ-STRING            PIC X(100)  VALUE SPACES.
       01  NACH-ERSETZ-STRING           PIC X(100)  VALUE SPACES.
       01  ERSETZ-WRK                   PIC X(250) VALUE SPACES.
       01  BS-START                     PIC S9(5) COMP.
       01  ZEILE                        PIC 9(5) COMP.
       01  ZEILE-SAV                    PIC 9(5) COMP.
       01  SETCMD-WRK                   LIKE SETCMD   OF FORMAT6.
       01  FILENAME-WRK                 LIKE FILENAME OF FORMAT6.
       01  LEN                          PIC 9(5) COMP.
       01  POS                          PIC 9(7) COMP.
       01  ROW-COUNT                    PIC 9(2).
       01  LEN2                         PIC 9(2).
       01  LEN3                         PIC 9(4).
       01  IDX                          PIC 9(4).
       01  UNDO-WRK                     PIC X(1).

     ‚* TABELLE MIT ZEILEN DER SOURCE
       01  SOURCE-POS                   PIC 9(5).
       01  SOURCE-MAX                   PIC 9(5).
       01  SOURCE-TAB.
        05  SOURCE-REC.
         10  AUSWAHL                    LIKE SRCSEQ01 OF FORMAT6.
         10  SRCSEQ                     LIKE SRCSEQ OF QSETSRC-P.
         10  SRCSEQ-RED REDEFINES SRCSEQ PIC 9(6).
         10  SRCDAT                     LIKE SRCDAT OF QSETSRC-P.
         10  SRCDTA                     LIKE SRCDTA OF QSETSRC-P.
         10  ZEILE-NEU                  PIC 9(1).

     ‚* TABELLE WIRD FÜR ERMITTLUNG DER LVLCHECK ID BENÖTIGT
       01 FMTID-TAB.
          05 TAB-ANZ-FMT                 PIC 9(03).
          05 FMTID-REC OCCURS 1 TO 100 TIMES DEPENDING ON TAB-ANZ-FMT.
            10 FMT-NAME                  PIC X(10).
            10 FMT-ID                    PIC X(13).

     ‚* TABELLE MIT FELDINFORMATIONEN
       01  XMLFFD-TAB.
        05 SRCTYP-REC   OCCURS   3 TIMES.
         10  SRCSEQ-REC OCCURS 100 TIMES.
          15 VPOS                       LIKE WHFOBO OF PXMLFFD-P.
          15 SLEN                       LIKE WHFLDB OF PXMLFFD-P.

     ‚* TABELLE MIT USERAUSWAHLEN
       01  USRCMD-TAB.
        05 USRCMD-REC OCCURS 50000 TIMES.
         10 AUSW-TYP                    PIC 9(3).
         10 SEQ-FMT                     LIKE SRCSEQ01 OF FORMAT6.
         10 SEQ-ACT                     LIKE SRCSEQ01 OF FORMAT6.
       01  USRCMD-LEN                   PIC 9(5).

       01  SEQ-WRK                      LIKE SRCSEQ01 OF FORMAT6.
       01  SEQ-ACT-WRK                  LIKE SRCSEQ01 OF FORMAT6.
       01  SRCDTA-WRK                   LIKE SRCDTA OF QSETSRC-P.
       01  MBR-WRK                      PIC X(10).
       01  FILE-WRK                     PIC X(10).
       01  LIB-WRK                      PIC X(10).
       01  READONLY-WRK                 PIC 9(1).

       01  SOURCE-CHANGED               PIC X(1).
       01  SOURCE-CHANGED-SAVE          PIC X(1).
       01  SOURCE-CHANGED-LINUX         PIC X(1).

       01  OPEN-CLOSE                   PIC X(1).
       01  SUCH-TYP                     PIC X(1).
       01  DATENENDE-WRK                PIC X(1).
       01  ERRMSG-TXT                   PIC X(78).
       01  OFFSET-WRK                   PIC S9(5) COMP.
       01  JA-NEIN                      PIC 9(1).
       01  RETCODE-SAV                  LIKE RET-CODE.
       01  FIRST-READ                   PIC X(1).
       01  DURCHGELESEN-WRK             PIC X(1).
       01  CHOICE-CHANGED               PIC X(1).
       01  SEARCH-REC                   PIC X(50).
       01  BLOCK-ID                     LIKE LFD-SLID-NR.
       01  DUMMY-10                     PIC X(10).
       01  SOURCE-PTR                   USAGE POINTER.
       01  BEFEHL-PTR                   USAGE POINTER.
       01  BEFEHL-MAX                   PIC 9(9).
       01  BEFEHL-WRK                   PIC 9(9).
       01  AKTION-WRK                   PIC X(1).
       01  GROSS-KLEIN-WRK              PIC X(1).
       01  DISPLAY-WRK-21               LIKE DISPLAY-WRK.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-MBR                        PIC X(10).
       01  X-FILE                       PIC X(10).
       01  X-LIB                        PIC X(10).
       01  X-READONLY                   PIC X(1).
       01  X-RETCODE                    PIC X(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-MBR
                                X-FILE
                                X-LIB
                                X-READONLY
                                X-RETCODE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* Standard INIT-Routine aufrufen
           PERFORM  PGM-INIT.

     ‚* ÖFFNEN BILDSCHIRM UND ABFANGEN FEHLER BEI ANDERER SPRACHE
           MOVE     "DMD0020DE" TO DISPLAY-WRK.
           MOVE     "DMD0021DE" TO DISPLAY-WRK-21.
           MOVE     "DMP0020" TO PGM-WRK.
           PERFORM  DISPLAY-OPEN.

           INITIALIZE X-RETCODE.

     ‚* VARIABELN IM DMP0026 LEEREN, DIE BEI DEM AUFRRUF DES SET
     ‚* INITIALISIERT WERDEN MÜSSEN!
           INITIALIZE UNDO-WRK.
           INITIALIZE DUMMY-10.
           CALL     "DMP0026" USING DUMMY-10
                                    DUMMY-10
                                    DUMMY-10
                                    MAP-POINTER OF LGMAPDTA-REC
                                    USRCMD-TAB
                                    SOURCE-MAX
                                    DSP-CUR-X
                                    DSP-CUR-Y
                                    UNDO-WRK
                                    BS-START
                                    RETCODE-SAV
           END-CALL.

     ‚* WURDEN PARAMETER RICHTIG ÜBERGEBEN?
           IF       X-MBR  = SPACES
            OR      X-FILE = SPACES
            OR      X-LIB  = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* OVR DURCHFÜHREN + GGF. SOURCE SPERREN
           MOVE     X-MBR      TO MBR-WRK.
           MOVE     X-FILE     TO FILE-WRK.
           MOVE     X-LIB      TO LIB-WRK.
           MOVE     X-READONLY TO READONLY-WRK.
           MOVE     "O"        TO OPEN-CLOSE.
           CALL     "DMP0023" USING MBR-WRK
                                    FILE-WRK
                                    LIB-WRK
                                    READONLY-WRK
                                    OPEN-CLOSE
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    MOVE "2" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* DB-MAIN INFORMATIONEN ERMITTELN
           PERFORM  GET-LVLCHECK.

           MOVE     "INPUT" TO DB-OPENMODE.
           SET      DB-PTR TO ADDRESS OF QSETSRC-P-X.
           PERFORM  DB-OPEN.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "3" TO X-RETCODE
                    GO TO ENDE
           END-IF.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    MOVE "INPUT" TO DB-OPENMODE
                         SET DB-PTR TO ADDRESS OF PXMLFFD-P-X
                         PERFORM  DB-OPEN
                    MOVE "IO"    TO DB-OPENMODE
                         SET DB-PTR TO ADDRESS OF PSETLOG-P-X
                         PERFORM  DB-OPEN
           END-IF.

     ‚* FELDINFORMATIONEN LESEN
           INITIALIZE XMLFFD-TAB.
           PERFORM  LESEN-PXMLFFD.
           DIVIDE   ROW-COUNT BY 3 GIVING ROW-COUNT.

     ‚* VARIABELN INITIALISIEREN
           SET      MAP-POINTER OF LGMAPDTA-REC TO NULL.
           SET      SOURCE-PTR                  TO NULL.
           SET      BEFEHL-PTR                  TO NULL.
           MOVE     AUS  TO INXX.
           MOVE     1   TO BS-START.
           MOVE     "S" TO SUCH-TYP.
           MOVE     "1" TO GROSS-KLEIN-WRK.
           INITIALIZE DATEN.
           INITIALIZE ZEILE.
           INITIALIZE FORMAT1.
           INITIALIZE FORMAT2.
           INITIALIZE FORMAT6.
           INITIALIZE SRCDTA-WRK.
           INITIALIZE BS-START.
           INITIALIZE BEFEHL-MAX.
           INITIALIZE BEFEHL-WRK.
           INITIALIZE SOURCE-TAB.
           INITIALIZE DATENENDE-WRK.
           INITIALIZE SOURCE-CHANGED.
           INITIALIZE SOURCE-CHANGED-SAVE.
           INITIALIZE SOURCE-CHANGED-LINUX.
           INITIALIZE DURCHGELESEN-WRK.
           INITIALIZE USRCMD-TAB.
           INITIALIZE CHOICE-CHANGED.
           INITIALIZE FILENAME-WRK.
           INITIALIZE SETCMD-WRK.
           INITIALIZE USRCMD-TAB.
           INITIALIZE SOURCE-MAX.
           INITIALIZE FIRST-READ.

           MOVE    10  TO AUSZEIL OF FORMAT1.
           MOVE     1  TO GROSS   OF FORMAT2.
           MOVE    "F" TO SCROLL  OF FORMAT2.
           ADD      1  TO SOURCE-MAX.
           INITIALIZE SOURCE-REC.
           MOVE     ZEROES  TO SRCSEQ OF SOURCE-REC.
           MOVE     ALL "*" TO SRCDTA OF SOURCE-REC.
           MOVE     ZEROS   TO SRCDAT OF SOURCE-REC.

     ‚* LESEN DER TEILDATEI
           PERFORM  LESEN-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    MOVE RET-CODE TO X-RETCODE
                    GO TO ENDE
           END-IF.
           IF       DATEN = ZERO
                    MOVE "5" TO X-RETCODE
                    GO TO ENDE
           END-IF.

           MOVE     AUS TO INXX.

     ‚* PRÜFEN OB DIE TEILDATEI WIEDERHERGESTELLT WERDEN KANN
           IF       X-READONLY NOT = "1"
                    PERFORM WIEDERHERSTELLEN
                    IF   JA-NEIN = ZERO
                         PERFORM CLEAR-PSETLOG
                    END-IF
                    IF   F12 = "1"
                         GO TO ENDE
                    END-IF
           END-IF.

       ANF010.
     ‚* DATEI AUSGEBEN
           MOVE     ZERO TO BS-START.
           PERFORM  AUSGABE-SUBFILE.
           IF       F3 = "1"
            AND     SOURCE-CHANGED = "*"
                    MOVE "PGT0002" TO PGM-TEXT-ID
                    MOVE ZERO TO JA-NEIN
                    CALL "LGPGM033" USING PGM-TEXT-ID
                                          JA-NEIN
                                          F12
                    END-CALL
                    IF   JA-NEIN = "1"
                         PERFORM SAVE-SOURCE
                         IF   ERRMSG-TXT NOT = SPACES
                              GO TO ANF010
                         END-IF
                    END-IF
                    IF   F12 = "1"
                         GO TO ANF010
                    END-IF
           END-IF.

       ENDE.
     ‚*  ARRAY FREIGEBEN, OVR ENTFERNEN UND TEILDATEI ENTSPERREN
           PERFORM  AKTION-ABSCHLIESSEN.

           PERFORM  DISPLAY-CLOSE.

     ‚* Standard EXIT-Routine aufrufen
           PERFORM  PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE AUSGEBEN
     ‚*--------------------------------------------------------------
       AUSGABE-SUBFILE SECTION.
       AUS-SFL-00.

           INITIALIZE FORMAT6.
     ‚* ZUERST AM ANFANG DES RECORDS AUFSETZEN
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.

     ‚* DANN DIE SÄTZE LESEN
           PERFORM  AUSGABE-ZEILEN.

       AUS-SFL-20.
           PERFORM  AUSGABE-HEADER.
           MOVE     CORR    INXX      TO  FORMAT6-O-INDIC.
           MOVE     "FMT06" TO FMT-WRK.
           MOVE     FORMAT6 TO WSREC-WRK.
           PERFORM  DISPLAY-WRITE.
           PERFORM  DISPLAY-READ.
           MOVE     WSREC-WRK            TO FORMAT6-I.
           MOVE     CORR FORMAT6-I       TO FORMAT6.
           MOVE     CORR FORMAT6-I-INDIC TO INXX-DTA.
           MOVE     CORR FORMAT6-I-INDIC TO INXX-SEQ.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.
           PERFORM  DISPLAY-CURSER.
           MOVE     CURSER-ZEILE  TO DSP-CUR-Y.
           MOVE     CURSER-SPALTE TO DSP-CUR-X.

           INITIALIZE ERRMSG-TXT.

     ‚* BEENDEN
           IF       IN03 OF FORMAT6-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO AUS-SFL-90
           END-IF.

     ‚* AKTUALISIEREN
           IF       IN05 OF FORMAT6-I-INDIC = AN
                    INITIALIZE USRCMD-TAB
                    GO TO AUS-SFL-00
           END-IF.

     ‚* BEFEHL
           IF       IN09 OF FORMAT6-I-INDIC = AN
            AND     BEFEHL-PTR NOT = NULL
     ‚*             POINTER AUSTAUSCHEN
                    SET SOURCE-PTR  TO MAP-POINTER  OF LGMAPDTA-REC
                    SET MAP-POINTER OF LGMAPDTA-REC TO BEFEHL-PTR
     ‚*             STRING LESEN
                    MOVE "SETCMD" TO DSP-CUR-FLD
                    MOVE MAP-READ TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE BEFEHL-WRK  TO MAP-KEY OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
                    IF   RET-CODE NOT = SPACES
                         MOVE BEFEHL-MAX TO BEFEHL-WRK
                    END-IF
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO
                                    SETCMD OF FORMAT6
                    SUBTRACT 1 FROM BEFEHL-WRK
     ‚*             POINTER ZURÜCKSETZTEN
                    SET BEFEHL-PTR  TO MAP-POINTER  OF LGMAPDTA-REC
                    SET MAP-POINTER OF LGMAPDTA-REC TO SOURCE-PTR
                    GO TO AUS-SFL-20
           END-IF.

     ‚* ABBRECHEN
           IF       IN12 OF FORMAT6-I-INDIC = AN
                    MOVE "1" TO F12
                    IF   X-READONLY = "1"
                         GO TO AUS-SFL-90
                    END-IF
                    GO TO AUS-SFL-20
           END-IF.

     ‚* WENN ETWAS IN DIE BEFEHLSZEILE GESCHRIEBEN WURDE,
     ‚* DANN DEN STRING SICHERN
           IF       SETCMD OF FORMAT6 NOT = SPACES
     ‚*             POINTER AUSTAUSCHEN
                    SET SOURCE-PTR  TO MAP-POINTER  OF LGMAPDTA-REC
                    SET MAP-POINTER OF LGMAPDTA-REC TO BEFEHL-PTR
     ‚*             STRING SICHERN
                    PERFORM TRIM-COMMAND
                    ADD  1   TO BEFEHL-MAX
                    MOVE BEFEHL-MAX TO BEFEHL-WRK
                    MOVE MAP-WRITE TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SETCMD OF FORMAT6 TO
                         MAP-RECORD  OF LGMAPDTA-REC
                    MOVE BEFEHL-MAX TO MAP-KEY OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
     ‚*             POINTER ZURÜCKSETZTEN
                    SET BEFEHL-PTR  TO MAP-POINTER  OF LGMAPDTA-REC
                    SET MAP-POINTER OF LGMAPDTA-REC TO SOURCE-PTR
           END-IF.

     ‚* ANZEIGE SET EINSTELLUNGEN
           IF       IN13 OF FORMAT6-I-INDIC = AN
                    PERFORM ANZEIGE-SET-EINSTELLUNGEN
                    GO TO AUS-SFL-00
           END-IF.

     ‚* SUCHEN/ÄNDERN
           IF       IN14 OF FORMAT6-I-INDIC = AN
                    PERFORM SUCHEN-ERSETZTEN-SOURCE
                    GO TO AUS-SFL-00
           END-IF.

     ‚* SUCHEN
           IF       IN16 OF FORMAT6-I-INDIC = AN
                    IF   SETCMD OF FORMAT6 NOT = SPACES
                         MOVE SETCMD OF FORMAT6 TO SETCMD-WRK
                    END-IF
     ‚*€            PERFORM TRIM-COMMAND
                    MOVE SETCMD-WRK(1:50) TO SEARCH-REC
                    IF   SEARCH-REC = SPACES
                     AND SUCH-TYP   = "S"
                         MOVE "LFS0301" TO DSP-ERR-ID
                         PERFORM ADD-ERR-TAB
                         GO TO AUS-SFL-20
                    END-IF
                    PERFORM SEARCH-SOURCE
                    GO TO AUS-SFL-00
           END-IF.

     ‚* ERSETZEN
           IF       IN17 OF FORMAT6-I-INDIC = AN
                    IF   EDIT OF FORMAT1 = SPACES
                         GO TO AUS-SFL-00
                    END-IF
                    PERFORM CHANGE-SOURCE
                    GO TO AUS-SFL-00
           END-IF.

     ‚* SYSTEMBEFEHL
           IF       IN21 OF FORMAT6-I-INDIC = AN
                    CALL "QCMD"
                    GO TO AUS-SFL-00
           END-IF.

     ‚* UNDO
           IF       IN22 OF FORMAT6-I-INDIC = AN
                    CALL "DMP0024" USING X-MBR
                                         X-FILE
                                         X-LIB
                                         MAP-POINTER
                                             OF LGMAPDTA-REC
                                         SOURCE-MAX
                                         DSP-CUR-X
                                         DSP-CUR-Y
                                         BS-START
                                         ROW-COUNT
                                         RETCODE-SAV
                    END-CALL
                    GO TO AUS-SFL-00
           END-IF.

     ‚* REDO
           IF       IN23 OF FORMAT6-I-INDIC = AN
                    INITIALIZE AKTION-WRK
                    CALL "DMP0025" USING X-MBR
                                         X-FILE
                                         X-LIB
                                         MAP-POINTER
                                             OF LGMAPDTA-REC
                                         SOURCE-MAX
                                         DSP-CUR-X
                                         DSP-CUR-Y
                                         AKTION-WRK
                                         BS-START
                                         ROW-COUNT
                                         RETCODE-SAV
                    END-CALL
                    GO TO AUS-SFL-00
           END-IF.

     ‚* IST IN EINER NEUEN ZEILE ETWAS EINGEGEBEN WORDEN?
           IF       RETCODE-SAV = "I"
                    ADD  1 TO ZEILE-SAV
                    IF   ZEILE-SAV > LENGTH OF INXX-DTA
                         MOVE LENGTH OF INXX-DTA TO ZEILE-SAV
                    END-IF
                    IF   INXX-DTA(ZEILE-SAV:1) = "1"
     ‚*                  IN DIE EINGEFÜGTE ZEILE EIN "I" SCHREIBEN
     ‚*                  DAMIT EINE NEUE ZEILE ERSTELLT WIRD
                         MOVE "I" TO FORMAT6(VPOS(1, ZEILE-SAV):
                                             SLEN(1, ZEILE-SAV))
                         INITIALIZE RETCODE-SAV
                    ELSE
                         MOVE "D" TO FORMAT6(VPOS(1, ZEILE-SAV):
                                             SLEN(1, ZEILE-SAV))
                         INITIALIZE RETCODE-SAV
                    END-IF
                    MOVE 1 TO INXX-SEQ(ZEILE-SAV:1)
           END-IF.

     ‚* AUSWERTEN DER GEÄNDERTEN SOURCE
           IF       READONLY-WRK NOT = 1
            AND     INXX-DTA     NOT = ZEROS
                    PERFORM CHECK-SOURCECHANGE
           END-IF.

     ‚* AUSWERTEN DER GEÄNDERTEN AUSWAHL
           IF       INXX-SEQ     NOT = ZEROS
                    PERFORM CHECK-CHOICECHANGE
           END-IF.

     ‚* AUSWAHLEN ABFRAGEN VOR DEM SCROLLEN
           IF       SETCMD OF FORMAT6 > SPACES
     ‚*€            PERFORM TRIM-COMMAND
                    EVALUATE SETCMD OF FORMAT6
                     WHEN "BOTTOM"
                     WHEN "B"
                     WHEN "END"
                     WHEN "E"      MOVE SPACES TO SETCMD OF FORMAT6
                                   COMPUTE BS-START = SOURCE-MAX - 23
                                   GO TO AUS-SFL-00
                     WHEN "TOP"
                     WHEN "T"
                     WHEN "ANFANG"
                     WHEN "A"      MOVE SPACES TO SETCMD OF FORMAT6
                                   MOVE ZERO TO BS-START
                                   GO TO AUS-SFL-00
                     WHEN "SAVE"   PERFORM SAVE-SOURCE
                     WHEN OTHER    MOVE "UNKNOWN COMMANDS" TO ERRMSG-TXT
                    END-EVALUATE
           END-IF.

     ‚* FEHLER AUFGETRETEN?
           IF       ERRMSG-TXT > SPACES
                    MOVE "*FIX"     TO DSP-ERR-ID
                    MOVE ERRMSG-TXT TO LFSMSG OF FORMAT6
                    PERFORM ADD-ERR-TAB
                    GO TO AUS-SFL-20
           END-IF.

     ‚* PAGEUP
           IF       IN26 OF FORMAT6-I-INDIC  = AN
                    IF   BS-START - ROW-COUNT >= ZEROES
                         SUBTRACT ROW-COUNT FROM BS-START
                    ELSE
                         MOVE ZERO TO BS-START
                    END-IF
           END-IF.

     ‚* PAGEDOWN
           IF       IN25 OF FORMAT6-I-INDIC  = AN
                    ADD  ROW-COUNT TO BS-START
                    IF   BS-START + ROW-COUNT >
                         SOURCE-MAX - 2 - (ROW-COUNT / 2)
                         COMPUTE BS-START = SOURCE-MAX
                                          - 2
                                          - (ROW-COUNT
                                             / 2)
                    END-IF
           END-IF.

           GO TO    AUS-SFL-00.

       AUS-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE SFL-ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-ZEILEN SECTION.
       AUS-ZEI-00.

           MOVE     1 TO ZEILE.
           IF       BS-START > SOURCE-MAX
                    MOVE ZERO TO BS-START
           END-IF.

           IF       BS-START > SOURCE-MAX - 2 - (ROW-COUNT / 2)
                    COMPUTE BS-START = SOURCE-MAX
                                     - 2
                                     - (ROW-COUNT / 2)
           END-IF.

           IF       BS-START < ZEROS
                    MOVE ZEROS TO BS-START
           END-IF.

     ‚* ZUERST AM ANFANG DES RECORDS AUFSETZEN
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     BS-START      TO MAP-LFDN   OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

           PERFORM  ROW-COUNT TIMES
                    MOVE MAP-READ-NEXT TO MAP-ACTION OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC
                    IF   RET-CODE NOT = SPACES
                         INITIALIZE SOURCE-REC
                         IF   DATENENDE-WRK = SPACES
                              MOVE ALL "*" TO SRCDTA OF SOURCE-REC
                              MOVE "1" TO DATENENDE-WRK
                         END-IF
     ‚*                  SEQ-NR PROTECTEN
                         INITIALIZE LEN2
                         ADD    ZEILE TO LEN2
                         STRING "SRCSEQ" DELIMITED BY SIZE
                                LEN2     DELIMITED BY SIZE
                         INTO   DSP-ATR-FLD
                         MOVE   1 TO DSP-ATR-PR
                         PERFORM ADD-ATR-TAB
     ‚*                  DATEN PROTECTEN
                         STRING "SRCDTA" DELIMITED BY SIZE
                                LEN2     DELIMITED BY SIZE
                         INTO   DSP-ATR-FLD
                         MOVE   1 TO DSP-ATR-PR
                         PERFORM ADD-ATR-TAB
                    END-IF
     ‚*             QUELLCODE FÜLLEN
                    MOVE SRCDTA OF SOURCE-REC TO
                         FORMAT6(VPOS(2, ZEILE):SLEN(2, ZEILE))
     ‚*             SEQUENZNUMMER FÜLLEN
                    PERFORM FILL-SEQ-NUMBER
     ‚*             DATUM FÜLLEN
                    MOVE SRCDAT OF SOURCE-REC TO
                         FORMAT6(VPOS(3, ZEILE):SLEN(3, ZEILE))
     ‚*             WENN DAS DATENENDE ERREICHT IST, SRCSEQ UND DATUM
     ‚*             MIT SPACES FÜLLEN
                    IF   RET-CODE NOT = SPACES
                         INITIALIZE SOURCE-REC
                         MOVE SPACES TO
                              FORMAT6(VPOS(1, ZEILE):SLEN(1, ZEILE))
                         MOVE SPACES TO
                              FORMAT6(VPOS(3, ZEILE):SLEN(3, ZEILE))
                    END-IF
     ‚*             WENN EINE NEUE ZEILE EINGEFÜGT WURDE, INSERT
     ‚*             AN DEN RAND SCHREIBEN
     ‚*€            IF   ZEILE-NEU OF SOURCE-REC IS NUMERIC
     ‚*€             AND ZEILE-NEU OF SOURCE-REC > ZEROES
     ‚*€                 MOVE X"22"    TO
     ‚*€                      FORMAT6(VPOS(2, ZEILE) + 79 :1)
     ‚*€                 MOVE "INSERT" TO
     ‚*€                      FORMAT6(VPOS(2, ZEILE) + 80:6)
     ‚*€                 INITIALIZE ZEILE-NEU OF SOURCE-REC
     ‚*€                 MOVE SOURCE-REC TO
     ‚*€                      MAP-RECORD OF LGMAPDTA-REC
     ‚*€                 MOVE "1" TO MAP-ACTION OF LGMAPDTA-REC
     ‚*€                 PERFORM CALL-LGMAPDTA
     ‚*€            END-IF
                    ADD 1 TO ZEILE
           END-PERFORM.
           INITIALIZE DATENENDE-WRK.

       AUS-ZEI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILENNUMMER FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-SEQ-NUMBER SECTION.
       FIL-SEQ-00.

           MOVE     "1" TO LEN.

     ‚* ZEILENNUMMER JETZT SCHONMAL FÜLLEN, DA AUCH KEIN SATZ GELESEN
     ‚* WERDEN KÖNNTE
           MOVE     SRCSEQ OF SOURCE-REC(1:4) TO
                    FORMAT6(VPOS(1, ZEILE):4).
           MOVE     "."                       TO
                    FORMAT6(VPOS(1, ZEILE) + 4:1).
           MOVE     SRCSEQ OF SOURCE-REC(5:2) TO
                    FORMAT6(VPOS(1, ZEILE) + 5:2).

           PERFORM  UNTIL SEQ-FMT OF USRCMD-REC(LEN) = SPACES
                    IF   SEQ-FMT OF USRCMD-REC(LEN)(1:4) =
                         SRCSEQ OF SOURCE-REC(1:4)
                     AND SEQ-FMT OF USRCMD-REC(LEN)(6:2) =
                         SRCSEQ OF SOURCE-REC(5:2)
                     AND SRCSEQ OF SOURCE-REC(1:4) =
                         FORMAT6(VPOS(1, ZEILE):4)
                     AND SRCSEQ OF SOURCE-REC(5:2) =
                         FORMAT6(VPOS(1, ZEILE) + 5:2)
                         MOVE SEQ-ACT OF USRCMD-REC(LEN) TO
                              FORMAT6(VPOS(1, ZEILE):SLEN(1, ZEILE))
     ‚*                  ZEILENNUMMER WEIß DARSTELLEN
                         INITIALIZE LEN2
                         ADD   ZEILE TO LEN2
                         STRING "SRCSEQ" DELIMITED BY SIZE
                                LEN2     DELIMITED BY SIZE
                         INTO  DSP-ATR-FLD
                         MOVE  COL-WHT TO DSP-ATR-COL
                         PERFORM ADD-ATR-TAB
                    END-IF
                    ADD  1 TO LEN
           END-PERFORM.

       FIL-SEQ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOMMANDO TRIMMEN (SPACES AM ANFANG WEGSCHNEIDEN)
     ‚*--------------------------------------------------------------
       TRIM-COMMAND SECTION.
       TRM-CMD-00.

           IF       SETCMD OF FORMAT6(1:1) NOT = SPACES
            OR      SETCMD OF FORMAT6          = SPACES
                    GO TO TRM-CMD-90
           END-IF.

           INITIALIZE LEN.
           INITIALIZE SETCMD-WRK.

           INSPECT  SETCMD OF FORMAT6 TALLYING LEN FOR LEADING SPACES.
           MOVE     SETCMD OF FORMAT6(LEN + 1:) TO SETCMD-WRK.
           MOVE     SETCMD-WRK TO SETCMD OF FORMAT6.

       TRM-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOPF FÜLLEN
     ‚*--------------------------------------------------------------
       AUSGABE-HEADER SECTION.
       AUS-HEA-00.

     ‚* SCHON GEFÜLLT?
           IF       FILENAME-WRK > SPACES
            AND     SOURCE-CHANGED = SOURCE-CHANGED-SAVE
                    MOVE FILENAME-WRK TO FILENAME OF FORMAT6
           ELSE
                    MOVE     SOURCE-CHANGED TO SOURCE-CHANGED-SAVE
                    INITIALIZE FILENAME OF FORMAT6
                    STRING   LIB-WRK        DELIMITED BY " "
                             "/"            DELIMITED BY SIZE
                             FILE-WRK       DELIMITED BY " "
                             "("            DELIMITED BY SIZE
                             MBR-WRK        DELIMITED BY " "
                             ") "           DELIMITED BY SIZE
                             SOURCE-CHANGED DELIMITED BY SIZE
                    INTO     FILENAME OF FORMAT6
     ‚*             MITTIG SETZEN
                    INITIALIZE LEN
                    INSPECT FILENAME OF FORMAT6
                            TALLYING LEN FOR CHARACTERS BEFORE SPACES
                    IF      SOURCE-CHANGED NOT = SPACES
                            ADD 2 TO LEN
                    END-IF
                    COMPUTE  LEN = FUNCTION LENGTH(FILENAME OF FORMAT6)
                                   - LEN
                    IF   LEN < FUNCTION LENGTH(FILENAME OF FORMAT6) - 1
                         COMPUTE LEN = (LEN / 2) + 1
                         INITIALIZE FILENAME-WRK
                         MOVE FILENAME OF FORMAT6 TO FILENAME-WRK(LEN:)
                         MOVE FILENAME-WRK TO FILENAME OF FORMAT6
                    END-IF
           END-IF.

     ‚* FELDER SPERREN BEI ANZEIGE-MODUS
           IF       READONLY-WRK = 1
                    MOVE AN  TO IN51 OF INXX
           ELSE
                    MOVE AUS TO IN51 OF INXX
           END-IF.

       AUS-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* QUELLE SICHERN
     ‚*--------------------------------------------------------------
       SAVE-SOURCE SECTION.
       SAV-SRC-00.

     ‚* TABELLE IN DATEI ÜBERNEHMEN. VORHER DATEI LÖSCHEN

     ‚* SICHERN IM ANZEIGEMODUS NICHT MÖGLICH
           IF       READONLY-WRK = "1"
                    MOVE "SAVING NOT ALLOWED IN READ ONLY MODE!" TO
                         ERRMSG-TXT
                    GO TO SAV-SRC-90
           END-IF.

     ‚* WURDE ÜBERHAUPT WAS GEÄNDERT?
           IF       SOURCE-CHANGED NOT = "*"
                    MOVE "NOTHING CHANGED, NO NEED TO SAVE FILE" TO
                         ERRMSG-TXT
                    GO TO SAV-SRC-90
           END-IF.

           SET      DB-PTR TO QSETSRC-DP.
           PERFORM  DB-CLOSE.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "ERROR SAVING SOURCE"              TO
                         ERRMSG-TXT
                    GO TO SAV-SRC-90
           END-IF.
           MOVE     "OUTPUT" TO DB-OPENMODE.
           SET      DB-PTR TO ADDRESS OF QSETSRC-P-X.
           PERFORM  DB-OPEN.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "ERROR SAVING SOURCE" TO
                         ERRMSG-TXT
                    GO TO SAV-SRC-90
           END-IF.

     ‚* AUF ARRAY AUFSETZTEN
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     ZERO          TO MAP-LFDN   OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           INITIALIZE SOURCE-POS.

     ‚* ERSTE UND LETZTE ZEILE NICHT SPEICHERN!
           PERFORM  WITH TEST BEFORE VARYING SOURCE-POS FROM 1 BY 1
                    UNTIL SOURCE-POS >= (SOURCE-MAX - 1)
                    MOVE MAP-READ-NEXT TO MAP-ACTION OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC
                    MOVE SRCSEQ OF SOURCE-REC TO SRCSEQ OF QSETSRC-P
                    MOVE SRCDTA OF SOURCE-REC TO SRCDTA OF QSETSRC-P
                    MOVE SRCDAT OF SOURCE-REC TO SRCDAT OF QSETSRC-P
                    SET  DB-PTR TO QSETSRC-DP
                    PERFORM DB-WRITE
                    IF   FILE-STATUS NOT = ZEROES
                     OR  RET-CODE    NOT = SPACES
                         MOVE "ERROR SAVING FILE!" TO
                              ERRMSG-TXT
                         GO TO SAV-SRC-90
                    END-IF
           END-PERFORM.

           SET      DB-PTR TO QSETSRC-DP.
           PERFORM  DB-CLOSE.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "OPENING FAILED, FILE NOT SAVED!" TO
                         ERRMSG-TXT
                    GO TO SAV-SRC-90
           END-IF.
           MOVE     "INPUT" TO DB-OPENMODE.
           SET      DB-PTR TO ADDRESS OF QSETSRC-P-X.
           PERFORM  DB-OPEN.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "OPENING FAILED, FILE NOT SAVED!" TO
                         ERRMSG-TXT
                    GO TO SAV-SRC-90
           END-IF.

           INITIALIZE SOURCE-CHANGED.
           MOVE     "*" TO SOURCE-CHANGED-LINUX.

       SAV-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB SOURCE GEÄNDERT WURDE
     ‚*--------------------------------------------------------------
       CHECK-SOURCECHANGE SECTION.
       CHK-SRC-00.

           MOVE     1 TO ZEILE.
           INITIALIZE SOURCE-REC.

       CHK-SRC-10.
           PERFORM  UNTIL INXX-DTA(ZEILE:1) = "1"
                    ADD 1 TO ZEILE
                    IF   ZEILE > ROW-COUNT
                         GO TO CHK-SRC-90
                    END-IF
           END-PERFORM.

           IF       BS-START < ZEROS
                    MOVE ZEROS TO BS-START
           END-IF.

     ‚* NÄCHTE ZEILE LESEN
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           COMPUTE  MAP-LFDN OF LGMAPDTA-REC = BS-START + ZEILE.
           PERFORM  CALL-LGMAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO CHK-SRC-90
           END-IF.

           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.

     ‚* PRÜFEN OB SICH WAS AN DER SOURCE GEÄNDERT HAT
           IF       FORMAT6(VPOS(2, ZEILE):SLEN(2, ZEILE)) NOT =
                    SRCDTA OF SOURCE-REC
     ‚*             ÄNDERUNG LOGGEN
                    MOVE "TXT" TO AUSWAHL OF SOURCE-REC
                    MOVE FORMAT6(VPOS(2, ZEILE):SLEN(2, ZEILE)) TO
                         SRCDTA-WRK
                    PERFORM WRITE-PSETLOG
                    MOVE "*" TO SOURCE-CHANGED
                    MOVE FORMAT6(VPOS(2, ZEILE):SLEN(2, ZEILE)) TO
                         SRCDTA OF SOURCE-REC(1:113)
                    PERFORM GET-TIME
                    MOVE DATUM TO SRCDAT OF SOURCE-REC
     ‚*             ÄNDERUNG IN LGMAPDTA SICHERN
                    MOVE MAP-WRITE TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SRCSEQ  OF SOURCE-REC(1:4) TO
                         MAP-KEY OF LGMAPDTA-REC(1:4)
                    MOVE "."                        TO
                         MAP-KEY OF LGMAPDTA-REC(5:1)
                    MOVE SRCSEQ  OF SOURCE-REC(5:2) TO
                         MAP-KEY OF LGMAPDTA-REC(6:2)
                    MOVE SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
           END-IF.

           ADD      1 TO ZEILE.
           IF       ZEILE <= ROW-COUNT
                    GO TO CHK-SRC-10
           END-IF.

       CHK-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EINE AUSWAHL VOR EINER SOURCEZEILE GETÄTIGT WURDE
     ‚*--------------------------------------------------------------
       CHECK-CHOICECHANGE SECTION.
       CHK-CHC-00.

           MOVE     1 TO ZEILE.
           INITIALIZE OFFSET-WRK.
           INITIALIZE FIRST-READ.
           INITIALIZE CHOICE-CHANGED.

           IF       BS-START < ZEROS
                    MOVE ZEROS TO BS-START
           END-IF.

     ‚* WENN ES KEINE ZEILE IM DISPLAY GIBT, MUSS MAN
     ‚* TROTZDEM EINE EINFÜGEN KÖNNEN
           IF       SOURCE-MAX = 2
                    INITIALIZE LEN
                    MOVE "1" TO  CHOICE-CHANGED
                    PERFORM USER-BEFEHLE
           END-IF.

       CHK-CHC-10.
           PERFORM  UNTIL INXX-SEQ(ZEILE:1) = "1"
                    ADD 1 TO ZEILE
                    IF   ZEILE > ROW-COUNT
                     AND READONLY-WRK NOT = 1
     ‚*                  AUSWAHLEN NUR AUSFÜHREN WENN EIN
     ‚*                  BEFEHL EINGEGEBEN WURDE
                         IF   CHOICE-CHANGED = "1"
                              INITIALIZE UNDO-WRK
                              CALL "DMP0026" USING X-MBR
                                                   X-FILE
                                                   X-LIB
                                                   MAP-POINTER
                                                    OF LGMAPDTA-REC
                                                   USRCMD-TAB
                                                   SOURCE-MAX
                                                   DSP-CUR-X
                                                   DSP-CUR-Y
                                                   UNDO-WRK
                                                   BS-START
                                                   RETCODE-SAV
                              END-CALL
                              IF   RETCODE-SAV NOT = "1"
                                   INITIALIZE USRCMD-LEN
                                   INITIALIZE USRCMD-TAB
                              ELSE
                                   INITIALIZE CHOICE-CHANGED
                              END-IF
                    END-IF
                         GO TO CHK-CHC-90
                    END-IF
           END-PERFORM.

           IF       BS-START < ZEROS
                    MOVE ZEROS TO BS-START
           END-IF.

     ‚* NÄCHTE ZEILE LESEN
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           COMPUTE  MAP-LFDN OF LGMAPDTA-REC = BS-START + ZEILE.
           PERFORM  CALL-LGMAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO CHK-CHC-90
           END-IF.

           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.

           MOVE     SRCSEQ OF SOURCE-REC(1:4) TO SEQ-WRK(1:4).
           MOVE     "."                       TO SEQ-WRK(5:1).
           MOVE     SRCSEQ OF SOURCE-REC(5:2) TO SEQ-WRK(6:2).

     ‚*    PRÜFEN OB SICH DIE AUSWAHL GEÄNDERT HAT
           IF       SEQ-WRK NOT =
                    FORMAT6(VPOS(1, ZEILE):SLEN(1, ZEILE))
                    INITIALIZE LEN
                    MOVE SEQ-WRK TO AUSWAHL OF SOURCE-REC
     ‚*             WENN DIE ZEILE LEER IST SOLL AN DIE STELLE
     ‚*             AUFGESETZT WERDEN
                    IF  FORMAT6(VPOS(1, ZEILE):SLEN(1, ZEILE)) = SPACES
                        MOVE MAP-LFDN OF LGMAPDTA-REC TO BS-START
                        SUBTRACT 1 FROM BS-START
                        GO TO CHK-CHC-90
                    END-IF
     ‚*             BEFEHL AUSWERTEN
                    PERFORM USER-BEFEHLE
     ‚*             WENN EINE ZEILENNUMMER EINGEGEBEN
     ‚*             WURDE, DORT HINSTRIGEN
                    IF   RET-CODE     = "Z"
                     OR  READONLY-WRK = 1
                         GO TO CHK-CHC-90
                    END-IF
                    MOVE "*" TO SOURCE-CHANGED
                    MOVE ZEILE TO ZEILE-SAV
                    MOVE "1" TO CHOICE-CHANGED
                    IF   ZEILE-SAV >= ROW-COUNT
                     AND SEQ-ACT OF USRCMD-REC(USRCMD-LEN) NOT = "D"
                         COMPUTE ZEILE-SAV = ROW-COUNT - 1
                         ADD 1 TO BS-START
                    END-IF
           ELSE
     ‚*             ES WURDE NICHTS GEÄNDERT, ALSO IN DIE ZEILE
     ‚*             SPRINGEN
                    COMPUTE BS-START = BS-START + ZEILE - 1
                    GO TO CHK-CHC-90
           END-IF.

           ADD      1 TO ZEILE.
           IF       ZEILE <= ROW-COUNT
                    GO TO CHK-CHC-10
           ELSE
                    IF   CHOICE-CHANGED = "1"
                     AND READONLY-WRK NOT = 1
                         INITIALIZE UNDO-WRK
                         CALL "DMP0026" USING X-MBR
                                              X-FILE
                                              X-LIB
                                              MAP-POINTER
                                                  OF LGMAPDTA-REC
                                              USRCMD-TAB
                                              SOURCE-MAX
                                              DSP-CUR-X
                                              DSP-CUR-Y
                                              UNDO-WRK
                                              BS-START
                                              RETCODE-SAV
                         END-CALL
                         IF   RETCODE-SAV NOT = "1"
                              INITIALIZE USRCMD-LEN
                              INITIALIZE USRCMD-TAB
                         ELSE
                              INITIALIZE CHOICE-CHANGED
                         END-IF
                    END-IF
           END-IF.

       CHK-CHC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN PXMLFFD
     ‚*--------------------------------------------------------------
       LESEN-PXMLFFD SECTION.
       LES-XML-00.

           INITIALIZE RET-CODE.
           INITIALIZE LEN.
           INITIALIZE ROW-COUNT.

     ‚* OBJEKTBIBLIOTHEK ERMITTELN
           INITIALIZE RTVOBJD-REC.
           MOVE     "*LIBL"     TO OBJLIB  OF RTVOBJD-REC.
           MOVE     "*FILE"     TO OBJTYPE OF RTVOBJD-REC.
           MOVE     DISPLAY-WRK TO OBJ     OF RTVOBJD-REC.
           CALL     "DMP0541" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.

           INITIALIZE PXMLFFDF OF PXMLFFD-P.
           MOVE     "*LIBL"     TO WHLIB  OF PXMLFFD-P.
           MOVE     DISPLAY-WRK TO WHFILE OF PXMLFFD-P.
           MOVE     SPACES      TO WHNAME OF PXMLFFD-P.
           MOVE     ZEROS       TO WHFOBO OF PXMLFFD-P.
           MOVE     SPACES      TO WHLIB2 OF PXMLFFD-P.
           SET      DB-PTR TO PXMLFFD-DP.
           PERFORM  DB-START-NL.
           IF       FILE-STATUS NOT = ZEROS
                    MOVE "1" TO RET-CODE
                    GO TO LES-XML-90
           END-IF.

       LES-XML-20.
           SET      DB-PTR TO PXMLFFD-DP.
           PERFORM  DB-READ-NEXT-NL.
           IF       FILE-STATUS NOT = ZEROS
                    GO TO LES-XML-90
           END-IF.

     ‚* STIMMT SCHLÜSSEL ÜBEREIN?
           IF       WHFILE OF PXMLFFD-P NOT = DISPLAY-WRK
                    GO TO LES-XML-90
           END-IF.

     ‚* PRÜFEN OB DIE RICHTIGEN FELDER GEFUNDEN WURDEN
           IF       WHFLDE OF PXMLFFD-P(1:3) NOT = "SRC"
                    GO TO LES-XML-20
           ELSE
     ‚*             ZEILE SICHERN
                    IF   WHFLDE OF PXMLFFD-P(7:2) NUMERIC
                         MOVE WHFLDE OF PXMLFFD-P(7:2) TO LEN
                    ELSE
                         GO TO LES-XML-20
                    END-IF
           END-IF.

     ‚* JE NACH FELD IN TABELLE SCHREIBEN
           EVALUATE WHFLDE OF PXMLFFD-P(4:3)
                    WHEN "SEQ" MOVE WHFOBO OF PXMLFFD-P TO VPOS(1, LEN)
                               MOVE WHFLDB OF PXMLFFD-P TO SLEN(1, LEN)
                    WHEN "DTA" MOVE WHFOBO OF PXMLFFD-P TO VPOS(2, LEN)
                               MOVE WHFLDB OF PXMLFFD-P TO SLEN(2, LEN)
                    WHEN "DAT" MOVE WHFOBO OF PXMLFFD-P TO VPOS(3, LEN)
                               MOVE WHFLDB OF PXMLFFD-P TO SLEN(3, LEN)
           END-EVALUATE.

     ‚* ANZAHL ZEILEN IM DISPLAY ERMITTELN
           ADD      1 TO ROW-COUNT.

           GO TO    LES-XML-20.

       LES-XML-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEILDATEI IN INTERNE TABELLE LESEN
     ‚*--------------------------------------------------------------
       LESEN-TEILDATEI SECTION.
       LES-MBR-00.

           SET      DB-PTR TO QSETSRC-DP.
           PERFORM  DB-READ-NEXT-NL.
           IF       FILE-STATUS NOT = ZEROES
                    ADD 1 TO SOURCE-MAX
                    INITIALIZE SOURCE-REC
                    MOVE ZEROES  TO SRCSEQ OF SOURCE-REC
                    MOVE ALL "*" TO SRCDTA OF SOURCE-REC
                    MOVE ZEROS   TO SRCDAT OF SOURCE-REC
                    MOVE MAP-WRITE TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SRCSEQ  OF SOURCE-REC(1:4) TO
                         MAP-KEY OF LGMAPDTA-REC(1:4)
                    MOVE "."                        TO
                         MAP-KEY OF LGMAPDTA-REC(5:1)
                    MOVE SRCSEQ  OF SOURCE-REC(5:2) TO
                         MAP-KEY OF LGMAPDTA-REC(6:2)
                    MOVE SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
           ELSE
                    MOVE 1 TO DATEN
                    ADD  1 TO SOURCE-MAX
                    IF   SOURCE-MAX = 50000
                         MOVE "4" TO RET-CODE
                         GO TO LES-MBR-90
                    END-IF
                    INITIALIZE SOURCE-REC
                    MOVE SRCSEQ OF QSETSRC-P TO SRCSEQ OF SOURCE-REC
                    MOVE SRCDTA OF QSETSRC-P TO SRCDTA OF SOURCE-REC
                    MOVE SRCDAT OF QSETSRC-P TO SRCDAT OF SOURCE-REC
                    MOVE MAP-WRITE  TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SRCSEQ  OF SOURCE-REC(1:4) TO
                         MAP-KEY OF LGMAPDTA-REC(1:4)
                    MOVE "."                        TO
                         MAP-KEY OF LGMAPDTA-REC(5:1)
                    MOVE SRCSEQ  OF SOURCE-REC(5:2) TO
                         MAP-KEY OF LGMAPDTA-REC(6:2)
                    MOVE SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
                    GO TO LES-MBR-00
           END-IF.

       LES-MBR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* USERBEFEHLE IN TABELLE SCHREIBEN
     ‚*--------------------------------------------------------------
       USER-BEFEHLE SECTION.
       USR-BEF-00.

           INITIALIZE RETCODE-SAV.
           INITIALIZE RET-CODE.
           INITIALIZE SEQ-ACT-WRK.

           MOVE     FORMAT6(VPOS(1, ZEILE) + LEN:SLEN(1, ZEILE))
                    TO SEQ-ACT-WRK.

           INITIALIZE POS.

           INSPECT  SEQ-ACT-WRK TALLYING POS FOR CHARACTERS BEFORE
                    INITIAL SPACES.

           IF       POS >= LENGTH OF SEQ-ACT-WRK
                    MOVE 7 TO POS
           END-IF.
           IF       POS = ZEROS
                    MOVE 1 TO POS
           END-IF.

     ‚* IST STRING NUMERISCH? DANN WURDE EINE ZEILENNUMMER
     ‚* EINGEGEBEN
           IF       SEQ-ACT-WRK(1:4)   IS NUMERIC
            AND     SEQ-ACT-WRK(6:2)   IS NUMERIC
            OR      SEQ-ACT-WRK(1:POS) IS NUMERIC
     ‚*             SATZ LESEN
                    IF   POS = LENGTH OF SEQ-ACT-WRK
                         MOVE MAP-LFDN OF LGMAPDTA-REC  TO LEN
                         MOVE SEQ-ACT-WRK TO MAP-KEY    OF LGMAPDTA-REC
                         MOVE MAP-READ    TO MAP-ACTION OF LGMAPDTA-REC
                    ELSE
                         MOVE SEQ-ACT-WRK(1:POS) TO
                              MAP-LFDN   OF LGMAPDTA-REC
                         MOVE MAP-READ-LFDN TO
                              MAP-ACTION OF LGMAPDTA-REC
                    END-IF
                    PERFORM CALL-LGMAPDTA
     ‚*             WURDE EIN SATZ GEFUNDEN, LFDN IN BS-START
     ‚*             SCHREIBEN
                    IF   RET-CODE = SPACES
                         MOVE MAP-LFDN OF LGMAPDTA-REC TO BS-START
                         SUBTRACT 1 FROM BS-START
                         MOVE "Z" TO RET-CODE
                         GO TO USR-BEF-90
                    ELSE
     ‚*                  ANSONSTEN LFDN AUF URSPÜNGLICHEN WERT
     ‚*                  SETZTEN
                         MOVE LEN    TO MAP-LFDN   OF LGMAPDTA-REC
                         MOVE MAP-READ-LFDN TO
                              MAP-ACTION OF LGMAPDTA-REC
                         PERFORM CALL-LGMAPDTA
                    END-IF
           END-IF.

           ADD      1 TO USRCMD-LEN.
     ‚* BEFEHL AUS STRING EXTRAHIEREN
           MOVE 1 TO LEN.
           PERFORM  WITH TEST BEFORE VARYING IDX FROM 1 BY 1
                    UNTIL IDX > LENGTH OF SEQ-ACT-WRK
                    IF   SEQ-ACT-WRK(IDX:1) NOT = SEQ-WRK(IDX:1)
                         MOVE SEQ-ACT-WRK(IDX:1) TO SEQ-ACT OF
                              USRCMD-REC(USRCMD-LEN)(LEN:1)
                         ADD 1 TO LEN
                    END-IF
           END-PERFORM.

           MOVE     SEQ-WRK TO SEQ-FMT OF USRCMD-REC(USRCMD-LEN).

     ‚* PRÜFEN OB AUSWAHL SCHON IN TABELLE STEHT
           MOVE     1 TO LEN.
           PERFORM  UNTIL SEQ-FMT OF USRCMD-REC(LEN) = SPACES
                    IF   SEQ-FMT OF USRCMD-REC(LEN) =
                         SEQ-FMT OF USRCMD-REC(USRCMD-LEN)
                     AND SEQ-ACT OF USRCMD-REC(LEN) =
                         SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                     AND LEN NOT = USRCMD-LEN
                         INITIALIZE USRCMD-REC(USRCMD-LEN)
                         SUBTRACT 1 FROM USRCMD-LEN
                         GO TO USR-BEF-90
                    END-IF
                    IF   SEQ-FMT OF USRCMD-REC(LEN) =
                         SEQ-FMT OF USRCMD-REC(USRCMD-LEN)
                     AND LEN NOT = USRCMD-LEN
                         MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN) TO
                              SEQ-ACT OF USRCMD-REC(LEN)
                         INITIALIZE USRCMD-REC(USRCMD-LEN)
                         SUBTRACT 1 FROM USRCMD-LEN
                         GO TO USR-BEF-90
                    END-IF
                    ADD  1 TO LEN
           END-PERFORM.

       USR-BEF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PSETLOG SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-PSETLOG SECTION.
       WRT-SET-00.

     ‚* WENN SOURCE NUR GELESEN WIRD, MUSS AUCH KEIN
     ‚* LOGGING GEMACHT WERDEN
           IF       X-READONLY = "1"
                    GO TO WRT-SET-90
           END-IF.

           MOVE     "D" TO AKTION-WRK.
           CALL     "DMP0025" USING X-MBR
                                    X-FILE
                                    X-LIB
                                    MAP-POINTER OF LGMAPDTA-REC
                                    SOURCE-MAX
                                    DSP-CUR-X
                                    DSP-CUR-Y
                                    AKTION-WRK
                                    BS-START
                                    ROW-COUNT
                                    RET-CODE
           END-CALL.

     ‚* NEUE BLOCK-ID ERMITELN
           MOVE     "SETBLKID" TO SL-ID.
           CALL     "LGPGM020" USING SL-ID
                                     BLOCK-ID
           END-CALL.

           INITIALIZE RET-CODE.

     ‚* LFDN AUS PSCHLUE ERMITTELN
           MOVE     "SETLOGID" TO SL-ID.
           CALL     "LGPGM020" USING SL-ID
                                     LFD-SLID-NR
           END-CALL.

           PERFORM  GET-TIME.

           INITIALIZE PSETLOGF OF PSETLOG-P.
           MOVE     X-LIB       TO SELIB  OF PSETLOG-P.
           MOVE     X-FILE      TO SEFILE OF PSETLOG-P.
           MOVE     X-MBR       TO SEMBR  OF PSETLOG-P.
           MOVE     LFD-SLID-NR TO SELFDN OF PSETLOG-P.
           MOVE     BLOCK-ID    TO SEZUSL OF PSETLOG-P.
           MOVE     SOURCE-REC  TO SEREC  OF PSETLOG-P.
           MOVE     SRCDTA-WRK  TO SEREC OF PSETLOG-P(1000:).
           MOVE     T-USER      TO SEMUSR OF PSETLOG-P.
           MOVE     T-TERM      TO SEMBS  OF PSETLOG-P.
           MOVE     DATE-8      TO SEMDT  OF PSETLOG-P.
           MOVE     TIME-6      TO SEMTI  OF PSETLOG-P.
           MOVE     PGM-WRK     TO SEPGM  OF PSETLOG-P.
           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-WRITE.

           INITIALIZE AUSWAHL OF SOURCE-REC.

       WRT-SET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PSETLOG LEEREN
     ‚*--------------------------------------------------------------
       CLEAR-PSETLOG SECTION.
       CLR-SET-00.

           INITIALIZE RET-CODE.
           INITIALIZE PSETLOGF OF PSETLOG-P.
           MOVE     X-LIB  TO SELIB  OF PSETLOG-P.
           MOVE     X-FILE TO SEFILE OF PSETLOG-P.
           MOVE     X-MBR  TO SEMBR  OF PSETLOG-P.
           MOVE     ZEROS  TO SELFDN OF PSETLOG-P.
           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-START-NL.
           IF       FILE-STATUS NOT = ZEROS
                    GO TO CLR-SET-90
           END-IF.

       CLR-SET-20.
           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-READ-NEXT-WL.
           IF       FILE-STATUS NOT = ZEROS
                    GO TO CLR-SET-90
           END-IF.

     ‚* STIMMEN DIE SCHLÜSSEL NOCH?
           IF       SELIB  OF PSETLOG-P NOT = X-LIB
            OR      SEFILE OF PSETLOG-P NOT = X-FILE
            OR      SEMBR  OF PSETLOG-P NOT = X-MBR
                    SET DB-PTR TO PSETLOG-DP
                    PERFORM DB-REWRITE
           ELSE
     ‚*             NUN DEN SATZ LÖSCHEN
                    SET     DB-PTR TO PSETLOG-DP
                    PERFORM DB-DELETE
                    GO TO CLR-SET-20
           END-IF.

       CLR-SET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE DURCHSUCHEN
     ‚*--------------------------------------------------------------
       SEARCH-SOURCE SECTION.
       SEA-SRC-00.

     ‚* AUFSETZEN
           INITIALIZE SEQ-WRK.
           IF       MAP-LFDN OF LGMAPDTA-REC - ROW-COUNT >= ZEROES
            AND     DURCHGELESEN-WRK = SPACES
                    SUBTRACT ROW-COUNT FROM MAP-LFDN OF LGMAPDTA-REC
                    ADD DSP-CUR-Y      TO   MAP-LFDN OF LGMAPDTA-REC
                    SUBTRACT 4         FROM MAP-LFDN OF LGMAPDTA-REC
     ‚*             WENN LFDN KLEINER 0 IST, DANN AUF 0 SETZEN, DA ES
     ‚*             KEINE LFDN KLEINER 0 GIBT
                    IF   MAP-LFDN OF LGMAPDTA-REC < ZEROS
                         MOVE ZEROS TO MAP-LFDN OF LGMAPDTA-REC
                    END-IF
           ELSE
                    MOVE ZERO  TO MAP-LFDN OF LGMAPDTA-REC
                    INITIALIZE DURCHGELESEN-WRK
           END-IF.
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

       SEA-SRC-20.
     ‚* NÄCHSTEN SATZ LESEN
           MOVE     MAP-READ-NEXT TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE ZEROS TO MAP-LFDN OF LGMAPDTA-REC
     ‚*             WURDE SOURCE SCHON EINMAL DURCHGELESEN?
                    IF   DURCHGELESEN-WRK = SPACES
     ‚*                  WENN NEIN, SRC VON OBER DURCHSUCHEN
                         MOVE "1" TO DURCHGELESEN-WRK
                         MOVE "PGM2425" TO DSP-ERR-ID
                         PERFORM ADD-ERR-TAB
                         MOVE SEARCH-REC TO DSP-VAR-TXT
                         PERFORM ADD-VAR-TAB
                         MOVE ZEROS TO MAP-LFDN OF LGMAPDTA-REC
                    ELSE
     ‚*                  JA, DANN SECTION VERLASSEN, UM KEIN LOOP
     ‚*                  ZU VERURSACEHEN
                         MOVE "PGM2425" TO DSP-ERR-ID
                         PERFORM ADD-ERR-TAB
                         MOVE SEARCH-REC TO DSP-VAR-TXT
                         PERFORM ADD-VAR-TAB
                    END-IF
                    GO TO SEA-SRC-90
           END-IF.
           INITIALIZE LEN3.
           MOVE     LENGTH OF QSETSRC-P TO LEN3.

     ‚* SOLL NACH STRING ODER DATUM GESUCHT WERDEN?
           IF       SUCH-TYP = "S"
            OR      SUCH-TYP = "B"
     ‚*             IST GESUCHTER STRING IN DER ZEILE?
                    CALL PROCEDURE "LGPGM0923" USING
                                  MAP-RECORD OF LGMAPDTA-REC
                                  LEN3
                                  SEARCH-REC
                                  RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         GO TO SEA-SRC-20
                    END-IF
           END-IF.

     ‚* SOLL NACH STRING ODER DATUM GESUCHT WERDEN?
           IF       SUCH-TYP = "D"
            OR      SUCH-TYP = "B"
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC
     ‚*             STIMMT DATUM ÜBEREIN?
                    EVALUATE OPTION OF FORMAT1
     ‚*                  KLEINER
                         WHEN "1" IF   SRCDAT OF SOURCE-REC <
                                       DT_VON OF FORMAT1(3:6)
                                       CONTINUE
                                  ELSE
                                       GO TO SEA-SRC-20
                                  END-IF
     ‚*                  GLEICH
                         WHEN "2" IF   SRCDAT OF SOURCE-REC =
                                       DT_VON OF FORMAT1(3:6)
                                       CONTINUE
                                  ELSE
                                       GO TO SEA-SRC-20
                                  END-IF
     ‚*                  GRÖßER
                         WHEN "3" IF   SRCDAT OF SOURCE-REC >
                                       DT_VON OF FORMAT1(3:6)
                                       CONTINUE
                                  ELSE
                                       GO TO SEA-SRC-20
                                  END-IF
     ‚*                  VON/BIS
                         WHEN "4" IF   SRCDAT OF SOURCE-REC >=
                                       DT_VON OF FORMAT1(3:6)
                                   AND SRCDAT OF SOURCE-REC <=
                                       DT_BIS OF FORMAT1(3:6)
                                       CONTINUE
                                  ELSE
                                       GO TO SEA-SRC-20
                                  END-IF
                         WHEN OTHER GO TO SEA-SRC-20
                    END-EVALUATE
                    IF   RET-CODE NOT = SPACES
                         GO TO SEA-SRC-20
                    END-IF
           END-IF.

     ‚* ERMITTELN AB WELCHER ZEILE DER BS AUSGEGEBEN WERDEN SOLL
           IF       (MAP-LFDN OF LGMAPDTA-REC -
                    AUSZEIL OF FORMAT1) <= ZERO
                    MOVE ZERO TO BS-START
     ‚*€            MOVE 1 TO BS-START
           ELSE
     ‚*             WENN DIE GESUCHTE ZEILE NOCH AUF DEM DISPLAY
     ‚*             IST, DANN BS-START NICHT NEU SETZEN
                    IF   MAP-LFDN OF LGMAPDTA-REC > BS-START +
                         ROW-COUNT
                     OR  MAP-LFDN OF LGMAPDTA-REC < BS-START
                         COMPUTE BS-START = MAP-LFDN OF LGMAPDTA-REC
                                          - AUSZEIL OF FORMAT1
                    END-IF
           END-IF.
           IF       BS-START + ROW-COUNT >= SOURCE-MAX
                    COMPUTE BS-START = SOURCE-MAX - ROW-COUNT - 2
           END-IF.

     ‚* CURSERPOSITION ERMITTELN
           COMPUTE  DSP-CUR-Y = MAP-LFDN OF LGMAPDTA-REC
                              - BS-START + 4.

     ‚* STELLE DES GESUCHTEN STRING ERMITTELN
           INITIALIZE LEN.
           INITIALIZE POS.
           INSPECT  SETCMD-WRK TALLYING POS
                    FOR CHARACTERS BEFORE INITIAL SPACES.
           IF       POS = ZERO
                    MOVE 1 TO POS
           END-IF.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.
           INSPECT  SRCDTA OF SOURCE-REC TALLYING LEN
                    FOR CHARACTERS BEFORE INITIAL SETCMD-WRK(1:POS).

           COMPUTE  DSP-CUR-X = LENGTH OF SRCSEQ OF SOURCE-REC
                              + LEN
                              + 3.

           MOVE     SRCSEQ OF SOURCE-REC(1:4) TO SEQ-WRK(1:4).
           MOVE     "."                       TO SEQ-WRK(5:1).
           MOVE     SRCSEQ OF SOURCE-REC(5:2) TO SEQ-WRK(6:2).

     ‚* ZEILENNUMMER WEIß DARSTELLEN
           INITIALIZE LEN2.
           COMPUTE  LEN2 = DSP-CUR-Y - 4.

           STRING   "SRCSEQ" DELIMITED BY SIZE
                    LEN2     DELIMITED BY SIZE
           INTO     DSP-ATR-FLD.
           MOVE     COL-WHT TO DSP-ATR-COL.
           PERFORM  ADD-ATR-TAB.

       SEA-SRC-90.
           EXIT.
      /
     ‚*-------------------------------------------------------------
     ‚* ERMITTELN DER AKTUALITÄTENID ZU DER AKTUELLEN DATEI
     ‚*-------------------------------------------------------------
       GET-LVLCHECK SECTION.
       GET-LVL-00.

     ‚* AKTUELLES BETRIEBSSYSTEM ERMITTELN
           CALL     PROCEDURE "LGUMF16" USING BETRIEBSSYSTEM.

           INITIALIZE DB-INFO-FILE OF QSETSRC-P-X.
           MOVE     "QSETSRC" TO DB-INFO-FILE OF QSETSRC-P-X.

     ‚* WINDOWS/LINUX UNTERSTÜTZEN KEINE AKTUALTITÄTENERMITTELUNG
           IF       BETRIEBSSYSTEM NOT = ZEROES
                    MOVE ALL "X" TO DB-INFO-LVLCHK OF QSETSRC-P-X
                    GO TO GET-LVL-90
           END-IF.

           CALL     "LGPGM528" USING DB-INFO-FILE OF QSETSRC-P-X
                                     X-LIB
                                     FMTID-TAB
                                     RET-CODE
           END-CALL.

     ‚*LVLCHECK ID IN VARIABLE EINTRAGEN
           MOVE     FMT-ID OF FMTID-REC(1) TO
                    DB-INFO-LVLCHK OF QSETSRC-P-X.

           IF       RET-CODE NOT = SPACES
                    MOVE "PGM0584" TO DSP-ERR-ID
                    PERFORM ADD-ERR-TAB
                    MOVE      "INVALID_LVLCHECK_ID" TO DSP-VAR-TXT
                    PERFORM   ADD-VAR-TAB
                    MOVE ALL "?" TO DB-INFO-LVLCHK OF QSETSRC-P-X
           END-IF.

       GET-LVL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHEN/ERSETZEN SOURCE
     ‚*--------------------------------------------------------------
       SUCHEN-ERSETZTEN-SOURCE SECTION.
       SUC-ERS-00.

           INITIALIZE RET-CODE.

     ‚*€   PERFORM  TRIM-COMMAND.

           IF       SETCMD OF FORMAT6 NOT = SPACES
                    MOVE SETCMD OF FORMAT6 TO SUCHEN OF FORMAT1
                    MOVE DATE-8            TO DT_VON OF FORMAT1
                    MOVE DATE-8            TO DT_BIS OF FORMAT1
           END-IF.
           MOVE     "SUCHEN"          TO DSP-CUR-FLD.

           MOVE     "FMT01" TO FMT-WRK.
           MOVE     FORMAT1 TO WSREC-WRK.
           PERFORM  DISPLAY-WRITE.
           PERFORM  DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           INITIALIZE DSP-ATTR.

     ‚* BEENDEN
           IF       IN03 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO SUC-ERS-90
           END-IF.

     ‚* ABBRECHEN
           IF       IN12 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO SUC-ERS-90
           END-IF.

     ‚* WURDE EIN VERGLEICHSOPERATOR ANGEGEBEN?
           IF       DT_VON OF FORMAT1 NOT = ZEROS
            AND     OPTION OF FORMAT1     = SPACES
                    MOVE "LFS0405" TO DSP-ERR-ID
                    MOVE "OPTION"  TO DSP-ERR-FLD
                    PERFORM ADD-ERR-TAB
                    GO TO SUC-ERS-00
           END-IF.

     ‚* IST VON-BIS DATUM GÜLTIG?
           IF       DT_VON OF FORMAT1 > DT_BIS OF FORMAT1
            AND     OPTION OF FORMAT1 = "4"
                    MOVE "LFS0553" TO DSP-ERR-ID
                    MOVE "DT_VON"  TO DSP-ERR-FLD
                    PERFORM ADD-ERR-TAB
                    MOVE "DT_BIS"  TO DSP-ERR-FLD
                    PERFORM ADD-ERR-TAB
                    GO TO SUC-ERS-00
           END-IF.

     ‚* SOLL NACH DATUM ODER NACH STRING GESUCHT WERDEN
           IF       SUCHOP OF FORMAT1     = ZEROS
            AND     DT_VON OF FORMAT1 NOT = ZEROS
                    MOVE "D"               TO SUCH-TYP
                    MOVE SPACES            TO SETCMD-WRK
                    MOVE SPACES            TO SETCMD OF FORMAT6
                    MOVE SPACES            TO SEARCH-REC
           ELSE
                    MOVE "S"               TO SUCH-TYP
                    MOVE SUCHEN OF FORMAT1 TO SETCMD-WRK
                    MOVE SUCHEN OF FORMAT1 TO SETCMD OF FORMAT6
                    MOVE SUCHEN OF FORMAT1 TO SEARCH-REC
           END-IF.

           IF       SUCHOP OF FORMAT1 NOT = ZEROS
                    MOVE "B"               TO SUCH-TYP
           END-IF.

     ‚* SOLLEN ALLE GEFUNDENEN STRINGS ERSETZT WERDEN?
           IF       ALLEERS OF FORMAT1  = 1
            AND     EDIT OF FORMAT1 NOT = SPACES
                    INITIALIZE DURCHGELESEN-WRK
                    PERFORM UNTIL DURCHGELESEN-WRK = "1"
                            INITIALIZE DSP-ATTR
                            PERFORM SEARCH-SOURCE
                            IF   DURCHGELESEN-WRK = SPACES
                                 PERFORM CHANGE-SOURCE
                            END-IF
                    END-PERFORM
                    INITIALIZE DSP-ATTR
                    GO TO SUC-ERS-90
           END-IF.

     ‚* SUCHEN
           IF       IN16 IN FORMAT1-I-INDIC = AN
                    IF   SEARCH-REC = SPACES
                     AND SUCH-TYP   = "S"
                         MOVE "LFS0301" TO DSP-ERR-ID
                         PERFORM ADD-ERR-TAB
                    ELSE
                         PERFORM SEARCH-SOURCE
                    END-IF
                    GO TO SUC-ERS-90
           END-IF.

       SUC-ERS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE ERSETZEN
     ‚*--------------------------------------------------------------
       CHANGE-SOURCE SECTION.
       CHG-SRC-00.

           INITIALIZE RET-CODE.
     ‚* ZEILE LESEN
           MOVE     SEQ-WRK  TO MAP-KEY    OF LGMAPDTA-REC.
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.
           MOVE     "TXT" TO AUSWAHL OF SOURCE-REC.

     ‚* STRING ERSTEZEN
           MOVE     SETCMD-WRK           TO VON-ERSETZ-STRING.
           MOVE     EDIT OF FORMAT1      TO NACH-ERSETZ-STRING.
           MOVE     SRCDTA OF SOURCE-REC TO ERSETZ-WRK.

           CALL     PROCEDURE "LGPGM0926" USING ERSETZ-WRK
                                                VON-ERSETZ-STRING
                                                NACH-ERSETZ-STRING
           END-CALL.

           MOVE     ERSETZ-WRK TO SRCDTA-WRK.
           PERFORM  WRITE-PSETLOG.

           MOVE     ERSETZ-WRK TO SRCDTA OF SOURCE-REC.

     ‚* SATZ ZURÜCKSCHREIBEN
           MOVE     SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC.
           MOVE     MAP-WRITE  TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

           MOVE     "*" TO SOURCE-CHANGED.

       CHG-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KANN DIE SOURCE WIEDERHERGESTELLT WERDEN?
     ‚*--------------------------------------------------------------
       WIEDERHERSTELLEN SECTION.
       SRC-WDH-00.

           INITIALIZE F12.

           INITIALIZE PSETLOGF OF PSETLOG-P.
           MOVE     X-LIB  TO SELIB  OF PSETLOG-P.
           MOVE     X-FILE TO SEFILE OF PSETLOG-P.
           MOVE     X-MBR  TO SEMBR  OF PSETLOG-P.
           MOVE     ZEROS  TO SELFDN OF PSETLOG-P.
           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-START-NL.
           IF       FILE-STATUS NOT = ZEROS
                    GO TO SRC-WDH-90
           END-IF.

           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-READ-NEXT-NL.
           IF       FILE-STATUS NOT = ZEROS
                    GO TO SRC-WDH-90
           END-IF.

           IF       X-LIB  = SELIB  OF PSETLOG-P
            AND     X-FILE = SEFILE OF PSETLOG-P
            AND     X-MBR  = SEMBR  OF PSETLOG-P
                    MOVE "PGT0003" TO PGM-TEXT-ID
                    MOVE ZERO TO JA-NEIN
                    CALL "LGPGM033" USING PGM-TEXT-ID
                                          JA-NEIN
                                          F12
                    END-CALL
                    IF   F12 = SPACES
                     AND JA-NEIN = "1"
                         MOVE "1" TO AKTION-WRK
                         CALL "DMP0025" USING X-MBR
                                              X-FILE
                                              X-LIB
                                              MAP-POINTER
                                               OF LGMAPDTA-REC
                                              SOURCE-MAX
                                              DSP-CUR-X
                                              DSP-CUR-Y
                                              AKTION-WRK
                                              BS-START
                                              ROW-COUNT
                                              RETCODE-SAV
                         END-CALL
                         MOVE "*" TO SOURCE-CHANGED
                    END-IF
           END-IF.

       SRC-WDH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SET EINSTELLUNGEN ANZEIGEN
     ‚*--------------------------------------------------------------
       ANZEIGE-SET-EINSTELLUNGEN SECTION.
       ANZ-SET-00.

           INITIALIZE RET-CODE.

           MOVE     "SCROLL" TO DSP-CUR-FLD.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  DISPLAY-WRITE.
           PERFORM  DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT2-I.
           MOVE     CORR FORMAT2-I TO FORMAT2.
           INITIALIZE DSP-ATTR.

     ‚* BEENDEN
           IF       IN03 OF FORMAT2-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SET-90
           END-IF.

     ‚* ABBRECHEN
           IF       IN12 OF FORMAT2-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SET-90
           END-IF.

     ‚* GROSS-KLEINSCHREIBUNG
           IF       GROSS OF FORMAT2 NOT = GROSS-KLEIN-WRK
                    PERFORM DISPLAY-CLOSE
                    IF   GROSS OF FORMAT2 = ZEROS
                         CALL "LFSOVRDSPC" USING DISPLAY-WRK
                                                 DISPLAY-WRK-21
                         END-CALL
                    ELSE
                         CALL "LFSDLTOVR"   USING DISPLAY-WRK
                         END-CALL
                    END-IF
                    PERFORM DISPLAY-OPEN
                    MOVE GROSS OF FORMAT2 TO GROSS-KLEIN-WRK
           END-IF.


       ANZ-SET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚*  ARRAY FREIGEBEN, OVR ENTFERNEN UND TEILDATEI ENTSPERREN
     ‚*---------------------------------------------------------------
       AKTION-ABSCHLIESSEN SECTION.
       AKT-ABS-00.

           IF       X-RETCODE NOT = "2"
                    SET     DB-PTR TO QSETSRC-DP
                    PERFORM DB-CLOSE
     ‚*             ARRAY IN LGMAPDTA FREIGEBEN
                    MOVE    MAP-CLEAR TO MAP-ACTION OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
           END-IF.

     ‚* OVR ENTFERNEN UND GGF. TEILDATEI ENTSPERREN
           MOVE     "C"        TO OPEN-CLOSE.
           CALL     "DMP0023" USING MBR-WRK
                                    FILE-WRK
                                    LIB-WRK
                                    READONLY-WRK
                                    OPEN-CLOSE
                                    RET-CODE
           END-CALL.

     ‚* PSETLOG LEEREN, DA BEARBEITUNG ABGESCHLOSSEN IST
           IF       X-READONLY NOT = "1"
            AND     X-RETCODE  NOT = "2"
                    PERFORM CLEAR-PSETLOG
           END-IF.

     ‚* WENN EINE QUELLE GEÄNDERT WURDE DANN MUSS UNTER LINUX
     ‚* DIE TABELLE ZURÜCK IN DIE DATEI GESCHRIEBEN WERDEN, DAMIT
     ‚* DIESE GESICHERT IST
           IF       SOURCE-CHANGED-LINUX = "*"
      *     AND     F3               NOT = "1"
            AND     READONLY-WRK     NOT = "1"
                    MOVE SPACES TO X-READONLY
           ELSE
                    MOVE "1"    TO X-READONLY
           END-IF.

       AKT-ABS-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* BILDSCHIRM BEARBEITUNG EINBINDEN
     ‚*---------------------------------------------------------------

           COPY DSPWORKING OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* ERMITTELN CURSERPOSITION
     ‚*---------------------------------------------------------------

           COPY CURSER OF QCPYSRC.

     ‚*---------------------------------------------------------------
     ‚* Aufruf zum ermitteln des Datums und der Uhrzeit
     ‚*---------------------------------------------------------------

           COPY GETTIME OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* Aufruf LGMAPDTA
     ‚*---------------------------------------------------------------

           COPY LGMAPDTA OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* Aufruf von Standardroutinen, die in jedem Programm beim Starten
     ‚* ausgeführt werden sollen.
     ‚*---------------------------------------------------------------

           COPY PGMINIT OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* Aufruf von Standardroutinen, die in jedem Programm beim
     ‚* Verlassen ausgeführt werden sollen.
     ‚*---------------------------------------------------------------

           COPY PGMEXIT OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* DBMAIN BEARBEITUNG EINBINDEN
     ‚*---------------------------------------------------------------

           COPY DBWORKING OF QCPYSRC.
      /
