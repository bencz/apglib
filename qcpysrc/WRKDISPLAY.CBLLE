     ‚*---------------------------------------------------------------*
     ‚* STANDARDFELDER DIE IN DEN MEISTEN PROGRAMMEN GENUTZT WERDEN   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  STANDARDFELDER FÜR DISPLAY                   *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*

       01  DISPLAY-WRK                    PIC X(10).
       01  REOPEN-DISPLAY-WRK        LIKE DISPLAY-WRK.
       01  FMT-WRK                        PIC X(10).
       01  WSREC-WRK                      PIC X(4096).

     ‚* INDIKATORSTEUERUNG AN/AUS
       01  AN                             PIC 1 VALUE B"1".
       01  AUS                            PIC 1 VALUE B"0".
     ‚
     ‚* ÜBERWACHUNGS- UND STEUERBEREICH DER BILDSCHIRMDATEI
       77  BS-STAT                        PIC X(2).
     ‚* DIE AUFTEILUNG DIESES PUFFERS IST IM COBOL-HANDBUCH
     ‚* REFERENZ IM KAPITEL ATTRIBUTENFORMATE BESCHRIEBEN
     ‚* QUELLE SC09-1240-2
       01  BS-ATR.
        05 PGM-DEV-NAME                  PIC X(10).
        05 DEV-DESC-NAME                 PIC X(10).
        05 USER-ID                       PIC X(10).
        05 DEV-CLASS                     PIC X(1).
        05 DEV-TYPE                      PIC X(6).
        05 REQ-DEV                       PIC X(1).
        05 ACQ-STATUS                    PIC X(1).
        05 INV-STATUS                    PIC X(1).
        05 DATA-AVAIL-STATUS             PIC X(1).
        05 DSP-DIM-ZEILE                 PIC S9(4) BINARY.
        05 DSP-DIM-SPALTE                PIC S9(4) BINARY.
        05 DSP-ALLOW-BLK                 PIC X(1).
        05 DSP-ONL-OFFL-STATUS           PIC X(1).
        05 DSP-LOCATION                  PIC X(1).
        05 DSP-TYPE                      PIC X(1).
        05 KEYBOARD-TYPE                 PIC X(1).
        05 CONV-STATUS                   PIC X(1).
        05 SYNCH-LVL                     PIC X(1).
        05 CONV-USED                     PIC X(1).
        05 RMT-LOC-NAME                  PIC X(8).
        05 LOCAL-LU-NAM                  PIC X(8).
        05 LOCAL-NETWRK-ID               PIC X(8).
        05 RMT-LU-NAME                   PIC X(8).
        05 RMT-NETWRK-ID                 PIC X(8).
        05 MODE-M                        PIC X(8).
        05 FILLER                        PIC X(43).

     ‚* I-O-FEEDBACK-AREA
     ‚* DIE AUFTEILUNG DIESES PUFFERS IST IM COBOL-HANDBUCH
     ‚* REFERENZ IM KAPITEL ATTRIBUTENFORMATE BESCHRIEBEN
     ‚* QUELLE SC09-1240-2
       01  BS-IO-AREA.
           05  FILLER                    PIC  X(144).
           05  IOF-FLAG-BITS             PIC  X(2).
           05  IOF-DSP-AID-BYTE          PIC  X(1).
           05  IOF-DSP-CURSER-LINE       PIC  X(1).
           05  IOF-DSP-CURSER-COL        PIC  X(1).
           05  IOF-ICF-DTALEN            PIC  S9(9) BINARY.
           05  IOF-SFL-REC-NBR           PIC  S9(4) BINARY.
           05  IOF-SFL-LOW-NBR           PIC  S9(4) BINARY.
           05  IOF-SFL-REC-COUNTER       PIC  S9(4) BINARY.
           05  IOF-WIN-CURSOR            PIC  X(2).
           05  IOF-RESERVED-17           PIC  X(17).
           05  IOF-MAJOR-RETCODE         PIC  X(2).
           05  IOF-MINOR-RETCODE         PIC  X(2).
           05  IOF-FILLER                PIC  X(41).
       01  SUBFILE-RECORD-NUMBER         PIC 9(4) VALUE  0.
       01  SFL-CHG                       PIC X(1).
       01  SFL-ZEILE                     PIC 9(4) VALUE 0.
       01  CURSER-ZEILE                  PIC 9(2).
       01  CURSER-SPALTE                 PIC 9(3).

       01  WS-CONTROL.
        05 WS-IND                        PIC XX.
        05 WS-TERMINAL                   PIC X(10).
        05 WS-FORMAT                     PIC X(10).

     ‚* RECORD FÜR CLP0055
       01  DSPOVRDSPF-REC.
        05 FROMFILE                      PIC X(10).
        05 TOLIB                         PIC X(10).
        05 TOFILE                        PIC X(10).

     ‚* DEKLARATION DER FUNKTIONSTATSTE
       01  F-KEYS.
        05 F1                            PIC X.
        05 F2                            PIC X.
        05 F3                            PIC X.
        05 F4                            PIC X.
        05 F5                            PIC X.
        05 F6                            PIC X.
        05 F7                            PIC X.
        05 F8                            PIC X.
        05 F9                            PIC X.
        05 F10                           PIC X.
        05 F11                           PIC X.
        05 F12                           PIC X.
        05 F13                           PIC X.
        05 F14                           PIC X.
        05 F15                           PIC X.
        05 F16                           PIC X.
        05 F17                           PIC X.
        05 F18                           PIC X.
        05 F19                           PIC X.
        05 F20                           PIC X.
        05 F21                           PIC X.
        05 F22                           PIC X.
        05 F23                           PIC X.
        05 F24                           PIC X.

     ‚* BYTE ORDER FÜR BINARY FELDER
       01  ENDIAN-MODE                   PIC 9(1) VALUE 0.
       01  BEFORE-WRK                    PIC 9(1) VALUE 0.
       01  WORK-BIN                      PIC S9(4) BINARY VALUE 0.
       01  WORK-BIN-RED REDEFINES WORK-BIN.
        05 WORK-BIN-CHAR1                PIC  X(1).
        05 WORK-BIN-CHAR2                PIC  X(1).

     ‚* INDIKATORSTEUERUNG
       01  DSPATR-WRK.
     ‚* NICHT PROTECTED
        05 COL-GRN                       PIC X(1) VALUE X"20".
        05 COL-GRN-PR                    PIC X(1) VALUE X"A0".
        05 COL-GRN-RI                    PIC X(1) VALUE X"21".
        05 COL-GRN-US                    PIC X(1) VALUE X"24".
        05 COL-GRN-UR                    PIC X(1) VALUE X"25".
        05 COL-BLU                       PIC X(1) VALUE X"3A".
        05 COL-BLU-RI                    PIC X(1) VALUE X"3B".
        05 COL-BLU-US                    PIC X(1) VALUE X"3E".
        05 COL-BLU-UR                    PIC X(1) VALUE X"3F".
        05 COL-PNK                       PIC X(1) VALUE X"38".
        05 COL-PNK-RI                    PIC X(1) VALUE X"39".
        05 COL-PNK-US                    PIC X(1) VALUE X"3C".
        05 COL-PNK-UR                    PIC X(1) VALUE X"3D".
        05 COL-RED                       PIC X(1) VALUE X"28".
        05 COL-RED-RI                    PIC X(1) VALUE X"29".
        05 COL-RED-US                    PIC X(1) VALUE X"2C".
        05 COL-RED-UR                    PIC X(1) VALUE X"2D".
        05 COL-TRQ                       PIC X(1) VALUE X"30".
        05 COL-TRQ-RI                    PIC X(1) VALUE X"31".
        05 COL-TRQ-US                    PIC X(1) VALUE X"34".
        05 COL-TRQ-UR                    PIC X(1) VALUE X"35".
        05 COL-WHT                       PIC X(1) VALUE X"22".
        05 COL-WHT-RI                    PIC X(1) VALUE X"23".
        05 COL-WHT-US                    PIC X(1) VALUE X"26".
        05 COL-WHT-UR                    PIC X(1) VALUE X"27".
        05 COL-YLW                       PIC X(1) VALUE X"32".
        05 COL-YLW-RI                    PIC X(1) VALUE X"33".
        05 COL-YLW-US                    PIC X(1) VALUE X"36".
        05 COL-YLW-UR                    PIC X(1) VALUE X"37".

        05 ATR-ND                        PIC X(1) VALUE X"27".
     ‚* PROTECTED
        05 ATR-PR                        PIC X(1) VALUE X"A0".
        05 ATR-PR-RI                     PIC X(1) VALUE X"A1".
        05 ATR-PR-HI                     PIC X(1) VALUE X"A2".
        05 ATR-PR-HI-RI                  PIC X(1) VALUE X"A3".
        05 ATR-PR-UL                     PIC X(1) VALUE X"A4".
        05 ATR-PR-UL-RI                  PIC X(1) VALUE X"A5".
        05 ATR-PR-UL-HI                  PIC X(1) VALUE X"A6".
        05 ATR-PR-ND                     PIC X(1) VALUE X"A7".
        05 ATR-PR-RED                    PIC X(1) VALUE X"A8".
        05 ATR-PR-RED-RI                 PIC X(1) VALUE X"A9".
        05 ATR-PR-TRQ                    PIC X(1) VALUE X"B0".
        05 ATR-PR-TRQ-RI                 PIC X(1) VALUE X"B1".
        05 ATR-PR-YLW                    PIC X(1) VALUE X"B2".
        05 ATR-PR-YLW-RI                 PIC X(1) VALUE X"B3".
        05 ATR-PR-PNK                    PIC X(1) VALUE X"B8".
        05 ATR-PR-PNK-RI                 PIC X(1) VALUE X"B9".
        05 ATR-PR-BLU                    PIC X(1) VALUE X"BA".
        05 ATR-PR-BLU-RI                 PIC X(1) VALUE X"BB".

       01  DSP-AKTION                    PIC X(20).
     ‚* DISPLAY-ATTRIBUTE
       01  DSP-ATTR.
     ‚*    CURSER AUF X/Y-KOORDINATEN SETZEN
        05 DSP-CURX                      PIC 9(3).
        05 DSP-CURY                      PIC 9(3).
     ‚*    CURSER AUF FELD SETZEN
        05 DSP-CUR-FLD                   PIC X(10).
     ‚*    FEHLER-ID
        05 DSP-ERR-ID                    PIC X(10).
     ‚*    FEHLERFELD
        05 DSP-ERR-FLD                   PIC X(10).
        05 DSP-ERR-FIX                   PIC X(132).
     ‚*    ANZ. VON FEHLERFELDERN
        05 DSP-ERR-POS                   PIC 9(4).
     ‚*    INT. TABELLE
        05 DSP-ERR-TAB                   OCCURS 30 TIMES.
         10 DSI-ERR-FLD                  PIC X(10).

     ‚* DSPMAIN-BEFEHLE
       01  DSPMAIN-COMMANDS.
        05 DSP-OPEN                      PIC X(1) VALUE "1".
        05 DSP-CLOSE                     PIC X(1) VALUE "2".
        05 DSP-WRITE                     PIC X(1) VALUE "3".
        05 DSP-READ                      PIC X(1) VALUE "4".
        05 DSP-SFLREAD                   PIC X(1) VALUE "5".
        05 DSP-SFLWRITE                  PIC X(1) VALUE "6".
        05 DSP-SFLREWRITE                PIC X(1) VALUE "7".
        05 DSP-SFLREADREC                PIC X(1) VALUE "8".
        05 DSP-CURSER                    PIC X(1) VALUE "C".
        05 DSP-READ-NDTA                 PIC X(1) VALUE "N".
     ‚*€
