       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0214.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SCHULDSCHEIN-ERINNERUNG                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  01.10.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SCHULDSCHEINERINNERUNG (WINDOW)              *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-XXD0214DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

     ‚*-----BILDSCHIRMDATEI -------------------------
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF XXD0214DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0214".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0214DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0214DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* PARAMETER FÜR DAS HLP0018
       01  NUM-INPUT                     PIC S9(21)V9(10).
       01  NUM-OUTPUT                    PIC X(40).
       01  FMT-OUTPUT-LEN                PIC 9(2).
       01  FMT-PRECISION                 PIC 9(2).
       01  FMT-DECIMAL                   PIC 9(2).
       01  FMT-EDTCDE                    PIC X(1).
       01  FMT-ROUNDED                   PIC 9(1).
       01  FMT-DECFMT                    PIC X(1).
       01  FMT-JUSTIFIED                 PIC 9(1).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0214" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0214DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE X-F12.
           INITIALIZE FORMAT1.

           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* ÜBERSCHRIFT
       ANF010.
           MOVE     "XXP0214*1"     TO MAINUEB OF FORMAT1.

           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

     ‚* EINGABEN LEER?
           IF       TUSER  OF FORMAT1 = SPACES
            OR      BETRAG OF FORMAT1 = ZEROES
                    GO TO ANF010
           END-IF.

     ‚* JETZT DEN BETRAG AUFBEREITEN
           PERFORM  AUFBEREITEN-BETRAG.
           PERFORM  WRITE-EMAIL.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* BETRAG AUFBEREITEN
     ‚*--------------------------------------------------------------
       AUFBEREITEN-BETRAG SECTION.
       AUF-BET-00.

           MOVE     SPACES            TO NUM-OUTPUT.
           MOVE     BETRAG OF FORMAT1 TO NUM-INPUT.
           MOVE     5                 TO FMT-OUTPUT-LEN.
           MOVE     4                 TO FMT-PRECISION.
           MOVE     2                 TO FMT-DECIMAL.
           MOVE     "3"               TO FMT-EDTCDE.
           MOVE     ZEROES            TO FMT-ROUNDED.
           MOVE     "J"               TO FMT-DECFMT.
           MOVE     1                 TO FMT-JUSTIFIED.
           CALL     PROCEDURE "HLP0018" USING NUM-INPUT
                                              NUM-OUTPUT
                                              FMT-OUTPUT-LEN
                                              FMT-PRECISION
                                              FMT-DECIMAL
                                              FMT-EDTCDE
                                              FMT-ROUNDED
                                              FMT-DECFMT
                                              FMT-JUSTIFIED
           END-CALL.

       AUF-BET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAIL SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-EMAIL SECTION.
       WRT-MAI-00.

     ‚* PRÜFEN OB MAIL GESCHRIEBEN WERDEN SOLL
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H051"            TO CFID   OF CFG-CPY.
           MOVE     "KD-SCHULDSCHEIN" TO CFKEY  OF CFG-CPY.
           MOVE     TUSER  OF FORMAT1 TO CFKEY2 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
            AND     CFFK01 OF CFG-CPY = 1
                    GO TO WRT-MAI-90
           END-IF.

           INITIALIZE MAIL-TAB.
     ‚* SENDER ZUSAMMENBAUEN
           MOVE     "SYSEH03"        TO MAIL-HOST   OF MAIL-TAB.
           MOVE     "25"             TO MAIL-PORT   OF MAIL-TAB.
           STRING   T-USER                  DELIMITED BY SPACES
                    "@EHRHARDT-PARTNER.COM" DELIMITED BY SPACES
               INTO MAIL-SENDER OF MAIL-TAB
           END-STRING.

     ‚* MAIL-TOPIC AUFRUFEN
           STRING   "KÜD-Erinnerung"   DELIMITED BY SPACES
                    ": "               DELIMITED BY SIZE
                    NUM-OUTPUT         DELIMITED BY SPACES
                    " EUR "            DELIMITED BY SIZE
                    "Kaffeekasse"      DELIMITED BY SIZE
               INTO MAIL-TOPIC OF MAIL-TAB
           END-STRING.

     ‚* EMAIL BODY ALLGEMEINE INFOS
           MOVE     1 TO MAIL-LEN.
           STRING   "Hallo,"                   DELIMITED BY SIZE
                    X"2525"                    DELIMITED BY SIZE
                    "der oben genannte "       DELIMITED BY SIZE
                    "Betrag ist noch offen."   DELIMITED BY SIZE
                    X"2525"                    DELIMITED BY SIZE
                    "MfG,"                     DELIMITED BY SIZE
                    X"25"                      DELIMITED BY SIZE
                    T-USER                     DELIMITED BY SPACES
                    X"252525"                  DELIMITED BY SIZE
                    "------------------------" DELIMITED BY SPACES
                    X"25"                      DELIMITED BY SIZE
                    "Diese E-Mail kann in APG" DELIMITED BY SIZE
                    "/CONFIG im Schlüssel H05" DELIMITED BY SIZE
                    "1 unter dem Eintrag KD-S" DELIMITED BY SIZE
                    "CHULDSCHEIN deaktiviert " DELIMITED BY SIZE
                    "werden."                  DELIMITED BY SIZE
               INTO MAIL-DATA
               WITH POINTER MAIL-LEN
           END-STRING.

     ‚* EMPFÄNGER ZUSAMMENSETZEN
           MOVE     1                TO MAIL-ANZAHL OF MAIL-EMPF.
           STRING   TUSER  OF FORMAT1       DELIMITED BY SPACES
                    "@ehrhardt-partner.com" DELIMITED BY SIZE
               INTO MAIL-ADRESSE OF MAIL-EMPF-REC(1)
           END-STRING.
     ‚* ABSENDER IMMER ALS KOPIE EINTRAGEN
           MOVE     1                TO MAIL-ANZAHL OF MAIL-KOPIE.
           STRING   T-USER                  DELIMITED BY SPACES
                    "@ehrhardt-partner.com" DELIMITED BY SIZE
               INTO MAIL-ADRESSE OF MAIL-KOPIE-REC(1)
           END-STRING.

     ‚* MAILPROGRAMM AUFRUFEN
           CALL     "HLP0006"  USING MAIL-HOST
                                     MAIL-PORT
                                     MAIL-SENDER
                                     MAIL-EMPF
                                     MAIL-KOPIE
                                     MAIL-BKOPIE
                                     MAIL-TOPIC
                                     MAIL-DATA
                                     MAIL-FILE
                                     MAIL-RETTXT
           END-CALL.

       WRT-MAI-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
