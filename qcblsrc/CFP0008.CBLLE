     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CFP0008.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* WINDOW ZUR SCHLÜSSELSELEKTION                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  WINDOW-ANZEIGE ZUR KEY-SELEKTION             *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚* BILDSCHIRMDATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-CFD0008DE
                  ORGANIZATION IS  TRANSACTION
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* KONFIGURATION
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY
            LABEL RECORDS ARE OMITTED.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF CFD0008DE.
      /
     ‚*--- KONFIGURATION
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* LISTE DER TEMPORÄREN INDIKATOREN
       01  INXX.
         02 IN01                          PIC 1.
         02 IN02                          PIC 1.
         02 IN91                          PIC 1.
         02 IN95                          PIC 1.
         02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  CFD0008DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CFD0008DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CFD0008DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CFD0008DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0008".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  AUSWAHL                       PIC X(2).
       01  KEY-SELEKTION                 LIKE CFKEY  OF PCONFIG-P.
       01  LEN                           PIC 9(4).

       01  REAL-VALUE                    LIKE CFKEY  OF PCONFIG-P.
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-CFID                        LIKE CFID   OF PCONFIG-P.
       01  X-CFKEY                       LIKE CFKEY  OF PCONFIG-P.
       01  X-CFKEY2                      LIKE CFKEY2 OF PCONFIG-P.
       01  X-CFKEY3                      LIKE CFKEY3 OF PCONFIG-P.
       01  X-CFKEY4                      LIKE CFKEY4 OF PCONFIG-P.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-CFID
                                         X-CFKEY
                                         X-CFKEY2
                                         X-CFKEY3
                                         X-CFKEY4
                                         X-F12.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0008" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0008DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PCONFIG-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚*€   INITIALIZE X-CFKEY, X-CFKEY2, X-CFKEY3, X-CFKEY4.
           INITIALIZE F3, F12.
           MOVE     ZEROES TO LEN.
           MOVE     SPACES TO KEY-SELEKTION.

       ANF010.
           MOVE     AUS TO INXX.
           MOVE     1 TO SFL-ZEILE.

           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
           ELSE
                    PERFORM ANZEIGE-SUBFILE
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

     ‚* SOLANGE WIEDERHOLEN BIS AUSWAHL ERFOLGT
           IF       X-CFKEY = SPACES
                    GO TO ANF010
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     X-CFID              TO CFID   OF PCONFIG-P.
           MOVE     X-CFKEY             TO CFKEY  OF PCONFIG-P.
           MOVE     X-CFKEY2            TO CFKEY2 OF PCONFIG-P.
           MOVE     X-CFKEY3            TO CFKEY3 OF PCONFIG-P.
           MOVE     X-CFKEY4            TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = X-CFID
                    GO TO LES-DAT-90
           END-IF.

     ‚* KOPFSÄTZE ÜBERLESEN
           IF       CFKOPF OF PCONFIG-P NOT = ZEROES
                    GO TO LES-DAT-20
           END-IF.

     ‚* BESONDERE SELEKTIONEN BEACHTEN?
           IF       X-CFKEY             NOT = SPACES
            AND     CFKEY  OF PCONFIG-P NOT = X-CFKEY
                    GO TO LES-DAT-20
           END-IF.
           IF       X-CFKEY2            NOT = SPACES
            AND     CFKEY2 OF PCONFIG-P NOT = X-CFKEY2
                    GO TO LES-DAT-20
           END-IF.
           IF       X-CFKEY3            NOT = SPACES
            AND     CFKEY3 OF PCONFIG-P NOT = X-CFKEY3
                    GO TO LES-DAT-20
           END-IF.
           IF       X-CFKEY4            NOT = SPACES
            AND     CFKEY4 OF PCONFIG-P NOT = X-CFKEY4
                    GO TO LES-DAT-20
           END-IF.

     ‚* BESTIMMTE WERTE NICHT ANZEIGEN
           IF       CFID   OF PCONFIG-P     = "K110"
            AND     CFKEY2 OF PCONFIG-P NOT = SPACES
                    GO TO LES-DAT-20
           END-IF.

     ‚* ANZEIGE BESTIMMTER TAGS?
           IF       LEN                        > ZEROES
            AND     KEY-SELEKTION(1:1)     NOT = "*"
                    IF   KEY-SELEKTION(1:LEN) NOT =
                         CFKEY  OF PCONFIG-P(1:LEN)
                         GO TO LES-DAT-20
                    END-IF
           END-IF.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
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
           MOVE     CORR PCONFIGF OF PCONFIG-P TO FORMAT2.

     ‚* BEI EINIGEN SCHLÜSSELN IST NICHT CFKEY DER ZIEHENDE WERT
           PERFORM  GET-REAL-VALUE.
           MOVE     REAL-VALUE   TO ANZVAL OF FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
     ‚*€   MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
     ‚* SELEKTION ANZEIGEN
           IF       LEN > ZEROES
                    MOVE KEY-SELEKTION TO CFKEY  OF FORMAT3
           END-IF.

           MOVE     ZEROES TO LEN.
           MOVE     AUS    TO INXX.
  ******   MOVE     1      TO  SUBFILE-RECORD-NUMBER.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
           IF       SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                    MOVE SFL-ZEILE TO  SUBFILE-RECORD-NUMBER
           END-IF.
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

     ‚* BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
           MOVE     9999 TO SFL-ZEILE.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* BEINHALTET DER KEY EINEN STERN, DANN IST EINE TEILAUFLISTUNG
     ‚* GEWÜNSCHT
           IF       CFKEY  OF FORMAT3      NOT = SPACES
             AND    CFKEY  OF FORMAT3(1:1) NOT = "*"
             AND    CFKEY  OF FORMAT3      NOT = KEY-SELEKTION
                    MOVE    ZEROES TO LEN
                    INSPECT CFKEY  OF FORMAT3 TALLYING LEN FOR
                            CHARACTERS BEFORE INITIAL "*"
     ‚*             STERN GEFUNDEN?
                    IF   LEN     > ZEROES
                     AND LEN NOT = LENGTH OF CFKEY  OF FORMAT3
                         MOVE CFKEY  OF FORMAT3 TO KEY-SELEKTION
                    ELSE
                         INITIALIZE LEN
                         MOVE CFKEY  OF FORMAT3 TO X-CFKEY
                    END-IF
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

     ‚* AUSWÄHLEN DES TAGS
           IF       AUSW OF FORMAT2 = "1"
                    MOVE CFKEY  OF FORMAT2 TO X-CFKEY
                    MOVE CFKEY2 OF FORMAT2 TO X-CFKEY2
                    MOVE CFKEY3 OF FORMAT2 TO X-CFKEY3
                    MOVE CFKEY4 OF FORMAT2 TO X-CFKEY4
                    GO TO ANZ-SFL-90
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
     ‚* SELEKTION ANZEIGEN
           IF       LEN > ZEROES
                    MOVE KEY-SELEKTION TO CFKEY OF FORMAT3
           END-IF.

           INITIALIZE LEN.
     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PPGMTXT FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
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

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

     ‚* BEINHALTET DER KEY EINEN STERN, DANN IST EINE TEILAUFLISTUNG
     ‚* GEWÜNSCHT
           IF       CFKEY  OF FORMAT3      NOT = SPACES
             AND    CFKEY  OF FORMAT3(1:1) NOT = "*"
                    MOVE    ZEROES TO LEN
                    INSPECT CFKEY  OF FORMAT3 TALLYING LEN FOR
                            CHARACTERS BEFORE INITIAL "*"
     ‚*             STERN GEFUNDEN?
                    IF   LEN     > ZEROES
                     AND LEN NOT = LENGTH OF CFKEY  OF FORMAT3
                         MOVE CFKEY  OF FORMAT3 TO KEY-SELEKTION
                    ELSE
                         INITIALIZE LEN
                         MOVE CFKEY  OF FORMAT3 TO X-CFKEY
                    END-IF
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CFP0008*3"       TO MAINUEB OF FORMAT3.
           MOVE     X-CFID            TO CFID    OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WERT ERMITTELN
     ‚*--------------------------------------------------------------
       GET-REAL-VALUE SECTION.
       GET-VAL-00.

           INITIALIZE REAL-VALUE.
     ‚* JE NACH ID ERMITTELN
           EVALUATE X-CFID
                    WHEN "CP"  MOVE  CFKEY3 OF PCONFIG-P TO REAL-VALUE
                    WHEN OTHER MOVE  CFKEY  OF PCONFIG-P TO REAL-VALUE
           END-EVALUATE.

       GET-VAL-90.
           EXIT.
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
