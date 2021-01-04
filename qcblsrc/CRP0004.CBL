     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CRP0004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* EINGABE SYNTAX-CODEWÖRTER                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SYNTAX-CODEWÖRTER                            *
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
                  ASSIGN       TO  WORKSTATION-CRD0004DE
                  ORGANIZATION IS  TRANSACTION
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* SYNTAX-WÖRTER
     ‚*-------------------------------------------------------------
           SELECT PCHKHGH-DP
                  ASSIGN       TO  DATABASE-PCHKHGH
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
            COPY DDS-ALL-FORMATS OF CRD0004DE.
      /
     ‚*--- SYNTAX
       FD  PCHKHGH-DP
           LABEL RECORDS ARE STANDARD.
       01  PCHKHGH-P.
           COPY DDS-ALL-FORMATS OF PCHKHGH.
      /
       WORKING-STORAGE SECTION.

     ‚* LISTE DER TEMPORÄREN INDIKATOREN
       01  INXX.
         02 IN01                          PIC 1.
         02 IN02                          PIC 1.
         02 IN41                          PIC 1.
         02 IN42                          PIC 1.
         02 IN91                          PIC 1.
         02 IN95                          PIC 1.
         02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  CRD0004DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CRD0004DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CRD0004DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CRD0004DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  FORMAT6X.
           COPY  DDS-FMT06-O OF  CRD0004DE
           REPLACING FMT06-O BY FORMAT6
                     FMT06-O-INDIC BY FORMAT6-O-INDIC.

       01  FORMAT6Y.
           COPY  DDS-FMT06-I OF  CRD0004DE
           REPLACING FMT06-I BY FORMAT6-I
                     FMT06-I-INDIC BY FORMAT6-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0004".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  FORMAT6S                      LIKE FORMAT6.

       01  AUSWAHL                       PIC X(2).
       01  ANZEIGE-TYP                   PIC 9(1).
       01  CMD-SELEKTION                 LIKE SYCMD  OF PCHKHGH-P.
       01  LEN                           PIC 9(4).
       01  LEN2                          PIC 9(4).

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.

     ‚* PARAMETER FÜR DAS HLP0011
       01  SYHEX-WRK                     LIKE SYHEX  OF PCHKHGH-P.
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-SYTYP                       LIKE SYTYP  OF PCHKHGH-P.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-SYTYP
                                         X-F12.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0004" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.
           OPEN     I-O   PCHKHGH-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE F3, F12.
           MOVE     SPACES TO CMD-SELEKTION.
           MOVE     ZEROES TO LEN.
           MOVE     1 TO SFL-ZEILE.

       ANF010.
           MOVE     AUS TO INXX.

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

           GO TO    ANF010.

       ENDE.
           CLOSE                 WS-DISPLAY
                                 PCHKHGH-DP.

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
           INITIALIZE PCHKHGHF OF PCHKHGH-P.
           MOVE     T-USER              TO SYUSER OF PCHKHGH-P.
           MOVE     X-SYTYP             TO SYTYP  OF PCHKHGH-P.
           MOVE     SPACES              TO SYCMD  OF PCHKHGH-P.
           START    PCHKHGH-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCHKHGH-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* KEY PRÜFEN
           IF       SYUSER OF PCHKHGH-P NOT = T-USER
            OR      SYTYP  OF PCHKHGH-P NOT = X-SYTYP
                    GO TO LES-DAT-90
           END-IF.

     ‚* ANZEIGE BESTIMMTER TAGS?
           IF       LEN                        > ZEROES
            AND     CMD-SELEKTION(1:1)     NOT = "*"
                    IF   CMD-SELEKTION(1:LEN) NOT =
                         SYCMD  OF PCHKHGH-P(1:LEN)
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
                    MOVE    CORR INXX  TO FMT03-O-INDIC
                    WRITE   SUBFILE WS-REC FORMAT IS "FMT03"
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
     ‚* INAKTIVE TAGS PINK ANZEIGEN
           IF       SYAKT  OF PCHKHGH-P = ZEROES
                    MOVE AN  TO IN42 OF INXX
           ELSE
                    MOVE AUS TO IN42 OF INXX
           END-IF.
           MOVE     CORR PCHKHGHF OF PCHKHGH-P TO FORMAT2.
           MOVE     SPACES       TO AUSW OF FORMAT2.
     ‚*€   MOVE     CORR INXX    TO FMT02-O-INDIC.
           MOVE     CORR FORMAT2 TO FMT02-O.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT02".
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
                    MOVE CMD-SELEKTION TO SYCMD OF FORMAT3
           END-IF.

           MOVE     ZEROES TO LEN.
           MOVE     AUS    TO INXX.
  ******   MOVE     1      TO  SUBFILE-RECORD-NUMBER.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           WRITE    WS-REC FORMAT IS "FMT05".
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
           MOVE     CORR   INXX     TO  FMT03-O-INDIC.
           MOVE     CORR   FORMAT3  TO  FMT03-O.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT03".
           MOVE     1      TO  SUBFILE-RECORD-NUMBER.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           READ     WS-DISPLAY  FORMAT IS "FMT03".
           MOVE     CORR FMT03-I      TO FORMAT3.
           MOVE     AUS  TO INXX

     ‚* BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
     ‚*€   MOVE     9999 TO SFL-ZEILE.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* ANLEGEN EINES NEUEN WORTES?
           PERFORM  ANLEGEN-CMD.
           IF       ERRFLD-WRK NOT = SPACES
     ‚*             BESCHREIBUNG FEHLT
                    MOVE     "ERR0001"         TO CPY-TXID
                    MOVE     SPRACHE-WRK       TO CPY-TXSPR
                    PERFORM  COPY-PGM-TXT
                    MOVE     CPY-TXTEXT        TO ERRMSG   OF FORMAT3
                    MOVE     AN TO IN01 OF INXX
                    MOVE     AN TO IN02 OF INXX
     ‚*             CURSER SETZEN
                    MOVE     AN TO IN41 OF INXX
                    GO TO ANZ-SFL-20
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           READ     SUBFILE WS-DISPLAY NEXT MODIFIED RECORD
                    FORMAT IS "FMT02".
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

           MOVE     CORR FMT02-I TO FORMAT2.

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
                         MOVE     CORR FORMAT2 TO FMT02-O
                         REWRITE  SUBFILE WS-REC FORMAT IS "FMT02"
                         GO TO ANZ-SFL-50
                    END-IF
           END-IF.

     ‚* ÄNDERN DES BEFEHLS
           IF       AUSW OF FORMAT2 = "2"
                    PERFORM CHANGE-CMD
                    INITIALIZE F12
           END-IF.

     ‚* AUSWÄHLEN DES BEFEHLS
           IF       AUSW OF FORMAT2 = "6"
                    PERFORM SWITCH-AKTIVITAET
           END-IF.

     ‚* LÖSCHEN EINES BEFEHLS
           IF       AUSW OF FORMAT2 = "4"
                    PERFORM  DELETE-REC
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2
           MOVE     CORR FORMAT2 TO FMT02-O

           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           REWRITE  SUBFILE WS-REC FORMAT IS "FMT02".

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
                    MOVE CMD-SELEKTION TO SYCMD OF FORMAT3
           END-IF.

           INITIALIZE LEN.
     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           WRITE    WS-REC FORMAT IS "FMT04".
           WRITE    WS-REC FORMAT IS "FMT05".
     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FMT03-O-INDIC.
           MOVE     CORR   FORMAT3  TO  FMT03-O.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT03".
           MOVE     1      TO  SUBFILE-RECORD-NUMBER.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           READ     WS-DISPLAY  FORMAT IS "FMT03".
           MOVE     CORR FMT03-I      TO FORMAT3.

           IF       IN12 OF FMT03-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

     ‚* ANLEGEN EINES NEUEN COMMANDS
           PERFORM  ANLEGEN-CMD.
           IF       ERRFLD-WRK NOT = SPACES
                    GO TO FEH-MEL-00
           END-IF.
           IF       LEN            > ZEROES
                    GO TO FEH-MEL-90
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WECHSELN DER AKTIVITÄT (AKTIV/INAKTIV)
     ‚*--------------------------------------------------------------
       SWITCH-AKTIVITAET SECTION.
       SWI-AKT-00.

           INITIALIZE PCHKHGHF OF PCHKHGH-P.
           MOVE     SYUSER OF FORMAT2   TO SYUSER OF PCHKHGH-P.
           MOVE     SYTYP  OF FORMAT2   TO SYTYP  OF PCHKHGH-P.
           MOVE     SYCMD  OF FORMAT2   TO SYCMD  OF PCHKHGH-P.
           READ     PCHKHGH-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO SWI-AKT-90
           END-IF.

           EVALUATE SYAKT  OF PCHKHGH-P
                    WHEN ZEROES
                         MOVE 1      TO SYAKT  OF PCHKHGH-P
                    WHEN 1
                         MOVE ZEROES TO SYAKT  OF PCHKHGH-P
           END-EVALUATE.

     ‚* USER/DATUM FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER           TO SYMSER OF PCHKHGH-P.
           MOVE     T-TERM           TO SYMBS  OF PCHKHGH-P.
           MOVE     PGM-WRK          TO SYMPGM OF PCHKHGH-P.
           MOVE     DATE-8           TO SYMDTA OF PCHKHGH-P.
           MOVE     TIME-6           TO SYMTIA OF PCHKHGH-P.

           REWRITE  PCHKHGH-P.

       SWI-AKT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN EINES NEUEN TAGS
     ‚*--------------------------------------------------------------
       ANLEGEN-CMD SECTION.
       ANL-CMD-00.

           INITIALIZE ERRFLD-WRK.
           MOVE     ZEROES TO LEN.
     ‚* WENN KEIN TAG EINGEGEBEN WURDE, DANN ROUTINE BEENDEN
           IF       SYCMD  OF FORMAT3 = SPACES
                    GO TO ANL-CMD-90
           END-IF.

     ‚* GIBT ES DEN TAG SCHON? DANN DIESEN ZURÜCKGEBEN
           PERFORM  CHECK-CMD.
           IF       RET-CODE = SPACES
                    GO TO ANL-CMD-90
           END-IF.

     ‚* BEINHALTET DER TAG EINEN STERN, DANN IST EINE TEILAUFLISTUNG
     ‚* GEWÜNSCHT
           IF       SYCMD  OF FORMAT3      NOT = SPACES
             AND    SYCMD  OF FORMAT3(1:1) NOT = "*"
                    MOVE    ZEROES TO LEN
                    INSPECT SYCMD  OF FORMAT3 TALLYING LEN FOR
                            CHARACTERS BEFORE INITIAL "*"
     ‚*             STERN GEFUNDEN?
                    IF   LEN     > ZEROES
                     AND LEN NOT = LENGTH OF SYCMD  OF FORMAT3
                         MOVE SYCMD  OF FORMAT3 TO CMD-SELEKTION
                         GO TO ANL-CMD-90
                    ELSE
                         INITIALIZE LEN
                    END-IF
           END-IF.

           INITIALIZE LEN2.
     ‚* DATENSATZ SCHREIBEN
           INITIALIZE PCHKHGHF OF PCHKHGH-P.
     ‚* DATEN FÜLLEN
           MOVE     T-USER                 TO SYUSER OF PCHKHGH-P.
           MOVE     X-SYTYP                TO SYTYP  OF PCHKHGH-P.
           MOVE     SYCMD  OF FORMAT3      TO SYCMD  OF PCHKHGH-P.

           COMPUTE  LEN2 = LENGTH OF SYCMD  OF FORMAT3.
           PERFORM  UNTIL SYCMD  OF FORMAT3(LEN2:1) NOT = SPACES
                     OR   LEN2                          = ZEROES
                    SUBTRACT 1 FROM LEN2
           END-PERFORM.
           MOVE     LEN2                   TO SYLEN  OF PCHKHGH-P.

     ‚* FARBWERT ERMITTELN
           EVALUATE X-SYTYP
                    WHEN "CONST"
                         MOVE "YELLOW" TO COLOR-WRK
                    WHEN "E+P"
                         MOVE "RED"    TO COLOR-WRK
                    WHEN "OTHER"
                         MOVE "PINK"   TO COLOR-WRK
                    WHEN OTHER
                         MOVE "BLUE"   TO COLOR-WRK
           END-EVALUATE.

           MOVE     SPACES          TO INVERS-WRK.
           MOVE     SPACES          TO BLINK-WRK.
           MOVE     SPACES          TO UNDERLINE-WRK.
           MOVE     SPACES          TO SYHEX-WRK.
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    SYHEX-WRK
           END-CALL.
           MOVE     SYHEX-WRK              TO SYHEX  OF PCHKHGH-P.
           MOVE     COLOR-WRK              TO SYCOLR OF PCHKHGH-P.

           MOVE     1                      TO SYAKT  OF PCHKHGH-P.
     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                 TO SYAUSR OF PCHKHGH-P.
           MOVE     T-TERM                 TO SYABS  OF PCHKHGH-P.
           MOVE     PGM-WRK                TO SYAPGM OF PCHKHGH-P.
           MOVE     DATE-8                 TO SYADTA OF PCHKHGH-P.
           MOVE     TIME-6                 TO SYATIA OF PCHKHGH-P.

           WRITE    PCHKHGH-P.

       ANL-CMD-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CRP0004*3"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT3.
           MOVE     X-SYTYP           TO SYTYP   OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN EINES BEFEHLS
     ‚*--------------------------------------------------------------
       CHANGE-CMD SECTION.
       CHG-CMD-00.

           INITIALIZE FORMAT6.
     ‚* ÜBERSCHRIFT
           MOVE     "CRP0004*6"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT6.
           MOVE     SPACES TO F3, F12.
           MOVE     AUS    TO INXX.

     ‚* LESEN DES BEFEHLS
           INITIALIZE PCHKHGHF OF PCHKHGH-P.
           MOVE     SYUSER OF FORMAT2   TO SYUSER OF PCHKHGH-P.
           MOVE     SYTYP  OF FORMAT2   TO SYTYP  OF PCHKHGH-P.
           MOVE     SYCMD  OF FORMAT2   TO SYCMD  OF PCHKHGH-P.
           READ     PCHKHGH-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-CMD-90
           END-IF.
     ‚* VON SPACES AUF ZEROES MAPPEN
           IF       SYINVS OF PCHKHGH-P = SPACES
                    MOVE ZEROES TO SYINVS OF PCHKHGH-P
           END-IF.
           IF       SYUNDR OF PCHKHGH-P = SPACES
                    MOVE ZEROES TO SYUNDR OF PCHKHGH-P
           END-IF.
           IF       SYBLNK OF PCHKHGH-P = SPACES
                    MOVE ZEROES TO SYBLNK OF PCHKHGH-P
           END-IF.
           MOVE     CORR PCHKHGHF OF PCHKHGH-P TO FORMAT6.

       CHG-CMD-10.
     ‚* FARBWERT ERMITTELN
           MOVE     SPACES            TO INVERS-WRK.
           MOVE     SPACES            TO BLINK-WRK.
           MOVE     SPACES            TO UNDERLINE-WRK.
           MOVE     SPACES            TO SYHEX-WRK.
           MOVE     SYCOLR OF FORMAT6 TO COLOR-WRK.
           IF       SYINVS OF FORMAT6 = "1"
                    MOVE SYINVS OF FORMAT6 TO INVERS-WRK
           END-IF.
           IF       SYUNDR OF FORMAT6 = "1"
                    MOVE SYUNDR OF FORMAT6 TO UNDERLINE-WRK
           END-IF.
           IF       SYBLNK OF FORMAT6 = "1"
                    MOVE SYBLNK OF FORMAT6 TO BLINK-WRK
           END-IF.
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    SYHEX-WRK
           END-CALL.
           MOVE     SYHEX-WRK           TO BEFEHL OF FORMAT6(1:1).
           MOVE     SYCMD  OF PCHKHGH-P TO BEFEHL OF FORMAT6(2:).

           MOVE     FORMAT6 TO FORMAT6S.
     ‚*€   MOVE     CORR   INXX     TO  FMT06-O-INDIC.
           MOVE     CORR   FORMAT6  TO  FMT06-O.
           WRITE    WS-REC FORMAT IS "FMT06".
           READ     WS-DISPLAY  FORMAT IS "FMT06".
           MOVE     CORR FMT06-I      TO FORMAT6.
     ‚*€   MOVE     AUS  TO INXX

     ‚* F12 ABBRECHEN
           IF       IN12 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO CHG-CMD-90
           END-IF.

     ‚* WENN WAS GEÄNDERT WURDE, ÄNDERUNG ANZEIGEN
           IF       FORMAT6 NOT = FORMAT6S
                    GO TO CHG-CMD-10
           END-IF.

     ‚* UPDATEN DES BEFEHLS
           INITIALIZE PCHKHGHF OF PCHKHGH-P.
           MOVE     SYUSER OF FORMAT2   TO SYUSER OF PCHKHGH-P.
           MOVE     SYTYP  OF FORMAT2   TO SYTYP  OF PCHKHGH-P.
           MOVE     SYCMD  OF FORMAT2   TO SYCMD  OF PCHKHGH-P.
           READ     PCHKHGH-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE CORR FORMAT6 TO PCHKHGHF OF PCHKHGH-P
     ‚*             VON ZEROES AUF SPACES MAPPEN
                    IF   SYINVS OF PCHKHGH-P = ZEROES
                         MOVE SPACES TO SYINVS OF PCHKHGH-P
                    END-IF
                    IF   SYUNDR OF PCHKHGH-P = ZEROES
                         MOVE SPACES TO SYUNDR OF PCHKHGH-P
                    END-IF
                    IF   SYBLNK OF PCHKHGH-P = ZEROES
                         MOVE SPACES TO SYBLNK OF PCHKHGH-P
                    END-IF
                    MOVE SYHEX-WRK   TO SYHEX  OF PCHKHGH-P
     ‚*             USER/DATUM FÜLLEN
                    PERFORM  COPY-GET-TIME
                    MOVE T-USER      TO SYMSER OF PCHKHGH-P
                    MOVE T-TERM      TO SYMBS  OF PCHKHGH-P
                    MOVE PGM-WRK     TO SYMPGM OF PCHKHGH-P
                    MOVE DATE-8      TO SYMDTA OF PCHKHGH-P
                    MOVE TIME-6      TO SYMTIA OF PCHKHGH-P
                    REWRITE PCHKHGH-P
           END-IF.

       CHG-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN EINES DATENSATZES
     ‚*--------------------------------------------------------------
       DELETE-REC SECTION.
       DEL-REC-00.

           INITIALIZE PCHKHGHF OF PCHKHGH-P.
           MOVE     SYUSER OF FORMAT2 TO SYUSER OF PCHKHGH-P.
           MOVE     SYTYP  OF FORMAT2 TO SYTYP  OF PCHKHGH-P.
           MOVE     SYCMD  OF FORMAT2 TO SYCMD  OF PCHKHGH-P.
           READ     PCHKHGH-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-REC-90
           END-IF.

           IF       SYSTAN OF PCHKHGH-P NOT = ZEROES
                    GO TO DEL-REC-90
           END-IF.

           INITIALIZE PCHKHGHF OF PCHKHGH-P.
           MOVE     SYUSER OF FORMAT2 TO SYUSER OF PCHKHGH-P.
           MOVE     SYTYP  OF FORMAT2 TO SYTYP  OF PCHKHGH-P.
           MOVE     SYCMD  OF FORMAT2 TO SYCMD  OF PCHKHGH-P.
           READ     PCHKHGH-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PCHKHGH-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PCHKHGH"   TO FILE-WRK
                    MOVE PCHKHGH-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       DEL-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN TAG SCHON EXISTIERT
     ‚*--------------------------------------------------------------
       CHECK-CMD SECTION.
       CHK-CMD-00.

           INITIALIZE RET-CODE.
     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PCHKHGHF OF PCHKHGH-P.
           MOVE     T-USER              TO SYUSER OF PCHKHGH-P.
           MOVE     X-SYTYP             TO SYTYP  OF PCHKHGH-P.
           MOVE     SYCMD  OF FORMAT3   TO SYCMD  OF PCHKHGH-P.
           READ     PCHKHGH-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-CMD-90
           END-IF.

       CHK-CMD-90.
           EXIT.
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
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
