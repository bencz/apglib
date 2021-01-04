       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              AJP0002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* NEUANLAGE EINES THEMAS                                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  04.06.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  NEUANLAGE EINES THEMAS                       *
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
                  ASSIGN       TO  WORKSTATION-AJD0002DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI THEMEN
     ‚*-------------------------------------------------------------
           SELECT PAJXTHM-DP
                  ASSIGN       TO  DATABASE-PAJXTHM
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TEXTE
     ‚*-------------------------------------------------------------
           SELECT PAJXTXT-DP
                  ASSIGN       TO  DATABASE-PAJXTXT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF AJD0002DE.
      /
     ‚*--- PROGRAMME
       FD  PAJXTHM-DP
           LABEL RECORDS ARE STANDARD.
       01  PAJXTHM-P.
           COPY DDS-ALL-FORMATS OF PAJXTHM.
     ‚*--- TAGS
       FD  PAJXTXT-DP
           LABEL RECORDS ARE STANDARD.
       01  PAJXTXT-P.
           COPY DDS-ALL-FORMATS OF PAJXTXT.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN43                          PIC 1.
        05 IN44                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  AJD0002DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  AJD0002DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  AJD0002DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  AJD0002DE
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

       01  PGM-WRK                       PIC X(10) VALUE "AJP0002".
       01  PGM-TYP                       PIC X(8)  VALUE "AJAX".

     ‚* SONSTIGE VARIABLEN
       01  AUSWAHL                       PIC X(2).
       01  NEUANLAGE                     PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-AXID                        LIKE AXID   OF PAJXTHM-P.
       01  X-AXKEY                       LIKE AXKEY  OF PAJXTHM-P.
       01  X-MODUS                       PIC X(1).
       01  X-PGM                         LIKE PGM-WRK.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-AXID
                                         X-AXKEY
                                         X-MODUS
                                         X-PGM
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AJP0002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "AJD0002DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PAJXTHM-DP
           OPEN     INPUT PAJXTXT-DP.

           INITIALIZE F3, F12.
           INITIALIZE NEUANLAGE.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     1 TO SFL-ZEILE.
           MOVE     AUS TO INXX.

       ANF020.
     ‚* PAJXTHM LESEN
           INITIALIZE PAJXTHMF OF PAJXTHM-P.
           MOVE     X-AXID              TO AXID   OF PAJXTHM-P.
           MOVE     X-AXKEY             TO AXKEY  OF PAJXTHM-P.
           READ     PAJXTHM-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PAJXTHMF OF PAJXTHM-P
                    MOVE 1      TO NEUANLAGE
           ELSE
                    MOVE ZEROES TO NEUANLAGE
           END-IF.

     ‚* BEI NEUANLAGE EINES UNTERTHEMAS, IST KLAR DAS WIR DEN DATENSATZ
     ‚* OBEN FINDEN, DA DIES DAS HAUPTTHEMA IST (X-AXKEY = SPACES).
     ‚* DARUM HIER VON HAND AUF NEUANLAGE SCHALTEN
           IF       X-AXKEY = SPACES
            AND     X-MODUS = "U"
                    MOVE 1      TO NEUANLAGE
           END-IF.

     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         MOVE "1" TO X-F3
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         MOVE "1" TO X-F12
                         GO TO ENDE
                    END-IF
                    GO TO ANF020
           END-IF.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-SUBFILE.
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

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PAJXTHM-DP
                                 PAJXTXT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
     ‚* LESEN ALLER VORHANDENEN TEXTE
           INITIALIZE PAJXTXTF OF PAJXTXT-P.
           MOVE     X-AXID                   TO PSID   OF PAJXTXT-P.
           MOVE     X-AXKEY                  TO PSKEY  OF PAJXTXT-P.
           MOVE     ZEROES                   TO PSLFPO OF PAJXTXT-P.
           START    PAJXTXT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PAJXTXT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       PSID   OF PAJXTXT-P    NOT = X-AXID
            OR      PSKEY  OF PAJXTXT-P    NOT = X-AXKEY
                    GO TO LES-DAT-90
           END-IF.

     ‚* AUSGABE EINER SUBFILE-ZEILE
           PERFORM  AUSGABE-SFL.
           IF       SUBFILE-RECORD-NUMBER < 9999
                    GO TO LES-DAT-20
           END-IF.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER SFL-ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-SFL SECTION.
       AUS-SFL-00.

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
                    MOVE    AUS     TO INXX
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PAJXTXTF OF PAJXTXT-P TO FORMAT2.
           MOVE     SPACES       TO AUSW OF FORMAT2.
     ‚*€   MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

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

           MOVE     PGM-TYP                    TO PGMTYP OF FORMAT3.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE AN  TO IN43 OF INXX
                    MOVE CORR PAJXTHMF OF PAJXTHM-P TO FORMAT3
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    MOVE AUS TO IN43 OF INXX
                    MOVE X-AXID                 TO AXID   OF FORMAT3
                    MOVE X-AXKEY                TO AXKEY  OF FORMAT3
           END-IF.

     ‚* BEI NEUANLAGE EINES HAUPTTHEMAS DAS UNTERTHEMA AUSBLENDEN
           EVALUATE X-MODUS
                    WHEN "H"
                         MOVE AUS TO IN44 OF INXX
                    WHEN OTHER
                         MOVE AN  TO IN43 OF INXX
                         MOVE AN  TO IN44 OF INXX
           END-EVALUATE.

     ‚* ANZEIGE SUBFILE
           IF       SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                    MOVE SFL-ZEILE TO  SUBFILE-RECORD-NUMBER
           END-IF.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AN     TO IN91  IN  INXX.
           MOVE     CORR INXX TO  FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.

     ‚* BEI DATENFREIGABE WIRD ZUM ENDE DES SFL-GESPRUNGEN
           MOVE     9999 TO SFL-ZEILE.

     ‚* F3 ENDE
           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F9 BESCHREIBUNG PFLEGEN
           IF       IN09 OF FORMAT3-I-INDIC = AN
                    IF   NEUANLAGE = ZEROES
                         PERFORM PFLEGEN-BESCHREIBUNG
                         IF   F3 = "1"
                              GO TO ANZ-SFL-90
                         END-IF
                    END-IF
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* EINTRAG ÄNDERN BZW. HINZUFÜGEN
           IF       NEUANLAGE = ZEROES
                    PERFORM  CHANGE-ENTRY
           ELSE
                    PERFORM  ADD-ENTRY
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
                    MOVE SPACES            TO RET-CODE
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

     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP                    TO PGMTYP OF FORMAT3.

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE CORR PAJXTHMF OF PAJXTHM-P TO FORMAT3
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    MOVE X-AXID                 TO AXID   OF FORMAT3
                    MOVE X-AXKEY                TO AXKEY  OF FORMAT3
           END-IF.

     ‚* BEI NEUANLAGE EINES HAUPTTHEMAS DAS UNTERTHEMA AUSBLENDEN
           EVALUATE X-MODUS
                    WHEN "H"
                         MOVE AUS TO IN44 OF INXX
                    WHEN OTHER
                         MOVE AN  TO IN43 OF INXX
                         MOVE AN  TO IN44 OF INXX
           END-EVALUATE.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FORMAT3-O-INDIC.
           MOVE     "FMT03"         TO FMT-WRK.
           MOVE     FORMAT3         TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS TO INXX.

           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO FEH-MEL-90
           END-IF.

     ‚* F9 BESCHREIBUNG PFLEGEN
           IF       IN09 OF FORMAT3-I-INDIC = AN
                    IF   NEUANLAGE = ZEROES
                         PERFORM PFLEGEN-BESCHREIBUNG
                         IF   F3 = "1"
                              GO TO FEH-MEL-90
                         END-IF
                    END-IF
                    MOVE "1" TO F5
                    GO TO FEH-MEL-90
           END-IF.

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

     ‚* EINTRAG ÄNDERN BZW. HINZUFÜGEN
           IF       NEUANLAGE = ZEROES
                    PERFORM  CHANGE-ENTRY
           ELSE
                    PERFORM  ADD-ENTRY
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILE-ÜBERSCHRIFTEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "AJP0000"         TO MAINUEB OF FORMAT3.
           MOVE     "AJP0003*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

           INITIALIZE PAJXTHMF OF PAJXTHM-P.
     ‚* KEY FÜLLEN
           MOVE     AXID   OF FORMAT3 TO AXID   OF PAJXTHM-P.
           MOVE     AXKEY  OF FORMAT3 TO AXKEY  OF PAJXTHM-P.

     ‚* DATEN FÜLLEN
           MOVE     AXKOM1 OF FORMAT3 TO AXKOM1 OF PAJXTHM-P.
           MOVE     AXKOM2 OF FORMAT3 TO AXKOM2 OF PAJXTHM-P.
           MOVE     AXKOM3 OF FORMAT3 TO AXKOM3 OF PAJXTHM-P.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO AXAUSR OF PAJXTHM-P.
           MOVE     T-TERM            TO AXABS  OF PAJXTHM-P.
           MOVE     PGM-WRK           TO AXAPGM OF PAJXTHM-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8            TO AXADTA OF PAJXTHM-P.
           MOVE     TIME-6            TO AXATIA OF PAJXTHM-P.

           WRITE    PAJXTHM-P.
           IF       FILE-STATUS = ZEROES
                    MOVE     AXID   OF FORMAT3 TO X-AXID
                    MOVE     AXKEY  OF FORMAT3 TO X-AXKEY
           END-IF.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       CHANGE-ENTRY SECTION.
       CHG-ENT-00.

           INITIALIZE PAJXTHMF OF PAJXTHM-P.
           MOVE     AXID   OF FORMAT3 TO AXID   OF PAJXTHM-P.
           MOVE     AXKEY  OF FORMAT3 TO AXKEY  OF PAJXTHM-P.
           READ     PAJXTHM-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-ENT-90
           END-IF.

     ‚* DATEN FÜLLEN
           MOVE     AXKOM1 OF FORMAT3 TO AXKOM1 OF PAJXTHM-P.
           MOVE     AXKOM2 OF FORMAT3 TO AXKOM2 OF PAJXTHM-P.
           MOVE     AXKOM3 OF FORMAT3 TO AXKOM3 OF PAJXTHM-P.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO AXMSER OF PAJXTHM-P.
           MOVE     T-TERM            TO AXMBS  OF PAJXTHM-P.
           MOVE     PGM-WRK           TO AXMPGM OF PAJXTHM-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8            TO AXMDTA OF PAJXTHM-P.
           MOVE     TIME-6            TO AXMTIA OF PAJXTHM-P.

           REWRITE  PAJXTHM-P.

       CHG-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BESCHREIBUNG PFLEGEN
     ‚*--------------------------------------------------------------
       PFLEGEN-BESCHREIBUNG SECTION.
       PFL-BSC-00.

           INITIALIZE F3, F12.
           MOVE     "2" TO AUSWAHL.
           CALL     "AJP0006" USING AXID   OF FORMAT3
                                    AXKEY  OF FORMAT3
                                    AUSWAHL
                                    PGM-WRK
                                    F3
                                    F12
           END-CALL.
           INITIALIZE F12.

       PFL-BSC-90.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
