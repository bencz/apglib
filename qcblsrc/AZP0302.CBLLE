       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              AZP0302.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PFLEGE EINER ÜBUNG + SOURCEN                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.06.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PFLEGE EINER ÜBUNG                           *
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
                  ASSIGN       TO  WORKSTATION-AZD0302DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PAZUUEB: AULFDN
     ‚*-------------------------------------------------------------
           SELECT PAZUUEB-DP
                  ASSIGN       TO  DATABASE-PAZUUEB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PAZUSRC: ASUSER, ASLFDN, ASSLIB, ASSRCN, ASSMBR
     ‚*-------------------------------------------------------------
           SELECT PAZUSRC-DP
                  ASSIGN       TO  DATABASE-PAZUSRC
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
           COPY DDS-ALL-FORMATS OF AZD0302DE.
      /
     ‚*--- PAZUUEB: AULFDN
       FD  PAZUUEB-DP
           LABEL RECORDS ARE STANDARD.
       01  PAZUUEB-P.
           COPY DDS-ALL-FORMATS OF PAZUUEB.
     ‚*--- PAZUSRC: ASUSER, ASLFDN, ASSLIB, ASSRCN, ASSMBR
       FD  PAZUSRC-DP
           LABEL RECORDS ARE STANDARD.
       01  PAZUSRC-P.
           COPY DDS-ALL-FORMATS OF PAZUSRC.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  AZD0302DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  AZD0302DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  AZD0302DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  AZD0302DE
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

       01  PGM-WRK                       PIC X(10) VALUE "AZP0302".
       01  PGM-TYP                       PIC X(8)  VALUE "AZ:VERW".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.

     ‚* PARAMETER FÜR DAS CRP0011
       01  SLIB-WRK                      PIC X(10).
       01  SRCN-WRK                      PIC X(10).
       01  SMBR-WRK                      PIC X(10).
       01  NO-CHECK-WRK                  PIC X(1).

     ‚* SONSTIGE VARIABLEN
       01  LEN                           PIC 9(4).
       01  AUSWAHL                       PIC X(2).
       01  NEUANLAGE                     PIC 9(1).

     ‚* PARAMETER FÜR DAS CRP0036
       01  SEQNBR-DUMMY                  PIC 9(4)V9(2).
       01  OPTION-WRK                    PIC X(1) VALUE "5".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-AULFDN                      LIKE AULFDN OF PAZUUEB-P.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-AULFDN
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AZP0302" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "AZD0302DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PAZUUEB-DP
                          PAZUSRC-DP.

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
     ‚* PAZUUEB LESEN
           INITIALIZE PAZUUEBF OF PAZUUEB-P.
           MOVE     X-AULFDN            TO AULFDN OF PAZUUEB-P.
           READ     PAZUUEB-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PAZUUEBF OF PAZUUEB-P
                    MOVE 1      TO NEUANLAGE
           ELSE
                    MOVE ZEROES TO NEUANLAGE
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
                    INITIALIZE F5
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

           CLOSE                 PAZUUEB-DP
                                 PAZUSRC-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
     ‚* DIE GENERELLEN SOURCEN SIND MIT *DEFAULT GEKENNZEICHNET
           INITIALIZE PAZUSRCF OF PAZUSRC-P.
           MOVE     "*DEFAULT"               TO ASUSER OF PAZUSRC-P.
           MOVE     X-AULFDN                 TO ASLFDN OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSLIB OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSRCN OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSMBR OF PAZUSRC-P.
           START    PAZUSRC-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

        LES-DAT-20.
           READ     PAZUSRC-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       ASUSER OF PAZUSRC-P    NOT = "*DEFAULT"
            OR      ASLFDN OF PAZUSRC-P    NOT = X-AULFDN
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
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PAZUSRCF OF PAZUSRC-P TO FORMAT2.
           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
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

       ANZ-SFL-10.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE CORR PAZUUEBF OF PAZUUEB-P TO FORMAT3
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
           END-IF.

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

     ‚* F3 ENDE
           IF       IN03 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* EINTRAG ÄNDERN BZW. HINZUFÜGEN
           IF       NEUANLAGE = ZEROES
                    PERFORM  CHANGE-ENTRY
           ELSE
                    PERFORM  ADD-ENTRY
           END-IF.

     ‚* WURDE EINE SOURCE EINGETRAGEN?
           IF       EINMBR OF FORMAT3 NOT = SPACES
                    PERFORM ANLEGEN-SOURCE
                    IF   RET-CODE NOT = SPACES
                         GO TO ANZ-SFL-10
                    END-IF
                    MOVE "1" TO F5
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
                    MOVE SPACES            TO RET-CODE
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

     ‚* LÖSCHEN DER SOURCE
           IF       AUSW OF FORMAT2 = "4"
                    PERFORM DELETE-SOURCE
           END-IF.

     ‚* ANZEIGEN DER SOURCE
           IF       AUSW OF FORMAT2 = "5"
                    PERFORM ANZEIGEN-SOURCE
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

       FEH-MEL-10.
     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE CORR PAZUUEBF OF PAZUUEB-P TO FORMAT3
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
           END-IF.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FORMAT3.
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


           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
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

     ‚* WURDE EINE SOURCE EINGETRAGEN?
           IF       EINMBR OF FORMAT3 NOT = SPACES
                    PERFORM ANLEGEN-SOURCE
                    IF   RET-CODE NOT = SPACES
                         GO TO FEH-MEL-10
                    END-IF
                    MOVE "1" TO F5
                    GO TO FEH-MEL-90
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
           MOVE     "AZP0300"         TO MAINUEB OF FORMAT3.
           MOVE     "AZP0302*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

           INITIALIZE PAZUUEBF OF PAZUUEB-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PAZUUEB" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK          TO AULFDN OF PAZUUEB-P.
           MOVE     LFDN-WRK          TO AULFDN OF FORMAT3.
           MOVE     LFDN-WRK          TO X-AULFDN.

     ‚* DATEN FÜLLEN
           MOVE     CORR FORMAT3 TO PAZUUEBF OF PAZUUEB-P.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO AUAUSR OF PAZUUEB-P.
           MOVE     T-TERM            TO AUABS  OF PAZUUEB-P.
           MOVE     PGM-WRK           TO AUAPGM OF PAZUUEB-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8            TO AUADTA OF PAZUUEB-P.
           MOVE     TIME-6            TO AUATIA OF PAZUUEB-P.

           WRITE    PAZUUEB-P.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       CHANGE-ENTRY SECTION.
       CHG-ENT-00.

           INITIALIZE PAZUUEBF OF PAZUUEB-P.
           MOVE     AULFDN OF FORMAT3 TO AULFDN OF PAZUUEB-P.
           READ     PAZUUEB-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN FÜLLEN
                    MOVE     CORR FORMAT3 TO PAZUUEBF OF PAZUUEB-P
     ‚*             USER FÜLLEN
                    MOVE     T-USER            TO AUMSER OF PAZUUEB-P
                    MOVE     T-TERM            TO AUMBS  OF PAZUUEB-P
                    MOVE     PGM-WRK           TO AUMPGM OF PAZUUEB-P
     ‚*             DATUM/UHRZEIT
                    PERFORM  COPY-GET-TIME
                    MOVE     DATE-8            TO AUMDTA OF PAZUUEB-P
                    MOVE     TIME-6            TO AUMTIA OF PAZUUEB-P
                    REWRITE  PAZUUEB-P
           END-IF.

       CHG-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN EINES TAGS
     ‚*--------------------------------------------------------------
       ANLEGEN-SOURCE SECTION.
       ANL-SRC-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB SOURCE VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     EINLIB OF FORMAT3 TO LIB     OF CHKOBJ-REC.
           MOVE     EINRCN OF FORMAT3 TO OBJ     OF CHKOBJ-REC.
           MOVE     EINMBR OF FORMAT3 TO MBR     OF CHKOBJ-REC.
           MOVE     "*FILE"           TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO ANL-SRC-90
           END-IF.

     ‚* PRÜFEN OB ES DIE SOURCE SCHON GIBT
           PERFORM  CHECK-SCHON-VORHANDEN.
           IF       RET-CODE NOT = SPACES
                    GO TO ANL-SRC-90
           END-IF.

     ‚* EINTRAG SCHREIBEN
           INITIALIZE PAZUSRCF OF PAZUSRC-P.
     ‚* KEY FÜLLEN
           MOVE     "*DEFAULT"        TO ASUSER OF PAZUSRC-P.
           MOVE     AULFDN OF FORMAT3 TO ASLFDN OF PAZUSRC-P.
           MOVE     EINLIB OF FORMAT3 TO ASSLIB OF PAZUSRC-P.
           MOVE     EINRCN OF FORMAT3 TO ASSRCN OF PAZUSRC-P.
           MOVE     EINMBR OF FORMAT3 TO ASSMBR OF PAZUSRC-P.

     ‚* SOURCEINFORMATIONEN ERMITTELN
           INITIALIZE RTVMBRD-REC.
           MOVE     EINLIB OF FORMAT3 TO OBJLIB   OF RTVMBRD-REC.
           MOVE     EINRCN OF FORMAT3 TO FILENAME OF RTVMBRD-REC.
           MOVE     EINMBR OF FORMAT3 TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE MBRTEXT  OF RTVMBRD-REC TO ASMTXT OF PAZUSRC-P
           END-IF.

     ‚* USER FÜLLEN, DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER            TO ASAUSR OF PAZUSRC-P.
           MOVE     T-TERM            TO ASABS  OF PAZUSRC-P.
           MOVE     PGM-WRK           TO ASAPGM OF PAZUSRC-P.
           MOVE     DATE-8            TO ASADTA OF PAZUSRC-P.
           MOVE     TIME-6            TO ASATIA OF PAZUSRC-P.

           WRITE    PAZUSRC-P.

       ANL-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN EINER SOURCE
     ‚*--------------------------------------------------------------
       DELETE-SOURCE SECTION.
       DEL-SRC-00.

           INITIALIZE PAZUSRCF OF PAZUSRC-P.
           MOVE     ASUSER OF FORMAT2 TO ASUSER OF PAZUSRC-P.
           MOVE     ASLFDN OF FORMAT2 TO ASLFDN OF PAZUSRC-P.
           MOVE     ASSLIB OF FORMAT2 TO ASSLIB OF PAZUSRC-P.
           MOVE     ASSRCN OF FORMAT2 TO ASSRCN OF PAZUSRC-P.
           MOVE     ASSMBR OF FORMAT2 TO ASSMBR OF PAZUSRC-P.
           READ     PAZUSRC-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PAZUSRC-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PAZUSRC"   TO FILE-WRK
                    MOVE PAZUSRC-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       DEL-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN EINER SOURCE
     ‚*--------------------------------------------------------------
       ANZEIGEN-SOURCE SECTION.
       ANZ-SRC-00.

           CALL     "CRP0036" USING ASSLIB OF FORMAT2
                                    ASSRCN OF FORMAT2
                                    ASSMBR OF FORMAT2
                                    SEQNBR-DUMMY
                                    OPTION-WRK
                                    F3
                                    F12
           END-CALL.

       ANZ-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EINE SRC EVTL. SCHON FÜR DAS PGM VORHANDEN IST
     ‚*--------------------------------------------------------------
       CHECK-SCHON-VORHANDEN SECTION.
       CHK-SVO-00.

           INITIALIZE RET-CODE.
     ‚* AUFSETZEN MIT DEM ERSTEN TAG
           INITIALIZE PAZUSRCF OF PAZUSRC-P.
           MOVE     "*DEFAULT"           TO ASUSER OF PAZUSRC-P.
           MOVE     AULFDN OF FORMAT3    TO ASLFDN OF PAZUSRC-P.
           MOVE     EINLIB OF FORMAT3    TO ASSLIB OF PAZUSRC-P.
           MOVE     EINRCN OF FORMAT3    TO ASSRCN OF PAZUSRC-P.
           MOVE     EINMBR OF FORMAT3    TO ASSMBR OF PAZUSRC-P.
           READ     PAZUSRC-DP WITH NO LOCK.
           IF       FILE-STATUS = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

       CHK-SVO-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
