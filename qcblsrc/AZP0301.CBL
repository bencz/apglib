       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              AZP0301.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE ALLER ÜBUNGEN                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  03.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE ALLER ÜBUNGEN                        *
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
                  ASSIGN       TO  WORKSTATION-AZD0301DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PAZUUEBL1: AUPRN1, AUPRN2, AUPRN3, AUANG, AUANGP
     ‚*-------------------------------------------------------------
           SELECT PAZUUEB-DL
                  ASSIGN       TO  DATABASE-PAZUUEBL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
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
           COPY DDS-ALL-FORMATS OF AZD0301DE.
      /
     ‚*--- PAZUUEBL1: AUPRN1, AUPRN2, AUPRN3, AUANG, AUANGP
       FD  PAZUUEB-DL
           LABEL RECORDS ARE STANDARD.
       01  PAZUUEB-L.
           COPY DDS-ALL-FORMATS OF PAZUUEBL1.
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

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  AZD0301DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  AZD0301DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  AZD0301DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  AZD0301DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  AZD0301DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  AZD0301DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "AZP0301".
       01  PGM-TYP                       PIC X(8)  VALUE "AZ:VERW".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.

       01  AUSWAHL                       PIC X(2).
       01  LEN                           PIC 9(4).

     ‚* PARAMETER FÜR DAS CFP0012
       01  USER-20A                      PIC X(20).
       01  0012-AKTION                   PIC X(20).

     ‚* PARAMETER FÜR DAS XXP0302
       01  AULFDN-WRK                    LIKE AULFDN OF PAZUUEB-L.

     ‚* PARAMETER FÜR DAS XXP0303
       01  USER-WRK                      LIKE T-USER.

     ‚* PARAMETER FÜR DAS XXP0305
       01  GROUP-WRK                     PIC X(20).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AZP0301" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "AZD0301DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PAZUUEB-DL.

           INITIALIZE F3, F12.
           INITIALIZE FORMAT1.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* NUR WENN MAN AUF SCHULGIF UND SCHULUNG ZUGREIFEN KANN
     ‚* DARF BZW. SOLLTE MAN DIESE ÜBERSICHT SEHEN KÖNNEN
           PERFORM  CHECK-SCHULUNGS-LIB.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* VORAUSWAHL ANZEIGEN
       ANF010.
           PERFORM  VORAUSWAHL.
           IF       F3 = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         GO TO ANF010
                    END-IF
                    GO TO ANF020
           END-IF.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3 = "1"
                    GO TO ENDE
           END-IF.
           IF       F5 = "1"
                    INITIALIZE F5
                    GO TO ANF020
           END-IF.
           IF       F12 = "1"
                    GO TO ANF010
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PAZUUEB-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SELEKTIEREN ALLER NÖTIGEN DATEN
     ‚*--------------------------------------------------------------
       VORAUSWAHL SECTION.
       VOR-AUS-00.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "AZP0301"         TO MAINUEB OF FORMAT1.
           MOVE     "AZP0301*1"       TO SUBUEB  OF FORMAT1.

       VOR-AUS-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, LEN.
     ‚* WURDE EIN PGM MIT STERN SELEKTIERT?
           IF       AUTEXT OF FORMAT1      NOT = SPACES
            AND     AUTEXT OF FORMAT1(1:1) NOT = "*"
                    MOVE     ZEROES TO LEN
                    INSPECT  AUTEXT OF FORMAT1
                             TALLYING LEN FOR CHARACTERS
                             BEFORE INITIAL "*"
           END-IF.

     ‚* DIE GENERELLEN SOURCEN SIND MIT *DEFAULT GEKENNZEICHNET
           INITIALIZE PAZUUEBF OF PAZUUEB-L.
           MOVE     ZEROES                   TO AUPRN1 OF PAZUUEB-L.
           MOVE     ZEROES                   TO AUPRN2 OF PAZUUEB-L.
           MOVE     ZEROES                   TO AUPRN3 OF PAZUUEB-L.
           MOVE     ZEROES                   TO AUANG  OF PAZUUEB-L.
           MOVE     ZEROES                   TO AUANGP OF PAZUUEB-L.
           START    PAZUUEB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

        LES-DAT-20.
           READ     PAZUUEB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       AUKDK  OF FORMAT1   NOT = SPACES
            AND     AUKDK  OF PAZUUEB-L NOT = AUKDK  OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

           IF       LEN > ZEROES
            AND     AUTEXT OF PAZUUEB-L(1:LEN) NOT =
                    AUTEXT OF FORMAT1(1:LEN)
                    GO TO LES-DAT-20
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
           MOVE     CORR PAZUUEBF OF PAZUUEB-L TO FORMAT2.
           CALL     "HLP0020" USING AUPRN1 OF PAZUUEB-L
                                    AUPRN2 OF PAZUUEB-L
                                    AUPRN3 OF PAZUUEB-L
                                    PRJNUM OF FORMAT2
           END-CALL.
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

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
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

     ‚* F3 ENDE
           IF       IN03 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F8 NEUE SOURCE HINZUFÜGEN
           IF       IN08 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    MOVE ZEROES TO AULFDN-WRK
                    PERFORM SHOW-ENTRY
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F20 NEUE SOURCE HINZUFÜGEN
           IF       IN20 IN FORMAT3-I-INDIC = AN
                    CALL "AZP0304"
                    END-CALL
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

     ‚* ANZEIGEN DER ÜBUNG
           IF       AUSW OF FORMAT2 = "1"
                    MOVE AULFDN OF FORMAT2 TO AULFDN-WRK
                    PERFORM SHOW-ENTRY
                    IF   F3 = "1"
                         GO TO ENDE
                    END-IF
           END-IF.

     ‚* LÖSCHEN DER ÜBUNGE
           IF       AUSW OF FORMAT2 = "4"
                    MOVE AULFDN OF FORMAT2 TO AULFDN-WRK
                    PERFORM DELETE-ENTRY
           END-IF.

     ‚* USER ZUORDNEN
           IF       AUSW OF FORMAT2 = "8"
                    MOVE AULFDN OF FORMAT2 TO AULFDN-WRK
                    PERFORM ADD-TO-USER
           END-IF.

     ‚* GRUPPE ZUORDNEN
           IF       AUSW OF FORMAT2 = "9"
                    MOVE AULFDN OF FORMAT2 TO AULFDN-WRK
                    PERFORM ADD-TO-GROUP
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

     ‚* F8 NEUANLAGE
           IF       IN08 IN FORMAT3-I-INDIC = AN
                    MOVE ZEROES TO AULFDN-WRK
                    PERFORM SHOW-ENTRY
                    IF   F3  = "1"
                         GO TO FEH-MEL-90
                    END-IF
                    INITIALIZE F12
                    GO TO FEH-MEL-90
           END-IF.

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
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
           MOVE     "AZP0301*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINTRAG ANZEIGEN/ANLEGEN
     ‚*--------------------------------------------------------------
       SHOW-ENTRY SECTION.
       SHW-ENT-00.

     ‚* PROGRAMM AUFRUFEN
           CALL     "AZP0302" USING AULFDN-WRK
                                    F3
                                    F12
           END-CALL.

       SHW-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINTRAG LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-ENTRY SECTION.
       DEL-ENT-00.

           INITIALIZE RET-CODE.

       DEL-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCEN DEM USER ZUTEILEN
     ‚*--------------------------------------------------------------
       ADD-TO-USER SECTION.
       ADD-TUS-00.

           INITIALIZE USER-20A.
           INITIALIZE RET-CODE.
     ‚* WINDOW ZUR EINGABE DES USER
           MOVE     "USER"  TO 0012-AKTION.
           CALL     "CFP0012" USING USER-20A
                                    0012-AKTION
                                    F12
           END-CALL.
           IF       F12      = "1"
            OR      USER-20A = SPACES
                    GO TO ADD-TUS-90
           END-IF.

     ‚* SOURCEN KOPIEREN
           MOVE     USER-20A TO USER-WRK.
           CALL     "AZP0303" USING USER-WRK
                                    AULFDN-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    GO TO ADD-TUS-90
           END-IF.

     ‚* SOLANGE WIEDERHOLEN BIS ENDE
           GO TO    ADD-TUS-00.

       ADD-TUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCEN DER GRUPPE ZUTEILEN
     ‚*--------------------------------------------------------------
       ADD-TO-GROUP SECTION.
       ADD-TGR-00.

           INITIALIZE GROUP-WRK.
           INITIALIZE RET-CODE.
     ‚* WINDOW ZUR EINGABE DES USER
           MOVE     "GROUP" TO 0012-AKTION.
           CALL     "CFP0012" USING GROUP-WRK
                                    0012-AKTION
                                    F12
           END-CALL.
           IF       F12       = "1"
            OR      GROUP-WRK = SPACES
                    GO TO ADD-TGR-90
           END-IF.

     ‚* SOURCEN KOPIEREN
           CALL     "AZP0305" USING GROUP-WRK
                                    AULFDN-WRK
           END-CALL.

       ADD-TGR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NACH "SCHULUNG" UND SCHULGIF" PRÜFEN
     ‚*--------------------------------------------------------------
       CHECK-SCHULUNGS-LIB SECTION.
       CHK-SLB-00.

           INITIALIZE CHKOBJ-REC.
           MOVE     "QSYS"              TO LIB     OF CHKOBJ-REC.
           MOVE     "SCHULUNG"          TO OBJ     OF CHKOBJ-REC.
           MOVE     "*LIB"              TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC NOT = 1
                    MOVE "1" TO RET-CODE
                    GO TO CHK-SLB-90
           END-IF.

       CHK-SLB-90.
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
