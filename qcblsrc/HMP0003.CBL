       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP0003.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* NEUANLAGE EINES PROGRAMMS                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  NEUANLAGE EINES PROGRAMMS                    *
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
                  ASSIGN       TO  WORKSTATION-HMD0003DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PPGMLIB-DP
                  ASSIGN       TO  DATABASE-PPGMLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TAGS
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DP
                  ASSIGN       TO  DATABASE-PTAGLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI TAGS SORTIERT NACH TAG
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL2
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
           COPY DDS-ALL-FORMATS OF HMD0003DE.
      /
     ‚*--- PROGRAMME
       FD  PPGMLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLIB-P.
           COPY DDS-ALL-FORMATS OF PPGMLIB.
     ‚*--- TAGS
       FD  PTAGLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-P.
           COPY DDS-ALL-FORMATS OF PTAGLIB.
      /
     ‚*--- TAGS
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL2.
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
           COPY  DDS-FMT02-O OF  HMD0003DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  HMD0003DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  HMD0003DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  HMD0003DE
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

       01  PGM-WRK                       PIC X(10) VALUE "HMP0003".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  RTVOBJD-REC                   TYPE RTVOBJD-TYPE.

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.

     ‚* PARAMETER FÜR DAS HMP0005
       01  PTTAG-WRK                     LIKE PTTAG  OF PTAGLIB-P.

     ‚* SONSTIGE VARIABLEN
       01  LEN                           PIC 9(4).
       01  PLFILE-WRK                    LIKE PLFILE OF PPGMLIB-P.
       01  AUSWAHL                       PIC X(2).
       01  NEUANLAGE                     PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PLLIB                       LIKE PLLIB  OF PPGMLIB-P.
       01  X-PLFILE                      LIKE PLFILE OF PPGMLIB-P.
       01  X-PGM                         LIKE PGM-WRK.
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-PLLIB
                                         X-PLFILE
                                         X-PGM
                                         X-F3
                                         X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP0003" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "HMD0003DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PPGMLIB-DP
                          PTAGLIB-DP.
           OPEN     INPUT PTAGLIB-DL.

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
     ‚* PPGMLIB LESEN
           INITIALIZE PPGMLIBF OF PPGMLIB-P.
           MOVE     X-PLLIB             TO PLLIB  OF PPGMLIB-P.
           MOVE     X-PLFILE            TO PLFILE OF PPGMLIB-P.
           READ     PPGMLIB-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PPGMLIBF OF PPGMLIB-P
                    MOVE 1      TO NEUANLAGE
           ELSE
                    MOVE ZEROES TO NEUANLAGE
           END-IF.

     ‚* ERMITTELN DES PROGRAMMTEXTES
           INITIALIZE RTVOBJD-REC.
           MOVE     X-PLLIB             TO OBJLIB   OF RTVOBJD-REC.
           MOVE     X-PLFILE            TO OBJ      OF RTVOBJD-REC.
           MOVE     "*PGM"              TO OBJTYPE  OF RTVOBJD-REC.
           CALL     "CLP0006" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    INITIALIZE RTVOBJD-REC
                    MOVE X-PLLIB             TO OBJLIB   OF RTVOBJD-REC
                    MOVE X-PLFILE            TO OBJ      OF RTVOBJD-REC
                    MOVE "*MODULE"           TO OBJTYPE  OF RTVOBJD-REC
                    CALL "CLP0006" USING RTVOBJD-REC
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         INITIALIZE RTVOBJD-REC
                         MOVE X-PLLIB        TO OBJLIB   OF RTVOBJD-REC
                         MOVE X-PLFILE       TO OBJ      OF RTVOBJD-REC
                         MOVE "*FILE"        TO OBJTYPE  OF RTVOBJD-REC
                         CALL "CLP0006" USING RTVOBJD-REC
                                              RET-CODE
                         END-CALL
                         IF   RET-CODE NOT = SPACES
                              INITIALIZE RTVOBJD-REC
                         END-IF
                    END-IF
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

           CLOSE                 PPGMLIB-DP
                                 PTAGLIB-DP
                                 PTAGLIB-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
     ‚* AUFSETZEN MIT DEM ERSTEN TAG
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     X-PLLIB                  TO PTLIB  OF PTAGLIB-L.
           MOVE     X-PLFILE                 TO PTFILE OF PTAGLIB-L.
           MOVE     SPACES                   TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

        LES-DAT-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       PTLIB  OF PTAGLIB-L    NOT = X-PLLIB
            OR      PTFILE OF PTAGLIB-L    NOT = X-PLFILE
                    GO TO LES-DAT-90
           END-IF.

     ‚* DIE LEER-TAGS SIND NUR FÜR DIE SUCHE VORHANDEN, DAHER
     ‚* ÜBERLESEN
           IF       PTTAG  OF PTAGLIB-L        = SPACES
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
                    MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    ZEROES  TO ANZREC-WRK
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
           MOVE     CORR PTAGLIBF OF PTAGLIB-L TO FORMAT2.
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

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE CORR PPGMLIBF OF PPGMLIB-P TO FORMAT3
                    IF   OBJTEXT OF RTVOBJD-REC NOT = SPACES
                         MOVE OBJATR  OF RTVOBJD-REC TO
                              PLSEU2  OF FORMAT3
                         MOVE OBJTEXT OF RTVOBJD-REC TO
                              PLMTXT  OF FORMAT3
                    END-IF
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    MOVE X-PLLIB                TO PLLIB  OF FORMAT3
                    MOVE X-PLFILE               TO PLFILE OF FORMAT3
                    MOVE OBJATR  OF RTVOBJD-REC TO PLSEU2 OF FORMAT3
                    MOVE OBJTEXT OF RTVOBJD-REC TO PLMTXT OF FORMAT3
           END-IF.

     ‚* ANZEIGE SUBFILE
           EVALUATE SFL-ZEILE
                    WHEN 1
                         MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                         MOVE 1 TO SUBFILE-RECORD-NUMBER
                    WHEN OTHER
                         IF   SFL-ZEILE <= ANZREC-WRK
                              MOVE SFL-ZEILE  TO SUBFILE-RECORD-NUMBER
                         ELSE
                              MOVE ANZREC-WRK TO SUBFILE-RECORD-NUMBER
                         END-IF
           END-EVALUATE.

           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
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

     ‚* F8 NEUEN TAG HINZUFÜGEN
           IF       IN08 IN FORMAT3-I-INDIC = AN
                    PERFORM ANLEGEN-TAG
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F9 BESCHREIBUNG PFLEGEN
           IF       IN09 IN FORMAT3-I-INDIC = AN
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

     ‚* LÖSCHEN DES TAGS
           IF       AUSW OF FORMAT2 = "4"
                    PERFORM DELETE-TAG
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

     ‚* WERTE FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN42 OF INXX
                    MOVE CORR PPGMLIBF OF PPGMLIB-P TO FORMAT3
                    IF   OBJTEXT OF RTVOBJD-REC NOT = SPACES
                         MOVE OBJATR  OF RTVOBJD-REC TO
                              PLSEU2  OF FORMAT3
                         MOVE OBJTEXT OF RTVOBJD-REC TO
                              PLMTXT  OF FORMAT3
                    END-IF
           ELSE
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    MOVE X-PLLIB                TO PLLIB  OF FORMAT3
                    MOVE X-PLFILE               TO PLFILE OF FORMAT3
                    MOVE OBJATR  OF RTVOBJD-REC TO PLSEU2 OF FORMAT3
                    MOVE OBJTEXT OF RTVOBJD-REC TO PLMTXT OF FORMAT3
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

     ‚* F8 NEUEN TAG HINZUFÜGEN
           IF       IN08 IN FORMAT3-I-INDIC = AN
                    PERFORM ANLEGEN-TAG
                    GO TO FEH-MEL-90
           END-IF.

     ‚* F9 BESCHREIBUNG PFLEGEN
           IF       IN09 IN FORMAT3-I-INDIC = AN
                    IF   NEUANLAGE = ZEROES
                         PERFORM PFLEGEN-BESCHREIBUNG
                         IF   F3 = "1"
                              GO TO ANZ-SFL-90
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
           MOVE     "HMP0000"         TO MAINUEB OF FORMAT3.
           MOVE     "HMP0003*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       ADD-ENTRY SECTION.
       ADD-ENT-00.

           INITIALIZE PPGMLIBF OF PPGMLIB-P.
     ‚* KEY FÜLLEN
           MOVE     PLLIB  OF FORMAT3 TO PLLIB  OF PPGMLIB-P.
           MOVE     PLFILE OF FORMAT3 TO PLFILE OF PPGMLIB-P.

     ‚* DATEN FÜLLEN
           MOVE     PLKOM1 OF FORMAT3 TO PLKOM1 OF PPGMLIB-P.
           MOVE     PLKOM2 OF FORMAT3 TO PLKOM2 OF PPGMLIB-P.
           MOVE     PLKOM3 OF FORMAT3 TO PLKOM3 OF PPGMLIB-P.
           MOVE     PLMTXT OF FORMAT3 TO PLMTXT OF PPGMLIB-P.
           MOVE     PLSEU2 OF FORMAT3 TO PLSEU2 OF PPGMLIB-P.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO PLAUSR OF PPGMLIB-P.
           MOVE     T-TERM            TO PLABS  OF PPGMLIB-P.
           MOVE     PGM-WRK           TO PLAPGM OF PPGMLIB-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8            TO PLADTA OF PPGMLIB-P.
           MOVE     TIME-6            TO PLATIA OF PPGMLIB-P.

           WRITE    PPGMLIB-P.

     ‚* TAG SCHREIBEN - OHNE TAG
           INITIALIZE PTAGLIBF OF PTAGLIB-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PTAGLIB" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK          TO PTLFDN OF PTAGLIB-P.

     ‚* DATEN FÜLLEN
           MOVE     PLLIB  OF FORMAT3 TO PTLIB  OF PTAGLIB-P.
           MOVE     PLFILE OF FORMAT3 TO PTFILE OF PTAGLIB-P.
           MOVE     SPACES            TO PTTAG  OF PTAGLIB-P.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO PTAUSR OF PTAGLIB-P.
           MOVE     T-TERM            TO PTABS  OF PTAGLIB-P.
           MOVE     PGM-WRK           TO PTAPGM OF PTAGLIB-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8            TO PTADTA OF PTAGLIB-P.
           MOVE     TIME-6            TO PTATIA OF PTAGLIB-P.

           WRITE    PTAGLIB-P.

       ADD-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN EINES EINTRAGS
     ‚*--------------------------------------------------------------
       CHANGE-ENTRY SECTION.
       CHG-ENT-00.

           INITIALIZE PPGMLIBF OF PPGMLIB-P.
           MOVE     PLLIB  OF FORMAT3 TO PLLIB  OF PPGMLIB-P.
           MOVE     PLFILE OF FORMAT3 TO PLFILE OF PPGMLIB-P.
           READ     PPGMLIB-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-ENT-90
           END-IF.

     ‚* DATEN FÜLLEN
           MOVE     PLKOM1 OF FORMAT3 TO PLKOM1 OF PPGMLIB-P.
           MOVE     PLKOM2 OF FORMAT3 TO PLKOM2 OF PPGMLIB-P.
           MOVE     PLKOM3 OF FORMAT3 TO PLKOM3 OF PPGMLIB-P.
           MOVE     PLMTXT OF FORMAT3 TO PLMTXT OF PPGMLIB-P.
           MOVE     PLSEU2 OF FORMAT3 TO PLSEU2 OF PPGMLIB-P.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO PLMSER OF PPGMLIB-P.
           MOVE     T-TERM            TO PLMBS  OF PPGMLIB-P.
           MOVE     PGM-WRK           TO PLMPGM OF PPGMLIB-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8            TO PLMDTA OF PPGMLIB-P.
           MOVE     TIME-6            TO PLMTIA OF PPGMLIB-P.

           REWRITE  PPGMLIB-P.

       CHG-ENT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN EINES TAGS
     ‚*--------------------------------------------------------------
       ANLEGEN-TAG SECTION.
       ANL-TAG-00.

     ‚* PROGRAMM ERSTMAL ANLEGEN
           IF       NEUANLAGE = ZEROES
                    PERFORM  CHANGE-ENTRY
           ELSE
                    PERFORM  ADD-ENTRY
           END-IF.

     ‚* ANLEGEN EINES TAGS
           INITIALIZE PTTAG-WRK, F12.
           CALL     "HMP0005" USING PTTAG-WRK
                                    F12
           END-CALL.
           IF       PTTAG-WRK = SPACES
            OR      F12 = "1"
                    GO TO ANL-TAG-90
           END-IF.

     ‚* PRÜFEN OB ES DEN TAG SCHON GIBT
           PERFORM  CHECK-SCHON-VORHANDEN.
           IF       RET-CODE NOT = SPACES
                    GO TO ANL-TAG-90
           END-IF.

     ‚* TAG SCHREIBEN
           INITIALIZE PTAGLIBF OF PTAGLIB-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PTAGLIB" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK          TO PTLFDN OF PTAGLIB-P.

     ‚* DATEN FÜLLEN
           MOVE     PLLIB  OF FORMAT3 TO PTLIB  OF PTAGLIB-P.
           MOVE     PLFILE OF FORMAT3 TO PTFILE OF PTAGLIB-P.
           MOVE     PTTAG-WRK         TO PTTAG  OF PTAGLIB-P.

     ‚* USER FÜLLEN
           MOVE     T-USER            TO PTAUSR OF PTAGLIB-P.
           MOVE     T-TERM            TO PTABS  OF PTAGLIB-P.
           MOVE     PGM-WRK           TO PTAPGM OF PTAGLIB-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8            TO PTADTA OF PTAGLIB-P.
           MOVE     TIME-6            TO PTATIA OF PTAGLIB-P.

           WRITE    PTAGLIB-P.

       ANL-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN EINES TAGS
     ‚*--------------------------------------------------------------
       DELETE-TAG SECTION.
       DEL-TAG-00.

           INITIALIZE PTAGLIBF OF PTAGLIB-P.
           MOVE     PTLFDN OF FORMAT2 TO PTLFDN OF PTAGLIB-P.
           READ     PTAGLIB-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PTAGLIB-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PTAGLIB"   TO FILE-WRK
                    MOVE PTAGLIB-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       DEL-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BESCHREIBUNG PFLEGEN
     ‚*--------------------------------------------------------------
       PFLEGEN-BESCHREIBUNG SECTION.
       PFL-BSC-00.

           INITIALIZE F3, F12.
           MOVE     "2" TO AUSWAHL.
           CALL     "HMP0006" USING PLLIB  OF FORMAT3
                                    PLFILE OF FORMAT3
                                    AUSWAHL
                                    PGM-WRK
                                    F3
                                    F12
           END-CALL.
           INITIALIZE F12.

       PFL-BSC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN TAG EVTL. SCHON FÜR DAS PGM VORHANDEN IST
     ‚*--------------------------------------------------------------
       CHECK-SCHON-VORHANDEN SECTION.
       CHK-TVO-00.

           INITIALIZE RET-CODE.
     ‚* AUFSETZEN MIT DEM ERSTEN TAG
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     X-PLLIB                  TO PTLIB  OF PTAGLIB-L.
           MOVE     X-PLFILE                 TO PTFILE OF PTAGLIB-L.
           MOVE     PTTAG-WRK                TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TVO-90
           END-IF.

        CHK-TVO-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TVO-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       PTLIB  OF PTAGLIB-L    NOT = X-PLLIB
            OR      PTFILE OF PTAGLIB-L    NOT = X-PLFILE
            OR      PTTAG  OF PTAGLIB-L    NOT = PTTAG-WRK
                    GO TO CHK-TVO-90
           END-IF.

     ‚* TAG IST VORHANDEN
           MOVE     "1" TO RET-CODE.

       CHK-TVO-90.
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
