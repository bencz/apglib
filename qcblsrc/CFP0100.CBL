       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0100.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* HAUPTMENÜ                                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  15.06.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE DES HAUPTMENÜS                       *
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
                  ASSIGN       TO  WORKSTATION-CFD0100DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PCFGMNU: MNMENU
     ‚*-------------------------------------------------------------
           SELECT PCFGMNU-DP
                  ASSIGN       TO  DATABASE-PCFGMNU
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PCFGMNUL1: MNPOS, MNUPOS, MNMENU
     ‚*-------------------------------------------------------------
           SELECT PCFGMNU-DL
                  ASSIGN       TO  DATABASE-PCFGMNUL1
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
           COPY DDS-ALL-FORMATS OF CFD0100DE.
      /
     ‚*--- PCFGMNU: MNMENU
       FD  PCFGMNU-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGMNU-P.
           COPY DDS-ALL-FORMATS OF PCFGMNU.
      /
     ‚*--- PCFGMNUL1: MNPOS, MNUPOS, MNMENU
       FD  PCFGMNU-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGMNU-L.
           COPY DDS-ALL-FORMATS OF PCFGMNUL1.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  CFD0100DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CFD0100DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CFD0100DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CFD0100DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0100".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  CMDEXC-REC                    TYPE CMDEXC-TYPE.
       01  ADDLIBLE-REC                  TYPE ADDLIBLE-TYPE.
       01  RMVLIBLE-REC                  TYPE RMVLIBLE-TYPE.

       01  AUSWAHL                       PIC X(2).

       01  MENU-EBENE                    LIKE MNMENU OF PCFGMNU-L.
       01  LIB-ADDED                     PIC 9(1).

     ‚* PARAMETER FÜR DAS HLP0008
       01  CALL-PGM                      PIC X(10).
       01  CALL-PARM                     PIC X(1027).
       01  BATCH-KZ                      PIC X(1) VALUE SPACES.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01   X-MENU                       PIC X(10).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-MENU.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0100" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0100DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PCFGMNU-DL
                          PCFGMNU-DP.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     1 TO SFL-ZEILE.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN ANHAND DER TAG-DATEI
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         IF   MENU-EBENE = SPACES
                              GO TO ENDE
                         ELSE
                              MOVE SPACES TO MENU-EBENE
                         END-IF
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
                    IF   MENU-EBENE = SPACES
                         GO TO ENDE
                    ELSE
                         MOVE SPACES TO MENU-EBENE
                    END-IF
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PCFGMNU-DL
                                 PCFGMNU-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK.
     ‚* PCFGMNU LESEN
           INITIALIZE PCFGMNUF OF PCFGMNU-L.
           MOVE     ZEROES              TO MNPOS  OF PCFGMNU-L.
           MOVE     ZEROES              TO MNUPOS OF PCFGMNU-L.
           MOVE     SPACES              TO MNMENU OF PCFGMNU-L.
           START    PCFGMNU-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCFGMNU-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* MENÜ SELEKTIERT?
           IF       MENU-EBENE NOT = SPACES
     ‚*             MENÜPUNKTE SELEKTIEREN
                    IF   MNRMNU OF PCFGMNU-L NOT = MENU-EBENE
                     OR  MNRMNU OF PCFGMNU-L     = MNMENU OF PCFGMNU-L
                         GO TO LES-DAT-20
                    END-IF
           ELSE
     ‚*             NUR HAUPTMENÜPUNKTE SELEKTIEREN
                    IF   MNMENU OF PCFGMNU-L NOT = MNRMNU OF PCFGMNU-L
                         GO TO LES-DAT-20
                    END-IF
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
           MOVE     CORR PCFGMNUF OF PCFGMNU-L TO FORMAT2.
     ‚* AUFRUFE ANDERER BIBLIOTHEKEN ANZEIGEN
           IF       MNLIB  OF PCFGMNU-L NOT = CONST-LIB
            AND     MNPGM  OF PCFGMNU-L NOT = SPACES
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    MOVE AUS TO IN41 OF INXX
           END-IF.

     ‚* EXISTIERT OBJEKT NICHT?
           IF       MNPGM  OF PCFGMNU-L NOT = SPACES
                    INITIALIZE CHKOBJ-REC
                    MOVE   MNLIB  OF PCFGMNU-L TO LIB     OF CHKOBJ-REC
                    MOVE   MNPGM  OF PCFGMNU-L TO OBJ     OF CHKOBJ-REC
                    MOVE   MNCTYP OF PCFGMNU-L TO OBJTYPE OF CHKOBJ-REC
                    CALL   "CLP0004" USING CHKOBJ-REC
                    END-CALL
                    IF   OBJ-EXIST OF CHKOBJ-REC = ZEROES
                         MOVE AN  TO IN42 OF INXX
                    ELSE
                         MOVE AUS TO IN42 OF INXX
                    END-IF
           END-IF.

     ‚* ÜBERSCHRIFT
           MOVE     MNTID  OF PCFGMNU-L TO CPY-TXID.
           MOVE     SPRACHE-WRK         TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT          TO MNTEXT OF FORMAT2.

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

     ‚* ANZEIGE SUBFILE
           IF       SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                    MOVE SFL-ZEILE TO  SUBFILE-RECORD-NUMBER
           END-IF.
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

     ‚* F21 KOMMANDOZEILE AUFRUFEN
           IF       IN21 IN FORMAT3-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO ANZ-SFL-10
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

     ‚* ÄNDERN DES DATENSATZES
           IF       AUSW OF FORMAT2 = "1"
                    IF   MNPGM  OF FORMAT2 = SPACES
                         PERFORM UNTERMENUE-SELEKT
                    ELSE
                         PERFORM AUSWAHL-AUSFUEHREN
                    END-IF
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
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

     ‚* AUSGABE INFOZEILE DASS DISPLAY LEER IST
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
           PERFORM  COPY-DISPLAY-SFLWRITE.
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

     ‚* F21 KOMMANDOZEILE AUFRUFEN
           IF       IN21 IN FORMAT3-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO FEH-MEL-90
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CFP0100"         TO MAINUEB OF FORMAT3.
           MOVE     "CFP0100*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* <bitte Kommentar eingeben>
     ‚*--------------------------------------------------------------
       UNTERMENUE-SELEKT SECTION.
       UNT-SEL-00.

           MOVE     MNMENU OF FORMAT2 TO MENU-EBENE.

       UNT-SEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN DER AUSWAHL
     ‚*--------------------------------------------------------------
       AUSWAHL-AUSFUEHREN SECTION.
       AUS-AUS-00.

     ‚* MENÜ LESEN
           INITIALIZE PCFGMNUF OF PCFGMNU-P.
           MOVE     MNMENU OF FORMAT2 TO MNMENU OF PCFGMNU-P.
           READ     PCFGMNU-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO AUS-AUS-90
           END-IF.

           INITIALIZE CHKOBJ-REC.
           MOVE     MNLIB  OF PCFGMNU-P TO LIB     OF CHKOBJ-REC
           MOVE     MNPGM  OF PCFGMNU-P TO OBJ     OF CHKOBJ-REC
           MOVE     MNCTYP OF PCFGMNU-P TO OBJTYPE OF CHKOBJ-REC
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    GO TO AUS-AUS-90
           END-IF.

     ‚* JE NACH PROGRAMMTYP ANDERES AUSFÜHREN
           EVALUATE MNCTYP OF PCFGMNU-P
                    WHEN "*PGM"
                         PERFORM PGM-AUFRUF
                    WHEN "*CMD"
                         PERFORM CMD-AUFRUF
           END-EVALUATE.

       AUS-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PROGRAMMAUFRUF DURCHFÜHREN
     ‚*--------------------------------------------------------------
       PGM-AUFRUF SECTION.
       PGM-AUF-00.

           INITIALIZE LIB-ADDED.
     ‚* BIBLIOTHEK VORSCHALTEN
           IF       MNLIB  OF FORMAT2(1:1) NOT = "Q"
            AND     MNLIB  OF FORMAT2      NOT = "*LIBL"
            AND     MNLIB  OF FORMAT2      NOT = CONST-LIB
                    MOVE 1 TO LIB-ADDED
     ‚*             ADDLIB AUSFÜHREN
                    INITIALIZE ADDLIBLE-REC
                    MOVE MNLIB  OF FORMAT2 TO LIB     OF ADDLIBLE-REC
                    MOVE "*LAST"           TO LIBPOS  OF ADDLIBLE-REC
                    CALL "CLP0007" USING ADDLIBLE-REC
                                         RET-CODE
                    END-CALL
           END-IF.

     ‚* PROGRAMM AUFRUFEN
           MOVE     MNPGM  OF FORMAT2 TO CALL-PGM.
           MOVE     MNPARM OF FORMAT2 TO CALL-PARM.

           CALL     "HLP0008" USING CALL-PGM
                                    CALL-PARM
                                    BATCH-KZ
                                    F3
                                    F12
           END-CALL.

     ‚* BIBLIOTHEK WIEDER ENTFERNEN
           IF       LIB-ADDED NOT = ZEROES
                    INITIALIZE RMVLIBLE-REC
                    MOVE MNLIB  OF FORMAT2 TO LIB OF RMVLIBLE-REC
                    CALL "CLP0009" USING RMVLIBLE-REC
                    END-CALL
           END-IF.

       PGM-AUF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* COMMAND AUSFÜHREN
     ‚*--------------------------------------------------------------
       CMD-AUFRUF SECTION.
       CMD-AUF-00.

     ‚* COMMAND AUFRUFEN
           INITIALIZE CMDEXC-REC.
           STRING   MNLIB  OF FORMAT2 DELIMITED BY SPACES
                    "/"               DELIMITED BY SIZE
                    MNPGM  OF FORMAT2 DELIMITED BY SPACES
                    " "               DELIMITED BY SIZE
                    MNPARM OF FORMAT2 DELIMITED BY SIZE
               INTO COMMAND OF CMDEXC-REC
           END-STRING.
           CALL     "CLP0057" USING CMDEXC-REC
                                    RET-CODE
           END-CALL.

       CMD-AUF-90.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
