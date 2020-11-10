     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CRP0019.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE TEMPLATE-PLATZHALTER                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE VON PLATZHALTERN                     *
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
                  ASSIGN       TO  WORKSTATION-CRD0019DE
                  ORGANIZATION IS  TRANSACTION
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY
            LABEL RECORDS ARE OMITTED.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF CRD0019DE.
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
           COPY  DDS-FMT02-O OF  CRD0019DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CRD0019DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CRD0019DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CRD0019DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0019".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  IND                           PIC 9(4).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-TMP-TAB                     TYPE TMPL-TAB-TYPE.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-TMP-TAB
                                         X-F12.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0019" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE X-F12.
           INITIALIZE FORMAT3.
           INITIALIZE F2, F3, F12.
           MOVE     1 TO SFL-ZEILE.

       ANF010.
           MOVE     AUS TO INXX.

           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
           ELSE
                    PERFORM ANZEIGE-SUBFILE
           END-IF.
           IF       F2 = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

           GO TO    ANF010.

       ENDE.
           CLOSE                 WS-DISPLAY.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE IND.
           INITIALIZE DATEN.

       LES-DAT-20.
           ADD      1 TO IND.
           IF       IND > 500
            OR      TMPCOMMAND OF X-TMP-TAB(IND) = SPACES
                    GO TO LES-DAT-90
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
           MOVE     IND                          TO LFDN   OF FORMAT2.
           MOVE     TMPCOMMAND OF X-TMP-TAB(IND) TO TMPCMD OF FORMAT2.
           MOVE     TMPVALUE   OF X-TMP-TAB(IND) TO TMPVAL OF FORMAT2.
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

           INITIALIZE F2.
           INITIALIZE FORMAT3.
     ‚* SELEKTION ANZEIGEN
           MOVE     AUS    TO INXX.

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

     ‚* F2 STARTEN
           IF       IN02 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F2
           END-IF.

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

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           READ     SUBFILE WS-DISPLAY NEXT MODIFIED RECORD
                    FORMAT IS "FMT02".
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

           MOVE     CORR FMT02-I TO FORMAT2.
     ‚* RECORD-NUMMER SICHERN, DAMIT BEI FEHLERMELDUNG DER CURSOR
     ‚* AN DER RICHTIGEN POSITION STEHT
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           IF       TMPVAL OF FORMAT2 NOT = SPACES
                    MOVE TMPVAL OF FORMAT2 TO
                         TMPVALUE OF X-TMP-TAB(LFDN OF FORMAT2)
           END-IF.

           MOVE     CORR FORMAT2 TO FMT02-O.
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

       FEH-MEL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CRP0019*3"       TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO MAINUEB OF FORMAT3.

     ‚* PARAMETER
           MOVE     "SFT0022"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0022 OF FORMAT3.

     ‚* WERT
           MOVE     "SFT0023"         TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO SFT0023 OF FORMAT3.

       SFL-HEA-90.
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
