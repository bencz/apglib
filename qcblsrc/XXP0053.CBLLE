     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0053.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE PROGRAMMELEMENTE                                      *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  07.12.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE VON PROGRAMMELEMENTEN                *
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
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
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
           COPY  DDS-FMT02-O OF  XXD0053DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0053DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0053DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0053DE
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP0053".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST  ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

       01  AUSWAHL                       PIC X(2).
       01  START-ERFOLGT                 PIC 9(1).

     ‚* SOURCE ZUM ARBEITEN
       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

       01  SRCSEQ-WRK                    LIKE SRCSEQ OF QSETSRC-WRK.
       01  MAPREC-WRK                    LIKE MAP-REC OF MAPDTA-REC.
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-POINTER                     USAGE POINTER.
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-POINTER
                                X-F12.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0053" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           MOVE     "XXD0053DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT3.
           INITIALIZE F3, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* POINTER ERSTELLEN
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-CREATE TO TRUE.
           MOVE     "VALUES"   TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.

       ANF010.
           MOVE     AUS TO INXX.
           PERFORM  ANZEIGE-SUBFILE.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

       ENDE.
           SET      X-POINTER TO PTR-MAIN OF PTRMAP-REC.
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
           INITIALIZE START-ERFOLGT.

       LES-DAT-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE     1             TO START-ERFOLGT
                    MOVE     ZEROES        TO MAP-LFDN OF MAPDTA-REC
                    MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           ELSE
                    MOVE     MAP-READ-NEXT TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           IF       RET-CODE NOT = SPACES
                    GO TO LES-DAT-90
           END-IF.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
     ‚*€            MOVE    ZEROES  TO ANZREC OF FORMAT3
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
           MOVE     MAP-KEY OF MAPDTA-REC       TO KEYVAL OF FORMAT2.
           MOVE     MAP-REC OF MAPDTA-REC       TO OPTVAL OF FORMAT2.
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
           MOVE     AUS    TO INXX.
           PERFORM  LESE-DATEN.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
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
           IF       DATEN = ZEROES
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT05" TO FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE     AUS    TO IN91  IN  INXX
           ELSE
                    MOVE     AN     TO IN91  IN  INXX
           END-IF.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.

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

     ‚* EINTRAG IN DER TABELLE HINZUFÜGEN
           IF       KEYVAL OF FORMAT3 NOT = SPACES
            OR      OPTVAL OF FORMAT3 NOT = SPACES
                    MOVE     KEYVAL OF FORMAT3 TO MAP-KEY OF MAPDTA-REC
                    MOVE     OPTVAL OF FORMAT3 TO MAP-REC OF MAPDTA-REC
                    MOVE     MAP-WRITE         TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    GO TO ANZ-SFL-00
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    GO TO ANZ-SFL-00
           END-IF.

           MOVE     WSREC-WRK      TO FORMAT2-I.
           MOVE     CORR FORMAT2-I TO FORMAT2.

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

     ‚* LÖSCHEN DES EINTRAGS
           IF       AUSW OF FORMAT2 = "4"
                    MOVE     KEYVAL OF FORMAT2 TO MAP-KEY OF MAPDTA-REC
                    MOVE     MAP-READ          TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE = SPACES
                         MOVE     MAP-DELETE  TO MAP-ACT OF MAPDTA-REC
                         PERFORM  COPY-CALL-MAPDTA
                    END-IF
           END-IF.

     ‚* EINTRAG IN DER TABELLE AKTUALISIEREN
           MOVE     KEYVAL OF FORMAT2 TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-READ          TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    MOVE     OPTVAL OF FORMAT2 TO MAP-REC OF MAPDTA-REC
                    MOVE     MAP-WRITE         TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
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
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0053"         TO MAINUEB OF FORMAT3.
     ‚*€   MOVE     "XXP0053*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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
