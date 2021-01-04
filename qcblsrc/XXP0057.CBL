       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0057.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUTOMATISCHE RESERVEFELD MARKIERUNG                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  15.09.2015                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  RESERVEFELDMARKIERUNG                        *
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
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                         ATTRIBUTE-DATA IS BS-ATRIBUTE,
                         I-O-FEEDBACK   IS FILE-IO-FEEDBACK.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-XXD0057DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF XXD0057DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN91                          PIC 1.
        05 IN92                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.
        05 IN97                          PIC 1.
        05 IN98                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0057DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0057DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0057DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0057DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  FORMAT12X.
           COPY  DDS-FMT12-O OF  XXD0057DE
           REPLACING FMT12-O BY FORMAT12
                     FMT12-O-INDIC BY FORMAT12-O-INDIC.

       01  FORMAT12Y.
           COPY  DDS-FMT12-I OF  XXD0057DE
           REPLACING FMT12-I BY FORMAT12-I
                     FMT12-I-INDIC BY FORMAT12-I-INDIC.

       01  FORMAT13X.
           COPY  DDS-FMT13-O OF  XXD0057DE
           REPLACING FMT13-O BY FORMAT13
                     FMT13-O-INDIC BY FORMAT13-O-INDIC.

       01  FORMAT13Y.
           COPY  DDS-FMT13-I OF  XXD0057DE
           REPLACING FMT13-I BY FORMAT13-I
                     FMT13-I-INDIC BY FORMAT13-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP0057".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST".

       01  AUSWAHL                        PIC X(2).
       01  DATEN-12                  LIKE DATEN.
       01  ANZREC02-WRK              LIKE ANZREC-WRK.
       01  ANZREC12-WRK              LIKE ANZREC-WRK.
       01  SFL-REC-02                LIKE SUBFILE-RECORD-NUMBER.
       01  SFL-REC-12                LIKE SUBFILE-RECORD-NUMBER.

     ‚* ALLE ZU ÄNDERNDEN PHYS. DATEIEN
       01  FILE-TAB.
        02 FMAX                           PIC 9(5) COMP-3.
        02 FILE-REC OCCURS 0 TO 9999 TIMES DEPENDING ON FMAX
                                           INDEXED   BY FIDX.
         04 FILE-REC2.
     ‚*   FILESTS: 0 = NICHT VERARBEITEN
     ‚*            1 = DATEI VERARBEITEN
          07 FILESTS                      PIC 9(1).
          07 MBRNAME                      PIC X(10).
          07 FILNAME                      PIC X(10).
          07 LIBNAME                      PIC X(10).

     ‚* ALLE KOMMENTARE
       01  CMT-TAB.
        02 CMAX                           PIC 9(5) COMP-3.
        02 CMT-REC OCCURS 0 TO 9999 TIMES DEPENDING ON CMAX
                                          INDEXED   BY CIDX.
         04 CMT-REC2.
     ‚*   CMTSTS: 0 = NICHT VERARBEITEN
     ‚*           1 = KOMMENTAR VERARBEITEN
          07 CMTSTS                       PIC 9(1).
          07 CMTFLD                       PIC X(10).
          07 CMTNAM                       PIC X(200).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0057" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0057DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    INITIALIZE FMAX OF FILE-TAB
                    INITIALIZE CMAX OF CMT-TAB
           END-IF.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.
           PERFORM  COPY-GET-TIME.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     1 TO SFL-ZEILE.

       ANF020.
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3  = "1"
            OR      F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER KOMMENTARE
     ‚*--------------------------------------------------------------
       READ-COMMENTS SECTION.
       REA-CMT-00.

           INITIALIZE DATEN, SFL-REC-02, ANZREC-WRK.

           PERFORM  WITH TEST BEFORE VARYING CIDX FROM 1 BY 1
                     UNTIL CIDX > CMAX OF CMT-TAB
     ‚*             AUSGABE EINER SUBFILE-ZEILE
                    IF   CMTSTS OF CMT-TAB(CIDX) = 1
                         PERFORM  WRITE-SUBFILE-FMT02
                    END-IF
           END-PERFORM.

       REA-CMT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       READ-PHYSICAL-FILES SECTION.
       REA-PFI-00.

           INITIALIZE DATEN-12, SFL-REC-12, ANZREC12-WRK.

           PERFORM  WITH TEST BEFORE VARYING FIDX FROM 1 BY 1
                     UNTIL FIDX > FMAX OF FILE-TAB
     ‚*             AUSGABE EINER SUBFILE-ZEILE
                    IF   FILESTS OF FILE-TAB(FIDX) = 1
                         PERFORM  WRITE-SUBFILE-FMT12
                    END-IF
           END-PERFORM.

       REA-PFI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER SFL-ZEILE
     ‚*--------------------------------------------------------------
       WRITE-SUBFILE-FMT02 SECTION.
       WRI-F02-00.

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
           MOVE     CORR CMT-REC2 OF CMT-TAB(CIDX) TO FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC02-WRK.
           ADD      1            TO SFL-REC-02.
           MOVE     SFL-REC-02   TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

       WRI-F02-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER SFL-ZEILE
     ‚*--------------------------------------------------------------
       WRITE-SUBFILE-FMT12 SECTION.
       WRI-F12-00.

     ‚* SUBFILE-CLEAR
           IF       DATEN-12 = ZEROES
                    MOVE    1 TO  DATEN-12
                    INITIALIZE FORMAT13
                    MOVE    AUS     TO FORMAT13-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    ZEROES  TO ANZREC2 OF FORMAT3
                    MOVE    AN      TO IN98 OF INXX
                    MOVE    AUS     TO IN92 OF INXX
                    MOVE    AUS     TO IN97 OF INXX
                    MOVE    CORR INXX  TO FORMAT13-O-INDIC
                    MOVE    "FMT13"  TO FMT-WRK
                    MOVE    FORMAT13 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT12.
           MOVE     CORR FILE-REC2 OF FILE-TAB(FIDX) TO FORMAT12.

           MOVE     SPACES       TO AUSW OF FORMAT12.
           MOVE     CORR INXX    TO FORMAT12-O-INDIC.
           ADD      1            TO ANZREC12-WRK.
           ADD      1            TO SFL-REC-12.
           MOVE     SFL-REC-12   TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT12"      TO FMT-WRK.
           MOVE     FORMAT12     TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

       WRI-F12-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
           INITIALIZE FORMAT13.
           MOVE     AUS    TO INXX.

       ANZ-SFL-10.
           PERFORM  READ-COMMENTS.
           PERFORM  READ-PHYSICAL-FILES.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
     ‚*€   EVALUATE SFL-ZEILE
     ‚*€       WHEN 1
     ‚*€            MOVE SFL-REC-02 TO SFL-ZEILE
     ‚*€            MOVE 1          TO SFL-REC-02
     ‚*€       WHEN OTHER
     ‚*€            IF   SFL-ZEILE <= ANZREC02-WRK
     ‚*€                 MOVE SFL-ZEILE  TO SFL-REC-02
     ‚*€            ELSE
     ‚*€                 MOVE ANZREC-WRK TO SFL-REC-02
     ‚*€            END-IF
     ‚*€   END-EVALUATE.
           IF       DATEN = ZEROES
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT05" TO FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE  AUS TO IN91 OF INXX
           ELSE
                    MOVE  AN  TO IN91 OF INXX
           END-IF.
           IF       DATEN-12 = ZEROES
                    MOVE  AUS TO IN92 OF INXX
           ELSE
                    MOVE  AN  TO IN92 OF INXX
           END-IF.

           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SFL-REC-02            TO SFRECNR OF FORMAT3.
           MOVE     SFL-REC-12            TO SFRECNR OF FORMAT13.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN98  IN  INXX.
           MOVE     AN     TO IN97  IN  INXX.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     CORR INXX TO FORMAT13-O-INDIC.

           MOVE     "FMT13"  TO FMT-WRK.
           MOVE     FORMAT13 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     "FMT03"  TO FMT-WRK.
           MOVE     FORMAT3  TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* NEUEN KOMMENTAR INS ARRAY ANFÜGEN
           IF       FLDNAM OF FORMAT3 NOT = SPACES
            AND     CMTNAM OF FORMAT3 NOT = SPACES
                    PERFORM ADD-COMMENT
                    INITIALIZE FLDNAM OF FORMAT3
                    INITIALIZE CMTNAM OF FORMAT3
                    GO TO ANZ-SFL-10
           END-IF.

           MOVE     SPACES TO SFL-CHG.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    IF   SFL-CHG = "1"
                         GO TO ANZ-SFL-10
                    ELSE
                         GO TO ANZ-SFL-20
                    END-IF
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
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "XXP0057"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0057*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
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
     ‚*--------------------------------------------------------------
     ‚* KOMMENTAR IN ARRAY EINFÜGEN
     ‚*--------------------------------------------------------------
       ADD-COMMENT SECTION.
       ADD-CMT-00.

           SET      CIDX TO 1.
           SEARCH   CMT-REC OF CMT-TAB
                    AT END
     ‚*                  NEUER KOMMENTAR
                         INITIALIZE CMT-REC2 OF CMT-REC(CIDX)
                         MOVE 1 TO CMTSTS OF CMT-REC(CIDX)
                         MOVE FLDNAM OF FORMAT3 TO
                              CMTFLD OF CMT-REC(CIDX)
                         MOVE CMTNAM OF FORMAT3 TO
                              CMTNAM OF CMT-REC(CIDX)
                         SET CMAX TO CIDX
                    WHEN CMTFLD OF CMT-REC(CIDX) = FLDNAM OF FORMAT3
     ‚*                  KOMMENTARÄNDERUNG
                         MOVE CMTNAM OF FORMAT3 TO
                              CMTNAM OF CMT-REC(CIDX)
           END-SEARCH.

       ADD-CMT-90.
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
