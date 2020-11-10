       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0012.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* HINZUFÜGEN EINES TEMPLATES                                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEMPLATE HINZUFÜGEN                          *
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
     ‚*    DATEI QSETSRC (NUR ZUM AUSLESEN)
     ‚*-------------------------------------------------------------
           SELECT QSETSRC-DP
                  ASSIGN       TO  DATABASE-QSETSRC
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DATEI QSETSRC
       FD  QSETSRC-DP
           LABEL RECORD IS STANDARD.
       01  QSETSRC-P.
           COPY DDS-ALL-FORMATS OF QSETSRC.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0012".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  SRCSEQ-WRK                    LIKE SRCSEQ OF QSETSRC-P.
       01  INSERT-LEN                    PIC 9(4).
       01  START-POS                     PIC 9(4).
       01  ENDE-POS                      PIC 9(4).
       01  POS                           PIC 9(4).
       01  IND                           PIC 9(4).
       01  IGNORE-WRK                    PIC 9(1).

       01  F12                           PIC X(1).

       01  PLATZHALTER-WRK               PIC 9(1).
       01  PLATZHALTER-TAB               TYPE TMPL-TAB-TYPE.
       01  TEMPLATE-VAL                  PIC X(35).
       01  TEMPLATE-CMD                  PIC X(50).
       01  SOURCE-WRK                    LIKE SRCDTA OF QSETSRC-P.

     ‚* PARAMETER FÜR DAS HLP0013
       01  TO-STR                        PIC X(100).
       01  VON-STR                       PIC X(100).
       01  ERSETZEN-WRK                  PIC X(250).
       01  MODE-WRK                      PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-TMPLIB                      PIC X(10).
       01  X-TMPRCN                      PIC X(10).
       01  X-TMPMBR                      PIC X(10).
       01  X-POINTER                     USAGE POINTER.
       01  X-SRCSEQ                      LIKE SRCSEQ OF QSETSRC-P.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR
                                         X-TMPLIB
                                         X-TMPRCN
                                         X-TMPMBR
                                         X-POINTER
                                         X-SRCSEQ
                                         X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0012" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           INITIALIZE X-RETCODE.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* KONFIGURATION ERMITTELN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H101"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE 0,01 TO CFNK01 OF CFG-CPY
           END-IF.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* JETZT TEMPLATE LESEN UND HINZUFÜGEN
           PERFORM  LESE-DATEN.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER SOURCZEILEN
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB DIE TEILDATEI VORHANDEN IST UND OB SIE VOM RICHTIGEN
     ‚* TYP IST
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO RET-CODE
                    GO TO LES-DAT-90
           END-IF.

     ‚* SETZEN OVRWRT FÜR QSETSRC UND ÖFFNEN DER DATEI
           PERFORM  SET-OVR-MBR.
           IF       RET-CODE NOT = SPACES
                    MOVE "2" TO RET-CODE
                    GO TO LES-DAT-90
           END-IF.

     ‚* PRÜFEN OB DORT ERSETZUNGEN VORHANDEN SIND, DIESE FANGEN
     ‚* MIT "%" AN UND HÖREN AUCH DAMIT AUF Z.B. "%SECTION%"
           PERFORM  CHECK-PLATZHALTER.
           IF       PLATZHALTER-WRK NOT = ZEROES
                    INITIALIZE F12
                    CALL "CRP0019" USING PLATZHALTER-TAB
                                         F12
                    END-CALL
                    IF   F12 = "1"
                         GO TO LES-DAT-80
                    END-IF
           END-IF.

     ‚* ÖFFNEN DER TEILDATEI
           OPEN     INPUT QSETSRC-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           INITIALIZE QSETSRC OF QSETSRC-P.
           MOVE     X-SRCSEQ              TO SRCSEQ-WRK.
           SET      MAP-PTR OF MAPDTA-REC TO X-POINTER.

       LES-DAT-20.
           READ     QSETSRC-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
            AND     FILE-STATUS NOT = "04"
                    GO TO LES-DAT-80
           END-IF.

     ‚* SOLL DIE ZEILE IGNORIERT WERDEN
           IF       SRCDTA OF QSETSRC-P(1:1) = "?"
                    PERFORM CHECK-YESNO
                    IF   IGNORE-WRK NOT = ZEROES
                         GO TO LES-DAT-20
                    END-IF
     ‚*             JETZT DEN WERT ENTFERNEN
                    MOVE SPACES TO SRCDTA OF QSETSRC-P(1:5)
           END-IF.

     ‚* WENN PLATZHALTER VORHANDEN SIND, DANN DIESE ERSETZEN
           IF       PLATZHALTER-WRK NOT = ZEROES
                    PERFORM ERSETZE-PLATZHALTER
           END-IF.

     ‚* ZEILENNUMMER BERECHNEN
           ADD      CFNK01 OF CFG-CPY  TO SRCSEQ-WRK.
           PERFORM  VERSCHIEBE-VORHANDENE-ZEILEN.
           MOVE     SRCSEQ-WRK         TO SRCSEQ  OF QSETSRC-P.
           MOVE     DATE-8(3:6)        TO SRCDAT  OF QSETSRC-P.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
     ‚* DAMIT DIESE ZEILEN IN DER SORTIERUNG GANZ HINTEN ERSCHEINEN
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-P          TO MAP-REC OF MAPDTA-REC(10:).
     ‚* ZEILE ALS NEU MARKIEREN
           MOVE     "N"                TO MAP-REC OF MAPDTA-REC(1:1).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-POINTER           TO MAP-PTR OF MAPDTA-REC.

           GO TO    LES-DAT-20.

     ‚* DATEI NUN WIEDER SCHLIEßEN
       LES-DAT-80.
           CLOSE    QSETSRC-DP.

           PERFORM  DLT-OVR-MBR.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSETZEN DER PLATZHALTER
     ‚*--------------------------------------------------------------
       ERSETZE-PLATZHALTER SECTION.
       ERS-PLZ-00.

     ‚* STRING ÜBERNEHMEN
           MOVE     SRCDTA OF QSETSRC-P TO ERSETZEN-WRK.

     ‚* TABELLE LEEREN
           MOVE     1 TO IND.
           PERFORM  WITH TEST BEFORE UNTIL
                    TMPCOMMAND OF PLATZHALTER-TAB(IND) = SPACES
                    MOVE TMPCOMMAND OF PLATZHALTER-TAB(IND) TO VON-STR
                    MOVE TMPVALUE   OF PLATZHALTER-TAB(IND) TO TO-STR
     ‚*             SOLL EIN STRING ERSETZT WERDEN?
                    IF   TO-STR NOT = SPACES
                         MOVE SPACES TO MODE-WRK
                         IF   TO-STR = "*SPACE"
                              MOVE SPACES TO TO-STR
                         END-IF
                         IF   VON-STR(1:1) = "$"
                              MOVE "1" TO MODE-WRK
                         END-IF
                         CALL PROCEDURE "HLP0013" USING ERSETZEN-WRK
                                                        VON-STR
                                                        TO-STR
                                                        MODE-WRK
                         END-CALL
                    END-IF
                    ADD  1 TO IND
           END-PERFORM.

     ‚* STRING ÜBERNEHMEN
           MOVE     ERSETZEN-WRK TO SRCDTA OF QSETSRC-P.

       ERS-PLZ-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SETZEN OVRWRT FÜR TEILDATEI
     ‚*---------------------------------------------------------------
       SET-OVR-MBR SECTION.
       SET-OVR-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN DER TEILDATEI
           INITIALIZE OVRDBF-REC.
           MOVE     "QSETSRC"  TO FROMFILE OF OVRDBF-REC.
           MOVE     X-TMPLIB   TO TOLIB    OF OVRDBF-REC.
           MOVE     X-TMPRCN   TO TOFILE   OF OVRDBF-REC.
           MOVE     X-TMPMBR   TO TOMBR    OF OVRDBF-REC.
           CALL     "CLP0011" USING OVRDBF-REC
                                    RET-CODE
           END-CALL.

       SET-OVR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ENTFERNEN OVERWRITE FÜR MEMBER
     ‚*---------------------------------------------------------------
       DLT-OVR-MBR SECTION.
       DEL-OVR-00.

     ‚* OVR ZURÜCKSETZEN
           INITIALIZE DELOVR-REC.
           MOVE     X-TMPRCN TO FROMFILE OF DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

       DEL-OVR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSCHIEBEN DER VORHANDENEN ZEILEN
     ‚*--------------------------------------------------------------
       VERSCHIEBE-VORHANDENE-ZEILEN SECTION.
       VER-VZE-00.

     ‚* AUFRUFEN DES PROGRAMMS ZUM VERSCHIEBEN
           CALL     "CRP0015" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    X-POINTER
                                    SRCSEQ-WRK
                                    RET-CODE
           END-CALL.
           INITIALIZE RET-CODE.

       VER-VZE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN ZUR INTERNEN TABELLE
     ‚*--------------------------------------------------------------
       ADD-TO-INTTAB SECTION.
       ADD-ITB-00.

           INITIALIZE TEMPLATE-VAL.
           INITIALIZE INSERT-LEN, TEMPLATE-CMD.
           COMPUTE  INSERT-LEN = ENDE-POS - START-POS + 1.
           IF       INSERT-LEN = ZEROES
                    GO TO ADD-ITB-90
           END-IF.

     ‚* SICHERN DES WERTES
           MOVE     SOURCE-WRK(START-POS:INSERT-LEN) TO TEMPLATE-CMD.
           IF       TEMPLATE-CMD = "%%"
            OR      TEMPLATE-CMD = "$$"
                    GO TO ADD-ITB-90
           END-IF.

     ‚* STANARDBEFEHLE?
           EVALUATE TEMPLATE-CMD
                    WHEN "%*USER*%"
                    WHEN "$*USER*$"
                         MOVE T-USER TO TEMPLATE-VAL
                    WHEN "%*TERM*%"
                    WHEN "$*TERM*$"
                         MOVE T-TERM TO TEMPLATE-VAL
                    WHEN "%*JBNR*%"
                    WHEN "$*JBNR*$"
                         MOVE T-JBNR TO TEMPLATE-VAL
           END-EVALUATE.
           IF       TEMPLATE-CMD(1:1) = "?"
                    MOVE "*YES" TO TEMPLATE-VAL
           END-IF.

           INITIALIZE IND.
     ‚* SOLANGE LAUFEN BIS WAS FREI IST
           PERFORM  500 TIMES
                    ADD  1 TO IND
     ‚*             GIBT ES DEN BEFEHL SCHON?
                    IF   TMPCOMMAND OF PLATZHALTER-TAB(IND) =
                         TEMPLATE-CMD
                         GO TO ADD-ITB-90
                    END-IF
                    IF   TMPCOMMAND OF PLATZHALTER-TAB(IND) = SPACES
                         MOVE SRCSEQ OF QSETSRC-P TO
                              SRCSEQ OF PLATZHALTER-TAB(IND)
                         MOVE TEMPLATE-CMD TO
                              TMPCOMMAND OF PLATZHALTER-TAB(IND)
                         MOVE TEMPLATE-VAL TO
                              TMPVALUE   OF PLATZHALTER-TAB(IND)
                         MOVE 1 TO PLATZHALTER-WRK
                         GO TO ADD-ITB-90
                    END-IF
           END-PERFORM.

       ADD-ITB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE EINTRÄGE LÖSCHEN
     ‚*--------------------------------------------------------------
       REMOVE-ALL-INTTAB SECTION.
       RMV-ITB-00.

     ‚* TABELLE LEEREN
           INITIALIZE IND.
           PERFORM  500 TIMES
                    ADD  1 TO IND
                    MOVE ZEROES TO SRCSEQ      OF PLATZHALTER-TAB(IND)
                    MOVE SPACES TO TMPCOMMAND  OF PLATZHALTER-TAB(IND)
                    MOVE SPACES TO TMPVALUE    OF PLATZHALTER-TAB(IND)
           END-PERFORM.

       RMV-ITB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-TEILDATEI SECTION.
       CHK-TED-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     X-TMPLIB     TO LIB     OF CHKOBJ-REC.
           MOVE     X-TMPRCN     TO OBJ     OF CHKOBJ-REC.
           MOVE     X-TMPMBR     TO MBR     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TED-90
           END-IF.

       CHK-TED-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN AUF PLATZHALTER UND FESTHALTEN IN EINER INT. TABELLE
     ‚*--------------------------------------------------------------
       CHECK-PLATZHALTER SECTION.
       CHK-PLZ-00.

           MOVE     ZEROES TO PLATZHALTER-WRK.
     ‚* TABELLE LÖSCHEN
           PERFORM  REMOVE-ALL-INTTAB.

           OPEN     INPUT QSETSRC-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           INITIALIZE QSETSRC OF QSETSRC-P.

       CHK-PLZ-20.
           READ     QSETSRC-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
            AND     FILE-STATUS NOT = "04"
                    GO TO CHK-PLZ-80
           END-IF.

           INITIALIZE START-POS, ENDE-POS.
           MOVE     SRCDTA OF QSETSRC-P TO SOURCE-WRK.

     ‚* SCHALTER ZUM AUSBLENDEN VON ZEILEN?
           IF       SRCDTA OF QSETSRC-P(1:1) = "?"
                    MOVE    1 TO START-POS
                    MOVE    5 TO ENDE-POS
                    PERFORM ADD-TO-INTTAB
           END-IF.

           INITIALIZE START-POS, ENDE-POS.
           MOVE     1 TO POS.
           PERFORM  WITH TEST BEFORE UNTIL POS = LENGTH OF SOURCE-WRK
                     OR SOURCE-WRK(POS:) = SPACES
     ‚*             ENDEPOSITION SICHERN
                    IF   (    SOURCE-WRK(POS:1) = "%"
                          OR  SOURCE-WRK(POS:1) = "$")
                     AND START-POS     NOT = ZEROES
                         MOVE POS TO ENDE-POS
                    END-IF
     ‚*             STARTPOSITION SICHERN
                    IF   (    SOURCE-WRK(POS:1) = "%"
                          OR  SOURCE-WRK(POS:1) = "$")
                     AND START-POS         = ZEROES
                         MOVE POS TO START-POS
                    END-IF
     ‚*             ALLES VORHANDEN?
                    IF   START-POS     NOT = ZEROES
                     AND ENDE-POS      NOT = ZEROES
                         PERFORM ADD-TO-INTTAB
                         MOVE ZEROES TO START-POS
                         MOVE ZEROES TO ENDE-POS
                    END-IF
                    ADD 1 TO POS
           END-PERFORM.

           GO TO    CHK-PLZ-20.

     ‚* DATEI NUN WIEDER SCHLIEßEN
       CHK-PLZ-80.
           CLOSE    QSETSRC-DP.

       CHK-PLZ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DIE ZEILE ANGEZEIGT WERDEN SOLL
     ‚*--------------------------------------------------------------
       CHECK-YESNO SECTION.
       CHK-YNO-00.

           INITIALIZE IGNORE-WRK.
     ‚* TABELLE LESEN
           MOVE     1 TO IND.
           PERFORM  WITH TEST BEFORE UNTIL
                    TMPCOMMAND OF PLATZHALTER-TAB(IND) = SPACES
                    IF   TMPCOMMAND OF PLATZHALTER-TAB(IND) =
                         SRCDTA OF QSETSRC-P(1:5)
                         IF   TMPVALUE OF PLATZHALTER-TAB(IND)(1:3) =
                              "*NO"
                              MOVE 1 TO IGNORE-WRK
                         END-IF
                    END-IF
                    ADD  1 TO IND
           END-PERFORM.

       CHK-YNO-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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
