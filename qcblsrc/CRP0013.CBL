       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0013.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* BEARBEITEN VON ZEILENBEFEHLEN                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ZEILENBEFEHLE AUSFÜHREN UND PTR BEARBEITEN   *
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
      /
       DATA DIVISION.
       FILE SECTION.
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

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* SOURCE ZUM ARBEITEN
       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0013".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  CNT                           PIC 9(4).
       01  IND                           PIC 9(4).
       01  SRCSEQ-WRK                    LIKE SRCSEQ OF QSETSRC-WRK.
       01  SRCSEQ-TRG                    LIKE SRCSEQ OF QSETSRC-WRK.
       01  SRCSEQ-TMP                    LIKE SRCSEQ OF QSETSRC-WRK.
       01  CHK-COMMAND                   PIC X(7).
       01  COMMAND-WRK                   PIC X(7).
     ‚*€01 USRCMD-TAB                    TYPE SEUCMD-TYPE
     ‚*€                                 DESCENDING KEY IS SRCSEQ OF
     ‚*€                                 TAB-REC INDEXED BY IDX.


        01 USRCMD-TAB.
        05 TAB-POS                     PIC 9(4).
        05 TAB-MAX                     PIC 9(4).
        05 TAB-REC        OCCURS 500 TIMES
                          DESCENDING KEY IS
                          SRCSEQ OF TAB-REC OF USRCMD-TAB
                          INDEXED BY IDX.
         07 SRCSEQ        PIC 9(4)V9(2).
         07 SRCCMD        PIC X(7).
         07 IS-DIRECT     PIC 9(1).
         07 IS-BLOCK      PIC 9(1).
         07 IS-OPEN       PIC 9(1).
         07 IS-TARGET     PIC 9(1).
         07 NEED-TARGET   PIC 9(1).
         07 TARGET-SEQ    PIC 9(4)V9(2).
       01  SORT-REC                      LIKE TAB-REC OF USRCMD-TAB.

       01  ROW-REPLACED                  PIC 9(1).
       01  NUMBER-TIMES-WRK              PIC 9(4) COMP.
       01  NUMBER-TIMES                  PIC 9(4) COMP.

     ‚* SCHALTER
       01  CHK-BLOCK                     PIC 9(1).
       01  CHK-DIRECT                    PIC 9(1).
       01  CHK-TARGET                    PIC 9(1).
       01  CHK-IS-TARGET                 PIC 9(1).
       01  COLOR-HLP                     PIC X(1).
       01  FORMAT-HLP                    PIC X(2).
       01  HEX-WRK                       PIC X(1).

     ‚* ZIEL-DATEN
       01  ZIEL-SRCSEQ                   LIKE SRCSEQ OF QSETSRC-WRK.
       01  VON-CNT                       LIKE CNT.
       01  NACH-IND                      LIKE IND.
       01  TARGET-COMMANDS               PIC 9(4).

     ‚* SCHALTER FÜR DIE PRÜFUNGEN
       01  EXEC-DIRECT-WRK               PIC 9(1).
       01  VALID-BLOCK-WRK               PIC 9(1).
       01  OPN-COMMAND-WRK               PIC 9(1).

     ‚* TEMPLATE-VARIABLEN
       01  TMP-LIB                       PIC X(10).
       01  TMP-RCN                       PIC X(10).
       01  TMP-MBR                       PIC X(10).

     ‚* PARAMETER FÜR DAS CRP0011
       01  NO-CHECK                      PIC X(1).

     ‚* PARAMETER FÜR DAS HLP0011
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-PTRVAL                      LIKE MAP-KEY OF MAPDTA-REC.
     ‚* PTR-MAIN (POINTERVERWALTUNG)
     ‚*  SOURCE-NNNN  = SOURCE-POINTER
     ‚*  LINETAG      = LINE-TAGGING
       01  X-POINTER                     USAGE POINTER.
       01  X-CMDTAB                      TYPE SEUCMD-TYPE.
       01  X-TT-VALUE                    PIC X(5).
       01  X-OPEN-CMD                    PIC X(7).
       01  X-SRC-CHG                     PIC 9(1).
       01  X-RET-CMD                     PIC X(7).
       01  X-ERRFLD                      PIC X(10).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR
                                         X-PTRVAL
                                         X-POINTER
                                         X-CMDTAB
                                         X-TT-VALUE
                                         X-OPEN-CMD
                                         X-SRC-CHG
                                         X-RET-CMD
                                         X-ERRFLD.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0013" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           INITIALIZE X-RET-CMD.
           INITIALIZE X-ERRFLD, X-OPEN-CMD.
           MOVE     X-CMDTAB TO USRCMD-TAB.
           PERFORM  SORT-TABLE.

     ‚* POINTER SETZEN
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.

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
                    MOVE  "QTMPSRC"   TO CFDAT2 OF CFG-CPY
                    MOVE  "APG"       TO CFDAT3 OF CFG-CPY
                    MOVE  0,01        TO CFNK01 OF CFG-CPY
           END-IF.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* PRÜFEN DER TABELLE AUF GÜLTIGKEIT (MAN KANN Z.B. KEIN MM
     ‚* UND CC GLEICHZEITIG AKTIV HABEN)
           PERFORM  CHECK-COMMAND-TABLE.
           IF       ERRFLD-WRK NOT = SPACES
                    MOVE ERRFLD-WRK TO X-ERRFLD
                    GO TO ENDE
           END-IF.

     ‚* GIBT ES BEFEHLE DIE DIREKT AUSGEFÜHRT WERDEN KÖNNEN?
     ‚* Z.B. KANN MAN OHNE WEITERES DEN BEFEHL "T" AUSFÜHREN
           PERFORM  CHECK-EXEC-DIRECT.

     ‚* GIBT ES ABGESCHLOSSENE BLOCKBEFEHLE (Z.B. TT...TT)?
           PERFORM  CHECK-VALID-BLOCK.

     ‚* GIBT ES ABGESCHLOSSENE BLOCKBEFEHLE (Z.B. TT...TT)?
           PERFORM  CHECK-TARGET-COMMAND.

     ‚* GIBT ES JETZT NOCH OFFENE BEFEHLE? DANN WERDEN DIE ZEILEN
     ‚* BEIM SCROLLEN DURCH DEN BEFEHL ERSETZT. Z.B. SIEHT MAN
     ‚* DANN STATT 0106.00 -> C______
           PERFORM  CHECK-OPEN-COMMAND.
           IF       OPN-COMMAND-WRK NOT = ZEROES
                    MOVE COMMAND-WRK TO X-OPEN-CMD
           END-IF.

       ENDE.
     ‚* ALLE FALSCHEN ELEMENTE KOMPLETT SAUBER MACHEN
           MOVE     ZEROES TO CNT.
           PERFORM  500 TIMES
                    ADD  1 TO CNT
                    IF   SRCSEQ OF USRCMD-TAB(CNT) = ZEROES
                         INITIALIZE TAB-REC OF USRCMD-TAB(CNT)
                    END-IF
           END-PERFORM.

     ‚* RECORD ZURÜCKGEBEN
           MOVE     USRCMD-TAB TO X-CMDTAB.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINER NEUEN ZEILE
     ‚*--------------------------------------------------------------
       EXEC-I-DIRECT SECTION.
       EXC-IDR-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-IDR-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* NEUE ZEILENNUMMER EINGEBEN
           ADD      CFNK01 OF CFG-CPY  TO SRCSEQ-WRK.

     ‚* SOURCE AUFBEREITEN
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     SRCSEQ-WRK         TO SRCSEQ OF QSETSRC-WRK.
           MOVE     DATE-8(3:6)        TO SRCDAT OF QSETSRC-WRK.
           MOVE     SPACES             TO SRCDTA OF QSETSRC-WRK.
           PERFORM  VERSCHIEBE-VORHANDENE-ZEILEN.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "N"                TO MAP-REC OF MAPDTA-REC(1:1).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1   TO X-SRC-CHG
                    MOVE "I" TO X-RET-CMD
           END-IF.

       EXC-IDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSCHIEBEN EINER ZEILE
     ‚*--------------------------------------------------------------
       EXEC-M-DIRECT SECTION.
       EXC-MDR-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-MDR-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
                    GO TO EXC-MDR-90
           END-IF.

     ‚* NEUE ZEILENNUMMER EINGEBEN
           MOVE     SRCSEQ-WRK         TO SRCSEQ-TMP.
           MOVE     SRCSEQ-TRG         TO SRCSEQ-WRK.
           ADD      CFNK01 OF CFG-CPY  TO SRCSEQ-WRK.

     ‚* SOURCE AUFBEREITEN
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     MAP-REC OF MAPDTA-REC(10:)   TO QSETSRC-WRK.
           MOVE     SRCSEQ-WRK         TO SRCSEQ OF QSETSRC-WRK.
           MOVE     DATE-8(3:6)        TO SRCDAT OF QSETSRC-WRK.
           PERFORM  VERSCHIEBE-VORHANDENE-ZEILEN.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "NM"               TO MAP-REC OF MAPDTA-REC(1:2).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1   TO X-SRC-CHG
     ‚*             JETZT DIE ALTE ZEILE LÖSCHEN
                    INITIALIZE MAPDTA-REC
                    MOVE X-SRCLIB       TO MAP-KEY OF MAPDTA-REC(1:10)
                    MOVE X-SRCRCN       TO MAP-KEY OF MAPDTA-REC(11:10)
                    MOVE X-SRCMBR       TO MAP-KEY OF MAPDTA-REC(21:10)
                    MOVE SRCSEQ-TMP     TO MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE X'FFFFFFFFFFFF' TO MAP-KEY OF MAPDTA-REC(38:6)
                    MOVE MAP-DELETE      TO MAP-ACT OF MAPDTA-REC
                    SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF
                                                      PTRMAP-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         MOVE "XXXXXXXX" TO ERRFLD-WRK
                    END-IF
           END-IF.

       EXC-MDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOPIEREN EINER ZEILE
     ‚*--------------------------------------------------------------
       EXEC-C-DIRECT SECTION.
       EXC-CDR-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-CDR-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
                    GO TO EXC-CDR-90
           END-IF.

     ‚* NEUE ZEILENNUMMER EINGEBEN
           MOVE     SRCSEQ-WRK         TO SRCSEQ-TMP.
           MOVE     SRCSEQ-TRG         TO SRCSEQ-WRK.
           ADD      CFNK01 OF CFG-CPY  TO SRCSEQ-WRK.

     ‚* SOURCE AUFBEREITEN
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     MAP-REC OF MAPDTA-REC(10:)   TO QSETSRC-WRK.
           MOVE     SRCSEQ-WRK         TO SRCSEQ OF QSETSRC-WRK.
           MOVE     DATE-8(3:6)        TO SRCDAT OF QSETSRC-WRK.
           PERFORM  VERSCHIEBE-VORHANDENE-ZEILEN.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "NC"               TO MAP-REC OF MAPDTA-REC(1:2).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1   TO X-SRC-CHG
           END-IF.

       EXC-CDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN EINER ZEILE
     ‚*--------------------------------------------------------------
       EXEC-D-DIRECT SECTION.
       EXC-DDR-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-DDR-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE LÖSCHEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-DELETE         TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1 TO X-SRC-CHG
           END-IF.

       EXC-DDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DBMAIN EXTENDED EINFÜGEN
     ‚*--------------------------------------------------------------
       EXEC-E-DIRECT SECTION.
       EXC-EDR-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-EDR-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
                    GO TO EXC-EDR-90
           END-IF.

     ‚* SOURCE AUFBEREITEN
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-WRK.
           MOVE     "*DB"              TO SRCDTA OF QSETSRC-WRK(7:3).
           MOVE     DATE-8(3:6)        TO SRCDAT OF QSETSRC-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "C"                TO MAP-REC OF MAPDTA-REC(1:1).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1   TO X-SRC-CHG
           END-IF.

       EXC-EDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MARKIEREN EINER ZEILE
     ‚*--------------------------------------------------------------
       EXEC-T-DIRECT SECTION.
       EXC-TDR-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-TDR-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
                    GO TO EXC-TDR-90
           END-IF.

     ‚* SOURCE AUFBEREITEN
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-WRK.
           MOVE     X-TT-VALUE         TO SRCDTA OF QSETSRC-WRK(1:5).
           MOVE     DATE-8(3:6)        TO SRCDAT OF QSETSRC-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "C"                TO MAP-REC OF MAPDTA-REC(1:1).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1   TO X-SRC-CHG
           END-IF.

       EXC-TDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOMMENTIEREN EINER ZEILE
     ‚*--------------------------------------------------------------
       EXEC-K-DIRECT SECTION.
       EXC-KDR-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-KDR-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
                    GO TO EXC-KDR-90
           END-IF.

     ‚* SOURCE AUFBEREITEN
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-WRK.
           MOVE     COL-WHT            TO SRCDTA OF QSETSRC-WRK(6:1).
           MOVE     "*"                TO SRCDTA OF QSETSRC-WRK(7:1).
TODO ‚* PRÜFEN OB DORT TEXT STEHT!
     ‚*€   MOVE     COL-GRN            TO SRCDTA OF QSETSRC-WRK(8:1).
           MOVE     DATE-8(3:6)        TO SRCDAT OF QSETSRC-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "C"                TO MAP-REC OF MAPDTA-REC(1:1).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1   TO X-SRC-CHG
           END-IF.

       EXC-KDR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOMMENTIEREN EINER ZEILE UND MARKIEREN
     ‚*--------------------------------------------------------------
       EXEC-KT-DIRECT SECTION.
       EXC-KTD-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-KTD-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
                    GO TO EXC-KTD-90
           END-IF.

     ‚* SOURCE AUFBEREITEN
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-WRK.
           MOVE     X-TT-VALUE         TO SRCDTA OF QSETSRC-WRK(1:5).
           MOVE     COL-WHT            TO SRCDTA OF QSETSRC-WRK(6:1).
           MOVE     "*"                TO SRCDTA OF QSETSRC-WRK(7:1).
TODO ‚* PRÜFEN OB DORT TEXT STEHT!
     ‚*€   MOVE     COL-GRN            TO SRCDTA OF QSETSRC-WRK(8:1).
           MOVE     DATE-8(3:6)        TO SRCDAT OF QSETSRC-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "C"                TO MAP-REC OF MAPDTA-REC(1:1).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1   TO X-SRC-CHG
           END-IF.

       EXC-KTD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINZUFÜGEN EINES TEMPLATES
     ‚*--------------------------------------------------------------
       EXEC-TEMPLATE-DIRECT SECTION.
       EXC-TMD-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-TMD-90
           END-IF.

           INITIALIZE RET-CODE.
     ‚* TEMPLATE ZUSAMMENBAUEN
           INITIALIZE TMP-LIB, TMP-RCN, TMP-MBR.
           MOVE     CFDAT3 OF CFG-CPY TO TMP-LIB.
           MOVE     CFDAT2 OF CFG-CPY TO TMP-RCN.
           IF       CHK-COMMAND(2:1) = "?"
                    INITIALIZE F12, NO-CHECK
                    CALL "CRP0011" USING TMP-LIB
                                         TMP-RCN
                                         TMP-MBR
                                         NO-CHECK
                                         F12
                    END-CALL
                    IF   F12 = "1"
                         GO TO EXC-TMD-90
                    END-IF
           ELSE
                    MOVE CHK-COMMAND(2:)   TO TMP-MBR
           END-IF.

     ‚* TEMPLATE HINZUFÜGEN
           CALL     "CRP0012" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    TMP-LIB
                                    TMP-RCN
                                    TMP-MBR
                                    PTR-RETURN OF PTRMAP-REC
                                    SRCSEQ-WRK
                                    RET-CODE
           END-CALL.

       EXC-TMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE MARKIEREN
     ‚*--------------------------------------------------------------
       EXEC-COLOR-DIRECT SECTION.
       EXC-COD-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO EXC-COD-90
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
                    GO TO EXC-COD-90
           END-IF.

     ‚* FUNKTION NUR BEI KOMMENTAREN ERLAUBT
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-WRK.
           IF       SRCDTA OF QSETSRC-WRK(7:1) NOT = "*"
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
                    GO TO EXC-COD-90
           END-IF.

     ‚* ERMITTELN DER FARBE
           MOVE     CHK-COMMAND(3:1) TO COLOR-HLP.
           MOVE     CHK-COMMAND(4:2) TO FORMAT-HLP.
           PERFORM  GET-COLOR.

     ‚* SOURCE AUFBEREITEN
           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-WRK.
           MOVE     HEX-WRK            TO SRCDTA OF QSETSRC-WRK(6:1).
           MOVE     DATE-8(3:6)        TO SRCDAT OF QSETSRC-WRK.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           MOVE     "C"                TO MAP-REC OF MAPDTA-REC(1:1).
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           ELSE
                    MOVE 1   TO X-SRC-CHG
           END-IF.

       EXC-COD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE MARKIEREN
     ‚*--------------------------------------------------------------
       EXEC-LINETAGGING-DIRECT SECTION.
       EXC-LTD-00.

     ‚* TAGPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     "LINETAG" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    SET      PTR-CREATE TO TRUE
                    MOVE     "LINETAG" TO PTR-KEY OF PTRMAP-REC
                    PERFORM  COPY-PTR-MAPPING
           END-IF.

           INITIALIZE ERRFLD-WRK.
     ‚* JETZT IN MAPDTA DEN TAG SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     CHK-COMMAND        TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           MOVE     SRCSEQ-WRK         TO MAP-REC OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "XXXXXXXX" TO ERRFLD-WRK
           END-IF.

       EXC-LTD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSCHIEBEN DER VORHANDENEN ZEILEN
     ‚*--------------------------------------------------------------
       VERSCHIEBE-VORHANDENE-ZEILEN SECTION.
       VER-VZE-00.

     ‚* SOURCEPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     X-PTRVAL TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO VER-VZE-90
           END-IF.

     ‚* AUFRUFEN DES PROGRAMMS ZUM VERSCHIEBEN
           CALL     "CRP0015" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    PTR-RETURN OF PTRMAP-REC
                                    SRCSEQ-WRK
                                    RET-CODE
           END-CALL.
           INITIALIZE RET-CODE.

       VER-VZE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TABELLE SORTIEREN
     ‚*--------------------------------------------------------------
       SORT-TABLE SECTION.
       SRT-TAB-00.

           INITIALIZE ROW-REPLACED.
           INITIALIZE NUMBER-TIMES.
           INITIALIZE NUMBER-TIMES-WRK.
           INITIALIZE SORT-REC.

     ‚* SORTIEREN BIS KEINE ZEILE MEHR GETAUSCHT WURDE
           PERFORM  WITH TEST AFTER UNTIL ROW-REPLACED = ZERO
                    INITIALIZE ROW-REPLACED
                    SET   IDX TO ZEROES
     ‚*             BERECHNEN WIE VIELE ZEILEN NOCH SORTIERT
     ‚*             WERDEN MÜSSEN
                    ADD 1 TO NUMBER-TIMES
                    COMPUTE NUMBER-TIMES-WRK = 500 - NUMBER-TIMES
                    END-COMPUTE
     ‚*             TABELLE DURCHGEHEN UND SORTIEREN
                    PERFORM NUMBER-TIMES-WRK TIMES
                      SET IDX UP BY 1
     ‚*               AKTUELLE ZEILE GRÖßER ALS NÄCHSTE? DANN TAUSCHEN
                      IF   SRCSEQ OF USRCMD-TAB(IDX) <
                           SRCSEQ OF USRCMD-TAB(IDX + 1)
                           MOVE 1 TO ROW-REPLACED
                           MOVE TAB-REC OF USRCMD-TAB(IDX) TO SORT-REC
                           MOVE TAB-REC OF USRCMD-TAB(IDX + 1) TO
                                TAB-REC OF USRCMD-TAB(IDX)
                           MOVE SORT-REC TO
                                TAB-REC OF USRCMD-TAB(IDX + 1)
                      END-IF
                    END-PERFORM
           END-PERFORM.

       SRT-TAB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBE ERMITTELN
     ‚*--------------------------------------------------------------
       GET-COLOR SECTION.
       GET-COL-00.

           INITIALIZE HEX-WRK.
           INITIALIZE COLOR-WRK, INVERS-WRK, UNDERLINE-WRK, BLINK-WRK.
     ‚* FARBWERT ERMITTELN
           EVALUATE COLOR-HLP
                    WHEN "G"    MOVE "GREEN    " TO COLOR-WRK
                    WHEN "B"    MOVE "BLUE     " TO COLOR-WRK
                    WHEN "W"    MOVE "WHITE    " TO COLOR-WRK
                    WHEN "P"    MOVE "PINK     " TO COLOR-WRK
                    WHEN "R"    MOVE "RED      " TO COLOR-WRK
                    WHEN "Y"    MOVE "YELLOW   " TO COLOR-WRK
                    WHEN "T"    MOVE "LIGHTBLUE" TO COLOR-WRK
                    WHEN SPACES MOVE "WHITE    " TO COLOR-WRK
           END-EVALUATE.

     ‚* UNTERSTRICHEN? INVERS?
           EVALUATE FORMAT-HLP
     ‚*             INVERS
                    WHEN "I"
                    WHEN "R"  MOVE 1 TO INVERS-WRK
     ‚*             UNTERSTRICHEN
                    WHEN "U"  MOVE 1 TO UNDERLINE-WRK
     ‚*             INVERS + UNTERSTRICHEN
                    WHEN "IU"
                    WHEN "RU" MOVE 1 TO INVERS-WRK
                              MOVE 1 TO UNDERLINE-WRK
           END-EVALUATE.

     ‚* JETZT DAS PROGRAMM AUFRUFEN UND DEN HEXWERT ERMITTELN
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    HEX-WRK
           END-CALL.

       GET-COL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DES BEFEHLS
     ‚*--------------------------------------------------------------
       CHECK-COMMAND SECTION.
       CHK-CMD-00.

           INITIALIZE CHK-BLOCK.
           INITIALIZE CHK-DIRECT.
           INITIALIZE CHK-TARGET.
           INITIALIZE CHK-IS-TARGET.
     ‚* ERSTMAL PRÜFEN OB DAS EIN DIREKT-BEFEHL IST
     ‚*
     ‚* STANDARD SEU-BEFEHLE MIT ZIEL
           IF       CHK-COMMAND      = "C"
            OR      CHK-COMMAND      = "M"
     ‚*     STANDARD SEU-BEFEHLE OHNE ZIEL
            OR      CHK-COMMAND      = "I"
            OR      CHK-COMMAND      = "D"
            OR      CHK-COMMAND      = "K"
            OR      CHK-COMMAND      = "KT"
     ‚*     ERWEITERTE SEU-BEFEHLE OHNE ZIEL
            OR      CHK-COMMAND      = "E"
            OR      CHK-COMMAND      = "DT     "
            OR      CHK-COMMAND      = "WW     "
            OR      CHK-COMMAND      = "WA     "
            OR      CHK-COMMAND      = "#P3    "
            OR      CHK-COMMAND      = "HOT    "
            OR      CHK-COMMAND      = "CKT    "
            OR      CHK-COMMAND      = "RP     "
            OR      CHK-COMMAND(1:2) = "AT"
            OR      CHK-COMMAND(1:1) = "T"
     ‚*     HSEU-BEFEHLE OHNE ZIEL
            OR      CHK-COMMAND(1:1) = ":"
            OR      CHK-COMMAND(1:1) = "."
                    MOVE 1 TO CHK-DIRECT
           END-IF.

     ‚* BRAUCHT DER BEFEHL EIN ZIEL?
           IF       CHK-COMMAND      = "C"
            OR      CHK-COMMAND      = "M"
            OR      CHK-COMMAND      = "CC"
            OR      CHK-COMMAND      = "MM"
                    MOVE 1 TO CHK-TARGET
           END-IF.

     ‚* IST DER BEFEHL EIN BLOCK-BEFEHL?
           IF       CHK-COMMAND      = "CC     "
            OR      CHK-COMMAND      = "MM     "
            OR      CHK-COMMAND      = "DD     "
            OR      CHK-COMMAND      = "EE"
            OR      CHK-COMMAND      = "KT     "
            OR      CHK-COMMAND      = "CKT    "
            OR      CHK-COMMAND(1:2) = "AT"
            OR      CHK-COMMAND(1:2) = "TT"
                    MOVE 1 TO CHK-BLOCK
           END-IF.

     ‚* IST DER BEFEHL EIN ZIEL? DANN JETZT EINEN OFFENEN BLOCK SUCHEN
           IF       CHK-COMMAND      = "A"
TODO ‚*€    OR      CHK-COMMAND      = "B"
                    MOVE 1 TO CHK-IS-TARGET
           END-IF.

     ‚* IST DER BEFEHL NICHTS VON ALLEDEM?
           IF       CHK-DIRECT    = ZEROES
            AND     CHK-BLOCK     = ZEROES
            AND     CHK-IS-TARGET = ZEROES
                    MOVE SPACES TO CHK-COMMAND
           END-IF.

       CHK-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER BEFEHLSTABELLE AUF GÜLTIGKEIT
     ‚*--------------------------------------------------------------
       CHECK-COMMAND-TABLE SECTION.
       CHK-CTB-00.

           INITIALIZE ERRFLD-WRK.
           MOVE     1 TO CNT.
           PERFORM  WITH TEST BEFORE UNTIL CNT   >= 500
                     OR SRCSEQ OF TAB-REC OF USRCMD-TAB(CNT) = ZEROES
                    MOVE SRCCMD OF USRCMD-TAB(CNT) TO CHK-COMMAND
                    PERFORM CHECK-COMMAND
                    IF   CHK-COMMAND = SPACES
                         MOVE SPACES TO SRCCMD OF USRCMD-TAB(CNT)
                         MOVE ZEROES TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             WERTE ÜBERNEHMEN
                    MOVE CHK-DIRECT TO IS-DIRECT OF USRCMD-TAB(CNT)
                    MOVE CHK-BLOCK  TO IS-BLOCK  OF USRCMD-TAB(CNT)
     ‚*             BLOCKBEFEHLE SIND ERSTMAL OFFEN
                    IF   CHK-BLOCK NOT = ZEROES
                         MOVE 1 TO IS-OPEN OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             BRAUCHT DER BEFEHL EIN ZIEL?
                    IF   CHK-TARGET NOT = ZEROES
                         MOVE 1 TO IS-OPEN     OF USRCMD-TAB(CNT)
                         MOVE 1 TO NEED-TARGET OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             IST DER BEFEHL EIN ZIEL?
                    IF   CHK-IS-TARGET NOT = ZEROES
                         MOVE 1 TO IS-TARGET   OF USRCMD-TAB(CNT)
                    END-IF
                    ADD  1                         TO CNT
           END-PERFORM.

     ‚* TABELL NEU SORTIEREN
           PERFORM  SORT-TABLE.

     ‚* HIER EINIGE WERTZE INITIALISIEREN
           INITIALIZE TARGET-COMMANDS, VON-CNT, ZIEL-SRCSEQ.

     ‚* PRÜFEN OB ES BEFEHLE GIBT DIE EIN ZIEL BRAUCHEN UND OB WIR
     ‚* DIESES ZIEL EVTL. SCHON HABEN.
           MOVE     1 TO CNT.
           PERFORM  WITH TEST BEFORE UNTIL CNT   >= 500
                     OR SRCSEQ OF TAB-REC OF USRCMD-TAB(CNT) = ZEROES
                    MOVE SRCCMD OF USRCMD-TAB(CNT) TO CHK-COMMAND
     ‚*             BRAUCHEN WIR EIN ZIEL?
                    IF   IS-OPEN     OF USRCMD-TAB(CNT) NOT = ZEROES
                     AND NEED-TARGET OF USRCMD-TAB(CNT) NOT = ZEROES
     ‚*                  MAXIMAL EINS SOLCHER BEFEHLE ERLAUBT
                         ADD 1 TO TARGET-COMMANDS
                         IF   TARGET-COMMANDS > 1
                              MOVE "YYYYYY" TO ERRFLD-WRK
                              GO TO CHK-CTB-90
                         END-IF
     ‚*                  IN EINER ZWEITEN SCHLEIFE EIN ZIEL SUCHEN
                         MOVE 1      TO IND
                         MOVE ZEROES TO ZIEL-SRCSEQ
                         PERFORM WITH TEST BEFORE UNTIL IND >= 500
                               OR SRCSEQ OF TAB-REC OF USRCMD-TAB(IND)
                                  = ZEROES
                               OR ZIEL-SRCSEQ NOT = ZEROES
                              IF   IS-TARGET OF USRCMD-TAB(IND) = 1
                                   MOVE SRCSEQ OF USRCMD-TAB(IND) TO
                                        ZIEL-SRCSEQ
                                   MOVE CNT TO VON-CNT
                                   MOVE IND TO NACH-IND
                              END-IF
                              ADD 1 TO IND
                         END-PERFORM
                    END-IF
                    ADD  1                         TO CNT
           END-PERFORM.

     ‚* WENN BEFEHLE SOWOHL BLOCK, ALS AUCH DIREKT-BEFEHLE SIND, DANN
     ‚* NUN PRÜFEN OB ES DAFÜR AUCH EINE PASSENDES ENDE GIBT. WENN
     ‚* NICHT, IST ES KEIN BLOCKBEFEHL

     ‚* ZIEL SOLLTE NUN KLAPPEN -> EINTRAGEN
           IF       VON-CNT NOT = ZEROES
                    MOVE ZIEL-SRCSEQ TO
                         TARGET-SEQ OF USRCMD-TAB(VON-CNT)
                    MOVE ZEROES      TO
                          IS-OPEN    OF USRCMD-TAB(VON-CNT)
                    MOVE ZEROES      TO
                          SRCSEQ     OF USRCMD-TAB(NACH-IND)
           END-IF.

     ‚* TABELL NEU SORTIEREN
           PERFORM  SORT-TABLE.

       CHK-CTB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB BESTIMMTE BEFEHLE DIREKT AUSGEFÜHRT WERDEN KÖNNEN
     ‚*--------------------------------------------------------------
       CHECK-EXEC-DIRECT SECTION.
       CHK-EXD-00.

           MOVE     1 TO CNT.
           PERFORM  WITH TEST BEFORE UNTIL CNT   >= 500
                     OR SRCSEQ OF TAB-REC OF USRCMD-TAB(CNT) = ZEROES
     ‚*             BEFEHL SICHERN
                    MOVE SRCSEQ OF USRCMD-TAB(CNT) TO SRCSEQ-WRK
                    MOVE SRCCMD OF USRCMD-TAB(CNT) TO CHK-COMMAND
     ‚*             NEUE ZEILE HINZUFÜGEN
                    IF   CHK-COMMAND = "I"
                         PERFORM EXEC-I-DIRECT
                         MOVE SRCSEQ-WRK TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             ZEILE LÖSCHEN
                    IF   CHK-COMMAND = "D"
                         PERFORM EXEC-D-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             ZEILE DBMAIN EXTENDED MARKIEREN *DB
                    IF   CHK-COMMAND = "E"
                         PERFORM EXEC-E-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             ZEILE MARKIEREN
                    IF   CHK-COMMAND = "T"
                         PERFORM EXEC-T-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             ZEILE KOMMENTIEREN
                    IF   CHK-COMMAND = "K"
                         PERFORM EXEC-K-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             ZEILE KOMMENTIEREN UND MARKIEREN
                    IF   CHK-COMMAND = "KT"
                         PERFORM EXEC-KT-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             TEMPLATE HINZUFÜGEN
                    IF   CHK-COMMAND(1:1) = ":"
                         PERFORM EXEC-TEMPLATE-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             LINETAGGING HINZUFÜGEN
                    IF   CHK-COMMAND(1:1) = "."
                         PERFORM EXEC-LINETAGGING-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             ZEILE FARBLICH MARKIEREN
                    IF   CHK-COMMAND(1:2) = "AT"
                         PERFORM EXEC-COLOR-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
                    ADD  1                         TO CNT
           END-PERFORM.

     ‚* TABELL NEU SORTIEREN
           PERFORM  SORT-TABLE.

       CHK-EXC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB WIR IN SICH GESCHLOSSENE BLOCKBEFEHLE HABEN
     ‚*--------------------------------------------------------------
       CHECK-VALID-BLOCK SECTION.
       CHK-VBL-00.

           INITIALIZE RET-CODE.

       CHK-VBL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN BEFEHL MIT EINEM ZIEL AUSGEFÜHRT WIRD
     ‚*--------------------------------------------------------------
       CHECK-TARGET-COMMAND SECTION.
       CHK-TAR-00.

           MOVE     1 TO CNT.
           PERFORM  WITH TEST BEFORE UNTIL CNT   >= 500
                     OR SRCSEQ OF TAB-REC OF USRCMD-TAB(CNT) = ZEROES
     ‚*             BEFEHL SICHERN
                    MOVE SRCSEQ     OF USRCMD-TAB(CNT) TO SRCSEQ-WRK
                    MOVE TARGET-SEQ OF USRCMD-TAB(CNT) TO SRCSEQ-TRG
                    MOVE SRCCMD     OF USRCMD-TAB(CNT) TO CHK-COMMAND
     ‚*             ZEILE VERSCHIEBEN
                    IF   CHK-COMMAND     = "M"
                     AND SRCSEQ-TRG NOT = ZEROES
                         PERFORM EXEC-M-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
     ‚*             ZEILE KOPIEREN
                    IF   CHK-COMMAND    = "C"
                     AND SRCSEQ-TRG NOT = ZEROES
                         PERFORM EXEC-C-DIRECT
                         MOVE ZEROES     TO SRCSEQ OF USRCMD-TAB(CNT)
                    END-IF
                    ADD  1                         TO CNT
           END-PERFORM.

     ‚* TABELL NEU SORTIEREN
           PERFORM  SORT-TABLE.

       CHK-TAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN BEFEHL NOCH OFFEN IST
     ‚*--------------------------------------------------------------
       CHECK-OPEN-COMMAND SECTION.
       CHK-OCM-00.

           INITIALIZE RET-CODE.

       CHK-OCM-90.
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
