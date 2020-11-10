       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0038.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCE-POINTER ÄNDERN                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.01.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SPOOL AN SOURCE-PTR HÄNGEN                   *
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
     ‚* TEMPORÄRE DATEI PTMPSEU
     ‚*-------------------------------------------------------------
           SELECT PTMPSEU-DP
                  ASSIGN       TO  DATABASE-PTMPSEU
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEMPORÄRE PTMPSEU
       FD  PTMPSEU-DP
           LABEL RECORDS ARE STANDARD.
       01  PTMPSEU-P.
           COPY DDS-ALL-FORMATS OF PTMPSEU.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0038".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

     ‚* FÜR LIKE-DEKLARATION
       01  SRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

       01  START-ERRORS                  PIC 9(1).
       01  ZEILE-WRK                     PIC 9(3).
       01  BEW-STUFE                     PIC X(2).
       01  TEXT-WRK                      PIC X(100).
       01  RED-RI-HEX-WRK                PIC X(1).

       01  SRCSEQ-RED                    PIC X(6).
       01  SRCSEQ-WRK REDEFINES SRCSEQ-RED PIC 9(4)V9(2).

     ‚* PARAMETER FÜR DAS HLP0011
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRC-LIB                     PIC X(10).
       01  X-SRC-FILE                    PIC X(10).
       01  X-SRC-MBR                     PIC X(10).
       01  X-SHOW-00                     PIC 9(1).
       01  X-SHOW-10                     PIC 9(1).
       01  X-SHOW-20                     PIC 9(1).
       01  X-SHOW-30                     PIC 9(1).
       01  X-SPOOL-TYPE                  PIC X(10).
       01  X-SRC-POINTER                 USAGE POINTER.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRC-LIB
                                X-SRC-FILE
                                X-SRC-MBR
                                X-SHOW-00
                                X-SHOW-10
                                X-SHOW-20
                                X-SHOW-30
                                X-SPOOL-TYPE
                                X-SRC-POINTER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0038" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* STARTWERTE ERMITTELN
           PERFORM  GET-COLOUR.

     ‚* TEMPORÄRE DATEIEN LESEN
           OPEN     INPUT PTMPSEU-DP.
           IF       FILE-STATUS = ZEROES
                    EVALUATE X-SPOOL-TYPE
                             WHEN "CBLLE"  PERFORM READ-TEMPFILE-CBLLE
     ‚*€                     WHEN "PRELOG" PERFORM READ-TEMPFILE-PRELOG
     ‚*€                     WHEN "CHKSRC" PERFORM READ-TEMPFILE-CHKSRC
                    END-EVALUATE
                    CLOSE PTMPSEU-DP
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ISEU STARTEN
     ‚*--------------------------------------------------------------
       READ-TEMPFILE-CBLLE SECTION.
       REA-TMP-00.

           INITIALIZE START-ERRORS.

       REA-TMP-20.
           READ     PTMPSEU-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-TMP-90
           END-IF.

     ‚* BESTIMMTE MELDUNGEN ÜBERLESEN
           IF       PTMPSEU-P(4:3)  = "ANW"
            OR      PTMPSEU-P(2:7)  = "5722WDS"
            OR      PTMPSEU-P(1:20) = SPACES
                    MOVE ZEROES          TO START-ERRORS
                    GO TO REA-TMP-20
           END-IF.

     ‚* AB HIER GEHTS LOS:
     ‚* 125  ID:    LNC1236  BEW.STUFE 30  FOLGENR. 009400
     ‚*      NACHRICHT . . . :   KEINE KLAUSEL PICTURE FÜR DATENELEMENT
     ‚* ...
     ‚*
     ‚* AM ANFANG STEHT IMMER DER "*"
           IF       PTMPSEU-P(1:1) = "*"
            AND     PTMPSEU-P(49:6) IS NUMERIC
                    MOVE 1               TO START-ERRORS
     ‚*             INTERNE ZEILENZÄHLER ZURÜCKSETZEN
                    INITIALIZE ZEILE-WRK
     ‚*             ZEILENNUMMER SICHERN
                    MOVE PTMPSEU-P(49:6) TO SRCSEQ-RED
     ‚*             BEWERTUNGSSTUFE SICHERN
                    MOVE PTMPSEU-P(36:2) TO BEW-STUFE
           END-IF.

     ‚* DAHER ÜBERLESEN WIR ERSTMAL ALLE ANDEREN
           IF       START-ERRORS = ZEROES
                    GO TO REA-TMP-20
           END-IF.

     ‚* JETZT KOPIEREN WIR DIE NÄCHSTEN ZEILEN, BIS EINE NEUE
     ‚* MELDUNG KOMMT
           EVALUATE BEW-STUFE ALSO TRUE
                    WHEN "00" ALSO X-SHOW-00 = 1
                    WHEN "10" ALSO X-SHOW-10 = 1
                    WHEN "20" ALSO X-SHOW-20 = 1
                    WHEN "30" ALSO X-SHOW-30 = 1
                         CONTINUE
                    WHEN OTHER
                         GO TO REA-TMP-20
           END-EVALUATE.

     ‚* JETZT SCHREIBEN
           MOVE     PTMPSEU-P(10:) TO TEXT-WRK.
           ADD      1              TO ZEILE-WRK.
           PERFORM  ADD-TO-PTR.

           GO TO    REA-TMP-20.

       REA-TMP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MELDUNG DEM POINTER HINZUFÜGEN
     ‚*--------------------------------------------------------------
       ADD-TO-PTR SECTION.
       ADD-PTR-00.

           INITIALIZE QSETSRC OF SRC-WRK.
           MOVE     SRCSEQ-WRK          TO SRCSEQ OF SRC-WRK.
           MOVE     ZEROES              TO SRCDAT OF SRC-WRK.
           MOVE     "##CP"              TO SRCDTA OF SRC-WRK(1:4).
           MOVE     RED-RI-HEX-WRK      TO SRCDTA OF SRC-WRK(6:1).
           MOVE     "*"                 TO SRCDTA OF SRC-WRK(7:1).
           MOVE     "CPLLST:"           TO SRCDTA OF SRC-WRK(9:7).
           MOVE     TEXT-WRK            TO SRCDTA OF SRC-WRK(17:).

           INITIALIZE MAPDTA-REC.
           MOVE     X-SRC-LIB         TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRC-FILE        TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRC-MBR         TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK        TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     "CPLLST"          TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     ZEILE-WRK         TO MAP-KEY OF MAPDTA-REC(45:3).
           MOVE     SRC-WRK           TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE         TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-SRC-POINTER.
           PERFORM  COPY-CALL-MAPDTA.

       ADD-PTR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBWERTE ANZEIGEN
     ‚*--------------------------------------------------------------
       GET-COLOUR SECTION.
       GET-COL-00.

     ‚* ALLE WERTE ZURÜCKSETZEN
           INITIALIZE RED-RI-HEX-WRK.
     ‚* ROTEN INVERSEN FARBWERT ERMITTELN
           MOVE     "RED"           TO COLOR-WRK.
           MOVE     "1"             TO INVERS-WRK.
           MOVE     SPACES          TO BLINK-WRK.
           MOVE     SPACES          TO UNDERLINE-WRK.
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    RED-RI-HEX-WRK
           END-CALL.

       GET-COL-90.
           EXIT.
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
