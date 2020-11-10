       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0031.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ZÄHLEN DER POINTEREINTRÄGE                                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  07.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  EINTRAG IM POINTER SUCHEN                    *
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
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0031".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

     ‚* FÜR LIKE-DEKLARATION
       01  SRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

     ‚* LAUFVARIABLEN
       01  IND                           PIC 9(4).

     ‚* VARIABLEN FÜR DIE SUCHROUTINE
       01  SUCH-TAB.
        05  SUCH-REC OCCURS 10.
         06  SUCH                        LIKE SRCDTA OF SRC-WRK.
         06  S-POS                       PIC 9(3) COMP.
         06  E-POS                       PIC 9(3) COMP.
         06 SRCDAT                       LIKE SRCDAT OF SRC-WRK.
         06 SRCDTA                       LIKE SRCDTA OF SRC-WRK.

       01  STR-TAB.
        05  STR-ANZ                      PIC 9(3) COMP.
        05  STR-ANZ2                     PIC 9(3) COMP.
        05  STR-REC OCCURS 100.
         06  STR                         LIKE SRCDTA OF SRC-WRK.
         06  S-LEN                       PIC 9(3) COMP.
         06  E-LEN                       PIC 9(3) COMP.

       01  SOURCE-TMP                    LIKE SRCDTA OF SRC-WRK.
       01  SOURCE-WRK                    LIKE SRCDTA OF SRC-WRK.
       01  SOURCE-FND                    LIKE SRCDTA OF SRC-WRK.
       01  GEFUNDEN                      PIC 9(01) COMP.
       01  POS                           PIC 9(3) COMP.
       01  POS2                          PIC 9(3) COMP.
       01  LEN                           PIC 9(3) COMP.
       01  LEN2                          PIC 9(3) COMP.
       01  STR-POS                       PIC 9(3) COMP.
       01  STR-ANF                       PIC 9(3) COMP.
       01  STR-LEN                       PIC 9(3) COMP.
       01  STR-WRK                       LIKE SRCDTA OF SRC-WRK.

       01  START-ZEILE                   PIC S9(9).
       01  START-ERFOLGT                 PIC 9(1).

     ‚* VARIABLEN FÜR DIE SUCHE
       01  FNDSTR-WRK                    LIKE SRCDTA OF SRC-WRK.
       01  FIND-STARTZEILE               LIKE SRCSEQ OF SRC-WRK.
       01  FIND-GEFUNDEN                 PIC 9(1).
       01  FIND-GEFUNDEN-POS             PIC 9(4).
       01  FIND-GEFUNDEN-ZEILE           LIKE SRCSEQ OF SRC-WRK.
       01  FIND-GEFUNDEN-KEY             LIKE MAP-KEY OF MAPDTA-REC.
       01  FIND-ZEILE-LEN                PIC 9(4).
       01  FIND-ZEILE-POS-ANFANG         PIC 9(4).

     ‚* PARAMETER FÜR DAS CCP0005
       01  PATTERN-WRK                   PIC X(200).
       01  C-RETCODE                     PIC S9(9) BINARY.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-POINTER                     USAGE POINTER.
       01  X-FNDSTR                      LIKE FNDSTR-WRK.
       01  X-FIND-TYPE                   PIC 9(1).
       01  X-START                       LIKE START-ZEILE.
       01  X-F-START                     LIKE FIND-STARTZEILE.
       01  X-RET-FOUND                   PIC 9(1).
       01  X-RET-POS                     LIKE FIND-GEFUNDEN-POS.
       01  X-RET-SEQ                     LIKE FIND-GEFUNDEN-ZEILE.
       01  X-RET-KEY                     LIKE FIND-GEFUNDEN-KEY.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-POINTER
                                         X-FNDSTR
                                         X-FIND-TYPE
                                         X-START
                                         X-F-START
                                         X-RET-FOUND
                                         X-RET-POS
                                         X-RET-SEQ
                                         X-RET-KEY.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0031" TO PGM-WRK.

           INITIALIZE X-RET-FOUND.
           INITIALIZE X-RET-POS.
           INITIALIZE X-RET-SEQ.
           INITIALIZE X-RET-KEY.

     ‚* STRING SUCHEN
           IF       X-FNDSTR NOT = SPACES
                    MOVE    X-FNDSTR  TO FNDSTR-WRK
                    MOVE    X-START   TO START-ZEILE
                    MOVE    X-F-START TO FIND-STARTZEILE
                    PERFORM FIND-TEXT
     ‚*             RÜCKGABEWERTE ÜBERNEHMEN
                    MOVE    FIND-GEFUNDEN       TO X-RET-FOUND
                    MOVE    FIND-GEFUNDEN-POS   TO X-RET-POS
                    MOVE    FIND-GEFUNDEN-ZEILE TO X-RET-SEQ
                    MOVE    FIND-GEFUNDEN-KEY   TO X-RET-KEY
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DIE SUCHE LÄUFT IN 2 SCHRITTEN AB:
     ‚*  1. DIE AKTUELL ANGEZEIGTEN ZEILEN DURCHSUCHEN
     ‚*  2. MIT MAPDTA DIE NÄCHSTEN ZEILEN ABSUCHEN BIS ZUM ENDE
     ‚*     DER TEILDATEI
     ‚*--------------------------------------------------------------
       FIND-TEXT SECTION.
       FND-TXT-00.

           INITIALIZE FIND-GEFUNDEN.
           INITIALIZE FIND-GEFUNDEN-KEY.
           INITIALIZE FIND-GEFUNDEN-POS.
           INITIALIZE FIND-GEFUNDEN-ZEILE.
           INITIALIZE STR-ANZ, STR-ANZ2.
     ‚* SUCHEN DES TEXTES MIT MAPDTA
           MOVE     ZEROES  TO START-ERFOLGT.
           SET      MAP-PTR OF MAPDTA-REC TO X-POINTER.

       FND-TXT-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE     1             TO START-ERFOLGT
                    MOVE     START-ZEILE   TO MAP-LFDN OF MAPDTA-REC
                    MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO FND-TXT-90
                    END-IF
           ELSE
                    MOVE     MAP-READ-NEXT TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO FND-TXT-90
                    END-IF
           END-IF.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO SRC-WRK.

     ‚* ZEILE ÜBERSPRINGEN?
           IF       SRCSEQ OF SRC-WRK <= FIND-STARTZEILE
                    GO TO FND-TXT-20
           END-IF.

     ‚* IST DER GESUCHTE TEXT VORHANDEN?
           IF       FNDSTR-WRK NOT = SPACES
                    EVALUATE X-FIND-TYPE
                             WHEN ZEROES PERFORM FIND-NORMAL
                             WHEN 1      PERFORM FIND-REGEX
                    END-EVALUATE
                    IF   RET-CODE NOT = SPACES
                         GO TO FND-TXT-20
                    ELSE
                         MOVE SRCSEQ OF SRC-WRK TO FIND-GEFUNDEN-ZEILE
                         MOVE MAP-KEY OF MAPDTA-REC TO
                              FIND-GEFUNDEN-KEY
                         MOVE 1                 TO FIND-GEFUNDEN
                         PERFORM FIND-TEXT-POSITION
                         GO TO FND-TXT-90
                    END-IF
           END-IF.

     ‚* NÄCHSTE ZEILE LESEN
           GO TO    FND-TXT-20.

       FND-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GEWÖHNLICHE STRING-SUCHE
     ‚*--------------------------------------------------------------
       FIND-NORMAL SECTION.
       FND-NRM-00.

           MOVE     SRCDTA OF SRC-WRK           TO RECORD-WRK.
           MOVE     LENGTH OF SRCDTA OF SRC-WRK TO RECORD-LEN.
           MOVE     FNDSTR-WRK                  TO SELEKTION-WRK.
           CALL     PROCEDURE "HLP0001" USING RECORD-WRK
                                              RECORD-LEN
                                              SELEKTION-WRK
                                              RET-CODE
           END-CALL.

       FND-NRM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHE MITTELS REGULAR EXPRESSION
     ‚*--------------------------------------------------------------
       FIND-REGEX SECTION.
       FND-RGX-00.

           INITIALIZE RET-CODE.
           INITIALIZE C-RETCODE.

     ‚* STRING MIT HEX 0 ANHÄNGEN
           MOVE     SRCDTA OF SRC-WRK           TO RECORD-WRK.
           MOVE     X"00"       TO RECORD-WRK(LENGTH OF SRC-WRK + 1:1).
     ‚* SUCHFOLGE AUFBEREITEN
           MOVE     FNDSTR-WRK                  TO PATTERN-WRK.
           MOVE     X"00"   TO PATTERN-WRK(LENGTH OF FNDSTR-WRK + 1:1).

           CALL     PROCEDURE "CCP0005" USING RECORD-WRK
                                              PATTERN-WRK
                                              C-RETCODE
           END-CALL.
           IF       C-RETCODE = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

       FND-RGX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* POSITION IM STRING FINDEN
     ‚*--------------------------------------------------------------
       FIND-TEXT-POSITION SECTION.
       FND-TXP-00.

           INITIALIZE SUCH-TAB.
           MOVE     SRCDTA OF SRC-WRK TO SOURCE-WRK.
           MOVE     FNDSTR-WRK        TO SUCH(01).
           PERFORM  SUCHEN.
           IF       S-POS(1) NOT = ZEROES
                    COMPUTE FIND-GEFUNDEN-POS = S-POS(1) + 11
                    END-COMPUTE
           END-IF.

       FND-TXP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DURCHSUCHEN STRINGTABELLE
     ‚*--------------------------------------------------------------
       SUCHEN SECTION.
       STR-SUC-00.

           INITIALIZE GEFUNDEN.
     ‚* ALLE SUCHERGEBNISSE LÖSCHEN
           MOVE     ZEROES TO IND.
           PERFORM  10 TIMES
                    ADD  1      TO IND
                    MOVE ZEROES TO S-POS(IND)
                    MOVE ZEROES TO E-POS(IND)
           END-PERFORM.

     ‚* SOURCE AUF GROßBUCHSTABEN UMSETZEN
           MOVE     1 TO IND.
           MOVE     FUNCTION UPPER-CASE(SOURCE-WRK) TO SOURCE-FND.

           MOVE     1 TO POS.
           MOVE     SUCH(1) TO STR-WRK.
           PERFORM  ERMITTELN-LAENGE.

           PERFORM  WITH TEST BEFORE UNTIL SOURCE-FND(IND:) = SPACES
                     OR IND >= LENGTH OF SOURCE-FND
                     OR STR-LEN + IND >  LENGTH OF SOURCE-FND
                    IF   SOURCE-FND(IND:STR-LEN) =
                         SUCH(1)(STR-ANF:STR-LEN)
     ‚*                  STARTPOSITION SICHERN
                         COMPUTE S-POS(POS) = IND
     ‚*                  ENDPOSITION SICHERN
                         COMPUTE E-POS(POS) = IND + STR-LEN
                         END-COMPUTE
                         MOVE 1 TO GEFUNDEN
                         ADD  1 TO POS
                    END-IF
                    ADD  1 TO IND
           END-PERFORM.

       STR-SUC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN LÄNGE EINES ÜBERGEBENEN STRINGS
     ‚*--------------------------------------------------------------
       ERMITTELN-LAENGE SECTION.
       ERM-LEN-00.

     ‚* WARSCHEINLICH WIRD DER NÄCHSTES SUCHSTRING IN DER TABELLE
     ‚* GESUCHT. DESHALB VORAB SCHONMAL PRÜFEN OB DIESER DAS IST.
     ‚* ANDERENFALLS WIRD NOCHMAL DIE KOMPLETTE TABELLE DURCHSUCHT.
           IF       STR-ANZ2 + 1 <= STR-ANZ
            AND     STR(STR-ANZ2 + 1) = STR-WRK
                    ADD 1 TO STR-ANZ2
                    MOVE S-LEN(STR-ANZ2) TO STR-ANF
                    MOVE E-LEN(STR-ANZ2) TO STR-LEN
                    GO TO ERM-LEN-90
           END-IF.

     ‚* IST FÜR DIESEN STRING BEREITS DIE SUCHE ERFOLGT, DANN
     ‚* GESICHERTE WERTE AUS DER TABELLE ÜBERNEHMEN (PERFORMANCE)
           INITIALIZE STR-ANZ2.
           PERFORM  UNTIL STR-ANZ2 >= STR-ANZ
                    ADD 1 TO STR-ANZ2
                    IF   STR(STR-ANZ2) = STR-WRK
                         MOVE S-LEN(STR-ANZ2) TO STR-ANF
                         MOVE E-LEN(STR-ANZ2) TO STR-LEN
                         GO TO ERM-LEN-90
                    END-IF
           END-PERFORM.

           INITIALIZE STR-ANF.
           INITIALIZE STR-LEN.
           PERFORM  WITH TEST BEFORE VARYING STR-POS FROM LENGTH OF
                    STR-WRK BY -1 UNTIL STR-POS = ZEROES
                    IF   STR-WRK(STR-POS:1) NOT = SPACE
                         IF   STR-LEN = ZEROES
     ‚*                       ABSCHLUSSZEICHEN
                              IF   STR-WRK(STR-POS:1) = "'"
                                   COMPUTE STR-LEN = STR-POS - 1
                              ELSE
                                   COMPUTE STR-LEN = STR-POS
                              END-IF
                         ELSE
                              IF   STR-WRK(STR-POS:1) = "'"
     ‚*                            ANFANGSZEICHEN
                                   COMPUTE STR-ANF = STR-POS + 1
                              END-IF
                         END-IF
                    END-IF
           END-PERFORM.

           IF       STR-ANF = ZEROES
                    MOVE 1 TO STR-ANF
           END-IF.
           COMPUTE  STR-LEN = STR-LEN - STR-ANF + 1.

     ‚* ERMITTELTE WERTE IN TABELLE ABLEGEN.
           ADD      1 TO STR-ANZ.
           IF       STR-ANZ > 100
                    MOVE 1 TO STR-ANZ
           END-IF.
           MOVE     STR-WRK TO STR-REC(STR-ANZ).
           MOVE     STR-ANF TO S-LEN(STR-ANZ).
           MOVE     STR-LEN TO E-LEN(STR-ANZ).

       ERM-LEN-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
