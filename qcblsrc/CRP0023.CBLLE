       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0023.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SYNTAXHIGHLIGHTING FÜR EINE SOURCE                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SYNTAXHIGHLIGHTING                           *
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

       01  PGM-WRK                       PIC X(10) VALUE "CRP0023".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  MAP-LFDN-WRK                  LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* SOURCE ZUM ARBEITEN
       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

     ‚* POSITIONSWERTE
       01  START-POS                     PIC 9(4).
       01  POS-TMP                       PIC 9(4).
       01  POS                           PIC 9(4).

       01  HEX-WRK                       PIC X(1).
       01  COL-GRN                       PIC X(1) VALUE X'20'.

       01  HGH-FOUND                     PIC 9(1).
       01  MAP-KEY-WRK                   LIKE MAP-KEY OF MAPDTA-REC.

     ‚* FELDER FÜR GET-NEXT-WORD
       01  WORD                          PIC X(80).
       01  GNW-IDX                       PIC 9(4).
       01  GNW-POS                       PIC 9(4).
       01  GNW-NEW-READ                  PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-POINTER                     USAGE POINTER.
       01  X-HGHTYP                      PIC X(10).
       01  X-SRCDTA                      LIKE SRCDTA OF QSETSRC-WRK.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-POINTER
                                         X-HGHTYP
                                         X-SRCDTA.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0023" TO PGM-WRK.

     ‚* WERTE INITALISIEREN
           INITIALIZE GNW-POS, GNW-IDX.
           MOVE     1 TO GNW-NEW-READ.

     ‚* SOLANGE LESEN BIS ALLE WÖRTER DURCHSUCHT SIND
           PERFORM  WITH TEST BEFORE UNTIL
                     GNW-POS >= LENGTH OF X-SRCDTA
     ‚*             WÖRTER DURCHSUCHEN
                    PERFORM GET-NEXT-WORD
                    IF   WORD NOT = SPACES
                         PERFORM GET-HIGHLIGHT
                         IF   HGH-FOUND NOT = ZEROES
                              PERFORM ADD-HIGHLIGHT
                         END-IF
                    END-IF
           END-PERFORM.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* HIGHLIGHT HINZUFÜGEN
     ‚*--------------------------------------------------------------
       ADD-HIGHLIGHT SECTION.
       ADD-HGH-00.

     ‚* ALLE ALTEN HEXZEICHEN ENTFERNEN
           MOVE     START-POS TO POS-TMP.
           PERFORM  WITH TEST BEFORE UNTIL POS-TMP >= GNW-POS
                    IF   X-SRCDTA(POS-TMP:1) < SPACES
                         MOVE SPACES TO X-SRCDTA(POS-TMP:1)
                    END-IF
                    ADD 1 TO POS-TMP
           END-PERFORM.

     ‚* VORDERES HEXZEICHEN IMMER, HINTERES NUR WENN FREI
           MOVE     HEX-WRK TO X-SRCDTA(START-POS:1).
           IF       X-SRCDTA(GNW-POS:1) = SPACES
                    MOVE COL-GRN TO X-SRCDTA(GNW-POS:1)
           END-IF.

       ADD-HGH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES HEX-WRTES
     ‚*--------------------------------------------------------------
       GET-HIGHLIGHT SECTION.
       GET-HGH-00.

           INITIALIZE HEX-WRK.
           INITIALIZE HGH-FOUND.
     ‚* WORTLÄNGE ERMITTELN
           MOVE     LENGTH OF WORD TO POS.
           PERFORM  WITH TEST BEFORE UNTIL WORD(POS:1) NOT = SPACES
                    SUBTRACT 1 FROM POS
           END-PERFORM.
           IF       WORD(POS:1) = "."
                    MOVE SPACES TO WORD(POS:1)
           END-IF.

     ‚* IN GROßBUCHSTABEN UMSETZEN
           MOVE     FUNCTION UPPER-CASE(WORD) TO WORD.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-HGHTYP           TO MAP-KEY  OF MAPDTA-REC(1:20).
           MOVE     WORD               TO MAP-KEY  OF MAPDTA-REC(21:).
           MOVE     MAP-READ           TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-POINTER.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    INITIALIZE MAPDTA-REC
                    MOVE     X-HGHTYP  TO MAP-KEY  OF MAPDTA-REC(1:20)
                    STRING   WORD DELIMITED BY SPACES
                             "."  DELIMITED BY SIZE
                    INTO     MAP-KEY-WRK
                    MOVE     MAP-KEY-WRK TO MAP-KEY OF MAPDTA-REC(21:)
                    MOVE     MAP-READ  TO MAP-ACT  OF MAPDTA-REC
                    SET      MAP-PTR OF MAPDTA-REC TO X-POINTER
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO GET-HGH-90
                    END-IF
           END-IF.

     ‚* WERT GEFUNDEN
           MOVE     1                          TO HGH-FOUND.
           MOVE     MAP-REC OF MAPDTA-REC(1:1) TO HEX-WRK.

       GET-HGH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NÄCHSTES WORD AUS STR-WRK ERMITTELN
     ‚*--------------------------------------------------------------
       GET-NEXT-WORD SECTION.
       GET-NWD-00.

           INITIALIZE WORD.
     ‚* STRING VON BEGIN AN LESEN
           IF       GNW-NEW-READ = 1
                    INITIALIZE GNW-NEW-READ
                    MOVE 1 TO  GNW-POS
           END-IF.

           INITIALIZE START-POS.
     ‚* FÜHRENDE SPACES ÜBERLESEN
           PERFORM  WITH TEST BEFORE VARYING GNW-POS FROM GNW-POS BY 1
                    UNTIL GNW-POS > LENGTH OF X-SRCDTA OR
                          X-SRCDTA(GNW-POS:1) > SPACES
                    CONTINUE
           END-PERFORM.

     ‚* STARTPOSITION SICHERN
           COMPUTE  START-POS = GNW-POS - 1
           END-COMPUTE.

     ‚* ALLE ZEICHEN BIS ZUM NÄCHSTEN SPACE ÜBERNEHMEN
           INITIALIZE GNW-IDX.
           PERFORM  WITH TEST BEFORE VARYING GNW-POS FROM GNW-POS BY 1
                    UNTIL GNW-POS > LENGTH OF X-SRCDTA OR
                          X-SRCDTA(GNW-POS:1) <= SPACES
                    ADD 1 TO GNW-IDX
                    MOVE X-SRCDTA(GNW-POS:1) TO WORD(GNW-IDX:1)
           END-PERFORM.

       GET-NWD-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
