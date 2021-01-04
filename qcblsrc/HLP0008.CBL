       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0008 IS RECURSIVE.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DYN. COBOL-AUFRUF (LGPGM986)                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DYNAMISCHER COBOL-AUFRUF                     *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*

       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                         ATTRIBUTE-DATA IS BS-ATRIBUTE,
                         I-O-FEEDBACK   IS FILE-IO-FEEDBACK,
                        LINKAGE PROCEDURE FOR "CFP9111",
                        LINKAGE SYS FOR "CEEHDLR",
                        LINKAGE SYS FOR "CEEHDLU".

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0008 ".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

     ‚* DA REKURSIVER AUFRUF, HIER EINE LOCAL-STORAGE SECTION
       LOCAL-STORAGE SECTION.

     ‚* ZÄHLERVARIABLE FÜR FÜLLEN PARAMETERSTRING AUS DYNCAL
       01  V-POS                         PIC 9(4).
     ‚* ZÄHLERVARIABLE FÜR PARAMETERAUSWERTUNGSSCHLEIFE
       01  POS                           PIC 9(4).
       01  POS2                          PIC 9(4).
     ‚* ANZAHL AN ERMITTELTEN PARAMETERN
       01  NUM-PARMS                     PIC 9(2).
     ‚* POSITION DES PARAMETERS MIT F3
       01  F3-POS                        PIC 9(3).
     ‚* POSITION DES PARAMETERS MIT F12
       01  F12-POS                       PIC 9(3).
     ‚* POSITION FÜR AUSWERTUNG DER PARAMETER
       01  START-POS                     PIC 9(4).

     ‚* TABELLE MIT PARAMETERN DIE AN DIE PROGRAMME ÜBERGEBEN WERDEN
       01  PARM-TAB.
        05 PARM-REC   OCCURS 40 TIMES.
     ‚*  PARAMETERDATEN
         10 PARM-DATA                    PIC X(1024).
     ‚*  LÄNGE DES PARAMETERS
         10 PARM-LEN                     PIC 9(4).

       01  PARM-WRK                      PIC X(1024).
       01  PARM-TMP                      PIC X(1024).

     ‚* VARIABLEN FÜR DEN HANDLER
       01  HANDLER-ERSTELLEN             PIC 9(1) VALUE 1.
       01  CH-PTR                        PROCEDURE-POINTER.
       01  TOKEN-PTR                     POINTER.
       01  FC                            PIC X(12) VALUE LOW-VALUE.
       01  FC-REDEF                      REDEFINES FC.
        02 SEV                           PIC 9(4) BINARY.
        02 MSGNO                         PIC 9(4) BINARY.
        02 FLAGS                         PIC X(01).
        02 FACIL                         PIC X(03).
        02 ISI                           PIC 9(9) BINARY.
       01  TOKEN-AREA                    PIC X(100) VALUE LOW-VALUE.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-PGMNAM                      PIC X(10).
     ‚*    ACHTUNG! ES WERDEN NUR 1024 STELLEN VERWENDET
       01  X-PARM                        PIC X(1027).
       01  X-BATCH                       PIC X(1).
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-PGMNAM
                                X-PARM
                                X-BATCH
                                X-F3
                                X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE NUM-PARMS.
           INITIALIZE START-POS.
           INITIALIZE F3-POS.
           INITIALIZE F12-POS.
           INITIALIZE PARM-TMP.

           MOVE     X-PARM TO PARM-WRK.

     ‚*  DURCHARBEITEN DES ÜBERGEBENEN PARAMETERSTRINGS ZUR ERMITTELUNG
     ‚*  DER EINZELNEN PARAMETER, SOWIE ERMITTELUNG POSITION VON F3 F12
           PERFORM  WITH TEST BEFORE VARYING POS FROM 1 BY 1 UNTIL
                    POS > LENGTH OF PARM-WRK
                    OR PARM-WRK(POS:) = SPACES
     ‚*             F3 GEFUNDEN?
                    IF   PARM-WRK(POS:3) = "&F3"
                         ADD 1 TO NUM-PARMS
                         MOVE SPACE TO PARM-DATA OF PARM-REC(NUM-PARMS)
                         MOVE 1     TO PARM-LEN  OF PARM-REC(NUM-PARMS)
                         MOVE NUM-PARMS TO F3-POS
                    END-IF
     ‚*             F12 GEFUNDEN?
                    IF   PARM-WRK(POS:4) = "&F12"
                         ADD 1 TO NUM-PARMS
                         MOVE SPACE TO PARM-DATA OF PARM-REC(NUM-PARMS)
                         MOVE 1     TO PARM-LEN  OF PARM-REC(NUM-PARMS)
                         MOVE NUM-PARMS TO F12-POS
                    END-IF
     ‚*             EIN HOCHKOMMA GEFUNDEN (START PARAMETER)
                    IF   PARM-WRK(POS:1) = "'"
                         IF   START-POS > ZEROES
                              ADD 1 TO NUM-PARMS
     ‚*                       EINEN KOMPLETTEN PARAMETER GEFUNDEN?
     ‚*                       DANN WERT IN TABELLE FÜLLEN
                              IF   POS - START-POS NOT = 1
                                   IF   POS + 1 <= LENGTH OF PARM-WRK
                                    AND PARM-WRK(POS + 1:1) = "'"
                                        ADD 1 TO POS
                                        SUBTRACT 1 FROM NUM-PARMS
                                   ELSE
                                        MOVE PARM-TMP(1:POS2 - 1) TO
                                             PARM-DATA OF
                                             PARM-REC(NUM-PARMS)
                                        COMPUTE PARM-LEN OF
                                          PARM-REC(NUM-PARMS) =
                                            (POS - START-POS) - 1
                                        END-COMPUTE
                                        MOVE ZEROES TO START-POS
                                   END-IF
                              ELSE
                                   IF   POS + 1 <= LENGTH OF PARM-WRK
                                    AND PARM-WRK(POS + 1:1) = "'"
                                        ADD 1 TO POS
                                        SUBTRACT 1 FROM NUM-PARMS
                                   ELSE
     ‚*                                 ALS PARAMETER NUR '' GEFUNDEN?
     ‚*                                 DANN 1 BYTE ANNEHMEN
                                        MOVE SPACES TO PARM-DATA OF
                                             PARM-REC(NUM-PARMS)
                                        MOVE 1 TO PARM-LEN OF
                                             PARM-REC(NUM-PARMS)
                                        MOVE ZEROES TO START-POS
                                   END-IF
                              END-IF
                         ELSE
                              MOVE POS TO START-POS
                              INITIALIZE PARM-TMP
                              INITIALIZE POS2
                         END-IF
                    END-IF
                    IF   START-POS NOT = ZEROS
                     AND POS + 1 <= LENGTH OF PARM-WRK
                         ADD  1 TO POS2
                         MOVE PARM-WRK(POS + 1:1) TO PARM-TMP(POS2:1)
                    END-IF
           END-PERFORM.

     ‚* IM BATCH KEINEN HANDLER, WEIL DAS DISPLAY AUCH NICHT
     ‚* ANGEZEIGT WERDEN KANN
           IF       X-BATCH NOT = SPACES
                    MOVE ZEROES TO HANDLER-ERSTELLEN
           ELSE
                    MOVE 1      TO HANDLER-ERSTELLEN
           END-IF.

     ‚* HANDLER ERSTELLEN
           IF       HANDLER-ERSTELLEN = 1
                    INITIALIZE FC-REDEF
                    SET      CH-PTR     TO ENTRY "CFP9111"
                    SET      TOKEN-PTR  TO ADDRESS OF TOKEN-AREA
                    CALL     "CEEHDLR" USING CH-PTR
                                             TOKEN-PTR
                                             FC
                    END-CALL
           END-IF.

     ‚* NUN JE NACH ANZAHL PARAMETER ANDEREN CALL TYPEN VERWENDEN
           EVALUATE NUM-PARMS
                 WHEN ZERO
                     CALL X-PGMNAM
                     END-CALL
                 WHEN 1
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                     END-CALL
                 WHEN 2
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                     END-CALL
                 WHEN 3
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                     END-CALL
                 WHEN 4
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                     END-CALL
                 WHEN 5
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                     END-CALL
                 WHEN 6
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                     END-CALL
                 WHEN 7
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                     END-CALL
                 WHEN 8
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                     END-CALL
                 WHEN 9
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                     END-CALL
                 WHEN 10
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                     END-CALL
                 WHEN 11
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                     END-CALL
                 WHEN 12
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                     END-CALL
                 WHEN 13
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                     END-CALL
                 WHEN 14
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                     END-CALL
                 WHEN 15
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                     END-CALL
                 WHEN 16
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                     END-CALL
                 WHEN 17
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                     END-CALL
                 WHEN 18
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                     END-CALL
                 WHEN 19
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                     END-CALL
                 WHEN 20
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                     END-CALL
                 WHEN 21
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                     END-CALL
                 WHEN 22
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                     END-CALL
                 WHEN 23
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                     END-CALL
                 WHEN 24
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                     END-CALL
                 WHEN 25
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                     END-CALL
                 WHEN 26
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                     END-CALL
                 WHEN 27
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                     END-CALL
                 WHEN 28
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                     END-CALL
                 WHEN 29
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                     END-CALL
                 WHEN 30
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                     END-CALL
                 WHEN 31
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                     END-CALL
                 WHEN 32
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                     END-CALL
                 WHEN 33
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                                         PARM-DATA(33)(1:PARM-LEN(33))
                     END-CALL
                 WHEN 34
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                                         PARM-DATA(33)(1:PARM-LEN(33))
                                         PARM-DATA(34)(1:PARM-LEN(34))
                     END-CALL
                 WHEN 35
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                                         PARM-DATA(33)(1:PARM-LEN(33))
                                         PARM-DATA(34)(1:PARM-LEN(34))
                                         PARM-DATA(35)(1:PARM-LEN(35))
                     END-CALL
                 WHEN 36
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                                         PARM-DATA(33)(1:PARM-LEN(33))
                                         PARM-DATA(34)(1:PARM-LEN(34))
                                         PARM-DATA(35)(1:PARM-LEN(35))
                                         PARM-DATA(36)(1:PARM-LEN(36))
                     END-CALL
                 WHEN 37
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                                         PARM-DATA(33)(1:PARM-LEN(33))
                                         PARM-DATA(34)(1:PARM-LEN(34))
                                         PARM-DATA(35)(1:PARM-LEN(35))
                                         PARM-DATA(36)(1:PARM-LEN(36))
                                         PARM-DATA(37)(1:PARM-LEN(37))
                     END-CALL
                 WHEN 38
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                                         PARM-DATA(33)(1:PARM-LEN(33))
                                         PARM-DATA(34)(1:PARM-LEN(34))
                                         PARM-DATA(35)(1:PARM-LEN(35))
                                         PARM-DATA(36)(1:PARM-LEN(36))
                                         PARM-DATA(37)(1:PARM-LEN(37))
                                         PARM-DATA(38)(1:PARM-LEN(38))
                     END-CALL
                 WHEN 39
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                                         PARM-DATA(33)(1:PARM-LEN(33))
                                         PARM-DATA(34)(1:PARM-LEN(34))
                                         PARM-DATA(35)(1:PARM-LEN(35))
                                         PARM-DATA(36)(1:PARM-LEN(36))
                                         PARM-DATA(37)(1:PARM-LEN(37))
                                         PARM-DATA(38)(1:PARM-LEN(38))
                                         PARM-DATA(39)(1:PARM-LEN(39))
                     END-CALL
                 WHEN 40
                     CALL X-PGMNAM USING PARM-DATA(1)(1:PARM-LEN(1))
                                         PARM-DATA(2)(1:PARM-LEN(2))
                                         PARM-DATA(3)(1:PARM-LEN(3))
                                         PARM-DATA(4)(1:PARM-LEN(4))
                                         PARM-DATA(5)(1:PARM-LEN(5))
                                         PARM-DATA(6)(1:PARM-LEN(6))
                                         PARM-DATA(7)(1:PARM-LEN(7))
                                         PARM-DATA(8)(1:PARM-LEN(8))
                                         PARM-DATA(9)(1:PARM-LEN(9))
                                         PARM-DATA(10)(1:PARM-LEN(10))
                                         PARM-DATA(11)(1:PARM-LEN(11))
                                         PARM-DATA(12)(1:PARM-LEN(12))
                                         PARM-DATA(13)(1:PARM-LEN(13))
                                         PARM-DATA(14)(1:PARM-LEN(14))
                                         PARM-DATA(15)(1:PARM-LEN(15))
                                         PARM-DATA(16)(1:PARM-LEN(16))
                                         PARM-DATA(17)(1:PARM-LEN(17))
                                         PARM-DATA(18)(1:PARM-LEN(18))
                                         PARM-DATA(19)(1:PARM-LEN(19))
                                         PARM-DATA(20)(1:PARM-LEN(20))
                                         PARM-DATA(21)(1:PARM-LEN(21))
                                         PARM-DATA(22)(1:PARM-LEN(22))
                                         PARM-DATA(23)(1:PARM-LEN(23))
                                         PARM-DATA(24)(1:PARM-LEN(24))
                                         PARM-DATA(25)(1:PARM-LEN(25))
                                         PARM-DATA(26)(1:PARM-LEN(26))
                                         PARM-DATA(27)(1:PARM-LEN(27))
                                         PARM-DATA(28)(1:PARM-LEN(28))
                                         PARM-DATA(29)(1:PARM-LEN(29))
                                         PARM-DATA(30)(1:PARM-LEN(30))
                                         PARM-DATA(31)(1:PARM-LEN(31))
                                         PARM-DATA(32)(1:PARM-LEN(32))
                                         PARM-DATA(33)(1:PARM-LEN(33))
                                         PARM-DATA(34)(1:PARM-LEN(34))
                                         PARM-DATA(35)(1:PARM-LEN(35))
                                         PARM-DATA(36)(1:PARM-LEN(36))
                                         PARM-DATA(37)(1:PARM-LEN(37))
                                         PARM-DATA(38)(1:PARM-LEN(38))
                                         PARM-DATA(39)(1:PARM-LEN(39))
                                         PARM-DATA(40)(1:PARM-LEN(40))
                     END-CALL
           END-EVALUATE.

     ‚* HANDLER LÖSCHEN
           IF       HANDLER-ERSTELLEN = 1
                    CALL     "CEEHDLU" USING CH-PTR
                                             FC
                    END-CALL
           END-IF.

     ‚* WAR IN DEN PARAMETERN EIN F3/F12 ENTHALTEN? DANN HIER WIEDER
     ‚* ZURÜCKGEBEN
           IF       F3-POS > ZEROES
                    MOVE PARM-DATA OF PARM-REC(F3-POS) TO X-F3
           END-IF.
           IF       F12-POS > ZEROES
                    MOVE PARM-DATA OF PARM-REC(F12-POS) TO X-F12
           END-IF.

       ENDE.
           GOBACK.
      /
