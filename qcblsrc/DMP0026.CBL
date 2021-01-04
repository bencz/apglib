       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              DMP0026.
     ‚*
     ˆ*****************************************************************
     ˆ* WENN DER SET UM EINEN BEFEHL ERWEITERT WIRD MUSS UNBEDINGT    *
     ˆ* AUCH DAS DMP0024 ANGEPASST WERDEN, DAMIT DIE UNDO-FUNKTION    *
     ˆ* KLAPPT. (IMMER DAS GEGENTEIL AUSFÜHREN)                       *
     ˆ*                                                               *
     Œ* BEISPIELE                                                     *ˆ
     ˆ* - BEFEHL I(NSERT) -> BEI DER UNDOFUNKTION MUSS                *
     ˆ*                      D(ELETE) AUSGEFÜHRT WERDEN.              *
     ˆ* - BEFEHL L(EFT)   -> BEI DER UNDOFUNKTION MUSS                *
     ˆ*                      R(IGHT) AUSGEFÜHRT WERDEN.               *
     ˆ*                                                               *
     Œ* SCHREIBEN DER LOGGINGDATEI:                                   *ˆ
     ˆ*   BEVOR EINE ZEILE ANGEÄNDERT WIRD MUSS DIE SECTION           *
     ˆ*                     ŒWRITE-PSETLOGˆ                           *
     ˆ*   AUFGERUFEN WERDEN. DIESE ERMITTELT DEN BEFEHL AUTOMATISCH   *
     ˆ*   UND FÜLLT DIE DATENBANK KORREKT.                            *
     ˆ*                                                               *
     ˆ* DAS REDO-PROGRAMM MUSS NICHT ANGEPASST WERDEN                 *
     ˆ*****************************************************************
     ‚*
            COPY COPYRIGHTC OF QCPYSRC.
     ‚*
     ‚*****************************************************************
     ‚* LAGERFÜHRUNGSSYSTEM  LFS 400                                  *
     ‚*---------------------------------------------------------------*
     ‚* COPYRIGHT BY  :  EHRHARDT + PARTNER GMBH & CO. KG             *
     ‚*                  SOFTWARE-SYSTEME FÜR WAREHOUSE-LOGISTIK      *
     ‚*                  56154 BOPPARD-BUCHHOLZ                       *
     ‚*                  +49 (0) 67 42 / 87 27 0                      *
     ‚*                  INFO@EHRHARDT-PARTNER.COM                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :           A. MUTTSCHELLER                     *
     ‚*                                                               *
     ‚* ERSTELLT AM   :           OKTOBER 2007                        *
     ‚*                                                               *
     ‚* PROBLEM       :           ERSATZ FÜR DEN IBM SEU              *
     ‚*                           USERBEFEHLE                         *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON            GRUND DER ÄNDERUNG                  *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
     ‚*-   PSETLOG - LOGDATEI FÜR UNDO/REDO
       01 PSETLOG-P-X.
         02 DB-INFO-DESC          PIC X(20) VALUE "PSETLOG-DP".
         02 FILLER                PIC X(12).
         02 PSETLOG-DP            USAGE POINTER.
            COPY PSETLOG    OF QDBCPY.
         02 PSETLOG-P.
            COPY DDS-ALL-FORMATS OF PSETLOG.

       01  PGM-WRK                        PIC X(10) VALUE "DMP0026   ".

           COPY COPYRIGHT OF QCPYSRC.

     ‚*  WORKBEREICH STANDARDFELDER DIALOGPROGRAMME
           COPY WRKSTDBS OF QCPYSRC.

     ‚*  WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF QCPYSRC.

     ‚*  WORKBEREICH STANDARDFELDER ALLE PROGRAMME
           COPY WRKSTDFLD OF QCPYSRC.

       01  SOURCE-POS                   PIC 9(5).
       01  SOURCE-MAX                   PIC 9(5).
       01  SOURCE-TAB.
        05  SOURCE-REC.
         10  AUSWAHL                    PIC X(7).
         10  SRCSEQ                     PIC 9(4)V9(2).
         10  SRCSEQ-RED REDEFINES SRCSEQ PIC 9(6).
         10  SRCDAT                     PIC X(6).
         10  SRCDTA                     PIC X(150).
         10  ZEILE-NEU                  PIC 9(1).

       01  SOURCE-SAV                   LIKE SOURCE-TAB.
     ‚* TABELLE MIT USERAUSWAHLEN
       01  USRCMD-TAB.
        05 USRCMD-REC OCCURS 50000 TIMES.
         10 AUSW-TYP                    PIC 9(3).
         10 SEQ-FMT                     PIC X(7).
         10 SEQ-ACT                     PIC X(7).
       01  USRCMD-LEN                   PIC 9(5).

     ‚* VARIABLEN ZUM KOPIEREN VON ZEILEN
       01  ZEILE-COPY                   LIKE SRCDTA OF SOURCE-REC.
       01  BEREICH-TAB.
        05 BER-ANF.
         10  AUSWAHL                    PIC X(7).
         10  SRCSEQ                     PIC 9(4)V9(2).
         10  SRCSEQ-RED REDEFINES SRCSEQ PIC 9(6).
         10  SRCDAT                     PIC X(6).
         10  SRCDTA                     PIC X(150).
         10  ZEILE-NEU                  PIC 9(1).
        05 BER-END.
         10  AUSWAHL                    PIC X(7).
         10  SRCSEQ                     PIC 9(4)V9(2).
         10  SRCSEQ-RED REDEFINES SRCSEQ PIC 9(6).
         10  SRCDAT                     PIC X(6).
         10  SRCDTA                     PIC X(150).
         10  ZEILE-NEU                  PIC 9(1).

       01  ZEILE                        PIC 9(5) COMP.
       01  SEQ-NEW                      PIC 9(6) COMP.
       01  UNDO-COUNT                   PIC 9(6).
       01  SEQ-VAL                      PIC 9(3) COMP.
       01  LEN                          PIC 9(5) COMP.
       01  LEN2                         PIC 9(5) COMP.
       01  IDX                          PIC 9(5) COMP.
       01  IDX2                         PIC 9(5) COMP.
       01  POS                          PIC 9(7) COMP.
       01  POS2                         PIC 9(7) COMP.
       01  POS2-SAV                     PIC 9(7) COMP.

       01  SOURCE-CHANGED               PIC X(1).
       01  SOURCE-CHANGED-SAVE          PIC X(1).
       01  SOURCE-CHANGED-LINUX         PIC X(1).

       01  OPEN-CLOSE                   PIC X(1).
       01  ROW-COUNT                    PIC 9(2).
       01  SRCDTA-WRK                   PIC X(150).
       01  SRCSEQ-ANF                   PIC X(7).
       01  SRCSEQ-END                   PIC X(7).
       01  AKTION-WRK                   PIC X(1).
       01  BS-START                     PIC S9(5) COMP.
       01  HIGHLIGHT-MODE               PIC 9(1) COMP.
       01  JA-NEIN                      PIC 9(1).
       01  SORT-AUSWAHL                 USAGE POINTER.
       01  MAP-KEY-SAV                  LIKE MAP-KEY OF LGMAPDTA-REC.
       01  BLOCK-ID                     LIKE LFD-SLID-NR.
       01  AUSWAHL-WRK                  PIC X(7).
       01  LFDN-WRK                     LIKE LFD-SLID-NR.
       01  BLOCK-ID-WRK                 LIKE LFD-SLID-NR.
       01  AUSWAHL-SAV                  PIC X(7).
       01  LFDN-SAV                     LIKE LFD-SLID-NR.
       01  BLOCK-ID-SAV                 LIKE LFD-SLID-NR.
       01  SRCSEQ-INS                   PIC 9(4)V9(5).

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-MBR                        PIC X(10).
       01  X-FILE                       PIC X(10).
       01  X-LIB                        PIC X(10).
       01  X-POINTER                    USAGE POINTER.
       01  X-USRCMD-TAB                 LIKE USRCMD-TAB.
       01  X-SOURCE-MAX                 LIKE SOURCE-MAX.
       01  X-DSP-CUR-X                  LIKE DSP-CUR-X.
       01  X-DSP-CUR-Y                  LIKE DSP-CUR-Y.
       01  X-UNDO                       PIC X(1).
       01  X-BS-START                   PIC S9(5) COMP.
       01  X-RETCODE                    PIC X(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-MBR
                                X-FILE
                                X-LIB
                                X-POINTER
                                X-USRCMD-TAB
                                X-SOURCE-MAX
                                X-DSP-CUR-X
                                X-DSP-CUR-Y
                                X-UNDO
                                X-BS-START
                                X-RETCODE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* Standard INIT-Routine aufrufen
           PERFORM  PGM-INIT.

     ‚* ÖFFNEN BILDSCHIRM UND ABFANGEN FEHLER BEI ANDERER SPRACHE
           MOVE     "DMP0026"   TO PGM-WRK.

           INITIALIZE IDX2.
           INITIALIZE LEN2.
           INITIALIZE SRCSEQ-ANF.
           INITIALIZE SRCSEQ-END.
           INITIALIZE LFDN-WRK.
           INITIALIZE SRCSEQ-INS.
           INITIALIZE BLOCK-ID-WRK.
           INITIALIZE POS2-SAV.
           INITIALIZE X-RETCODE.
           INITIALIZE SOURCE-REC.
           INITIALIZE UNDO-COUNT.
           INITIALIZE AUSWAHL-WRK.
           INITIALIZE SOURCE-CHANGED.
           SET      SORT-AUSWAHL TO NULL.
           IF       X-MBR  = SPACES
            OR      X-FILE = SPACES
            OR      X-LIB  = SPACES
                    INITIALIZE BEREICH-TAB
                    INITIALIZE SOURCE-TAB
                    INITIALIZE USRCMD-TAB
                    INITIALIZE USRCMD-LEN
                    GO TO ENDE
           END-IF.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    MOVE "IO"    TO DB-OPENMODE
                         SET DB-PTR TO ADDRESS OF PSETLOG-P-X
                         PERFORM  DB-OPEN
                    INITIALIZE BEREICH-TAB
           END-IF.

           PERFORM  GET-TIME.

           INITIALIZE USRCMD-TAB.
           MOVE     1 TO USRCMD-LEN.
           MOVE     X-USRCMD-TAB TO USRCMD-TAB.

     ‚* PRIORITÄT DER AUSWAHLEN FESTLEGEN
           PERFORM  GET-AUSWAHL-PRIO.

     ‚* AUSWAHLEN AUSFÜHREN
           INITIALIZE LGMAPDTA-REC.
           SET      MAP-POINTER OF LGMAPDTA-REC TO X-POINTER.
           MOVE     1 TO USRCMD-LEN.

     ‚* VARIABLE ZEILE-NEU IN LGMAPDTA INITIALISIEREN
           PERFORM  INITIALIZE-ZEILE-NEU.

           PERFORM  UNTIL SEQ-ACT OF USRCMD-REC(USRCMD-LEN) = SPACES
                    PERFORM USER-BEFEHLE
                    ADD 1 TO USRCMD-LEN
           END-PERFORM.

     ‚* AM ENDE GENERELL DIE SEQ-NR NEU AUFBAUEN
           PERFORM  RECREATE-SEQ.

     ‚* WENN EINE AUSWAHL GETROFFEN WURDE, DANN ANZAHL
     ‚* GEÄNDERTER SÄTZE IN PSETLOG SCHREIBEN
           IF       X-UNDO = SPACES
                    PERFORM WRITE-ANZAHL-POSITIONEN
           END-IF.

           IF       X-RETCODE = "1"
                    INITIALIZE BEREICH-TAB
           ELSE
                    IF  X-UNDO = SPACES
                         MOVE "D" TO AKTION-WRK
                         MOVE 21  TO ROW-COUNT
                         INITIALIZE  BS-START
                         CALL "DMP0025" USING X-MBR
                                              X-FILE
                                              X-LIB
                                              MAP-POINTER
                                                  OF LGMAPDTA-REC
                                              SOURCE-MAX
                                              X-DSP-CUR-X
                                              X-DSP-CUR-Y
                                              AKTION-WRK
                                              BS-START
                                              ROW-COUNT
                                              RET-CODE
                         END-CALL
                    END-IF
           END-IF.

       ENDE.
     ‚* Standard EXIT-Routine aufrufen
           PERFORM  PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* USERBEFEHLE AUSFÜHREN
     ‚*--------------------------------------------------------------
       USER-BEFEHLE SECTION.
       USR-BEF-00.

           INITIALIZE RET-CODE.
           INITIALIZE X-RETCODE.

     ‚* WIE OFT SOLL DER BEFEHL AUSGEFÜHRT WERDEN?
           INITIALIZE LEN.
           INSPECT  SEQ-ACT OF USRCMD-REC(USRCMD-LEN) TALLYING LEN
                    FOR CHARACTERS BEFORE INITIAL SPACES.
           SUBTRACT 3 FROM LEN.
           IF       LEN > ZEROS
            AND     SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(4:LEN) NUMERIC
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(4:LEN) TO LEN
           ELSE
                    MOVE 1 TO LEN
           END-IF.

           PERFORM  LEN TIMES
     ‚*             DREISTELLIGE BEFEHLE
                    EVALUATE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:3)
     ‚*               FARBEN
                      WHEN "ATW"
                      WHEN "ATG"
                      WHEN "ATR"
                      WHEN "ATB"
                      WHEN "AT-" PERFORM SETZEN-FARBE
     ‚*               KOMMENTAR
                      WHEN "KK-" PERFORM DELETE-COMMENT
                                 GO TO USR-BEF-90
                    END-EVALUATE
           END-PERFORM.

     ‚* WIE OFT SOLL DER BEFEHL AUSGEFÜHRT WERDEN?
           INITIALIZE LEN.
           INSPECT  SEQ-ACT OF USRCMD-REC(USRCMD-LEN) TALLYING LEN
                    FOR CHARACTERS BEFORE INITIAL SPACES.
           SUBTRACT 2 FROM LEN.
           IF       LEN > ZEROS
            AND     SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(3:LEN) NUMERIC
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(3:LEN) TO LEN
           ELSE
                    MOVE 1 TO LEN
           END-IF.

           PERFORM  LEN TIMES
     ‚*             ZWEISTELLIGE BEFEHLE
                    EVALUATE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:2)
     ‚*               KOMMENTAR
                      WHEN "KK"  PERFORM COMMENT-LINE
                                 GO TO USR-BEF-90
                      WHEN "K-"  PERFORM DELETE-COMMENT
                                 GO TO USR-BEF-90
     ‚*               VERDOPPELN
                      WHEN "RP"  PERFORM REPEAT-LINE
     ‚*               LÖSCHEN
                      WHEN "DD"  PERFORM DELETE-LINE
                                 GO TO USR-BEF-90
     ‚*               BEREICH VERSCHIEBEN
                      WHEN "MM"
     ‚*               BEREICH KOPIEREN
                      WHEN "CC"  PERFORM COPY-LINE
                                 MOVE "1" TO X-RETCODE
                                 GO TO USR-BEF-90
     ‚*               NACH LINKS VERSCHIEBEN
                      WHEN "LL"
     ‚*               NACH RECHTS VERSCHIEBEN
                      WHEN "RR"  PERFORM STRING-VERSCHIEBEN
                                 IF   SRCDTA OF BER-END = SPACES
                                      INITIALIZE LEN2
                                      GO TO USR-BEF-90
                                 END-IF
                    END-EVALUATE
           END-PERFORM.

     ‚* WIE OFT SOLL DER BEFEHL AUSGEFÜHRT WERDEN?
           INITIALIZE LEN.
           INSPECT  SEQ-ACT OF USRCMD-REC(USRCMD-LEN) TALLYING LEN
                    FOR CHARACTERS BEFORE INITIAL SPACES.
           SUBTRACT 1 FROM LEN.
           IF       LEN > ZEROS
            AND     SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(2:LEN) NUMERIC
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(2:LEN) TO LEN
           ELSE
                    MOVE 1 TO LEN
           END-IF.

           PERFORM  LEN TIMES
     ‚*             EINSTELLIGE BEFEHLE
                    EVALUATE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:1)
     ‚*               KOMMENTAR
                      WHEN "K"   PERFORM COMMENT-LINE
     ‚*               EINFÜGEN
                      WHEN "I"   PERFORM INSERT-LINE
     ‚*               LÖSCHEN
                      WHEN "D"   PERFORM DELETE-LINE
     ‚*               VERSCHIEBEN
                      WHEN "M"
     ‚*               KOPIEREN
                      WHEN "C"   PERFORM COPY-LINE
                                 MOVE "1" TO X-RETCODE
     ‚*               EINFÜGEN BEFORE/AFTER
                      WHEN "B"
                      WHEN "A" PERFORM INSERT-COPY-LINE
                               INITIALIZE X-RETCODE
     ‚*               NACH LINKS VERSCHIEBEN
                      WHEN "L"
     ‚*               NACH RECHTS VERSCHIEBEN
                      WHEN "R"   IF   SEQ-ACT OF
                                      USRCMD-REC(USRCMD-LEN)(2:1)
                                      IS NUMERIC
                                  OR  SEQ-ACT OF
                                      USRCMD-REC(USRCMD-LEN)(2:1)
                                      = SPACES
                                      PERFORM STRING-VERSCHIEBEN
                                 END-IF
     ‚*               + -> EINE ZEILE NACH UNTEN GEHEN
                      WHEN "+"   ADD 1 TO X-BS-START
     ‚*               - -> EINE ZEILE NACH OBEN GEHEN
                      WHEN "-"   SUBTRACT 1 FROM X-BS-START
                    END-EVALUATE
           END-PERFORM.

       USR-BEF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE EINFÜGEN
     ‚*--------------------------------------------------------------
       INSERT-LINE SECTION.
       INS-LIN-00.

           IF       X-SOURCE-MAX = 50000
                    GO TO INS-LIN-90
           END-IF.

           INITIALIZE SOURCE-REC.
           INITIALIZE RET-CODE.
           ADD      1 TO X-SOURCE-MAX.
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN)(1:4) TO
                    SRCSEQ OF SOURCE-REC(1:4).
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN)(6:2) TO
                    SRCSEQ OF SOURCE-REC(5:2).
     ‚*    WENN EINE KOPIERTE ZEILE EINGEFÜGT WERDEN SOLL
     ‚*    DANN WERT AUS VARIABLE NEHMEN
           IF       SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:1) = "A"
            OR      SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:1) = "B"
                    MOVE ZEILE-COPY TO SRCDTA OF SOURCE-REC
           ELSE
                    MOVE SPACES     TO SRCDTA OF SOURCE-REC
     ‚*             NEUE BLOCK-ID ERMITELN
                    IF   X-UNDO = SPACES
                     AND UNDO-COUNT < 1
                         MOVE "SETBLKID" TO SL-ID
                         CALL "LGPGM020" USING SL-ID
                                               BLOCK-ID
                         END-CALL
                    END-IF
           END-IF.
           IF       X-UNDO NOT = "1"
                    MOVE DATE-8(3:6) TO SRCDAT OF SOURCE-REC
           END-IF.
           MOVE     1           TO ZEILE-NEU  OF SOURCE-REC.
           MOVE     SOURCE-REC  TO MAP-RECORD OF LGMAPDTA-REC.
           MOVE     "I"  TO AUSWAHL OF SOURCE-REC.

     ‚*    ÄNDERUNGEN LOGGEN
           PERFORM  WRITE-PSETLOG.

     ‚*    NÄCHSTE FREIE SEQ-NR ERMITTELN
     ‚*    WENN EINE KOPIERTE ZEILE VOR DER AUSGEWÄHLTEN
     ‚*    EINGEFÜGT WERDEN SOLL, SRCSEQ UM 1 KLEINER
     ‚*    MACHEN
           IF       SEQ-ACT OF USRCMD-REC(USRCMD-LEN) = "B"
                    SUBTRACT 1 FROM SRCSEQ OF SOURCE-REC
           END-IF.
           MOVE     ZEROS  TO SRCSEQ-INS(1:4).
           IF       SRCSEQ OF SOURCE-REC NOT NUMERIC
                    MOVE ZEROS TO SRCSEQ OF SOURCE-REC
           END-IF.
           ADD      SRCSEQ OF SOURCE-REC TO SRCSEQ-INS.

           PERFORM  UNTIL RET-CODE NOT = SPACES
                    ADD  0,00001 TO SRCSEQ-INS
                    MOVE MAP-READ TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SRCSEQ-INS(1:4) TO
                         MAP-KEY OF LGMAPDTA-REC(1:4)
                    MOVE "."             TO
                         MAP-KEY OF LGMAPDTA-REC(5:1)
                    MOVE SRCSEQ-INS(5:5) TO
                         MAP-KEY OF LGMAPDTA-REC(6:5)
                    PERFORM CALL-LGMAPDTA
           END-PERFORM.

     ‚*    DATEN IN LGMAPDTA SCHEIBEN
           MOVE     SOURCE-REC    TO MAP-RECORD OF LGMAPDTA-REC.
           MOVE     MAP-WRITE TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

     ‚*    NUN DIE CURSERPOSITION ERMITTELN
     ‚*    ABER NUR BEIM 1. MAL
           IF       SOURCE-CHANGED = SPACES
     ‚*             EINE ZEILE NACH UNTEN GEHEN
     ‚*             ABER NUR WENN ES NICHT DIE LETZTE ZEILE IST
                    IF   X-DSP-CUR-Y < 25
                         ADD  1 TO X-DSP-CUR-Y
                    END-IF
     ‚*             NUN ERMITTELN WO DAS 1. ZEICHEN DES VORHERIGEN
     ‚*             SATZES IST
                    MOVE MAP-READ TO MAP-ACTION   OF LGMAPDTA-REC
                    PERFORM  CALL-LGMAPDTA
                    SUBTRACT 1 FROM MAP-LFDN OF LGMAPDTA-REC
                    MOVE MAP-READ-LFDN TO MAP-ACTION   OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
     ‚*             DER SATZ IST ERMITTELT, JETZT DAS 1. ZEICHEN SUCHEN
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC
                    INITIALIZE LEN
                    INSPECT SRCDTA OF SOURCE-REC TALLYING LEN FOR
                            LEADING SPACES
                    IF   LEN = LENGTH OF SRCDTA OF SOURCE-REC
                         MOVE ZERO TO LEN
                    END-IF
                    IF   LEN > 70
                         MOVE 9 TO LEN
                    END-IF
                    COMPUTE X-DSP-CUR-X = LEN
                                  + 3
                                  + LENGTH OF SRCSEQ OF SOURCE-REC
           END-IF.

           MOVE     "*" TO SOURCE-CHANGED.
           IF       SEQ-ACT OF USRCMD-REC(USRCMD-LEN) NOT = "A"
            AND     SEQ-ACT OF USRCMD-REC(USRCMD-LEN) NOT = "B"
                    MOVE "I" TO X-RETCODE
           END-IF.

       INS-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE VERDOPPELN
     ‚*--------------------------------------------------------------
       REPEAT-LINE SECTION.
       REP-LIN-00.

           IF       X-SOURCE-MAX = 50000
                    GO TO REP-LIN-90
           END-IF.

     ‚* NEUE BLOCK-ID ERMITELN
           IF       X-UNDO = SPACES
                    MOVE "SETBLKID" TO SL-ID
                    CALL "LGPGM020" USING SL-ID
                                          BLOCK-ID
                    END-CALL
           END-IF.

     ‚* ZEILE LESEN FÜR LOGGING
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY OF LGMAPDTA-REC.
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.
           MOVE     "RP" TO AUSWAHL OF SOURCE-REC.
           PERFORM  WRITE-PSETLOG.

           INITIALIZE SOURCE-REC.
           INITIALIZE RET-CODE.
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN)(1:4) TO
                    SRCSEQ     OF SOURCE-REC(1:4).
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN)(6:2) TO
                    SRCSEQ     OF SOURCE-REC(5:2).
           MOVE     SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC.

     ‚*    NÄCHSTE FREIE SEQ-NR ERMITTELN
           MOVE     ZEROS  TO SRCSEQ-INS(1:4).
           ADD      SRCSEQ OF SOURCE-REC TO SRCSEQ-INS.

           PERFORM  UNTIL RET-CODE NOT = SPACES
                    ADD  0,00001 TO SRCSEQ-INS
                    MOVE MAP-READ TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SRCSEQ-INS(1:4) TO
                         MAP-KEY OF LGMAPDTA-REC(1:4)
                    MOVE "."             TO
                         MAP-KEY OF LGMAPDTA-REC(5:1)
                    MOVE SRCSEQ-INS(5:5) TO
                         MAP-KEY OF LGMAPDTA-REC(6:5)
                    PERFORM CALL-LGMAPDTA
           END-PERFORM.

     ‚* ZEILE LESEN UM INHALT ZU ERHALTEN
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY OF LGMAPDTA-REC.
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     SRCSEQ-INS(1:4) TO
                    MAP-KEY OF LGMAPDTA-REC(1:4)
           MOVE     "."              TO
                    MAP-KEY OF LGMAPDTA-REC(5:1)
           MOVE     SRCSEQ-INS(5:5) TO
                    MAP-KEY OF LGMAPDTA-REC(6:5)

           MOVE     MAP-RECORD  OF LGMAPDTA-REC TO SOURCE-REC.
           IF       X-UNDO NOT = "1"
                    MOVE DATE-8(3:6) TO SRCDAT OF SOURCE-REC
           END-IF.
           MOVE     1           TO ZEILE-NEU    OF SOURCE-REC.
           MOVE     SOURCE-REC  TO MAP-RECORD   OF LGMAPDTA-REC.

     ‚*    DATEN IN LGMAPDTA SCHEIBEN
           MOVE     MAP-WRITE TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           ADD      1 TO X-SOURCE-MAX.

     ‚*    NUN DIE CURSERPOSITION ERMITTELN
     ‚*    ABER NUR BEIM 1. MAL
           IF       SOURCE-CHANGED = SPACES
     ‚*             EINE ZEILE NACH UNTEN GEHEN
                    IF   X-DSP-CUR-Y < 25
                         ADD  1 TO X-DSP-CUR-Y
                    END-IF
     ‚*             NUN ERMITTELN WO DAS 1. ZEICHEN DES VORHERIGEN
     ‚*             SATZES IST
                    MOVE MAP-READ TO MAP-ACTION OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
     ‚*             DER SATZ IST ERMITTELT, JETZT DAS 1. ZEICHEN SUCHEN
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC
                    INITIALIZE LEN
                    INSPECT SRCDTA OF SOURCE-REC TALLYING LEN FOR
                            LEADING SPACES
                    COMPUTE X-DSP-CUR-X = LEN
                                     + 3
                                     + LENGTH OF SRCSEQ OF SOURCE-REC
           END-IF.

           MOVE     "*" TO SOURCE-CHANGED.

       REP-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-LINE SECTION.
       DEL-LIN-00.

     ‚* ZEILE AUS LGMAPDTA LESEN UND ZWISCHENSPEICHERN
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.

     ‚* SOLL NUR EINE ODER MEHRERE ZEILEN KOPIERT WERDEN?
           IF       SRCSEQ OF BER-ANF = ZEROS
                    MOVE SOURCE-REC TO BER-ANF OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-ANF
                    INITIALIZE POS2
                    MOVE MAP-LFDN   OF LGMAPDTA-REC TO POS2
                    IF   SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:2) = "DD"
                         MOVE "1" TO X-RETCODE
                         GO TO DEL-LIN-90
                    END-IF
           ELSE
                    MOVE SOURCE-REC TO BER-END OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-END
                    IF   SRCSEQ OF BER-ANF = SRCSEQ OF BER-END
                         GO TO DEL-LIN-90
                    END-IF
                    INITIALIZE X-RETCODE
           END-IF.

     ‚* SIND AUSWAHLEN IDENTISCH?
           IF       AUSWAHL OF BER-ANF NOT = AUSWAHL OF BER-END
            AND     AUSWAHL OF BER-END NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO DEL-LIN-90
           END-IF.

     ‚* NEUE BLOCK-ID ERMITELN
           IF       X-UNDO = SPACES
                    MOVE "SETBLKID" TO SL-ID
                    CALL "LGPGM020" USING SL-ID
                                          BLOCK-ID
                    END-CALL
           END-IF.

       DEL-LIN-20.
           SUBTRACT 1 FROM X-SOURCE-MAX.

     ‚* NÄCHSTEN SATZ LESEN
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     POS2 TO MAP-LFDN OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.

           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.
           MOVE     "D" TO AUSWAHL OF SOURCE-REC.

     ‚*    ÄNDERUNG LOGGEN
           PERFORM  WRITE-PSETLOG.

     ‚*    ZEILE AUS LGMAPDTA LÖSCHEN
           IF       RET-CODE = SPACES
                    MOVE MAP-DELETE TO MAP-ACTION OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
           END-IF.

           IF       SRCSEQ-RED OF SOURCE-REC < SRCSEQ-RED OF BER-END
                    GO TO DEL-LIN-20
           END-IF.

           INITIALIZE BEREICH-TAB.

           MOVE     "*" TO SOURCE-CHANGED.

       DEL-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE EINFÄRBEN
     ‚*--------------------------------------------------------------
       SETZEN-FARBE SECTION.
       SET-COL-00.

     ‚* NEUE BLOCK-ID ERMITELN
           IF       X-UNDO = SPACES
                    MOVE "SETBLKID" TO SL-ID
                    CALL "LGPGM020" USING SL-ID
                                          BLOCK-ID
                    END-CALL
           END-IF.

           INITIALIZE RET-CODE.
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY    OF LGMAPDTA-REC.
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD TO SOURCE-REC.
           PERFORM  WRITE-PSETLOG.

           EVALUATE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:3)
     ‚*             FARBE ENTFERNEN
                    WHEN "AT-" PERFORM VARYING IDX FROM 1 BY 1
                               UNTIL IDX > LENGTH OF SRCDTA
                                           OF SOURCE-REC
                                IF   SRCDTA OF SOURCE-REC(IDX:1)
                                     NOT < X"20"
                                 AND SRCDTA OF SOURCE-REC(IDX:1)
                                     NOT > X"3F"
                                     MOVE SPACE TO
                                          SRCDTA OF SOURCE-REC(IDX:1)
                                END-IF
                               END-PERFORM
     ‚*             WEIß
                    WHEN "ATW" MOVE X"22" TO
                         SRCDTA OF SOURCE-REC(6:1)
     ‚*             GRÜN
                    WHEN "ATG" MOVE " " TO
                         SRCDTA OF SOURCE-REC(6:1)
     ‚*             ROT
                    WHEN "ATR" MOVE X"28" TO
                         SRCDTA OF SOURCE-REC(6:1)
     ‚*             BLAU
                    WHEN "ATB" MOVE X"3A" TO
                         SRCDTA OF SOURCE-REC(6:1)
           END-EVALUATE.
           MOVE     SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC.
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY    OF LGMAPDTA-REC.
           MOVE     MAP-WRITE  TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

       SET-COL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE KOPIEREN
     ‚*--------------------------------------------------------------
       COPY-LINE SECTION.
       CPY-LIN-00.

           INITIALIZE RET-CODE.
           INITIALIZE SOURCE-REC.
           INITIALIZE ZEILE-COPY.

     ‚* ZEILE AUS LGMAPDTA LESEN UND ZWISCHENSPEICHERN
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.

     ‚*    SOLL NUR EINE ODER MEHRERE ZEILEN KOPIERT WERDEN?
           IF       SRCSEQ OF BER-ANF = ZEROS
                    MOVE SOURCE-REC TO BER-ANF OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-ANF
     ‚*             NEUE BLOCK-ID ERMITELN
                    IF   X-UNDO = SPACES
                         MOVE "SETBLKID" TO SL-ID
                         CALL "LGPGM020" USING SL-ID
                                               BLOCK-ID
                         END-CALL
                    END-IF
                    PERFORM WRITE-PSETLOG
           ELSE
                    MOVE SOURCE-REC TO BER-END OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-END
     ‚*             SIND AUSWAHLEN IDENTISCH?
                    IF   AUSWAHL OF BER-ANF NOT = AUSWAHL OF BER-END
                     AND AUSWAHL OF BER-END NOT = SPACES
                         MOVE "1" TO X-RETCODE
                         GO TO CPY-LIN-90
                    END-IF
     ‚*             NUN ALLE ZEILEN IN DEM MM BLOCK LOGGEN
     ‚*             1. SATZ AUS LGMAPDTA LESEN UND AUSGEBEN
                    MOVE MAP-READ TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SRCSEQ  OF BER-ANF(1:4) TO
                         MAP-KEY OF LGMAPDTA-REC(1:4)
                    MOVE "."                    TO
                         MAP-KEY OF LGMAPDTA-REC(5:1)
                    MOVE SRCSEQ  OF BER-ANF(5:2) TO
                         MAP-KEY OF LGMAPDTA-REC(6:2)
                    PERFORM CALL-LGMAPDTA
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO
                         SOURCE-REC
                    INITIALIZE POS2
     ‚*             NUN DIE SÄTZE DURCHLESEN
                    PERFORM UNTIL SRCSEQ-RED OF SOURCE-REC >=
                            SRCSEQ-RED OF BER-END
                            MOVE MAP-READ-NEXT TO MAP-ACTION
                            PERFORM CALL-LGMAPDTA
                            MOVE MAP-RECORD OF LGMAPDTA-REC TO
                                 SOURCE-REC
                            PERFORM WRITE-PSETLOG
                    END-PERFORM
           END-IF.

       CPY-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE EINFÜGEM
     ‚*--------------------------------------------------------------
       INSERT-COPY-LINE SECTION.
       INS-CPY-00.

           INITIALIZE RET-CODE.
           INITIALIZE SOURCE-REC.

     ‚* IST EINE KOPIERTE ZEILE IN DER VARIABLE?
           IF       SRCSEQ OF BER-ANF = ZEROS
                    MOVE "1" TO RET-CODE
                    GO TO INS-CPY-90
           END-IF.

     ‚* WENN DAS A/B ZWISCHEN DEN CC/MM STEHT MUSS
     ‚* AUS DER SECTION GEGANGEN WERDEN, DA SONST EINE
     ‚* ENDLOSSCHLEIFE ENTSTEHT
           MOVE     SRCSEQ OF BER-ANF(1:4) TO SRCSEQ-ANF(1:4).
           MOVE     "."                    TO SRCSEQ-ANF(5:1).
           MOVE     SRCSEQ OF BER-ANF(5:2) TO SRCSEQ-ANF(6:2).
           MOVE     SRCSEQ OF BER-END(1:4) TO SRCSEQ-END(1:4).
           MOVE     "."                    TO SRCSEQ-END(5:1).
           MOVE     SRCSEQ OF BER-END(5:2) TO SRCSEQ-END(6:2).
           IF       SEQ-FMT OF USRCMD-REC(USRCMD-LEN) > SRCSEQ-ANF
            AND     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) < SRCSEQ-END
                    MOVE "W" TO X-RETCODE
                    GO TO INS-CPY-90
           END-IF.

     ‚* 1. SATZ AUS LGMAPDTA LESEN UND AUSGEBEN
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     SRCSEQ  OF BER-ANF(1:4) TO
                    MAP-KEY OF LGMAPDTA-REC(1:4)
           MOVE     "."                    TO
                    MAP-KEY OF LGMAPDTA-REC(5:1)
           MOVE     SRCSEQ  OF BER-ANF(5:2) TO
                    MAP-KEY OF LGMAPDTA-REC(6:2)
           PERFORM  CALL-LGMAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO RET-CODE
                    GO TO INS-CPY-90
           END-IF.
           INITIALIZE POS2.
           MOVE     MAP-LFDN OF LGMAPDTA-REC TO POS2.

       INS-CPY-20.
     ‚* NÄCHSTEN SATZ LESEN
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     POS2 TO MAP-LFDN OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.
           MOVE     MAP-KEY    OF LGMAPDTA-REC TO MAP-KEY-SAV.

     ‚* VARIABLE AUFBAUEN
           INITIALIZE SRCSEQ-INS.
           ADD      SRCSEQ     OF SOURCE-REC   TO SRCSEQ-INS.
     ‚* WENN KEY 5 NACHKOMMASTELLEN HAT, DANN ALLE 5 NEHMEN
           IF       MAP-KEY    OF LGMAPDTA-REC(6:5) IS NUMERIC
                    MOVE MAP-KEY OF LGMAPDTA-REC(6:5) TO
                         SRCSEQ-INS(5:5)
           END-IF.
           MOVE     SRCDTA     OF SOURCE-REC   TO ZEILE-COPY.

           IF       SRCSEQ-INS            <= SRCSEQ OF BER-END
            OR      SRCSEQ-RED OF BER-END  = ZEROS
                    PERFORM INSERT-LINE
     ‚*             WENN DIE EINGEFÜGTE SATZ-NR GRÖßER ALS
     ‚*             DIE KOPIERTE IST, DANN EINEN SATZ
     ‚*             WEITERLESEN
                    IF   SRCSEQ-RED OF SOURCE-REC
                          > SRCSEQ-RED OF BER-ANF
                         ADD 1 TO POS2
                    ELSE
     ‚*                  ANSONSTEN 2 SÄTZE, DA DER NÄCHSTE DER SATZ
     ‚*                  WÄRE, DER ZUVOR GELESEN WURDE
                         ADD 2 TO POS2
                    END-IF
     ‚*             SOLL DIE ZEILE VERSCHOBEN WERDEN?
     ‚*             DANN SATZ LÖSCHEN
                    IF   AUSWAHL OF BER-ANF(1:1) = "M"
                         MOVE MAP-DELETE TO MAP-ACTION OF LGMAPDTA-REC
                         MOVE MAP-KEY-SAV TO MAP-KEY OF LGMAPDTA-REC
                         PERFORM CALL-LGMAPDTA
                         SUBTRACT 1 FROM POS2
                         SUBTRACT 1 FROM X-SOURCE-MAX
                    END-IF
                    IF   SRCSEQ-RED OF BER-END NOT = ZEROS
                         GO TO INS-CPY-20
                    END-IF
           END-IF.

           INITIALIZE BEREICH-TAB.

       INS-CPY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PSETLOG SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-PSETLOG SECTION.
       WRT-SET-00.

           IF       X-UNDO NOT = SPACES
                    GO TO WRT-SET-90
           END-IF.

     ‚* LFDN AUS PSCHLUE ERMITTELN
           MOVE     "SETLOGID" TO SL-ID.
           CALL     "LGPGM020" USING SL-ID
                                     LFD-SLID-NR
           END-CALL.

           PERFORM  GET-TIME.

           INITIALIZE PSETLOGF OF PSETLOG-P.
           MOVE     SEQ-ACT OF USRCMD-REC(USRCMD-LEN) TO AUSWAHL OF
                                                         SOURCE-REC.

     ‚* ZAHLEN IN SPACES UMWANDELN
           PERFORM  WITH TEST BEFORE VARYING IDX FROM 1 BY 1
                    UNTIL IDX = LENGTH OF AUSWAHL OF SOURCE-REC
                    IF   AUSWAHL OF SOURCE-REC(IDX:1) IS NUMERIC
                         MOVE SPACE TO AUSWAHL OF SOURCE-REC(IDX:1)
                    END-IF
           END-PERFORM.

           MOVE     X-LIB       TO SELIB  OF PSETLOG-P.
           MOVE     X-FILE      TO SEFILE OF PSETLOG-P.
           MOVE     X-MBR       TO SEMBR  OF PSETLOG-P.
           MOVE     LFD-SLID-NR TO SELFDN OF PSETLOG-P.
           MOVE     X-UNDO      TO SEUNDO OF PSETLOG-P.
           MOVE     BLOCK-ID    TO SEZUSL OF PSETLOG-P.
     ‚*    WENN EINE ZEILE GELÖSCHT WERDEN SOLL UND DIE
     ‚*    NÄCHSTE AKTION IST AUCH EINE LÖSCHUNG MUSS
     ‚*    GEPRÜFT WERDEN, OB DIE NÄCHSTE ZEILE UNTER DER
     ‚*    AKTUELLEN IST, DA BEIM UNDO DIE SEQ-NR NICHT MEHR STIMMT
           IF       USRCMD-LEN NOT = 1
            AND     SEQ-ACT OF USRCMD-REC(USRCMD-LEN) = "D "
                    IF   SEQ-FMT OF USRCMD-REC(USRCMD-LEN) <
                         SEQ-FMT OF USRCMD-REC(USRCMD-LEN + 1)
                     OR  SEQ-FMT OF USRCMD-REC(USRCMD-LEN) >
                         SEQ-FMT OF USRCMD-REC(USRCMD-LEN - 1)
                         ADD 1 TO IDX2
                         SUBTRACT IDX2 FROM SRCSEQ OF SOURCE-REC
                         MOVE SOURCE-REC TO SEREC OF PSETLOG-P
                         ADD IDX2 TO SRCSEQ OF SOURCE-REC
                    ELSE
                         MOVE SOURCE-REC TO SEREC OF PSETLOG-P
                    END-IF
           ELSE
                    MOVE SOURCE-REC TO SEREC OF PSETLOG-P
           END-IF.

           MOVE     T-USER      TO SEMUSR OF PSETLOG-P.
           MOVE     T-TERM      TO SEMBS  OF PSETLOG-P.
           MOVE     DATE-8      TO SEMDT  OF PSETLOG-P.
           MOVE     TIME-6      TO SEMTI  OF PSETLOG-P.
           MOVE     PGM-WRK     TO SEPGM  OF PSETLOG-P.
           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-WRITE.

           IF       AUSWAHL OF SOURCE-REC = AUSWAHL-WRK
            AND     SEZUSL  OF PSETLOG-P  = BLOCK-ID-WRK
                    ADD  1 TO UNDO-COUNT
           ELSE
                    INITIALIZE AUSWAHL-SAV
                    INITIALIZE LFDN-SAV
                    INITIALIZE BLOCK-ID-SAV
                    MOVE AUSWAHL OF SOURCE-REC TO AUSWAHL-SAV
                    MOVE SELFDN  OF PSETLOG-P  TO LFDN-SAV
                    MOVE SEZUSL  OF PSETLOG-P  TO BLOCK-ID-SAV
                    IF   X-UNDO = SPACES
                     AND UNDO-COUNT NOT = ZEROS
                         PERFORM WRITE-ANZAHL-POSITIONEN
                    END-IF
                    MOVE 1 TO UNDO-COUNT
                    MOVE AUSWAHL-SAV  TO AUSWAHL-WRK
                    MOVE LFDN-SAV     TO LFDN-WRK
                    MOVE BLOCK-ID-SAV TO BLOCK-ID-WRK
           END-IF.

           INITIALIZE AUSWAHL OF SOURCE-REC.

       WRT-SET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUE LAUFENDE NUMMER VERGEBEN
     ‚*--------------------------------------------------------------
       RECREATE-SEQ SECTION.
       REC-SEQ-00.

     ‚* JE NACH SOURCEGRÖßE ANDERE SCHRITTGRÖßE FÜR NUMMERIERUNG
           INITIALIZE SEQ-NEW.
           IF       X-SOURCE-MAX > 9999
                    MOVE 10  TO SEQ-VAL
           ELSE
                    MOVE 100 TO SEQ-VAL
           END-IF.

     ‚* AUF ARRAY AUFSETZTEN
           INITIALIZE SOURCE-POS.

     ‚* LAUFENDE NUMMER NEU ANORDNEN
           PERFORM  WITH TEST BEFORE VARYING SOURCE-POS FROM 1 BY 1
                    UNTIL SOURCE-POS = (X-SOURCE-MAX - 1)
                    INITIALIZE LGMAPDTA-REC
                    INITIALIZE SOURCE-REC
                    MOVE MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SOURCE-POS TO MAP-LFDN   OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC
                    ADD  SEQ-VAL TO SEQ-NEW
                    MOVE SEQ-NEW TO SRCSEQ-RED OF SOURCE-REC
                    INITIALIZE MAP-KEY OF LGMAPDTA-REC
                    MOVE SRCSEQ  OF SOURCE-REC(1:4) TO
                         MAP-KEY OF LGMAPDTA-REC(1:4)
                    MOVE "."                        TO
                         MAP-KEY OF LGMAPDTA-REC(5:1)
                    MOVE SRCSEQ  OF SOURCE-REC(5:2) TO
                         MAP-KEY OF LGMAPDTA-REC(6:2)
                    MOVE SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC
                    MOVE MAP-CHGKEY TO MAP-ACTION OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
           END-PERFORM.

           MOVE     MAP-SORT TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

       REC-SEQ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHLPRIORITÄT ERMITTELN
     ‚*--------------------------------------------------------------
       GET-AUSWAHL-PRIO SECTION.
       GET-PRI-00.

           INITIALIZE IDX.

     ‚* PRIORITÄT DER AUSWAHLEN FESTLEGEN
           SET      MAP-POINTER OF LGMAPDTA-REC TO SORT-AUSWAHL.
           PERFORM  UNTIL SEQ-ACT OF USRCMD-REC(USRCMD-LEN) = SPACES
                    EVALUATE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         WHEN "A"   MOVE 900 TO AUSW-TYP OF
                                                USRCMD-REC(USRCMD-LEN)
                         WHEN "B"   MOVE 900 TO AUSW-TYP OF
                                                USRCMD-REC(USRCMD-LEN)
                         WHEN "D"   MOVE 700 TO AUSW-TYP OF
                                                USRCMD-REC(USRCMD-LEN)
                         WHEN "I"   MOVE 800 TO AUSW-TYP OF
                                                USRCMD-REC(USRCMD-LEN)
                         WHEN "MM"
                         WHEN "CC"  MOVE 050 TO AUSW-TYP OF
                                                USRCMD-REC(USRCMD-LEN)
                         WHEN "M"
                         WHEN "C"   MOVE 100 TO AUSW-TYP OF
                                                USRCMD-REC(USRCMD-LEN)
                         WHEN OTHER MOVE 999 TO AUSW-TYP OF
                                                USRCMD-REC(USRCMD-LEN)
                    END-EVALUATE
     ‚*             DATENSATZ IN SPEICHER SCHREIBEN
                    STRING USRCMD-REC(USRCMD-LEN) DELIMITED BY SIZE
                           USRCMD-LEN             DELIMITED BY SIZE
                    INTO   MAP-KEY OF LGMAPDTA-REC
                    MOVE USRCMD-REC(USRCMD-LEN) TO
                         MAP-RECORD OF LGMAPDTA-REC
                    MOVE MAP-WRITE TO MAP-ACTION OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
                    ADD 1 TO USRCMD-LEN
           END-PERFORM.

     ‚* NUN ARRAY SORTIEREN
           MOVE     MAP-SORT TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

     ‚* TABELLE LEEREN
           INITIALIZE USRCMD-TAB.
           MOVE     1 TO IDX.
           SUBTRACT 1 FROM USRCMD-LEN

     ‚* ARRAY IN TABELLE ZURÜCKSCHREIBEN
           PERFORM  USRCMD-LEN TIMES
     ‚*             DATENSATZ AUS SPEICHER LESEN
                    MOVE IDX TO     MAP-LFDN OF LGMAPDTA-REC
                    SUBTRACT 1 FROM MAP-LFDN OF LGMAPDTA-REC
                    MOVE MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO USRCMD-REC(IDX)
                    ADD 1 TO IDX
           END-PERFORM.

     ‚* ARRAY LEEREN
           MOVE     MAP-CLEAR TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

       GET-PRI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STRING NACH LINKS/RECHTS VERSCHEIBEN
     ‚*--------------------------------------------------------------
       STRING-VERSCHIEBEN SECTION.
       MOV-STR-00.

           INITIALIZE RET-CODE.
           INITIALIZE SRCDTA-WRK.

           ADD      1 TO LEN2.
     ‚* DATENSATZ ERMITTELN
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY    OF LGMAPDTA-REC.
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD TO SOURCE-REC.
     ‚* SOLL NUR EINE ODER MEHRERE ZEILEN KOPIERT WERDEN?
           IF       SRCSEQ OF BER-ANF = ZEROS
                    MOVE SOURCE-REC TO BER-ANF OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-ANF
                    INITIALIZE POS2
                    INITIALIZE POS2-SAV
                    MOVE MAP-LFDN   OF LGMAPDTA-REC TO POS2
                    MOVE POS2 TO POS2-SAV
                    IF   SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:2) = "LL"
                     OR  SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:2) = "RR"
                         MOVE "1" TO X-RETCODE
                         GO TO MOV-STR-90
                    END-IF
           ELSE
                    MOVE SOURCE-REC TO BER-END OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-END
                    IF   SRCSEQ OF BER-ANF = SRCSEQ OF BER-END
                         INITIALIZE BER-END
                    END-IF
                    INITIALIZE X-RETCODE
           END-IF.

     ‚* SIND AUSWAHLEN IDENTISCH?
           IF       AUSWAHL OF BER-ANF NOT = AUSWAHL OF BER-END
            AND     AUSWAHL OF BER-END NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO MOV-STR-90
           END-IF.

     ‚* NEUE BLOCK-ID ERMITELN
           IF       LEN2 = 1
                    IF   X-UNDO = SPACES
                         MOVE "SETBLKID" TO SL-ID
                         CALL "LGPGM020" USING SL-ID
                                               BLOCK-ID
                         END-CALL
                    END-IF
           END-IF.

       MOV-STR-20.
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     POS2 TO MAP-LFDN OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD TO SOURCE-REC.

           PERFORM  WRITE-PSETLOG.

           IF       SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:1) = "L"
     ‚*             NUR VERSCHIEBEN WENN NOCH PLATZ IST
                    IF   SRCDTA OF SOURCE-REC(1:1) NOT = SPACES
                         GO TO MOV-STR-90
                    END-IF
                    PERFORM WITH TEST BEFORE VARYING IDX FROM 1 BY 1
                            UNTIL IDX = LENGTH OF SRCDTA OF SOURCE-REC
                            MOVE SRCDTA OF SOURCE-REC(IDX + 1:1) TO
                                 SRCDTA-WRK(IDX:1)
                    END-PERFORM
                    MOVE SPACE TO SRCDTA-WRK(LENGTH OF SRCDTA-WRK:1)
           ELSE
     ‚*             NUR VERSCHIEBEN WENN NOCH PLATZ IST
                    IF   SRCDTA OF SOURCE-REC(LENGTH OF
                         SRCDTA OF SOURCE-REC:1) NOT = SPACES
                         GO TO MOV-STR-90
                    END-IF
     ‚*             STRING NACH RECHTS VERSCHIEBEN
                    PERFORM WITH TEST BEFORE VARYING IDX FROM 1 BY 1
                            UNTIL IDX = LENGTH OF SRCDTA OF SOURCE-REC
                            MOVE SRCDTA OF SOURCE-REC(IDX:1) TO
                                 SRCDTA-WRK(IDX + 1:1)
                    END-PERFORM
                    MOVE SPACE TO SRCDTA-WRK(1:1)
           END-IF.

           INITIALIZE SRCDTA OF SOURCE-REC.
           MOVE     SRCDTA-WRK  TO SRCDTA OF SOURCE-REC.
           IF       X-UNDO NOT = "1"
                    MOVE DATE-8(3:6) TO SRCDAT OF SOURCE-REC
           END-IF.

     ‚* NUN SATZ WIEDER IN ARRAY SCHREIBEN
           MOVE     MAP-WRITE TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

           IF       SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:2) = "LL"
            OR      SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:2) = "RR"
                    IF   SRCSEQ-RED OF SOURCE-REC <
                         SRCSEQ-RED OF BER-END
                         ADD 1 TO POS2
                         GO TO MOV-STR-20
                    END-IF
           END-IF.
           IF       LEN = LEN2
                    INITIALIZE LEN2
                    INITIALIZE BEREICH-TAB
           ELSE
                    MOVE POS2-SAV TO POS2
           END-IF.

       MOV-STR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINE ZEILE KOMMENTIEREN
     ‚*--------------------------------------------------------------
       COMMENT-LINE SECTION.
       COM-LIN-00.

           INITIALIZE RET-CODE.
           INITIALIZE SRCDTA-WRK.

     ‚* DATENSATZ ERMITTELN
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY    OF LGMAPDTA-REC.
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.
     ‚*    SOLL NUR EINE ODER MEHRERE ZEILEN KOPIERT WERDEN?
           IF       SRCSEQ OF BER-ANF = ZEROS
                    MOVE SOURCE-REC TO BER-ANF OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-ANF
                    INITIALIZE POS2
                    MOVE MAP-LFDN   OF LGMAPDTA-REC TO POS2
                    IF   SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:2) = "KK"
                         MOVE "1" TO X-RETCODE
                         GO TO COM-LIN-90
                    END-IF
           ELSE
                    MOVE SOURCE-REC TO BER-END OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-END
                    IF   SRCSEQ OF BER-ANF = SRCSEQ OF BER-END
                         GO TO COM-LIN-90
                    END-IF
                    INITIALIZE X-RETCODE
           END-IF.

     ‚* SIND AUSWAHLEN IDENTISCH?
           IF       AUSWAHL OF BER-ANF NOT = AUSWAHL OF BER-END
            AND     AUSWAHL OF BER-END NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO COM-LIN-90
           END-IF.

     ‚* NEUE BLOCK-ID ERMITELN
           IF       X-UNDO = SPACES
                    MOVE "SETBLKID" TO SL-ID
                    CALL "LGPGM020" USING SL-ID
                                          BLOCK-ID
                    END-CALL
           END-IF.

       COM-LIN-20.
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     POS2 TO MAP-LFDN OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD TO SOURCE-REC.

           PERFORM  WRITE-PSETLOG.

           MOVE     "*"   TO SRCDTA OF SOURCE-REC(7:1).
           MOVE     X"22" TO SRCDTA OF SOURCE-REC(6:1).

     ‚* NUN SATZ WIEDER IN ARRAY SCHREIBEN
           MOVE     MAP-WRITE TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

           IF       SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:2) = "KK"
                    IF   SRCSEQ-RED OF SOURCE-REC <
                         SRCSEQ-RED OF BER-END
                         ADD 1 TO POS2
                         GO TO COM-LIN-20
                    END-IF
           END-IF.
           INITIALIZE BEREICH-TAB.

       COM-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOMMENTAR AUS ZEILE LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-COMMENT SECTION.
       DEL-COM-00.

           INITIALIZE RET-CODE.
           MOVE     SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                    MAP-KEY    OF LGMAPDTA-REC.
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.
     ‚*    SOLL NUR EINE ODER MEHRERE ZEILEN KOPIERT WERDEN?
           IF       SRCSEQ OF BER-ANF = ZEROS
                    MOVE SOURCE-REC TO BER-ANF OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-ANF
                    INITIALIZE POS2
                    MOVE MAP-LFDN   OF LGMAPDTA-REC TO POS2
                    IF   SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:3) = "KK-"
                         MOVE "1" TO X-RETCODE
                         GO TO DEL-COM-90
                    END-IF
           ELSE
                    MOVE SOURCE-REC TO BER-END OF BEREICH-TAB
                    MOVE SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                         TO AUSWAHL OF BER-END
                    IF   SRCSEQ OF BER-ANF = SRCSEQ OF BER-END
                         GO TO DEL-COM-90
                    END-IF
                    INITIALIZE X-RETCODE
           END-IF.

     ‚* SIND AUSWAHLEN IDENTISCH?
           IF       AUSWAHL OF BER-ANF NOT = AUSWAHL OF BER-END
            AND     AUSWAHL OF BER-END NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO DEL-COM-90
           END-IF.

     ‚* NEUE BLOCK-ID ERMITELN
           IF       X-UNDO = SPACES
                    MOVE "SETBLKID" TO SL-ID
                    CALL "LGPGM020" USING SL-ID
                                          BLOCK-ID
                    END-CALL
           END-IF.

       DEL-COM-20.
           MOVE     MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC.
           MOVE     POS2 TO MAP-LFDN OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.
           PERFORM  WRITE-PSETLOG.

           INITIALIZE RET-CODE.
           PERFORM  VARYING IDX FROM 1 BY 1
                    UNTIL IDX > LENGTH OF SRCDTA OF SOURCE-REC
                    IF   SRCDTA OF SOURCE-REC(IDX:1) = X"22"
                     OR  SRCDTA OF SOURCE-REC(IDX:1) = "*"
                         MOVE SPACE TO SRCDTA OF SOURCE-REC(IDX:1)
                    END-IF
           END-PERFORM.

           MOVE     SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC.
           MOVE     MAP-WRITE  TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

           IF       SEQ-ACT OF USRCMD-REC(USRCMD-LEN)(1:3) = "KK-"
                    IF   SRCSEQ-RED OF SOURCE-REC <
                         SRCSEQ-RED OF BER-END
                         ADD 1 TO POS2
                         GO TO DEL-COM-20
                    END-IF
           END-IF.
           INITIALIZE BEREICH-TAB.

       DEL-COM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZAHL POSITIONEN IN LOG-DATEI SCHREIBEN
     ‚* ERSTER POS-SATZ = KOPFSATZ
     ‚*--------------------------------------------------------------
       WRITE-ANZAHL-POSITIONEN SECTION.
       WRT-POS-00.

           INITIALIZE RET-CODE.
           INITIALIZE PSETLOGF OF PSETLOG-P.
           MOVE     X-LIB        TO SELIB  OF PSETLOG-P.
           MOVE     X-FILE       TO SEFILE OF PSETLOG-P.
           MOVE     X-MBR        TO SEMBR  OF PSETLOG-P.
           MOVE     LFDN-WRK     TO SELFDN OF PSETLOG-P.
           MOVE     BLOCK-ID-WRK TO SEZUSL OF PSETLOG-P.
           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-START-NL.
           IF       FILE-STATUS NOT = ZEROS
                    GO TO WRT-POS-90
           END-IF.

       WRT-POS-20.
           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-READ-NEXT-WL.
           IF       FILE-STATUS NOT = ZEROS
                    GO TO WRT-POS-90
           END-IF.

     ‚* STIMMEN DIE SCHLÜSSEL NOCH?
           IF       SELIB  OF PSETLOG-P NOT = X-LIB
            OR      SEFILE OF PSETLOG-P NOT = X-FILE
            OR      SEMBR  OF PSETLOG-P NOT = X-MBR
            OR      SELFDN OF PSETLOG-P NOT = LFDN-WRK
            OR      SEZUSL OF PSETLOG-P NOT = BLOCK-ID-WRK
                    SET DB-PTR TO PSETLOG-DP
                    PERFORM DB-REWRITE
                    GO TO WRT-POS-90
           END-IF.

           MOVE     UNDO-COUNT TO SEANPO OF PSETLOG-P.
           SET      DB-PTR TO PSETLOG-DP.
           PERFORM  DB-REWRITE.

       WRT-POS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE-NEU IN LGMAPDTA INITIALISIEREN
     ‚*--------------------------------------------------------------
       INITIALIZE-ZEILE-NEU SECTION.
       INI-NEU-00.

     ‚* AUF ARRAY AUFSETZTEN
           INITIALIZE SOURCE-POS.

     ‚* LAUFENDE NUMMER NEU ANORDNEN
           PERFORM  WITH TEST BEFORE VARYING SOURCE-POS FROM 1 BY 1
                    UNTIL SOURCE-POS = (X-SOURCE-MAX - 1)
                    MOVE MAP-READ-LFDN TO MAP-ACTION OF LGMAPDTA-REC
                    MOVE SOURCE-POS    TO MAP-LFDN   OF LGMAPDTA-REC
                    PERFORM CALL-LGMAPDTA
                    MOVE MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC
                    IF   ZEILE-NEU OF SOURCE-REC NOT = ZEROS
                         INITIALIZE ZEILE-NEU OF SOURCE-REC
                         MOVE SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC
                         MOVE MAP-READ TO MAP-ACTION OF LGMAPDTA-REC
                         PERFORM CALL-LGMAPDTA
                    END-IF
           END-PERFORM.

       INI-NEU-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* Aufruf zum ermitteln des Datums und der Uhrzeit
     ‚*---------------------------------------------------------------

           COPY GETTIME OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* Aufruf LGMAPDTA
     ‚*---------------------------------------------------------------

           COPY LGMAPDTA OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* Aufruf von Standardroutinen, die in jedem Programm beim Starten
     ‚* ausgeführt werden sollen.
     ‚*---------------------------------------------------------------

           COPY PGMINIT OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* Aufruf von Standardroutinen, die in jedem Programm beim
     ‚* Verlassen ausgeführt werden sollen.
     ‚*---------------------------------------------------------------

           COPY PGMEXIT OF QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* DBMAIN BEARBEITUNG EINBINDEN
     ‚*---------------------------------------------------------------

           COPY DBWORKING OF QCPYSRC.

