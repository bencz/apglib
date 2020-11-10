       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              DMP0025.
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
     ‚* ERSTELLT AM   :           NOVEMBER 2007                       *
     ‚*                                                               *
     ‚* PROBLEM       :           ERSATZ FÜR DEN IBM SEU              *
     ‚*                           REDO                                *
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
     ‚*-   PSETLOG - LOGDATEI FÜR REDO
       01 PSETLOG-L-X.
         02 DB-INFO-DESC          PIC X(20) VALUE "PSETLOG-DL".
         02 FILLER                PIC X(12).
         02 PSETLOG-DL            USAGE POINTER.
            COPY PSETLOGL2  OF QDBCPY.
         02 PSETLOG-L.
            COPY DDS-ALL-FORMATS OF PSETLOGL2.

     ‚*-   PSETLOG - LOGDATEI FÜR WIEDERHERSTELLEN
       01 PSETLOG-L1-X.
         02 DB-INFO-DESC          PIC X(20) VALUE "PSETLOG-DL1".
         02 FILLER                PIC X(12).
         02 PSETLOG-DL1           USAGE POINTER.
            COPY PSETLOGL3 OF QDBCPY.
         02 PSETLOG-L1.
            COPY DDS-ALL-FORMATS OF PSETLOGL3.

       01  PGM-WRK                        PIC X(10) VALUE "DMP0025   ".

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

       01  SOURCE-REC-SAV               LIKE SOURCE-REC OF SOURCE-TAB.

     ‚* TABELLE MIT USERAUSWAHLEN
       01  USRCMD-TAB.
        05 USRCMD-REC OCCURS 50000 TIMES.
         10 AUSW-TYP                    PIC 9(3).
         10 SEQ-FMT                     PIC X(7).
         10 SEQ-ACT                     PIC X(7).
       01  USRCMD-LEN                   PIC 9(5).

       01  UNDO-WRK                     PIC X(1).
       01  IDX                          PIC 9(5) COMP.
       01  LEN                          PIC 9(5) COMP.

       01  AUSWAHL-WRK                  LIKE AUSWAHL OF SOURCE-REC.
       01  ZEILE-WRK                    LIKE SRCDTA  OF SOURCE-REC.
       01  ANPO-WRK                     LIKE SEANPO  OF PSETLOG-L.
       01  LFDN-WRK                     LIKE SELFDN  OF PSETLOG-L.

       01  PSETLOG-WRK.
           COPY DDS-ALL-FORMATS OF PSETLOGL1.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-MBR                        PIC X(10).
       01  X-FILE                       PIC X(10).
       01  X-LIB                        PIC X(10).
       01  X-POINTER                    USAGE POINTER.
       01  X-SOURCE-MAX                 LIKE SOURCE-MAX.
       01  X-DSP-CUR-X                  LIKE DSP-CUR-X.
       01  X-DSP-CUR-Y                  LIKE DSP-CUR-Y.
       01  X-AKTION                     PIC X(1).
       01  X-BS-START                   PIC S9(5) COMP.
       01  X-ROW-COUNT                  PIC 9(2).
       01  X-RETCODE                    PIC X(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-MBR
                                X-FILE
                                X-LIB
                                X-POINTER
                                X-SOURCE-MAX
                                X-DSP-CUR-X
                                X-DSP-CUR-Y
                                X-AKTION
                                X-BS-START
                                X-ROW-COUNT
                                X-RETCODE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* Standard INIT-Routine aufrufen
           PERFORM  PGM-INIT.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    MOVE "INPUT" TO DB-OPENMODE
                         SET DB-PTR TO ADDRESS OF PSETLOG-L1-X
                         PERFORM  DB-OPEN
                    MOVE "IO"    TO DB-OPENMODE
                         SET DB-PTR TO ADDRESS OF PSETLOG-L-X
                         PERFORM  DB-OPEN
           END-IF.

           MOVE     1   TO IDX.
           INITIALIZE X-RETCODE.
           INITIALIZE SOURCE-REC.
           INITIALIZE USRCMD-TAB.
           INITIALIZE USRCMD-LEN.

     ‚* AUSWAHLEN AUSFÜHREN
           INITIALIZE LGMAPDTA-REC.
           INITIALIZE PSETLOGF OF PSETLOG-WRK.
           SET      MAP-POINTER OF LGMAPDTA-REC TO X-POINTER.

           IF       X-AKTION = "1"
                    PERFORM READ-PSETLOG-L1
                    PERFORM CURSERPOSITION-ERMITTELN
           ELSE
                    PERFORM  READ-PSETLOG-L
           END-IF.

           PERFORM  CALL-DMP0026.

       ENDE.
     ‚* Standard EXIT-Routine aufrufen
           PERFORM  PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LOGGING DATEI LESEN UND IN TABELLE SCHREIBEN
     ‚*--------------------------------------------------------------
       READ-PSETLOG-L SECTION.
       REA-DL2-00.

           INITIALIZE PSETLOGF OF PSETLOG-WRK.
           INITIALIZE RET-CODE.
           MOVE     1 TO IDX.

           INITIALIZE PSETLOGF OF PSETLOG-L.
           MOVE     X-LIB     TO SELIB  OF PSETLOG-L.
           MOVE     X-FILE    TO SEFILE OF PSETLOG-L.
           MOVE     X-MBR     TO SEMBR  OF PSETLOG-L.
           MOVE     ZEROS     TO SELFDN OF PSETLOG-L.
           MOVE     ZEROS     TO SEZUSL OF PSETLOG-L.
           SET      DB-PTR TO PSETLOG-DL.
           PERFORM  DB-START-NL.
           IF       FILE-STATUS NOT = ZEROS
                    PERFORM CALL-DMP0026
                    GO TO REA-DL2-90
           END-IF.

       REA-DL2-20.
           SET      DB-PTR TO PSETLOG-DL.
           PERFORM  DB-READ-NEXT-WL.
           IF       FILE-STATUS NOT = ZEROS
                    PERFORM CALL-DMP0026
                    GO TO REA-DL2-90
           END-IF.

     ‚* STIMMEN DIE SCHLÜSSEL NOCH?
           IF       SELIB  OF PSETLOG-L NOT = X-LIB
            OR      SEFILE OF PSETLOG-L NOT = X-FILE
            OR      SEMBR  OF PSETLOG-L NOT = X-MBR
                    SET DB-PTR TO PSETLOG-DL
                    PERFORM DB-REWRITE
                    PERFORM CALL-DMP0026
                    GO TO REA-DL2-90
           END-IF.

     ‚* SOLLEN ALLE SÄTZE ALS NICHT UNDO GESETZT WERDEN?
           IF       X-AKTION = "D"
                    MOVE "2"   TO SEUNDO OF PSETLOG-L
                    SET DB-PTR TO PSETLOG-DL
                    PERFORM DB-REWRITE
                    GO TO REA-DL2-20
           END-IF.

     ‚* IST EIN BLOCK FERTIG?
           IF       SEZUSL OF PSETLOG-L   NOT = SEZUSL OF PSETLOG-WRK
            AND     SEZUSL OF PSETLOG-WRK NOT = ZEROS
                    PERFORM CALL-DMP0026
                    SET DB-PTR TO PSETLOG-DL
                    PERFORM DB-REWRITE
                    GO TO REA-DL2-20
           END-IF.

           MOVE     PSETLOG-L TO PSETLOG-WRK.
           MOVE     SEREC OF PSETLOG-L(1:LENGTH OF SOURCE-REC)
                    TO SOURCE-REC.
           MOVE     AUSWAHL OF SOURCE-REC TO AUSWAHL-WRK.
           IF       SEANPO OF PSETLOG-L > ZEROS
                    MOVE SEANPO OF PSETLOG-L TO ANPO-WRK
                    MOVE SELFDN OF PSETLOG-L TO LFDN-WRK
           END-IF.

     ‚* NUN DATENSATZ AUBEREITEN UND IN TABELLE SCHREIBEN
           IF       AUSWAHL OF SOURCE-REC      = "TXT"
     ‚*€    OR      AUSWAHL OF SOURCE-REC(1:1) = "D"
            OR      AUSWAHL OF SOURCE-REC(1:1) = "K"
            OR      AUSWAHL OF SOURCE-REC(1:2) = "AT"
                    MOVE     SRCDTA OF SOURCE-REC TO ZEILE-WRK
                    PERFORM  CHANGE-SOURCE
           ELSE
                    PERFORM  AUFBEREITEN-TABELLE
           END-IF.

     ‚* SATZ MARKIEREN DASS DER UNDO ERFOLGT IST
           MOVE     SPACES TO SEUNDO OF PSETLOG-L.

           SET      DB-PTR TO PSETLOG-DL.
           PERFORM  DB-REWRITE.

           PERFORM  CURSERPOSITION-ERMITTELN.

           GO TO    REA-DL2-20.

       REA-DL2-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LOGGING DATEI LESEN UND IN TABELLE SCHREIBEN
     ‚*--------------------------------------------------------------
       READ-PSETLOG-L1 SECTION.
       REA-DL3-00.

           INITIALIZE PSETLOGF OF PSETLOG-WRK.
           INITIALIZE RET-CODE.
           MOVE     1 TO IDX.

           INITIALIZE PSETLOGF OF PSETLOG-L1.
           MOVE     X-LIB     TO SELIB  OF PSETLOG-L1.
           MOVE     X-FILE    TO SEFILE OF PSETLOG-L1.
           MOVE     X-MBR     TO SEMBR  OF PSETLOG-L1.
           MOVE     ZEROS     TO SELFDN OF PSETLOG-L1.
           MOVE     ZEROS     TO SEZUSL OF PSETLOG-L1.
           SET      DB-PTR TO PSETLOG-DL1.
           PERFORM  DB-START-NL.
           IF       FILE-STATUS NOT = ZEROS
                    PERFORM CALL-DMP0026
                    GO TO REA-DL3-90
           END-IF.

       REA-DL3-20.
           SET      DB-PTR TO PSETLOG-DL1.
           PERFORM  DB-READ-NEXT-WL.
           IF       FILE-STATUS NOT = ZEROS
                    PERFORM CALL-DMP0026
                    GO TO REA-DL3-90
           END-IF.

     ‚* STIMMEN DIE SCHLÜSSEL NOCH?
           IF       SELIB  OF PSETLOG-L1 NOT = X-LIB
            OR      SEFILE OF PSETLOG-L1 NOT = X-FILE
            OR      SEMBR  OF PSETLOG-L1 NOT = X-MBR
                    PERFORM CALL-DMP0026
                    GO TO REA-DL3-90
           END-IF.

     ‚* IST EIN BLOCK FERTIG?
           IF       SEZUSL OF PSETLOG-L   NOT = SEZUSL OF PSETLOG-WRK
            AND     SEZUSL OF PSETLOG-WRK NOT = ZEROS
                    PERFORM CALL-DMP0026
           END-IF.

           MOVE     PSETLOG-L1 TO PSETLOG-WRK.
           MOVE     SEREC OF PSETLOG-L1(1:LENGTH OF SOURCE-REC)
                    TO SOURCE-REC.
           MOVE     AUSWAHL OF SOURCE-REC TO AUSWAHL-WRK.
           IF       SEANPO OF PSETLOG-L1 > ZEROS
                    MOVE SEANPO OF PSETLOG-L1 TO ANPO-WRK
                    MOVE SELFDN OF PSETLOG-L1 TO LFDN-WRK
           END-IF.

     ‚* WENN NUR TEXT GEÄNDERT WURDE MUSS DER AKTUELLE TEXT
     ‚* GEZOGEN WRDEN
           IF       AUSWAHL-WRK      = "TXT"
                    MOVE SEREC OF PSETLOG-L1(1000:LENGTH OF
                         SRCDTA OF SOURCE-REC) TO
                         SRCDTA OF SOURCE-REC
                    MOVE     SRCDTA OF SOURCE-REC TO ZEILE-WRK
                    PERFORM  CHANGE-SOURCE
           END-IF.

     ‚* NUN DATENSATZ AUBEREITEN UND IN TABELLE SCHREIBEN
           PERFORM  AUFBEREITEN-TABELLE.

           GO TO    REA-DL3-20.

       REA-DL3-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TABELLE AUFBEREITEN
     ‚*--------------------------------------------------------------
       AUFBEREITEN-TABELLE SECTION.
       AUF-TAB-00.

           INITIALIZE RET-CODE.
           IF       ANPO-WRK = SEANPO OF PSETLOG-WRK
            OR      ANPO-WRK = 1
            OR      SELFDN OF PSETLOG-WRK = LFDN-WRK + ANPO-WRK - 1
                    ADD  1 TO USRCMD-LEN
                    MOVE AUSWAHL OF SOURCE-REC TO
                         SEQ-ACT OF USRCMD-REC(USRCMD-LEN)
                    MOVE SRCSEQ  OF SOURCE-REC(1:4) TO
                         SEQ-FMT OF USRCMD-REC(USRCMD-LEN)(1:4)
                    MOVE "."                        TO
                         SEQ-FMT OF USRCMD-REC(USRCMD-LEN)(5:1)
                    MOVE SRCSEQ  OF SOURCE-REC(5:2) TO
                         SEQ-FMT OF USRCMD-REC(USRCMD-LEN)(6:2)
           END-IF.

       AUF-TAB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE ERSETZEN
     ‚*--------------------------------------------------------------
       CHANGE-SOURCE SECTION.
       CHG-SRC-00.

           INITIALIZE RET-CODE.
     ‚* ZEILE LESEN
           MOVE     SRCSEQ OF SOURCE-REC(1:4) TO
                    MAP-KEY   OF LGMAPDTA-REC(1:4).
           MOVE     "."                       TO
                    MAP-KEY   OF LGMAPDTA-REC(5:1).
           MOVE     SRCSEQ OF SOURCE-REC(5:2) TO
                    MAP-KEY   OF LGMAPDTA-REC(6:2).
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.

           MOVE     AUSWAHL-WRK TO AUSWAHL OF SOURCE-REC.
           MOVE     SOURCE-REC  TO SEREC  OF PSETLOG-L.
           INITIALIZE SRCDTA OF SOURCE-REC.
           MOVE     ZEILE-WRK   TO SRCDTA OF SOURCE-REC.

     ‚* SATZ ZURÜCKSCHREIBEN
           MOVE     SOURCE-REC TO MAP-RECORD OF LGMAPDTA-REC.
           MOVE     MAP-WRITE  TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.

       CHG-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DMP0026 AUFRUFEN
     ‚*--------------------------------------------------------------
       CALL-DMP0026 SECTION.
       CAL-021-00.

           INITIALIZE RET-CODE.

     ‚* PRÜFEN OB DMP0026 AUFGERUFEN WERDEN MUSS
           IF       SEQ-ACT OF USRCMD-REC(1) = SPACES
                    GO TO CAL-021-90
           END-IF.

           MOVE     "2" TO UNDO-WRK.
           CALL     "DMP0026" USING X-MBR
                                    X-FILE
                                    X-LIB
                                    MAP-POINTER OF LGMAPDTA-REC
                                    USRCMD-TAB
                                    X-SOURCE-MAX
                                    X-DSP-CUR-X
                                    X-DSP-CUR-Y
                                    UNDO-WRK
                                    X-BS-START
                                    RET-CODE
           END-CALL.

           INITIALIZE USRCMD-TAB.
           INITIALIZE USRCMD-LEN.

       CAL-021-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* CURSERPOSITION ERMITTELN
     ‚*--------------------------------------------------------------
       CURSERPOSITION-ERMITTELN SECTION.
       GET-CUR-00.

           INITIALIZE RET-CODE.
     ‚* IN WELCHER ZEILE STEHT DIE ÄNDERUNG?
           IF       AUSWAHL OF SOURCE-REC      NOT = "TXT"
            AND     AUSWAHL OF SOURCE-REC      NOT = "KK"
            AND     AUSWAHL OF SOURCE-REC      NOT = "KK-"
            AND     AUSWAHL OF SOURCE-REC      NOT = "K "
            AND     AUSWAHL OF SOURCE-REC      NOT = "K-"
            AND     AUSWAHL OF SOURCE-REC(1:2) NOT = "AT"
            AND     USRCMD-LEN                 NOT = ZEROS
                    MOVE SEQ-FMT OF USRCMD-REC(USRCMD-LEN) TO
                         MAP-KEY OF LGMAPDTA-REC
           ELSE
                    MOVE SRCSEQ OF SOURCE-REC(1:4) TO
                         MAP-KEY OF LGMAPDTA-REC(1:4)
                    MOVE "."                       TO
                         MAP-KEY OF LGMAPDTA-REC(5:1)
                    MOVE SRCSEQ OF SOURCE-REC(5:2) TO
                         MAP-KEY OF LGMAPDTA-REC(6:2)
           END-IF.
           MOVE     MAP-READ TO MAP-ACTION OF LGMAPDTA-REC.
           PERFORM  CALL-LGMAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO GET-CUR-90
           END-IF.

           MOVE     MAP-RECORD OF LGMAPDTA-REC TO SOURCE-REC.

           IF       SRCSEQ-RED OF SOURCE-REC = ZEROS
                    GO TO GET-CUR-90
           END-IF.

     ‚* ERMITTELN AB WELCHER ZEILE DER BS AUSGEGEBEN WERDEN SOLL
           IF       (MAP-LFDN OF LGMAPDTA-REC - 10) <= ZERO
                    MOVE ZEROS TO X-BS-START
           ELSE
                    IF   MAP-LFDN OF LGMAPDTA-REC > X-BS-START +
                         X-ROW-COUNT
                     OR  MAP-LFDN OF LGMAPDTA-REC < X-BS-START
                         COMPUTE X-BS-START = MAP-LFDN OF LGMAPDTA-REC
                                            - 10
                    END-IF
           END-IF.
           IF       X-BS-START + X-ROW-COUNT >= X-SOURCE-MAX
                    COMPUTE X-BS-START = X-SOURCE-MAX
                                       - X-ROW-COUNT
                                       - 2
           END-IF.

     ‚* CURSERPOSITION ERMITTELN
           COMPUTE  X-DSP-CUR-Y = MAP-LFDN OF LGMAPDTA-REC
                                - X-BS-START + 4.

           INITIALIZE LEN.
           INSPECT  SRCDTA OF SOURCE-REC TALLYING LEN FOR
                    LEADING SPACES.
           IF       LEN = LENGTH OF SRCDTA OF SOURCE-REC
                    MOVE ZERO TO LEN
           END-IF.
           IF       LEN > 70
                    MOVE 9 TO LEN
           END-IF.
           COMPUTE  X-DSP-CUR-X = LEN
                                + 3
                                + LENGTH OF SRCSEQ OF SOURCE-REC
           END-COMPUTE.

       GET-CUR-90.
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
      /
