       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0026.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ZITAT ERMITTELN                                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  21.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERMITTELN EINES ZITATS                       *
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
     ‚* PCFGZITL1: ZICAT, ZITYPE, ZILFDN
     ‚*-------------------------------------------------------------
           SELECT PCFGZIT-DL
                  ASSIGN       TO  DATABASE-PCFGZITL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PCFGZITL1: ZICAT, ZITYPE, ZILFDN
       FD  PCFGZIT-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGZIT-L.
           COPY DDS-ALL-FORMATS OF PCFGZITL1.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0026".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

     ‚* POINTER
       01  READ-PTR                      USAGE POINTER.
       01  RESULT-PTR                    USAGE POINTER.

     ‚* SCHALTER
       01  ALL-TYPES                     PIC 9(1).
       01  ALL-CATEGORY                  PIC 9(1).
       01  DONT-USE                      PIC 9(1).
       01  ROW-COUNT                     PIC 9(9).

     ‚* SONSTIGE
       01  INVERS-CAT                    PIC X(21).
       01  INVERS-TYPE                   PIC X(21).

     ‚* PARAMETER FÜR DAS CCP0004
       01  RANDOM-NUM                    PIC S9(9) BINARY.
       01  RANDOM-MAX                    PIC S9(9) BINARY.
       01  RANDOM-SEED                   PIC S9(9) BINARY.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-POINTER                     USAGE POINTER.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-POINTER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0026" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN INPUT PCFGZIT-DL
           END-IF.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE ALL-TYPES.
           INITIALIZE ALL-CATEGORY.
     ‚* DEN POINTER ANALYSIEREN, DORT KÖNNEN MEHRERE EINTRÄGE
     ‚* VORHANDEN SEIN, DIE ALLE KATEGORIEN ZULASSEN.
           IF       X-POINTER NOT = NULL
                    PERFORM  CHECK-POINTER-INPUT
           ELSE
                    MOVE 1 TO ALL-CATEGORY
                    MOVE 1 TO ALL-TYPES
           END-IF.

     ‚* JETZT DIE DATENBANK EINLESEN
           PERFORM  READ-DATABASE.
           IF       ROW-COUNT > ZEROES
                    PERFORM GET-RANDOM-ROW
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DATENBANK LESEN
     ‚*--------------------------------------------------------------
       READ-DATABASE SECTION.
       REA-DBA-00.

     ‚* NEUEN POINTER FÜR DIE DB-EINTRÄGE ERSTELLEN
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-CREATE OF PTRMAP-REC TO TRUE.
           MOVE     "RESULT"   TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      RESULT-PTR TO PTR-RETURN OF PTRMAP-REC.

           INITIALIZE ROW-COUNT.
     ‚* LESEN DER PCFGZIT
           INITIALIZE PCFGZITF OF PCFGZIT-L.
           MOVE     SPACES      TO ZICAT  OF PCFGZIT-L.
           MOVE     SPACES      TO ZITYPE OF PCFGZIT-L.
           MOVE     ZEROES      TO ZILFDN OF PCFGZIT-L.
           START    PCFGZIT-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-DBA-90
           END-IF.

       REA-DBA-20.
           READ     PCFGZIT-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-DBA-90
           END-IF.

     ‚* SOLL DIESE KATEGORIE ANGEZEIGT WERDEN?
           PERFORM  CHECK-CATEGORY.
           IF       DONT-USE NOT = ZEROES
                    GO TO REA-DBA-20
           END-IF.

     ‚* SOLL DIESER TYP ANGEZEIGT WERDEN?
           PERFORM  CHECK-TYPE
           IF       DONT-USE NOT = ZEROES
                    GO TO REA-DBA-20
           END-IF.

     ‚* ZEILENZÄHLER
           ADD      1 TO ROW-COUNT.

           INITIALIZE MAPDTA-REC.
           MOVE     ROW-COUNT           TO MAP-KEY OF MAPDTA-REC.
           MOVE     ZILFDN OF PCFGZIT-L TO MAP-REC OF MAPDTA-REC(1:9).
           MOVE     ZITEXT OF PCFGZIT-L TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO RESULT-PTR.
           PERFORM  COPY-CALL-MAPDTA.

           GO TO    REA-DBA-20.

       REA-DBA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* JETZT EINE BELIEBIGE ZEILE IM RESULT-SET FINDEN
     ‚*--------------------------------------------------------------
       GET-RANDOM-ROW SECTION.
       GET-ROW-00.

           PERFORM  COPY-GET-TIME.

           MOVE     ROW-COUNT TO RANDOM-MAX.
           MOVE     ZEROES    TO RANDOM-NUM.
           MOVE     DATE-8    TO RANDOM-SEED.
           CALL     PROCEDURE "CCP0004" USING RANDOM-SEED
                                              RANDOM-MAX
                                              RANDOM-NUM
           END-CALL.

     ‚* DEN NEUEN TEXT LESEN ...
           INITIALIZE MAPDTA-REC.
           MOVE     RANDOM-NUM TO ROW-COUNT.
           MOVE     ROW-COUNT           TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-READ            TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO RESULT-PTR.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
     ‚*             ... UND HINZUFÜGEN
                    MOVE   "QUOTE"             TO MAP-KEY OF MAPDTA-REC
                    MOVE   MAP-WRITE           TO MAP-ACT OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO RESULT-PTR
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.

       GET-ROW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÜBERPRÜFEN DES ÜBERGEBENEN POINTERS
     ‚*--------------------------------------------------------------
       CHECK-POINTER-INPUT SECTION.
       CHK-INP-00.

           INITIALIZE RET-CODE.
     ‚* LESEN DES KATEGORIE-POINTER
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-READ OF PTRMAP-REC TO TRUE.
           MOVE     "CATEGORY" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC.
           IF       READ-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE  "*ALL"        TO MAP-KEY  OF MAPDTA-REC
                    MOVE  MAP-READ      TO MAP-ACT  OF MAPDTA-REC
                    SET   MAP-PTR OF MAPDTA-REC TO READ-PTR
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE = SPACES
                         MOVE 1 TO ALL-CATEGORY
                    END-IF
           END-IF.

     ‚* LESEN DES TYPEN-POINTER
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-READ OF PTRMAP-REC TO TRUE.
           MOVE     "TYPE"     TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC.
           IF       READ-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE  "*ALL"        TO MAP-KEY  OF MAPDTA-REC
                    MOVE  MAP-READ      TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE = SPACES
                         MOVE 1 TO ALL-TYPES
                    END-IF
           END-IF.

       CHK-INP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB SATZ VON DER KATEGORIE HER PASST
     ‚*--------------------------------------------------------------
       CHECK-CATEGORY SECTION.
       CHK-CAT-00.

           INITIALIZE DONT-USE.
     ‚* SCHONMAL FÜRS LESEN VORBEREITEN
           MOVE     "!"                   TO INVERS-CAT(1:1).
           MOVE     ZICAT  OF PCFGZIT-L   TO INVERS-CAT(2:).

     ‚* ES GIBT 3 SORTEN VON EINTRÄGEN
     ‚*  1: *ALL   -> ICH WILL ALLE KATEGORIEN
     ‚*  2: <CAT>  -> ICH WILL NUR DIE KATEGORIE <CAT>
     ‚*  3: !<CAT> -> ICH WILL DIE KATEGORIE <CAT> NICHT
     ‚* LESEN DES KATEGORIE-POINTER
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-READ OF PTRMAP-REC TO TRUE.
           MOVE     "CATEGORY" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC.
           IF       READ-PTR NOT = NULL
     ‚*             GIBT ES EINEN EINTRAG DER DIES AKTIVIERT?
                    INITIALIZE MAPDTA-REC
                    MOVE  ZICAT  OF PCFGZIT-L TO MAP-KEY  OF MAPDTA-REC
                    MOVE  MAP-READ            TO MAP-ACT  OF MAPDTA-REC
                    SET   MAP-PTR OF MAPDTA-REC TO READ-PTR
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE     NOT = SPACES
                     AND ALL-CATEGORY     = ZEROES
                         MOVE 1 TO DONT-USE
                    END-IF
     ‚*             GIBT ES EINEN EINTRAG DER DIES DEAKTIVIERT?
                    INITIALIZE MAPDTA-REC
                    MOVE  INVERS-CAT          TO MAP-KEY  OF MAPDTA-REC
                    MOVE  MAP-READ            TO MAP-ACT  OF MAPDTA-REC
                    SET   MAP-PTR OF MAPDTA-REC TO READ-PTR
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE = SPACES
                         MOVE 1 TO DONT-USE
                    END-IF
           END-IF.

       CHK-CAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB SATZ VOM TYP HER PASST
     ‚*--------------------------------------------------------------
       CHECK-TYPE SECTION.
       CHK-TYP-00.

           INITIALIZE DONT-USE.
     ‚* SCHONMAL FÜRS LESEN VORBEREITEN
           MOVE     "!"                   TO INVERS-TYPE(1:1).
           MOVE     ZITYPE OF PCFGZIT-L   TO INVERS-TYPE(2:).

     ‚* ES GIBT 3 SORTEN VON EINTRÄGEN
     ‚*  1: *ALL    -> ICH WILL ALLE TYPEN
     ‚*  2: <TYPE>  -> ICH WILL NUR DEN <TYPE>
     ‚*  3: !<TYPE> -> ICH WILL DIE KATEGORIE <TYPE> NICHT
     ‚* LESEN DES KATEGORIE-POINTER
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-READ OF PTRMAP-REC TO TRUE.
           MOVE     "TYPE"     TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC.
           IF       READ-PTR NOT = NULL
     ‚*             GIBT ES EINEN EINTRAG DER DIES AKTIVIERT?
                    INITIALIZE MAPDTA-REC
                    MOVE  ZITYPE OF PCFGZIT-L TO MAP-KEY  OF MAPDTA-REC
                    MOVE  MAP-READ            TO MAP-ACT  OF MAPDTA-REC
                    SET   MAP-PTR OF MAPDTA-REC TO READ-PTR
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE      NOT = SPACES
                     AND ALL-TYPES         = ZEROES
                         MOVE 1 TO DONT-USE
                    END-IF
     ‚*             GIBT ES EINEN EINTRAG DER DIES DEAKTIVIERT?
                    INITIALIZE MAPDTA-REC
                    MOVE  INVERS-TYPE         TO MAP-KEY  OF MAPDTA-REC
                    MOVE  MAP-READ            TO MAP-ACT  OF MAPDTA-REC
                    SET   MAP-PTR OF MAPDTA-REC TO READ-PTR
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE = SPACES
                         MOVE 1 TO DONT-USE
                    END-IF
           END-IF.

       CHK-TYP-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
