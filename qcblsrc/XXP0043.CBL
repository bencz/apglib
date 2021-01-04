       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0043.
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
     ‚* FUNKTION      :  ERMITTELN EINER ZUFÄLLIGEN BESTELLUNG        *
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
     ‚* PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PFODAWL1: FAMAKU, FABJH, FABJA, FABMO, FABTA
     ‚*-------------------------------------------------------------
           SELECT PFODAW-DL
                  ASSIGN       TO  DATABASE-PFODAWL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
     ‚*--- PFODAWL1: FAMAKU, FABJH, FABJA, FABMO, FABTA
       FD  PFODAW-DL
           LABEL RECORDS ARE STANDARD.
       01  PFODAW-L.
           COPY DDS-ALL-FORMATS OF MAT-PFODAWL1.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0043".
       01  PGM-TYP                       PIC X(8)  VALUE "FUN".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  ADDLIBLE-REC                  TYPE ADDLIBLE-TYPE.
       01  RMVLIBLE-REC                  TYPE RMVLIBLE-TYPE.

       01  MODE-SAV                      PIC X(20).
       01  REST-SAV                      PIC X(30).
       01  USER-WRK                      LIKE T-USER.

     ‚* SCHALTER
       01  ROW-COUNT                     PIC 9(4).
       01  READ-PTR                      USAGE POINTER.

     ‚* PARAMETER FÜR DAS CCP0004
       01  RANDOM-NUM                    PIC S9(9) BINARY.
       01  RANDOM-MAX                    PIC S9(9) BINARY.
       01  RANDOM-SEED                   PIC S9(9) BINARY.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚* "*USER"   = ALTE BESTELLUNGEN DURCHSUCHEN
     ‚* "*ALLUSR" = ALLE USER
     ‚* "*PLAN"   = GESAMTE SPEISEKARTE BETRACHTEN
       01  X-MODE                        PIC X(20).
       01  X-RESTAURANT                  PIC X(30).
       01  X-POINTER                     USAGE POINTER.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-MODE
                                X-RESTAURANT
                                X-POINTER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0043" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
     ‚*             ALLE EINTRÄGE AUS DEM SPEISEPLAN UND ALLE
     ‚*             BISHERIGEN BESTELLUNGEN DIREKT EINLESEN
                    OPEN INPUT PCONFIG-DP
                    MOVE SPACES TO MODE-SAV
                    MOVE SPACES TO REST-SAV
           END-IF.

           IF       X-MODE       NOT = MODE-SAV
            OR      X-RESTAURANT NOT = REST-SAV
                    MOVE     X-MODE       TO MODE-SAV
                    MOVE     X-RESTAURANT TO REST-SAV
     ‚*             POINTER LEEREN
                    SET      PTR-MAIN OF PTRMAP-REC  TO X-POINTER
                    SET      PTR-CLEAR OF PTRMAP-REC TO TRUE
                    PERFORM  COPY-PTR-MAPPING
                    SET      X-POINTER TO NULL
     ‚*             SPEISEPLAN LESEN
                    IF   X-MODE = "*PLAN"
                         PERFORM  READ-PCONFIG
                    END-IF
     ‚*             ESSENSLISTE LESEN
                    PERFORM  ADD-MAT-LIB
                    PERFORM  CHECK-OBJECTS
                    IF   RET-CODE = SPACES
                         OPEN  INPUT PFODAW-DL
                         IF   FILE-STATUS     = ZEROES
                              IF   X-MODE NOT = "*PLAN"
                                   PERFORM  READ-PFODAW
                              END-IF
                              CLOSE PFODAW-DL
                         END-IF
                         PERFORM  RMV-MAT-LIB
                    END-IF
           END-IF.

     ‚* NUN ERMITTELN WIEVIEL EINTRÄGE WIR HABEN
           PERFORM  GET-ROW-COUNT.
           IF       ROW-COUNT > ZEROES
                    PERFORM GET-RANDOM-ROW
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DES SPEISEPLANS
     ‚*--------------------------------------------------------------
       READ-PCONFIG SECTION.
       REA-CFG-00.

           INITIALIZE ROW-COUNT.
     ‚* NEUEN POINTER FÜR DIE DB-EINTRÄGE ERSTELLEN
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-CREATE OF PTRMAP-REC TO TRUE.
           MOVE     "FOODLIST" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC.
           SET      X-POINTER  TO PTR-MAIN   OF PTRMAP-REC.

     ‚* JETZT DEN SPEISEPLAN LESEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "K200"              TO CFID   OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-CFG-90
           END-IF.

       REA-CFG-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-CFG-90
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = "K200"
                    GO TO REA-CFG-90
           END-IF.

     ‚* NUR EIN BESTIMMTES RESTAURANT?
           IF       X-RESTAURANT        NOT = SPACES
            AND     CFKEY2 OF PCONFIG-P NOT = X-RESTAURANT
                    GO TO REA-CFG-20
           END-IF.

     ‚* ZEILENZÄHLER
           ADD      1 TO ROW-COUNT.

           INITIALIZE MAPDTA-REC.
           MOVE     ROW-COUNT           TO MAP-KEY OF MAPDTA-REC.
           MOVE     CFTXT1 OF PCONFIG-P TO MAP-REC OF MAPDTA-REC.
           MOVE     MAP-WRITE           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO READ-PTR.
           PERFORM  COPY-CALL-MAPDTA.

           GO TO    REA-CFG-20.

       REA-CFG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DIE EIGENEN BESTELLUNGEN AUSLESEN
     ‚*--------------------------------------------------------------
       READ-PFODAW SECTION.
       REA-FOD-00.

           INITIALIZE ROW-COUNT.
     ‚* NEUEN POINTER FÜR DIE DB-EINTRÄGE ERSTELLEN
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-CREATE OF PTRMAP-REC TO TRUE.
           MOVE     "FOODLIST" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC.
           SET      X-POINTER  TO PTR-MAIN   OF PTRMAP-REC.

           EVALUATE X-MODE
               WHEN "*USER"   MOVE T-USER TO USER-WRK
               WHEN "*ALLUSR" MOVE SPACES TO USER-WRK
               WHEN OTHER     MOVE X-MODE TO USER-WRK
           END-EVALUATE.

     ‚* MIT TAG UND MONAT AUFSETZEN
           INITIALIZE PFODAWF OF PFODAW-L.
           MOVE     USER-WRK    TO FAMAKU OF PFODAW-L.
           MOVE     99          TO FABJH  OF PFODAW-L.
           MOVE     99          TO FABJA  OF PFODAW-L.
           MOVE     99          TO FABMO  OF PFODAW-L.
           MOVE     99          TO FABTA  OF PFODAW-L.
           START    PFODAW-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-FOD-90
           END-IF.

       REA-FOD-20.
           READ     PFODAW-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-FOD-90
           END-IF.

           IF       FAMAKU OF PFODAW-L NOT = USER-WRK
            AND     USER-WRK           NOT = SPACES
                    GO TO REA-FOD-90
           END-IF.

     ‚* NUR EIN BESTIMMTES RESTAURANT?
           IF       X-RESTAURANT       NOT = SPACES
            AND     FARTRT OF PFODAW-L NOT = X-RESTAURANT
                    GO TO REA-FOD-20
           END-IF.

     ‚* ZEILENZÄHLER
           ADD      1 TO ROW-COUNT.

           INITIALIZE MAPDTA-REC.
           MOVE     ROW-COUNT           TO MAP-KEY OF MAPDTA-REC.
           MOVE     FAGERT OF PFODAW-L  TO MAP-REC OF MAPDTA-REC.
           MOVE     MAP-WRITE           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO READ-PTR.
           PERFORM  COPY-CALL-MAPDTA.

           GO TO    REA-FOD-20.

       REA-FOD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAT-BIBLIOTHEK VORSCHALTEN
     ‚*--------------------------------------------------------------
       ADD-MAT-LIB SECTION.
       ADD-MAT-00.

     ‚* "MAT" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE ADDLIBLE-REC.
           MOVE     "MAT"        TO LIB     OF ADDLIBLE-REC.
           MOVE     "*LAST"      TO LIBPOS  OF ADDLIBLE-REC.
           CALL     "CLP0007" USING ADDLIBLE-REC
                                    RET-CODE
           END-CALL.

       ADD-MAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAT-BIBLIOTHEK ENTFERNEN
     ‚*--------------------------------------------------------------
       RMV-MAT-LIB SECTION.
       RMV-MAT-00.

     ‚* "AMU" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE RMVLIBLE-REC.
           MOVE     "MAT"        TO LIB     OF RMVLIBLE-REC.
           CALL     "CLP0009" USING RMVLIBLE-REC
           END-CALL.

       RMV-MAT-90.
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
           MOVE     TIME-6    TO RANDOM-SEED.
           CALL     PROCEDURE "CCP0004" USING RANDOM-SEED
                                              RANDOM-MAX
                                              RANDOM-NUM
           END-CALL.

     ‚* DEN NEUEN TEXT LESEN ...
           INITIALIZE MAPDTA-REC.
           MOVE     RANDOM-NUM TO ROW-COUNT.
           MOVE     ROW-COUNT           TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-READ            TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC.
           IF       READ-PTR NOT = NULL
     ‚*             ... UND HINZUFÜGEN
                    MOVE   "ENTRY"             TO MAP-KEY OF MAPDTA-REC
                    MOVE   MAP-WRITE           TO MAP-ACT OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO READ-PTR
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.

       GET-ROW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÜBERPRÜFEN DES ÜBERGEBENEN POINTERS
     ‚*--------------------------------------------------------------
       GET-ROW-COUNT SECTION.
       GET-RWC-00.

           INITIALIZE RET-CODE.
     ‚* LESEN DES KATEGORIE-POINTER
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-READ OF PTRMAP-REC TO TRUE.
           MOVE     "FOODLIST" TO PTR-KEY OF PTRMAP-REC
           PERFORM  COPY-PTR-MAPPING.
           SET      READ-PTR   TO PTR-RETURN OF PTRMAP-REC.
           IF       READ-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE   MAP-GETCOUNT  TO MAP-ACT  OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO READ-PTR
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE = SPACES
                         MOVE MAP-LFDN OF MAPDTA-REC TO ROW-COUNT
                    END-IF
           END-IF.

       GET-RWC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEIEN AUCH GIBT
     ‚*--------------------------------------------------------------
       CHECK-OBJECTS SECTION.
       CHK-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     "PFODAW"     TO OBJ     OF CHKOBJ-REC.
           MOVE     "*LIBL"      TO LIB     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-OBJ-90
           END-IF.

       CHK-OBJ-90.
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
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
