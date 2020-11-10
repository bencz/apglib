       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0034.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* BEFEHLSPOINTER                                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  01.07.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AKTIONEN FÜR BEFEHLSPOINTER                  *
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

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0034".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PTR-MAIN                    USAGE POINTER.
       01  X-PTR-KEY                     LIKE PTR-KEY OF PTRMAP-REC.
       01  X-MAP-LFDN                    LIKE MAP-LFDN OF MAPDTA-REC.
       01  X-AKTION                      PIC X(20).
       01  X-SEUCMD                      PIC X(40).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PTR-MAIN
                                X-PTR-KEY
                                X-MAP-LFDN
                                X-AKTION
                                X-SEUCMD.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0034" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* LESEN DES POINTERS
           SET      PTR-READ   OF PTRMAP-REC TO TRUE.
           MOVE     X-PTR-KEY                TO PTR-KEY OF PTRMAP-REC.
           SET      PTR-MAIN OF PTRMAP-REC   TO X-PTR-MAIN.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    SET      PTR-CREATE OF PTRMAP-REC TO TRUE
                    MOVE     X-PTR-KEY  TO PTR-KEY OF PTRMAP-REC
                    PERFORM  COPY-PTR-MAPPING
           END-IF.
           SET      X-PTR-MAIN TO PTR-MAIN OF PTRMAP-REC.

           EVALUATE X-AKTION
     ‚*             COMMAND HINZUFÜGEN
                    WHEN "ADD"
                         PERFORM  ADD-SEUCMD
     ‚*             LETZTEN COMMAND ERMITTELN
                    WHEN "GET"
                         IF   X-MAP-LFDN > ZEROES
                              PERFORM  GET-SEUCMD
                              SUBTRACT 1 FROM X-MAP-LFDN
                         ELSE
                              PERFORM  GET-MAXCMD
                              IF   X-MAP-LFDN > ZEROES
                                   PERFORM  GET-SEUCMD
                              END-IF
                         END-IF
           END-EVALUATE.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SEUCMD HINZUFÜGEN
     ‚*--------------------------------------------------------------
       ADD-SEUCMD SECTION.
       ADD-CMD-00.

     ‚* DER EINTRAG AUCH ALS LAUFENDE NUMMER ANHÄNGEN
           INITIALIZE MAPDTA-REC.
           MOVE     MAP-GETCOUNT        TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    MOVE   MAP-LFDN OF MAPDTA-REC TO X-MAP-LFDN
                    ADD    1                      TO X-MAP-LFDN
     ‚*             NACH GETCOUNT STEHT DIE ANZAHL IN DER LAUFENDEN
     ‚*             NUMMER. DIESE ÜBERNEHMEN WIR ALS KEY
                    MOVE   X-MAP-LFDN     TO MAP-KEY OF MAPDTA-REC
                    MOVE   X-SEUCMD       TO MAP-REC OF MAPDTA-REC
                    MOVE   MAP-WRITE      TO MAP-ACT OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO
                           PTR-RETURN OF PTRMAP-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.

       ADD-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LETZTEN BEFEHL ERMITTELN
     ‚*--------------------------------------------------------------
       GET-SEUCMD SECTION.
       GET-SCD-00.

           INITIALIZE MAPDTA-REC.
           MOVE     X-MAP-LFDN     TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-READ       TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    MOVE MAP-REC OF MAPDTA-REC TO X-SEUCMD
           END-IF.

       GET-SCD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SEUCMD HINZUFÜGEN
     ‚*--------------------------------------------------------------
       GET-MAXCMD SECTION.
       GET-MMD-00.

           INITIALIZE MAPDTA-REC.
           MOVE     MAP-GETCOUNT        TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    MOVE   MAP-LFDN OF MAPDTA-REC TO X-MAP-LFDN
           END-IF.

       GET-MMD-90.
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
