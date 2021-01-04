       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0033.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* FÜLLEN HIGHLIGHT-POINTER                                      *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.07.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  HIGHLIGHTPOINTER FÜR SEU FÜLLEN              *
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
     ‚* PHYSISCHE DATEI SYNTAX-HIGHLIGHTING
     ‚*-------------------------------------------------------------
           SELECT PCHKHGH-DL
                  ASSIGN       TO  DATABASE-PCHKHGHL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- SYNTAXHIGHLIGHTING
       FD  PCHKHGH-DL
           LABEL RECORDS ARE STANDARD.
       01  PCHKHGH-L.
           COPY DDS-ALL-FORMATS OF PCHKHGHL1.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0033".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.

     ‚* SCHALTER
       01  HGH-DATEN                     PIC 9(1).
       01  RELOAD-WRK                    PIC 9(1).

     ‚* PARAMETER FÜR DAS CRP0020
       01  SYTYP-WRK                     LIKE SYTYP  OF PCHKHGH-L.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PTR-MAIN                    USAGE POINTER.
       01  X-SRC-TYPE                    LIKE SRCTYPE OF RTVMBRD-REC.
       01  X-USER                        LIKE T-USER.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PTR-MAIN
                                X-SRC-TYPE
                                X-USER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0033" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN INPUT PCHKHGH-DL
           END-IF.

     ‚* LESEN DES POINTERS
           SET      PTR-READ   OF PTRMAP-REC TO TRUE.
           MOVE     "HIGHLIGHT"              TO PTR-KEY OF PTRMAP-REC.
           SET      PTR-MAIN OF PTRMAP-REC   TO X-PTR-MAIN.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC NOT = NULL
                    SET      PTR-DELETE OF PTRMAP-REC TO TRUE
                    MOVE     "HIGHLIGHT" TO PTR-KEY OF PTRMAP-REC
                    PERFORM  COPY-PTR-MAPPING
                    SET      PTR-RETURN OF PTRMAP-REC TO NULL
           END-IF.

     ‚* NEUEN POINTER ERSTELLEN
           SET      PTR-CREATE OF PTRMAP-REC TO TRUE.
           MOVE     "HIGHLIGHT" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.

           PERFORM  GET-SYNTAX-HGH-CMD.
           SET      X-PTR-MAIN TO PTR-MAIN OF PTRMAP-REC.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER SYNTAX-HIGHLIGHTING-WÖRTER
     ‚*--------------------------------------------------------------
       GET-SYNTAX-HGH-CMD SECTION.
       GET-SHC-00.

           INITIALIZE RELOAD-WRK.

       GET-SHC-10.
           INITIALIZE SYTYP-WRK.
     ‚* ERMITTELN DES ZU VERWENDENDEN SOURCE-TYPEN
           CALL     "CRP0020" USING X-USER
                                    X-SRC-TYPE
                                    SYTYP-WRK
           END-CALL.

           INITIALIZE HGH-DATEN.
     ‚* ERMITTELN DER SYNTAX-EINTRÄGE
           INITIALIZE PCHKHGHF OF PCHKHGH-L.
           MOVE     X-USER              TO SYUSER OF PCHKHGH-L.
           MOVE     SPACES              TO SYTYP  OF PCHKHGH-L.
           MOVE     ZEROES              TO SYLEN  OF PCHKHGH-L.
           MOVE     SPACES              TO SYCMD  OF PCHKHGH-L.
           START    PCHKHGH-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    IF   HGH-DATEN       = ZEROES
                     AND RELOAD-WRK      = ZEROES
                         MOVE 1 TO RELOAD-WRK
                         CALL "CRP0003"
                         END-CALL
                         GO TO GET-SHC-10
                    END-IF
                    GO TO GET-SHC-90
           END-IF.

       GET-SHC-20.
           READ     PCHKHGH-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    IF   HGH-DATEN       = ZEROES
                     AND RELOAD-WRK      = ZEROES
                         MOVE 1 TO RELOAD-WRK
                         CALL "CRP0003"
                         END-CALL
                         GO TO GET-SHC-10
                    END-IF
                    GO TO GET-SHC-90
           END-IF.

           IF       SYUSER OF PCHKHGH-L NOT = X-USER
                    IF   HGH-DATEN       = ZEROES
                     AND RELOAD-WRK      = ZEROES
                         MOVE 1 TO RELOAD-WRK
                         CALL "CRP0003"
                         END-CALL
                         GO TO GET-SHC-10
                    END-IF
                    GO TO GET-SHC-90
           END-IF.

     ‚* EINTRAG AKTIV?
           IF       SYAKT  OF PCHKHGH-L     = ZEROES
                    GO TO GET-SHC-20
           END-IF.

           MOVE     1 TO HGH-DATEN.
     ‚* MAPDTA-POINTER FÜLLEN
           INITIALIZE MAPDTA-REC.
           EVALUATE SYTYP  OF PCHKHGH-L
                    WHEN SYTYP-WRK
                         MOVE SYTYP-WRK  TO MAP-KEY OF MAPDTA-REC(1:20)
                    WHEN "E+P"
                         MOVE "E+P"      TO MAP-KEY OF MAPDTA-REC(1:20)
                    WHEN "OTHER"
                         MOVE "OTHER"    TO MAP-KEY OF MAPDTA-REC(1:20)
           END-EVALUATE.
           MOVE     SYCMD  OF PCHKHGH-L  TO MAP-KEY OF MAPDTA-REC(21:).
           MOVE     SYHEX  OF PCHKHGH-L  TO MAP-REC OF MAPDTA-REC.
           MOVE     MAP-WRITE            TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.

           GO TO    GET-SHC-20.

       GET-SHC-90.
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
