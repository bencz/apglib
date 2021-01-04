       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8100.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ARCHIVIEREN EINES DATENSATZES                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ARCHIVIEREN EINES DATENSATZES                *
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
     ‚* PHYSISCHE DATEI ARCHIV
     ‚*-------------------------------------------------------------
           SELECT PARCHIV-DP
                  ASSIGN       TO  DATABASE-PARCHIV
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- ARCHIV
       FD  PARCHIV-DP
           LABEL RECORDS ARE STANDARD.
       01  PARCHIV-P.
           COPY DDS-ALL-FORMATS OF PARCHIV.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP8100".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-HSLIB                       LIKE HSLIB  OF PARCHIV-P.
       01  X-HSFILE                      LIKE HSFILE OF PARCHIV-P.
       01  X-HSAPGM                      LIKE HSAPGM OF PARCHIV-P.
       01  X-RECORD                      LIKE RECORD-WRK.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-HSLIB
                                X-HSFILE
                                X-HSAPGM
                                X-RECORD.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP8100" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PARCHIV-DP.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-HSLIB   = SPACES
            OR      X-HSFILE  = SPACES
            OR      X-HSAPGM  = SPACES
                    GO TO ENDE
           END-IF.

     ‚* NÄCHSTE LAUFENDE NUMMER ERMITTELN
           MOVE     "PARCHIV" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.

     ‚* SCHREIBEN DES DATENSATZES
           INITIALIZE PARCHIVF OF PARCHIV-P.
           MOVE     LFDN-WRK               TO HSLFDN OF PARCHIV-P.
           MOVE     X-HSLIB                TO HSLIB  OF PARCHIV-P.
           MOVE     X-HSFILE               TO HSFILE OF PARCHIV-P.
           MOVE     X-RECORD               TO HSREC  OF PARCHIV-P.
           MOVE     T-USER                 TO HSAUSR OF PARCHIV-P.
           MOVE     T-TERM                 TO HSABS  OF PARCHIV-P.
           MOVE     X-HSAPGM               TO HSAPGM OF PARCHIV-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8                 TO HSADTA OF PARCHIV-P.
           MOVE     TIME-6                 TO HSATIA OF PARCHIV-P.

           WRITE    PARCHIV-P.

       ENDE.
           CLOSE                 PARCHIV-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
