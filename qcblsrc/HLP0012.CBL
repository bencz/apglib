       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0012.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERMITTELN OBJEKTTYP (DMP1500)                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  POINTER AUSGEBEN                             *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*

       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚* TEMPORÄRE DATEI PMAPDTA
     ‚*-------------------------------------------------------------
           SELECT PMAPDTA-DP
                  ASSIGN       TO  DATABASE-PMAPDTA
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEMPORÄRE PMAPDTA
       FD  PMAPDTA-DP
           LABEL RECORDS ARE STANDARD.
       01  PMAPDTA-P.
           COPY DDS-ALL-FORMATS OF PMAPDTA.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚*  WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0012   ".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

     ‚* VARIABLEN FÜR LGCRTDUP BZW. CRTPF
       01  LIB-WRK                       PIC X(10).
       01  FILE-WRK                      PIC X(10).
       01  LEN-ALPHA                     PIC X(6).

       01  LFDN-MAP                      LIKE MAP-LFDN OF MAPDTA-REC.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-POINTER                     USAGE POINTER.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-POINTER.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* POINTER ÜBERGEBEN?
           IF       X-POINTER = NULL
                    GO TO ENDE
           END-IF.

     ‚* TEMP. DATEI ERSTELLEN
           MOVE     "PMAPDTA"   TO FILE-WRK.
           MOVE     "QTEMP"     TO LIB-WRK.
           MOVE     "002000"    TO LEN-ALPHA.
           CALL     "CFP8006"  USING LIB-WRK
                                     FILE-WRK
                                     LEN-ALPHA
                                     RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* UND DIE DATEI ÖFFNEN
           OPEN     OUTPUT PMAPDTA-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* SCHREIBEN LGMAPDTA
           PERFORM  FILL-PMAPDTA.

     ‚* DATEI SCHLIEßEN
           CLOSE    PMAPDTA-DP.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ALLES ÜBERSETZT IST
     ‚*--------------------------------------------------------------
       FILL-PMAPDTA SECTION.
       FIL-MAP-00.

           INITIALIZE LFDN-MAP.
           SET      MAP-PTR OF MAPDTA-REC TO X-POINTER.

       FIL-MAP-20.
           MOVE     LFDN-MAP      TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACE
                    GO TO FIL-MAP-90
           END-IF.

     ‚* DATENSATZ SCHREIBEN
           INITIALIZE PMAPDTA-P.
           MOVE     MAP-LFDN OF MAPDTA-REC TO PMAPDTA-P(1:20).
           MOVE     MAP-KEY  OF MAPDTA-REC TO PMAPDTA-P(21:100).
           MOVE     MAP-REC  OF MAPDTA-REC TO PMAPDTA-P(121:).
           WRITE    PMAPDTA-P.

     ‚* WIEDERHOLEN FÜR ALLE EINTRÄGE IN MAPDTA
           ADD      1 TO LFDN-MAP.
           GO TO    FIL-MAP-20.

       FIL-MAP-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* Aufruf LGMAPDTA
     ‚*---------------------------------------------------------------

           COPY MAPDTA OF APG-QCPYSRC.
      /
