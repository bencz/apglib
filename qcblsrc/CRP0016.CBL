       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0016.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* KOPIEREN EINES POINTERS                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  21.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  POINTER KOPIEREN                             *
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

       01  PGM-WRK                       PIC X(10) VALUE "CRP0016".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  MAP-LFDN-WRK                  LIKE MAP-LFDN OF MAPDTA-REC.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-FROM-POINTER                USAGE POINTER.
       01  X-TO-POINTER                  USAGE POINTER.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-FROM-POINTER
                                         X-TO-POINTER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0016" TO PGM-WRK.

     ‚* IST DER VON-POINTER GEFÜLLT?
           IF       X-FROM-POINTER = NULL
                    GO TO ENDE
           END-IF.

     ‚* JETZT DEN NACH-POINTER CLEAREN
           IF       X-TO-POINTER NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO X-TO-POINTER
                    PERFORM COPY-CALL-MAPDTA
                    SET X-TO-POINTER TO NULL
           END-IF.

     ‚* JETZT IN MAPDTA DEN POINTER LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     ZEROES TO MAP-LFDN-WRK.

       ANF020.
           MOVE     MAP-LFDN-WRK       TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN      TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-FROM-POINTER.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT DEN WERT NEU SCHREIBEN
           MOVE     MAP-WRITE          TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-TO-POINTER.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-TO-POINTER TO MAP-PTR OF MAPDTA-REC.

           ADD      1 TO MAP-LFDN-WRK.
           GO TO    ANF020.

       ENDE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
