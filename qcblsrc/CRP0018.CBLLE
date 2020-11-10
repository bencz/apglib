       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0018.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ZÄHLEN DER POINTEREINTRÄGE                                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ZÄHLEN EINTRÄGE                              *
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

       01  PGM-WRK                       PIC X(10) VALUE "CRP0018".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  MAP-LFDN-WRK                  LIKE MAP-LFDN OF MAPDTA-REC.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-POINTER                     USAGE POINTER.
       01  X-ANZ-REC                     PIC 9(9).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-POINTER
                                         X-ANZ-REC.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0018" TO PGM-WRK.

           INITIALIZE X-ANZ-REC.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     ZEROES TO MAP-LFDN-WRK.

       ANF020.
           MOVE     MAP-LFDN-WRK       TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN      TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-POINTER.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           IF       MAP-KEY OF MAPDTA-REC(1:1) NOT = "*"
                    ADD      1 TO X-ANZ-REC
           END-IF.

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
