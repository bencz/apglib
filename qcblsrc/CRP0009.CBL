       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0009.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN OB POINTER-EINTRAG VORHANDEN IST                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SUCHEN EINES RECORDS IM POINTER              *
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

       01  PGM-WRK                       PIC X(10) VALUE "CRP0009".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  MAP-LFDN-WRK                  LIKE MAP-LFDN OF MAPDTA-REC.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-POINTER                     USAGE POINTER.
       01  X-SUCHE                       LIKE MAP-REC  OF MAPDTA-REC.
       01  X-KEY                         LIKE MAP-KEY  OF MAPDTA-REC.
       01  X-LFDN                        LIKE MAP-LFDN OF MAPDTA-REC.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-POINTER
                                         X-SUCHE
                                         X-KEY
                                         X-LFDN
                                         X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0009" TO PGM-WRK.

           INITIALIZE X-KEY.
           INITIALIZE X-LFDN.
           INITIALIZE X-RETCODE.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     ZEROES TO MAP-LFDN-WRK.

       ANF020.
           MOVE     MAP-LFDN-WRK       TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN      TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-POINTER.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* IST DER GESUCHTE TEXT VORHANDEN?
           MOVE     MAP-REC OF MAPDTA-REC           TO RECORD-WRK.
           MOVE     LENGTH OF MAP-REC OF MAPDTA-REC TO RECORD-LEN.
           MOVE     X-SUCHE                         TO SELEKTION-WRK.
           CALL     PROCEDURE "HLP0001" USING RECORD-WRK
                                              RECORD-LEN
                                              SELEKTION-WRK
                                              RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE MAP-KEY  OF MAPDTA-REC TO X-KEY
                    MOVE MAP-LFDN OF MAPDTA-REC TO X-LFDN
                    GO TO ENDE
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
