       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0015 IS RECURSIVE.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN OB POINTER-KEY VORHANDEN IST                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  04.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN OB KEY VORHANDEN IST                  *
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

       01  PGM-WRK                       PIC X(10) VALUE "CRP0015".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  SRCSEQ-TMP                    PIC 9(4)V9(2).

      /
       LOCAL-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* SOURCE ZUM ARBEITEN
       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

       01  SRCSEQ-WRK                    PIC 9(4)V9(2).
       01  MAPREC-SAV                    LIKE MAP-REC OF MAPDTA-REC.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-POINTER                     USAGE POINTER.
       01  X-SRCSEQ                      PIC 9(4)V9(2).
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR
                                         X-POINTER
                                         X-SRCSEQ
                                         X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0015" TO PGM-WRK.

           MOVE     X-SRCSEQ           TO SRCSEQ-WRK.
           MOVE     X-SRCSEQ           TO SRCSEQ-TMP.

           INITIALIZE X-RETCODE.
     ‚* JETZT IN MAPDTA SUCHEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-POINTER.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
     ‚*             DIE ZEILE IST VORHANDEN, JETZT PRÜFEN OB DIE
     ‚*             NÄCHSTE ZEILE VORHANDEN IST. WENN DIESE NICHT
     ‚*             DA IST, DANN KÖNNEN WIR DIE ZEILE ERSETZEN
                    MOVE MAP-REC OF MAPDTA-REC TO MAPREC-SAV
                    ADD  0,01          TO SRCSEQ-WRK
                    CALL "CRP0015" USING X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR
                                         X-POINTER
                                         SRCSEQ-WRK
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
     ‚*                  MAPDTA LÖSCHEN
                         MOVE X-SRCLIB  TO MAP-KEY OF MAPDTA-REC(1:10)
                         MOVE X-SRCRCN  TO MAP-KEY OF MAPDTA-REC(11:10)
                         MOVE X-SRCMBR  TO MAP-KEY OF MAPDTA-REC(21:10)
                         MOVE X-SRCSEQ  TO MAP-KEY OF MAPDTA-REC(31:6)
                         MOVE X'FFFFFFFFFFFF' TO
                              MAP-KEY OF MAPDTA-REC(38:6)
                         MOVE MAP-READ   TO MAP-ACT OF MAPDTA-REC
                         MOVE MAP-DELETE TO MAP-ACT OF MAPDTA-REC
                         PERFORM COPY-CALL-MAPDTA
     ‚*                  MAPDTA NEUSCHREIBEN
                         MOVE X-SRCLIB  TO MAP-KEY OF MAPDTA-REC(1:10)
                         MOVE X-SRCRCN  TO MAP-KEY OF MAPDTA-REC(11:10)
                         MOVE X-SRCMBR  TO MAP-KEY OF MAPDTA-REC(21:10)
                         MOVE SRCSEQ-WRK TO MAP-KEY OF MAPDTA-REC(31:6)
                         MOVE X'FFFFFFFFFFFF' TO
                              MAP-KEY OF MAPDTA-REC(38:6)
                         MOVE MAPREC-SAV(10:)  TO QSETSRC-WRK
                         MOVE SRCSEQ-WRK  TO SRCSEQ OF QSETSRC-WRK
                         MOVE QSETSRC-WRK TO MAP-REC OF MAPDTA-REC(10:)
                         MOVE MAP-WRITE  TO MAP-ACT OF MAPDTA-REC
                         PERFORM COPY-CALL-MAPDTA
                         MOVE "1" TO X-RETCODE
                    END-IF
           ELSE
                    MOVE "1" TO X-RETCODE
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
