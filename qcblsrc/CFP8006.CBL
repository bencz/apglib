       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8006.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERSTELLEN EINER TEMPORÄREN DATEI                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERSTELLEN EINER TEMPORÄREN DATEI             *
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
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP8006".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-LIB                         PIC X(10).
       01  X-FILE                        PIC X(10).
       01  X-SIZE                        PIC X(6).
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-LIB
                                X-FILE
                                X-SIZE
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP8006" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           INITIALIZE X-RETCODE.
     ‚* ERSTELLEN DER TEMPORÄREN DATEI
           INITIALIZE CRTPF-REC.
           MOVE     X-LIB           TO TOLIB    OF CRTPF-REC.
           MOVE     X-FILE          TO TOPGM    OF CRTPF-REC.
           MOVE     X-SIZE          TO RECSIZE  OF CRTPF-REC.
           CALL     "CLP0031" USING CRTPF-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
