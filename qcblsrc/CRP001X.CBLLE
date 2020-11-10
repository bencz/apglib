       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP001X.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCE ANZEIGEN/EDITIEREN                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGEN UND ÄNDERN EINER SOURCE (SEU)       *
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

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-SRCSEQ                      LIKE SRCSEQ OF QSETSRC-P.
       01  X-OPTION                      PIC X(1).
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-SRCSEQ
                                X-OPTION
                                X-F3
                                X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           IF       BED1 = ZEROES
                    IF   BED2 = ZEROES
                         IF   BED3 = ZEROES
                              IF   BED4 = ZEROES
                                   CONTINUE
                              END-IF
                         END-IF
                    END-IF
           END-IF.

       ENDE.
           GOBACK.
      /
