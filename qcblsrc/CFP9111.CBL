       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9111.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ABSTURZ HANDLEN                                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  30.07.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ABSTURZ ABFANGEN UND PROTOKOLLIEREN          *
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

       01  PGM-WRK                       PIC X(10) VALUE "CFP9111".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  FC                            PIC X(12) VALUE LOW-VALUE.
       01  FC-REDEF                      REDEFINES FC.
        02 SEV                           PIC 9(4) BINARY.
        02 MSGNO                         PIC 9(4) BINARY.
        02 FLAGS                         PIC X(01).
        02 FACIL                         PIC X(03).
        02 ISI                           PIC 9(9) BINARY.



       01  MSGSTR.
        02  VARSTR-LENGTH       PIC S9(4) BINARY.
        02  VARSTR-TEXT.
         03  VARSTR-CHAR         PIC X,
                                   OCCURS 0 TO 256 TIMES
                                   DEPENDING ON VARSTR-LENGTH
                                         OF MSGSTR.
       01  FEEDBACK.
        02  CONDITION-TOKEN-VALUE.
     ‚*€    COPY  CEEIGZCT.
         03  CASE-1-CONDITION-ID.
          04  SEVERITY    PIC S9(4) BINARY.
          04  MSG-NO      PIC S9(4) BINARY.
         03  CASE-2-CONDITION-ID
             REDEFINES CASE-1-CONDITION-ID.
          04  CLASS-CODE  PIC S9(4) BINARY.
          04  CAUSE-CODE  PIC S9(4) BINARY.
         03  CASE-SEV-CTL    PIC X.
         03  FACILITY-ID     PIC XXX.
        02  I-S-INFO            PIC S9(9) BINARY.

        77  RTN-NAME        PIC X(80).
        77  MSGDEST         PIC S9(9) BINARY.
        77  STRING-POINTER  PIC S9(4) BINARY.

     ‚* PARAMETER FÜR CFP9110
       01  EXTERNAL-PGM-ERRCDE           PIC X(10).
       01  F12                           PIC X(1).

     ‚* PARAMETER FÜR CCP0002
       01  UTF-WRK                       PIC X(1).
       01  HEX-OUT                       PIC X(10).
       01  BIN-LEN                       PIC S9(9) BINARY.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CURR-TOKEN                  PIC X(12).
       01  X-TOKEN-PTR                   PIC S9(9) BINARY.
       01  X-RC                          PIC S9(9) BINARY.
       01  X-NEW-TOKEN                   PIC X(12).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CURR-TOKEN
                                X-TOKEN-PTR
                                X-RC
                                X-NEW-TOKEN.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9111 " TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* FEHLER ÜBERNEHMEN
           MOVE     X-CURR-TOKEN TO FC.
           PERFORM  GET-EXTERNAL-ERROR.

     ‚* ERMITTELN WELCHES PROGRAMM MIT WELCHEN FEHLER ABGESTÜRZT IST
     ‚*€   CALL     "CEE3GRN" USING RTN-NAME, FEEDBACK
     ‚*€   END-CALL.

           CALL     "CFP9110" USING EXTERNAL-PGM-ERRCDE
                                    F12
           END-CALL.
           IF       F12 NOT = "1"
                    CALL     "CFP9107"
                    END-CALL
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* FEHLERMELDUNG ERMITTELN
     ‚*--------------------------------------------------------------
       GET-EXTERNAL-ERROR SECTION.
       GET-EXT-00.

           INITIALIZE EXTERNAL-PGM-ERRCDE.
     ‚* WERT UMRECHNEN
           MOVE     2         TO BIN-LEN.
           MOVE     SPACES    TO HEX-OUT.
           MOVE     SPACES    TO UTF-WRK.
           CALL     PROCEDURE "CCP0002" USING MSGNO OF FC-REDEF
                                              BIN-LEN
                                              HEX-OUT
                                              UTF-WRK
           END-CALL.
           MOVE     FACIL OF FC-REDEF TO EXTERNAL-PGM-ERRCDE(1:3).
           MOVE     HEX-OUT           TO EXTERNAL-PGM-ERRCDE(4:).

       GET-EXT-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
