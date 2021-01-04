       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9200.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* POINTERVERWALTUNG (DMP9000)                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  23.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ZENTRALE POINTERVERWALTUNG                   *
     ‚*                                                               *
     ‚* PARAMETER X-ACTION:                                           *
     ‚* "01" <READ> RÜCKGABE POINTER MITTELS˜ID‚                      *
     ‚* "02" <CREATE> NEUE˜ID‚UND˜POINTER‚ANLEGEN                     *
     ‚* "03" <DELETE>˜DATEN‚UND˜ID‚LÖSCHEN                            *
     ‚* "04" <ADD>   ˜POINTER‚MIT˜ID‚VERKNÜPFEN                       *
     ‚* "99" <CLEAR> GESAMTEN˜SPEICHER FREIGEBEN‚(ALLE ID'S + MAIN)   *
     š* "DB" <DEBUG> ID<BLANK> ERSTELLEN ID ÜBERSICHT                 *
     š*              ID<NAME> AUSAGE DATENINHALT DER ID               *
     š*              AUSGABE ERFOLGT IN: QTEMP/PMAPDTA                *
     š* "PR" <PRINT> ID<BLANK> ID ÜBERSICHT IM SPOOL AUSGEBEN         *
     š*              ID<NAME> ID-DATEN IM SPOOL AUSGEN                *
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
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                         ATTRIBUTE-DATA IS BS-ATRIBUTE,
                         I-O-FEEDBACK   IS FILE-IO-FEEDBACK.
      /

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

     ‚* WORKBEREICH STANDARDFELDER ALLE PROGRAMME
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9200   ".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  ACTION-WRK                    PIC X(1).
       01  PTR-WRK                       PIC X(70).

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-PTR-MAIN                    LIKE MAP-PTR OF MAPDTA-REC.
       01  X-PTR-PARM                    LIKE MAP-PTR OF MAPDTA-REC.
       01  X-ID                          LIKE MAP-KEY OF MAPDTA-REC.
       01  X-ACTION                      PIC X(2).
       01  X-SPLF-NAME             LIKE PTR-SPLF-NAME   OF PTRMAP-REC.
       01  X-SPLF-MODE             LIKE PTR-SPLF-MODE   OF PTRMAP-REC.
       01  X-SPLF-USRDTA           LIKE PTR-SPLF-USRDTA OF PTRMAP-REC.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-PTR-MAIN
                                X-PTR-PARM
                                X-ID
                                X-ACTION
                                X-SPLF-NAME
                                X-SPLF-MODE
                                X-SPLF-USRDTA.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* NOCH KEIN MAIN-POINTER VORHANDEN, DANN EINEN GENERIEREN
           IF       X-PTR-MAIN = NULL
                    PERFORM GET-POINTER
                    SET X-PTR-MAIN TO MAP-PTR
           END-IF.

           MOVE     X-ACTION TO PTR-ACTION.

     ‚* JE NACH AKTION ENTSPRECHEND VERZWEIGEN
           EVALUATE TRUE
                    WHEN PTR-READ   PERFORM READ-ID
                    WHEN PTR-CREATE PERFORM CREATE-ID
                    WHEN PTR-DELETE PERFORM DELETE-ID
                    WHEN PTR-ADD    PERFORM ADD-ID
                    WHEN PTR-CLEAR  PERFORM CLEAR-ALL
                    WHEN PTR-DEBUG  PERFORM DEBUG-OUTPUT
                    WHEN PTR-PRINT  PERFORM PRINT
           END-EVALUATE.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SPEICHER ERZEUGEN UM ID ABZULEGEN
     ‚*--------------------------------------------------------------
       GET-POINTER SECTION.
       GET-PTR-00.

           SET      MAP-PTR TO NULL.

           INITIALIZE MAPDTA-REC.
           MOVE     MAP-GET-POINTER TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.

       GET-PTR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÜBER DIE ID MAPDTA LESEN UND DEN POINTER ZURÜCKGEBEN
     ‚*--------------------------------------------------------------
       READ-ID SECTION.
       REA-IDS-00.

           SET      MAP-PTR  TO X-PTR-MAIN.

     ‚* AUSLESEN DER ID UND POINTER ZURÜCKGEBEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-ID     TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-READ TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    SET X-PTR-PARM TO MAP-RPT OF MAPDTA-REC
           ELSE
                    SET X-PTR-PARM TO NULL
           END-IF.

       REA-IDS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ID IN MAPDTA ANLEGEN
     ‚*--------------------------------------------------------------
       CREATE-ID SECTION.
       CRT-IDS-00.

     ‚* PRÜFEN OB POINTER SCHON ANGELEGT IST
           PERFORM  READ-ID.
           IF       X-PTR-PARM = NULL
     ‚*             NEUEN POINTER FÜR NEUE ID GENERIEREN
                    PERFORM GET-POINTER
                    SET X-PTR-PARM TO MAP-PTR
     ‚*             JETZT ID MIT POINTER ABLEGEN
                    PERFORM ADD-ID
           END-IF.

       CRT-IDS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ID AUS MAPDTA LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-ID SECTION.
       DLT-IDS-00.

     ‚* POINTER ÜBER ID ERMITTELN UM SPEICHER WIEDER FREIZUGEBEN
           PERFORM  READ-ID.
           IF       RET-CODE NOT = SPACES
                    GO TO DLT-IDS-90
           END-IF.

           SET      MAP-PTR TO X-PTR-PARM.

     ‚* ZUNÄCHST DEN BELEGTEN SPEICHER WIEDER FREIGEBEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-ID        TO MAP-KEY    OF MAPDTA-REC.
           MOVE     MAP-CLEAR   TO MAP-ACT    OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.

           SET      MAP-PTR TO X-PTR-MAIN.

     ‚* JETZT DEN EINTRAG AUS DER MAIN-TABELLE LÖSCHEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-ID        TO MAP-KEY    OF MAPDTA-REC.
           MOVE     MAP-DELETE  TO MAP-ACT    OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.

           SET      X-PTR-PARM TO NULL.

       DLT-IDS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ID IN MAPDTA ANLEGEN (MIT ÜBERGEBENEN POINTER)
     ‚*--------------------------------------------------------------
       ADD-ID SECTION.
       ADD-IDS-00.

           SET      MAP-PTR TO X-PTR-MAIN

     ‚* JETZT ID UND POINTER ABLEGEN
           INITIALIZE MAPDTA-REC.
           SET      MAP-RPT   OF MAPDTA-REC TO X-PTR-PARM.
           MOVE     X-ID      TO MAP-KEY    OF MAPDTA-REC.
           MOVE     MAP-WRITE TO MAP-ACT    OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.

       ADD-IDS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAPDTA ZUR ANALYSEZWECKE AUSGEBEN (JE ID)
     ‚*--------------------------------------------------------------
       DEBUG-OUTPUT SECTION.
       DEB-UGS-00.

     ‚* POINTER ERMITTELN
           IF       X-ID NOT = SPACES
                    PERFORM READ-ID
                    IF   RET-CODE NOT = SPACES
                         GO TO DEB-UGS-90
                    END-IF
           ELSE
     ‚*             KEINE ID ÜBERGEBEN, DANN ERFOLGT DIE AUSGABE DES
     ‚*             MAIN-POINTERS. DIE DATEN WO DIE POINTER HINTER DER
     ‚*             ID ABGELEGT SIND
                    SET X-PTR-PARM TO X-PTR-MAIN
           END-IF.

     ‚* AUSGABE ERFOLGT IN DATEI QTEMP/PMAPDTA
           CALL     "HLP0012" USING X-PTR-PARM
           END-CALL.

       DEB-UGS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAPDTA ZUR ANALYSEZWECKE IM SPOOL AUSGEBEN (JE ID)
     ‚*--------------------------------------------------------------
       PRINT SECTION.
       PRT-DTA-00.

     ‚* POINTER ERMITTELN
           IF       X-ID NOT = SPACES
                    PERFORM READ-ID
                    IF   RET-CODE NOT = SPACES
                         GO TO PRT-DTA-90
                    END-IF
           ELSE
     ‚*             KEINE ID ÜBERGEBEN, DANN ERFOLGT DIE AUSGABE DES
     ‚*             MAIN-POINTERS. DIE DATEN WO DIE POINTER HINTER DER
     ‚*             ID ABGELEGT SIND
                    SET X-PTR-PARM TO X-PTR-MAIN
           END-IF.

     ‚* DATEN AUS MAPDTA IM SPOOL AUSGEBEN
           CALL     "DMP9002" USING X-PTR-PARM
                                    X-SPLF-NAME
                                    X-SPLF-MODE
                                    X-SPLF-USRDTA
           END-CALL.

       PRT-DTA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GESAMTEN SPEICHER FREIGEN
     ‚*--------------------------------------------------------------
       CLEAR-ALL SECTION.
       CLR-ALL-00.

           SET      MAP-PTR TO X-PTR-MAIN.

           INITIALIZE MAPDTA-REC.
           MOVE     MAP-READ-LFDN TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACE
     ‚*             STRING IN POINTER KONVERTIEREN
                    SET X-PTR-PARM TO MAP-RPT OF MAPDTA-REC
     ‚*             ID LÖSCHEN
                    MOVE MAP-DELETE TO MAP-ACT OF MAPDTA-REC
                    PERFORM COPY-CALL-MAPDTA
     ‚*             SPEICHER DER ID FREIGEBEN
                    SET  MAP-PTR TO X-PTR-PARM
                    MOVE MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM COPY-CALL-MAPDTA
     ‚*             IM LOOP BIS ALLE ID'S UND DATEN GELÖSCHT SIND
                    GO TO CLR-ALL-00
           END-IF.

     ‚* MAIN FREIGEBEN
           SET      MAP-PTR     TO X-PTR-MAIN.
           MOVE     MAP-CLEAR   TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.

           SET      X-PTR-MAIN TO NULL.

       CLR-ALL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
