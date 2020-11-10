     ‚*--------------------------------------------------------------
     ‚* MAPDTA AUSFÜHREN
     ‚*--------------------------------------------------------------
       COPY-CALL-MAPDTA SECTION.
       CPY-MAP-00C.

           INITIALIZE RET-CODE.
     ‚* LÄNGE DES DATENSATZES ERMITTELN
           INITIALIZE MAP-LEN OF MAPDTA-REC.
           COMPUTE  MAP-LEN OF MAPDTA-REC =
                    LENGTH OF MAP-REC OF MAPDTA-REC.
           CALL     PROCEDURE "MAPDTA" USING MAP-ACT     OF MAPDTA-REC
                                             MAP-PTR     OF MAPDTA-REC
                                             MAP-KEY     OF MAPDTA-REC
                                             MAP-REC     OF MAPDTA-REC
                                             MAP-RPT     OF MAPDTA-REC
                                             MAP-LEN     OF MAPDTA-REC
                                             MAP-LFDN    OF MAPDTA-REC
                                             RET-CODE
           END-CALL.

       CPY-MAP-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* CFP9200 AUSFÜHREN (POINTERVERWALTUNG)
     ‚*--------------------------------------------------------------
       COPY-PTR-MAPPING SECTION.
       PTR-MAP-00C.

     ‚* SOLL EIN POINTER MIT EINER ID VERKNÜPFT WERDEN?
           IF       PTR-ADD OF PTRMAP-REC
                    SET PTR-PARM  OF PTRMAP-REC TO
                        PTR-INPUT OF PTRMAP-REC
           END-IF.

     ‚* AUFRUF POINTERVERWALTUNGSPROGRAMM
           CALL     PROCEDURE "CFP9200" USING
                                      PTR-MAIN        OF PTRMAP-REC
                                      PTR-PARM        OF PTRMAP-REC
                                      PTR-KEY         OF PTRMAP-REC
                                      PTR-ACTION      OF PTRMAP-REC
                                      PTR-SPLF-NAME   OF PTRMAP-REC
                                      PTR-SPLF-MODE   OF PTRMAP-REC
                                      PTR-SPLF-USRDTA OF PTRMAP-REC
           END-CALL.

     ‚* ERMITTELTEN POINTER ZURÜCKGEBEN
           IF       NOT PTR-ADD OF PTRMAP-REC
                    SET PTR-RETURN OF PTRMAP-REC TO
                        PTR-PARM   OF PTRMAP-REC
           END-IF.

       PTR-MAP-90C.
           EXIT.
      /
