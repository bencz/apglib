/*‚*-----------------------------------------------------------------*/
/*‚* DYNAMISCHER SPEICHER (MAPDTA)                                 */
/*‚*---------------------------------------------------------------  */
/*‚*                                                                 */
/*‚* AUTOR         :  A. PIEGER                                      */
/*‚*                                                                 */
/*‚* ERSTELLT AM   :  12.11.2009                                     */
/*‚*                                                                 */
/*‚* FUNKTION      :  SPEICHER                                       */
/*‚*                                                                 */
/*‚* ÄNDERUNGEN:                                                     */
/*‚*‚DATUM      VON   GRUND DER ÄNDERUNG                             */
/*‚*                                                                 */
/*‚****************************************************************  */
#include "APGGLOBAL.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

typedef struct MAPDTA_arrayRec
{
  char recKey[100];
  char *recData;
};

typedef struct MAPDTA_pgmStruct
{
  int arrayCurrentSize;
  int arrayMaxSize;
  int arraySorted;
  int arrayCount;
  char maxKeyVal[100];
  struct MAPDTA_arrayRec *dataArray;
};

struct MAPDTA_arrayRec MAPDTA_searchRec;
struct MAPDTA_arrayRec *MAPDTA_searchResult;
int MAPDTA_retcode;
int MAPDTA_i;

static int sortcomp(const void *value1, const void *value2) {
            struct MAPDTA_arrayRec *mi1 = (struct MAPDTA_arrayRec *) value1;
            struct MAPDTA_arrayRec *mi2 = (struct MAPDTA_arrayRec *) value2;
            return memcmp(mi1->recKey, mi2->recKey, 100);
}

/*‚Wert in dynamischen Container schreiben, auslesen oder löschen    */
/*‚Parameter:                                                        */
/*‚1. Aktionscode, PIC X(1), (1 = Schreiben,                         */
/*‚                           2 = Abfragen,                          */
/*‚                           3 = Löschen,                           */
/*‚                           4 = Alles für ein Programm löschen),   */
/*‚                           5 = Nächsten Satz lesen,               */
/*‚                           6 = Vorherigen Satz lesen              */
/*‚                           7 = Mit ArrayCount aufsetzten          */
/*‚                         8/9 = Key über ArrayCount ändern         */
/*‚                               zuvor muss auf den Array aufgesetzt*/
/*‚                               werden (Auswahl 2,5,6,7)           */
/*‚                               8=ohne Sort                        */
/*‚                               9=mit  Sort                        */
/*‚                           S = Array sortieren                    */
/*‚                                                                  */
/*‚2. Zeigerstruktur, USAGE POINTER                                  */
/*‚   dient zur Zuordnung von einem Speicherbereich zu einem Programm*/
/*‚   Wird beim ersten Aufruf zurückgegeben und muss bei den         */
/*‚   folgenden Aufrufen wieder mitgegeben werden                    */
/*‚3. Zugriffschlüssel für die Daten, PIC X(100)                      */
/*‚4. Datensatz (Input/Output), Beliebige Länge                      */
/*‚5. Länge des Datensatzes, PIC S9(9) COMP-4                        */
/*‚6. Zahl zum Aufsetzten über ArrayCount PIC S9(9) COMP-4           */
/*‚7. Return-Code, PIC X(1)                                          */
DLLEXPORT
void MAPDTA (char *x_action, struct MAPDTA_pgmStruct **x_pgmStruct, char *x_key,
               char *x_data, int *x_dataLen, int *x_lfdn, char *x_retcode)
{
  /*‚Wurde die PGMSTRUCT schonmal ermittelt? */
  if(*x_pgmStruct == NULL)
  {
    /*‚Speicher reservieren und zurückgeben */
    *x_pgmStruct = calloc(1, sizeof(struct MAPDTA_pgmStruct));
    if(*x_pgmStruct == NULL)
    {
      x_retcode[0] = '1';
      return;
    }
    /*‚Elemente werden durch calloc initialisiert */
    (*x_pgmStruct)->arraySorted = 1;
  }

  /*‚Je nach Aktionscode nun die jeweilige Aktion durchführen */
  switch(x_action[0])
  {
    /*‚Setzen */
    case '1':
    /*‚Ist noch genug Platz im Array? */
      if((*x_pgmStruct)->arrayMaxSize == (*x_pgmStruct)->arrayCurrentSize)
      {
        /*‚Falls nicht, dann Array vergrößern */
        (*x_pgmStruct)->arrayMaxSize+= 100;
        /*‚Realloc funktioniert auch beim ersten Aufruf wenn dataArray noch NULL ist*/
        (*x_pgmStruct)->dataArray =
          realloc((*x_pgmStruct)->dataArray,
          (*x_pgmStruct)->arrayMaxSize * sizeof(struct MAPDTA_arrayRec));
      }

      /*‚Ist das Element größer als, das bisher größte eingefügte, dann muss es neu sein */
      MAPDTA_retcode = memcmp((*x_pgmStruct)->maxKeyVal, x_key, 100);
      if(MAPDTA_retcode < 0)
      {
        (*x_pgmStruct)->arrayCurrentSize++;
        memcpy((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recKey, x_key, 100);
        (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recData = malloc(*x_dataLen);
        memcpy((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recData, x_data, *x_dataLen);
        /*‚Neuen größten Key festhalten */
        memcpy(&(*x_pgmStruct)->maxKeyVal, x_key, 100);
      }else
      {
        /*‚Prüfen ob das Element schon vorhanden ist */
        /*‚Ggf. vorher sortieren */
        if((*x_pgmStruct)->arraySorted == 0)
        {
          /*‚Array nun neu sortieren */
          qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
                sizeof(struct MAPDTA_arrayRec), sortcomp);
          (*x_pgmStruct)->arraySorted = 1;
        }
        /*‚Wenn der Key gleich dem größten Key ist, dann kann dieser direkt ersetzt werden */
        if(MAPDTA_retcode != 0)
        {
          memcpy(&MAPDTA_searchRec.recKey, x_key, 100);
          MAPDTA_searchResult = bsearch(&MAPDTA_searchRec, (*x_pgmStruct)->dataArray,
                  (*x_pgmStruct)->arrayCurrentSize, sizeof(struct MAPDTA_arrayRec), sortcomp);
        }else
        {
          MAPDTA_searchResult = &(*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1];
        }
        /*‚Nicht gefunden -> Neuer Eintrag*/
        if(MAPDTA_searchResult == NULL)
        {
          (*x_pgmStruct)->arrayCurrentSize++;
          memcpy((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recKey, x_key, 100);
          (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recData = malloc(*x_dataLen);
          memcpy((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recData,
                 x_data, *x_dataLen);
          /*‚Da dieser Key NICHT größer ist als der bisher größte ist, muss sortiert werden */
          (*x_pgmStruct)->arraySorted = 0;
        }else
        /*‚Gefunden -> Bestehenden Eintrag ändern */
        {
          memcpy(MAPDTA_searchResult->recKey, x_key, 100);
          memcpy(MAPDTA_searchResult->recData, x_data, *x_dataLen);
          /*‚Es wurde ein bestehender Eintrag geändert => Keine Sortierung nötig */
          (*x_pgmStruct)->arraySorted = 1;
        }
      }

      x_retcode[0] = ' ';
      break;

    /*‚Abfragen/Aufsetzen */
    case '2':
      if((*x_pgmStruct)->arraySorted == 0)
      {
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      }
      memcpy(&MAPDTA_searchRec.recKey, x_key, 100);
      MAPDTA_searchResult = bsearch(&MAPDTA_searchRec, (*x_pgmStruct)->dataArray,
              (*x_pgmStruct)->arrayCurrentSize, sizeof(struct MAPDTA_arrayRec), sortcomp);
      if(MAPDTA_searchResult == NULL)
      {
        memset(x_data, ' ', *x_dataLen);
        x_retcode[0] = '3';
      }
      else
      {
        memcpy(x_data, MAPDTA_searchResult->recData, *x_dataLen);
        (*x_pgmStruct)->arrayCount = (int)(MAPDTA_searchResult - (*x_pgmStruct)->dataArray);
        *x_lfdn                    = (*x_pgmStruct)->arrayCount;
        x_retcode[0] = ' ';
      }
      break;

    /*‚Einen Eintrag entfernen */
    case '3':
      /*‚Gibt es überhaupt Daten für dieses Programm */
      if(*x_pgmStruct == NULL)
      {
        x_retcode[0] = ' ';
        return;
      }
      /*‚Gibt es schon das Datenarry Daten für dieses Programm */
      if((*x_pgmStruct)->dataArray == NULL)
      {
        free(*x_pgmStruct);
        *x_pgmStruct = NULL;
        x_retcode[0] = ' ';
        return;
      }
      /*‚Programmzeiger vorhanden, aber gibt es auch Einträge darin? */
      if((*x_pgmStruct)->arrayMaxSize == 0 || (*x_pgmStruct)->arrayCurrentSize == 0)
      {
        /*‚Keine Einträge vorhanden, dann Zeiger freigeben und Ende */
        free((*x_pgmStruct)->dataArray);
        free(*x_pgmStruct);
        *x_pgmStruct = NULL;
        x_retcode[0] = ' ';
        return;
      }

      if((*x_pgmStruct)->arraySorted == 0)
      {
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      }
      /*‚Ist der zu löschende Wert der größte im Array? */
      /*‚Dann direkt entfernen */
      if(memcmp(x_key, &(*x_pgmStruct)->maxKeyVal, 100) == 0)
      {
        /*‚Wert im Array löschen */
        free((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recData);
        (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recData = NULL;
        memset(&(*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recKey,
               0, 100);

        /*‚Array verkleinern */
        (*x_pgmStruct)->arrayCurrentSize--;

        /*‚Neuen größten Wert ermitteln */
        /*‚Sofern noch ein Element im Array ist */
        if((*x_pgmStruct)->arrayCurrentSize > 0)
        {
          memcpy(&(*x_pgmStruct)->maxKeyVal,
                 &(*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recKey,
                 100);
        }
        else
        {
          /*‚Wenn das Array ansonsten leer ist, dann wird der max. Keywert geleert */
          memset(&(*x_pgmStruct)->maxKeyVal, 0, 100);
        }
        x_retcode[0] = ' ';
        break;
      }

      /*‚Element im Array suchen */
      memcpy(&MAPDTA_searchRec.recKey, x_key, 100);
      MAPDTA_searchResult = bsearch(&MAPDTA_searchRec, (*x_pgmStruct)->dataArray,
              (*x_pgmStruct)->arrayCurrentSize, sizeof(struct MAPDTA_arrayRec), sortcomp);
      if(MAPDTA_searchResult == NULL)
      {
        memset(x_data, ' ', *x_dataLen);
        x_retcode[0] = '3';
      }
      else
      {
        /*‚Nun das Element entfernen und die Lücke schließen */
        /*‚LFDN ermitteln */
        (*x_pgmStruct)->arrayCount = (int)(MAPDTA_searchResult - (*x_pgmStruct)->dataArray);
        *x_lfdn                    = (*x_pgmStruct)->arrayCount;
        /*‚Wert im Array löschen */
        free((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recData);
        (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recData = NULL;
        memset(&(*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recKey,
               0, 100);

        /*‚Array verkleinern */
        (*x_pgmStruct)->arrayCurrentSize--;

        /*‚Neuen größten Wert ermitteln */
        /*‚Sofern noch ein Element im Array ist */
        if((*x_pgmStruct)->arrayCurrentSize > 0)
        {
          /*‚Jetzt noch alle Einträge nach dem gelöschten nach vorne schieben */
          for(MAPDTA_i=(*x_pgmStruct)->arrayCount;
              MAPDTA_i<(*x_pgmStruct)->arrayCurrentSize;MAPDTA_i++)
          {
             (*x_pgmStruct)->dataArray[MAPDTA_i] = (*x_pgmStruct)->dataArray[MAPDTA_i + 1];
          }
          memcpy(&(*x_pgmStruct)->maxKeyVal,
                 &(*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCurrentSize - 1].recKey,
                 100);
          (*x_pgmStruct)->arrayCount--;
        }
        else
        {
          /*‚Wenn das Array ansonsten leer ist, dann wird der max. Keywert geleert */
          memset(&(*x_pgmStruct)->maxKeyVal, 0, 100);
        }
        x_retcode[0] = ' ';
      }
      break;

    /*‚Alle Daten für ein Programm löschen */
    case '4':
      /*‚Gibt es überhaupt Daten für dieses Programm */
      if(*x_pgmStruct == NULL)
      {
        x_retcode[0] = ' ';
        return;
      }
      /*‚Gibt es schon das Datenarry Daten für dieses Programm */
      if((*x_pgmStruct)->dataArray == NULL)
      {
        free(*x_pgmStruct);
        *x_pgmStruct = NULL;
        x_retcode[0] = ' ';
        return;
      }
      /*‚Programmzeiger vorhanden, aber gibt es auch Einträge darin? */
      if((*x_pgmStruct)->arrayMaxSize == 0 || (*x_pgmStruct)->arrayCurrentSize == 0)
      {
        /*‚Keine Einträge vorhanden, dann Zeiger freigeben und Ende */
        free((*x_pgmStruct)->dataArray);
        free(*x_pgmStruct);
        *x_pgmStruct = NULL;
        x_retcode[0] = ' ';
        return;
      }
      /*‚Einträge vorhanden, nun alle Einträge die Belegt sind freigeben */
      for(MAPDTA_retcode = 0;MAPDTA_retcode<(*x_pgmStruct)->arrayCurrentSize;MAPDTA_retcode++)
      {
        if((*x_pgmStruct)->dataArray[MAPDTA_retcode].recData != NULL)
        {
          free((*x_pgmStruct)->dataArray[MAPDTA_retcode].recData);
        }
      }

      /*‚Nun auch noch den Datenbereich und den Programmzeiger freigeben */
      free((*x_pgmStruct)->dataArray);
      free(*x_pgmStruct);
      *x_pgmStruct = NULL;
      x_retcode[0] = ' ';
      break;

    /*‚Nächsten Satz lesen */
    case '5':
      /*‚Ggf. vorher sortieren */
      if((*x_pgmStruct)->arraySorted == 0)
      {
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      }
      /*‚Wenn ArrayCount kleiner Null, dann Null einfügen */
      if ((*x_pgmStruct)->arrayCount < 0)
      {
          (*x_pgmStruct)->arrayCount = 0;
      }

      if (((*x_pgmStruct)->arrayCount + 1) < (*x_pgmStruct)->arrayCurrentSize
       && (*x_pgmStruct)->dataArray != NULL)
      {
        (*x_pgmStruct)->arrayCount++;
        memcpy(x_data, (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recData
        , *x_dataLen);
        memcpy(x_key, (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recKey
        , 100);
        *x_lfdn = (*x_pgmStruct)->arrayCount;
        x_retcode[0] = ' ';
      }
      else
      {
        memset(x_data, ' ', *x_dataLen);
        x_retcode[0] = '5';
      }
      return;
      break;

    /*‚Vorherigen Satz lesen */
    case '6':
      /*‚Ggf. vorher sortieren */
      if((*x_pgmStruct)->arraySorted == 0)
      {
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      }
      /*‚Wenn ArrayCount größer als ArrayCurrentSize ist, dann ArrayCount auf die */
      /*‚letzte Position setzen (ArrayCurrentSize - 1) */
      if (((*x_pgmStruct)->arrayCount - 1)>= (*x_pgmStruct)->arrayCurrentSize)
      {
          (*x_pgmStruct)->arrayCount  = (*x_pgmStruct)->arrayCurrentSize;
      }

      if ((*x_pgmStruct)->arrayCount >= 0
       && (*x_pgmStruct)->dataArray != NULL)
      {
        (*x_pgmStruct)->arrayCount--;
        memcpy(x_data, (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recData
        , *x_dataLen);
        memcpy(x_key, (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recKey
        , 100);
        *x_lfdn = (*x_pgmStruct)->arrayCount;
        x_retcode[0] = ' ';
      }
      else
      {
        memset(x_data, ' ', *x_dataLen);
        x_retcode[0] = '6';
      }
      return;
      break;

    /*‚Mit ArrayCount aufsetzten */
    case '7':
      /*‚Ggf. vorher sortieren */
      if((*x_pgmStruct)->arraySorted == 0)
      {
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      }
      if (*x_lfdn >= 0 && *x_lfdn < (*x_pgmStruct)->arrayCurrentSize)
      {
        (*x_pgmStruct)->arrayCount = *x_lfdn;
        memcpy(x_key, (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recKey, 100);
        memcpy(x_data, (*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recData, *x_dataLen);
        x_retcode[0] = ' ';
      }
      else
      {
        memset(x_data, ' ', *x_dataLen);
        x_retcode[0] = '7';
      }
      return;
      break;

    /*‚Key + Datensatz über LFDN ändern ohne Sort */
    case '8':
      /*‚Ggf. vorher sortieren */
      if((*x_pgmStruct)->arraySorted == 0)
      {
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      }
      if ((*x_pgmStruct)->arrayCount >= 0
       && (*x_pgmStruct)->arrayCount < (*x_pgmStruct)->arrayCurrentSize)
      {
        memcpy((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recKey, x_key, 100);
        memcpy((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recData, x_data, *x_dataLen);

        MAPDTA_retcode = memcmp((*x_pgmStruct)->maxKeyVal, x_key, 100);
        if(MAPDTA_retcode < 0)
        {
          memcpy(&(*x_pgmStruct)->maxKeyVal, x_key, 100);
        }
        x_retcode[0] = ' ';
      }
      else
      {
        memset(x_data, ' ', *x_dataLen);
        x_retcode[0] = '8';
      }
      return;
      break;

    /*‚Key + Datensatz über LFDN ändern mit Sort */
    case '9':
      /*‚Ggf. vorher sortieren */
      if((*x_pgmStruct)->arraySorted == 0)
      {
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      }
      if ((*x_pgmStruct)->arrayCount >= 0
       && (*x_pgmStruct)->arrayCount < (*x_pgmStruct)->arrayCurrentSize)
      {
        memcpy((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recKey, x_key, 100);
        memcpy((*x_pgmStruct)->dataArray[(*x_pgmStruct)->arrayCount].recData, x_data, *x_dataLen);
        x_retcode[0] = ' ';
        MAPDTA_retcode = memcmp((*x_pgmStruct)->maxKeyVal, x_key, 100);
        if(MAPDTA_retcode < 0)
        {
          memcpy(&(*x_pgmStruct)->maxKeyVal, x_key, 100);
        }
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      }
      else
      {
        memset(x_data, ' ', *x_dataLen);
        x_retcode[0] = '8';
      }
      return;
      break;

    /*‚Array sortieren */
    case 'S':
        /*‚Array nun neu sortieren */
        qsort((*x_pgmStruct)->dataArray, (*x_pgmStruct)->arrayCurrentSize,
              sizeof(struct MAPDTA_arrayRec), sortcomp);
        (*x_pgmStruct)->arraySorted = 1;
      return;
      break;

    /*‚Array Anzahl zurückgeben */
    case 'C':
      *x_lfdn = (*x_pgmStruct)->arrayCurrentSize;
      return;
      break;

    /*‚Nur einen Pointer zurückgeben, also hier nichts tun */
    case 'P':
      if(*x_pgmStruct == NULL)
      {
        x_retcode[0] = '4';
      }
      return;
      break;

    /*‚Bei anderen Aktionen -> Fehler */
    default:
      x_retcode[0] = '2';
      return;
      break;
  }

  return;
}
