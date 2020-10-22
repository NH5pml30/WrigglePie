/* main.c, Kholiavin Nikolai, M3138 */

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <conio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* List node structure */
typedef struct tagLIST LIST;
struct tagLIST
{
  int Data;
  LIST *Next;
};

/* Queue structure */
typedef struct
{
  LIST
    *Head,
    *Tail;
} QUEUE;

/* Push element to queue */
int QueuePush( QUEUE *Q, int Val )
{
  LIST *el = malloc(sizeof(LIST));
  if (el == NULL)
    return 0;

  el->Data = Val;
  el->Next = NULL;
  if (Q->Head == NULL)
    Q->Head = el;
  else
    Q->Tail->Next = el;
  Q->Tail = el;
  return 1;
}

/* Pop element from queue */
void QueuePop( QUEUE *Q )
{
  if (Q->Head == NULL)
    return;
  Q->Head = Q->Head->Next;
  if (Q->Head == NULL)
    Q->Tail = NULL;
}

/* Get head from queue */
int QueueHead( QUEUE *Q )
{
  if (Q->Head == NULL)
    return 0;
  return Q->Head->Data;
}

/* Delete queue */
void DeleteQueue( QUEUE *Q )
{
  LIST *cur = Q->Head, *save;

  while (cur != NULL)
  {
    save = cur->Next;
    free(cur);
    cur = save;
  }

  Q->Head = Q->Tail = NULL;
}

/* Get line from input */
void GetStr( char *Str, int Max )
{
  int i = 0, ch;

  while ((ch = getchar()) != '\n')
    if (Str != NULL && i < Max - 1)
      Str[i++] = ch;
  if (Str != NULL && i < Max)
    Str[i] = 0;
}

/* Main program function */
int main( void )
{
  QUEUE Q = {0};
  int i, M;
  char line[100];

  scanf("%i", &M);
  getchar();
  for (i = 0; i < M; i++)
  {
    int val;

    GetStr(line, 100);
    if (sscanf(line, "+ %i", &val) == 1)
      QueuePush(&Q, val);
    else
    {
      printf("%i\n", QueueHead(&Q));
      QueuePop(&Q);
    }
  }

  DeleteQueue(&Q);
  return 0;
}

/* end of 'main.c' file */
