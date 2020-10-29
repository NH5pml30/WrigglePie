/* main.c, Kholiavin Nikolai, M3138 */

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <conio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* queue structure */
typedef struct
{
  int *Data;
  size_t Capacity;

  size_t Front, Back;
} QUEUE;

/* Create new queue function */
int CreateQueue( QUEUE *Q )
{
  if ((Q->Data = malloc(sizeof(int) * 10)) == 0)
    return 0;
  Q->Capacity = 10;
  Q->Front = -1;
  Q->Back = 0;
  return 1;
}

/* Push element to queue */
int QueuePush( QUEUE *Q, int Val )
{
  if (Q->Back == Q->Front)
  {
    int *new_data = malloc((Q->Capacity * 2) * sizeof(int));
    if (new_data == NULL)
      return 0;

    memcpy(new_data, Q->Data + Q->Front, (Q->Capacity - Q->Front) * sizeof(int));
    if (Q->Back != 0)
      memcpy(new_data + Q->Capacity - Q->Front, Q->Data, Q->Back * sizeof(int));
    free(Q->Data);

    Q->Data = new_data;
    Q->Front = 0;
    Q->Back = Q->Capacity;
    Q->Capacity *= 2;
  }

  Q->Data[Q->Back] = Val;
  if (Q->Front == -1)
    Q->Front = Q->Back;
  Q->Back = (Q->Back + 1) % Q->Capacity;
  return 1;
}

/* Pop element from queue */
void QueuePop( QUEUE *Q )
{
  Q->Front = (Q->Front + 1) % Q->Capacity;

  if (Q->Capacity >= 20 && (Q->Capacity - Q->Front + Q->Back) % Q->Capacity <= Q->Capacity / 2)
  {
    int *new_data = malloc((Q->Capacity / 2) * sizeof(int));
    if (new_data == NULL)
      return;

    if (Q->Back < Q->Front)
    {
      memcpy(new_data, Q->Data + Q->Front, (Q->Capacity - Q->Front) * sizeof(int));
      memcpy(new_data + Q->Capacity - Q->Front, Q->Data, Q->Back * sizeof(int));
    }
    else
      memcpy(new_data, Q->Data + Q->Front, (Q->Back - Q->Front) * sizeof(int));
    free(Q->Data);

    Q->Data = new_data;
    Q->Back = 0;
    Q->Front = 0;
    Q->Capacity /= 2;
  }
  else if (Q->Front == Q->Back)
    Q->Front = -1, Q->Back = 0;
}

/* Get head element from queue */
int QueueHead( QUEUE *Q )
{
  if (Q->Front == -1)
    return 0;
  return Q->Data[Q->Front];
}

/* Delete queue */
void DeleteQueue( QUEUE *Q )
{
  free(Q->Data);
  Q->Capacity = 0;
  Q->Back = 0;
  Q->Front = -1;
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
  QUEUE Q;
  int i, M;
  char line[100];

  CreateQueue(&Q);
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
