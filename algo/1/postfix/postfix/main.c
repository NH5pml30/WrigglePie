/* main.c, Kholiavin Nikolai, M3138 */

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

typedef struct
{
  int Data;
  int Min;
} DATA;

/* stack structure */
typedef struct
{
  DATA *Data;
  size_t Size, Capacity;
} MIN_STACK;

/* Create stack  function */
int CreateStack( MIN_STACK *S )
{
  if ((S->Data = malloc(10 * sizeof(DATA))) == NULL)
    return 0;
  S->Capacity = 10;
  S->Size = 0;
  return 1;
}

/* Push element to stack */
int StackPush( MIN_STACK *S, int Val )
{
  if (S->Data == NULL)
    return 0;

  /* Grow */
  if (S->Size + 1 > S->Capacity)
  {
    DATA *new_data = malloc((S->Capacity * 2) * sizeof(DATA));
    memcpy(new_data, S->Data, S->Size * sizeof(DATA));
    free(S->Data);
    S->Data = new_data;
    S->Capacity *= 2;
  }

  S->Data[S->Size].Data = Val;
  if (S->Size > 0)
    S->Data[S->Size].Min = min(Val, S->Data[S->Size - 1].Min);
  else
    S->Data[S->Size].Min = Val;
  S->Size++;
  return 1;
}

/* Pop element from stack */
void StackPop( MIN_STACK *S )
{
  if (S->Data == NULL || S->Size == 0)
    return;

  S->Size--;

  /* Shrink */
  if (S->Capacity >= 20 && S->Capacity / 2 >= S->Size)
  {
    DATA *new_data = malloc((S->Capacity / 2) * sizeof(DATA));
    memcpy(new_data, S->Data, S->Size * sizeof(DATA));
    free(S->Data);
    S->Data = new_data;
    S->Capacity /= 2;
  }
}

/* Get stack top */
int StackGetTop( MIN_STACK *S )
{
  if (S->Data == NULL)
    return 0;
  return S->Data[S->Size - 1].Data;
}

/* Get stack minimum */
int StackGetMin( MIN_STACK *S )
{
  if (S->Data == NULL)
    return 0;
  return S->Data[S->Size - 1].Min;
}

/* Delete stack */
void DeleteStack( MIN_STACK *S )
{
  if (S->Data != NULL)
  {
    free(S->Data);
    S->Capacity = S->Size = 0;
  }
}

typedef struct
{
  MIN_STACK S1, S2;
} MIN_QUEUE;

/* Create stack  function */
int CreateQueue( MIN_QUEUE *Q )
{
  if (!CreateStack(&Q->S1))
    return 0;
  if (!CreateStack(&Q->S2))
  {
    DeleteStack(&Q->S1);
    return 0;
  }
  return 1;
}

/* Push element to queue */
int QueuePush( MIN_QUEUE *Q, int Val )
{
  return StackPush(&Q->S2, Val);
}

/* Pop element from queue */
void QueuePop( MIN_QUEUE *Q )
{
  if (Q->S1.Size == 0)
    while (Q->S2.Size != 0)
    {
      int val = StackGetTop(&Q->S2);
      StackPop(&Q->S2);
      StackPush(&Q->S1, val);
    }
  StackPop(&Q->S1);
}

/* Get head from queue */
int QueueHead( MIN_QUEUE *Q )
{
  if (Q->S1.Size == 0)
    while (Q->S2.Size != 0)
    {
      int val = StackGetTop(&Q->S2);
      StackPop(&Q->S2);
      StackPush(&Q->S1, val);
    }
  return StackGetTop(&Q->S1);
}

/* Get minimum from queue */
int QueueGetMin( MIN_QUEUE *Q )
{
  if (Q->S1.Size != 0)
    if (Q->S2.Size != 0)
      return min(StackGetMin(&Q->S1), StackGetMin(&Q->S2));
    else
      return StackGetMin(&Q->S1);
  else
    if (Q->S2.Size != 0)
      return StackGetMin(&Q->S2);
    else
      return 0;
}

/* Delete queue */
void DeleteQueue( MIN_QUEUE *Q )
{
  DeleteStack(&Q->S1);
  DeleteStack(&Q->S2);
}

/* Get line from input */
void GetStr( char *Str, int Max )
{
  int i = 0, ch = 0;

  while ((ch = getchar()) != '\n' && ch != EOF)
    if (Str != NULL && i < Max - 1)
      Str[i++] = ch;
  if (Str != NULL && i < Max)
    Str[i] = 0;
}

/* Main program function */
int main( void )
{
  MIN_QUEUE Q;
  int i, n, m, k, a, b, c, last1 = 0, last2 = 0;
  long long sum = 0;

  CreateQueue(&Q);

  scanf("%i %i %i\n%i %i %i", &n, &m, &k, &a, &b, &c);

  for (i = 0; i < k && i < m; i++)
  {
    int x;
    scanf("%i", &x);
    QueuePush(&Q, x);

    if (i == k - 2)
      last2 = x;
    if (i == k - 1)
      last1 = x;
  }

  if (i < k)
    while (i < k)
    {
      int x;
      scanf("%i", &x);

      if (i >= m)
      {
        sum += QueueGetMin(&Q);
        QueuePop(&Q);
      }
      QueuePush(&Q, x);

      if (i == k - 2)
        last2 = x;
      if (i == k - 1)
        last1 = x;
      i++;
    }

  while (i < n)
  {
    int x = a * last2 + b * last1 + c;

    if (i >= m)
    {
      sum += QueueGetMin(&Q);
      QueuePop(&Q);
    }
    QueuePush(&Q, x);
    last2 = last1, last1 = x;

    i++;
  }

  sum += QueueGetMin(&Q);

  printf("%lld\n", sum);
  DeleteQueue(&Q);
  return 0;
}

/* end of 'main.c' file */
