/* main.c, Kholiavin Nikolai, M3138 */

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>

/* stack structure */
typedef struct
{
  int *Data;
  size_t Size, Capacity;
} STACK;

/* Create stack  function */
int CreateStack( STACK *S )
{
  if ((S->Data = malloc(10 * sizeof(int))) == NULL)
    return 0;
  S->Capacity = 10;
  S->Size = 0;
  return 1;
}

/* Push element to stack */
int StackPush( STACK *S, int Val )
{
  if (S->Data == NULL)
    return 0;

  /* Grow */
  if (S->Size + 1 > S->Capacity)
  {
    int *new_data = malloc((S->Capacity * 2) * sizeof(int));
    memcpy(new_data, S->Data, S->Size * sizeof(int));
    free(S->Data);
    S->Data = new_data;
    S->Capacity *= 2;
  }

  S->Data[S->Size++] = Val;
  return 1;
}

/* Pop element from stack */
void StackPop( STACK *S )
{
  if (S->Data == NULL || S->Size == 0)
    return;

  S->Size--;

  /* Shrink */
  if (S->Capacity >= 20 && S->Capacity / 2 >= S->Size)
  {
    int *new_data = malloc((S->Capacity / 2) * sizeof(int));
    memcpy(new_data, S->Data, S->Size * sizeof(int));
    free(S->Data);
    S->Data = new_data;
    S->Capacity /= 2;
  }
}

/* Get stack top */
int StackGetTop( STACK *S )
{
  if (S->Data == NULL)
    return 0;
  return S->Data[S->Size - 1];
}

/* Delete stack */
void DeleteStack( STACK *S )
{
  if (S->Data != NULL)
  {
    free(S->Data);
    S->Capacity = S->Size = 0;
  }
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
  STACK S;
  int i, M;
  char line[100];

  CreateStack(&S);
  scanf("%i", &M);
  getchar();
  for (i = 0; i < M; i++)
  {
    int val;

    GetStr(line, 100);
    if (sscanf(line, "+ %i", &val) == 1)
      StackPush(&S, val);
    else
    {
      printf("%i\n", StackGetTop(&S));
      StackPop(&S);
    }
  }

  DeleteStack(&S);
  return 0;
}

/* end of 'main.c' file */
