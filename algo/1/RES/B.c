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

/* Stack structure */
typedef struct
{
  LIST *Top;
} STACK;

/* Push element to stack */
int StackPush( STACK *S, int Val )
{
  LIST *el = malloc(sizeof(LIST));

  if (el == NULL)
    return 0;
  el->Data = Val;
  el->Next = S->Top;
  S->Top = el;
  return 1;
}

/* Pop element from stack */
void StackPop( STACK *S )
{
  if (S->Top == NULL)
    return;
  S->Top = S->Top->Next;
}

/* Get top from stack */
int StackGetTop( STACK *S )
{
  if (S->Top == NULL)
    return 0;
  return S->Top->Data;
}

/* Delete stack */
void DeleteStack( STACK *S )
{
  LIST *cur = S->Top, *save;

  while (cur != NULL)
  {
    save = cur->Next;
    free(cur);
    cur = save;
  }

  S->Top = NULL;
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
  STACK S = {0};
  int i, M;
  char line[100];

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
