/* main.c, Kholiavin Nikolai, M3138 */

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <conio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* List node structure */
typedef struct tagLIST LIST;
struct tagLIST
{
  uint16_t Data;
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
int QueuePush( QUEUE *Q, uint16_t Val )
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
uint16_t QueueHead( QUEUE *Q )
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

/* label data */
typedef struct
{
  char *Name;
  int Place;
} LABEL;

/* label vector structure */
typedef struct
{
  LABEL *Data;
  size_t Size, Capacity;
} VECTOR;

/* Create vector function */
int CreateVector( VECTOR *V )
{
  if ((V->Data = malloc(10 * sizeof(LABEL))) == NULL)
    return 0;
  V->Capacity = 10;
  V->Size = 0;
  return 1;
}

/* Push element to vector */
int VectorPushBack( VECTOR *V, LABEL Label )
{
  if (V->Data == NULL)
    return 0;

  /* Grow */
  if (V->Size + 1 > V->Capacity)
  {
    LABEL *new_data = malloc((V->Capacity * 2) * sizeof(LABEL));
    memcpy(new_data, V->Data, V->Size * sizeof(LABEL));
    free(V->Data);
    V->Data = new_data;
    V->Capacity *= 2;
  }

  V->Data[V->Size++] = Label;
  return 1;
}

/* Pop element from stack */
void VectorPopBack( VECTOR *V )
{
  if (V->Data == NULL || V->Size == 0)
    return;

  V->Size--;

  /* Shrink */
  if (V->Capacity >= 20 && V->Capacity / 2 >= V->Size)
  {
    LABEL *new_data = malloc((V->Capacity / 2) * sizeof(LABEL));
    memcpy(new_data, V->Data, V->Size * sizeof(LABEL));
    free(V->Data);
    V->Data = new_data;
    V->Capacity /= 2;
  }
}

/* Get vector element */
LABEL VectorGet( VECTOR *V, size_t Index )
{
  if (V->Data == NULL || Index >= V->Size)
  {
    LABEL l;
    l.Place = -1;
    l.Name = NULL;
    return l;
  }
  return V->Data[Index];
}

/* Delete vector */
void DeleteVector( VECTOR *V )
{
  if (V->Data != NULL)
  {
    size_t i;

    for (i = 0; i < V->Size; i++)
      if (V->Data[i].Name != NULL)
        free(V->Data[i].Name);
    free(V->Data);
    V->Capacity = V->Size = 0;
  }
}

int FindLabelPlace( VECTOR *V, char *Name )
{
  size_t i;

  for (i = 0; i < V->Size; i++)
  {
    LABEL l = VectorGet(V, i);
    if (strcmp(l.Name, Name) == 0)
      return l.Place;
  }
  return -1;
}

/* Main program function */
int main( void )
{
  QUEUE Q = {0};
  uint16_t regs[26];
  int i, run = 1;
  char *program = NULL;
  long len = 0;
  VECTOR Labels;

  while (fgetc(stdin) != EOF)
    len++;
  rewind(stdin);
  program = malloc(len);
  fread(program, 1, len, stdin);

  CreateVector(&Labels);

  // Find labels
  for (i = 0; i < len; i++)
    if (program[i] == ':')
    {
      // Add label
      int end = ++i;
      LABEL label;

      while (end < len && program[end] != '\n')
        end++;

      label.Name = malloc(end - i + 1);
      memcpy(label.Name, &program[i], end - i);
      label.Name[end - i] = 0;
      label.Place = end;

      VectorPushBack(&Labels, label);
      i = end;
    }
    else
    {
      // Skip to next line
      while (i < len && program[i] != '\n')
        i++;
    }

  // Execute
  for (i = 0; i < len && run; i++)
  {
    uint16_t x, y;
    char *string;
    int end, b, jump;
    char ch = program[i];

    switch (ch)
    {
    case '+':
      x = QueueHead(&Q);
      QueuePop(&Q);
      y = QueueHead(&Q);
      QueuePop(&Q);
      QueuePush(&Q, x + y);
      break;
    case '-':
      x = QueueHead(&Q);
      QueuePop(&Q);
      y = QueueHead(&Q);
      QueuePop(&Q);
      QueuePush(&Q, x - y);
      break;
    case '*':
      x = QueueHead(&Q);
      QueuePop(&Q);
      y = QueueHead(&Q);
      QueuePop(&Q);
      QueuePush(&Q, x * y);
      break;
    case '/':
      x = QueueHead(&Q);
      QueuePop(&Q);
      y = QueueHead(&Q);
      QueuePop(&Q);
      if (y != 0)
        QueuePush(&Q, x / y);
      else
        QueuePush(&Q, 0);
      break;
    case '%':
      x = QueueHead(&Q);
      QueuePop(&Q);
      y = QueueHead(&Q);
      QueuePop(&Q);
      if (y != 0)
        QueuePush(&Q, x % y);
      else
        QueuePush(&Q, 0);
      break;
    case '>':
      x = QueueHead(&Q);
      QueuePop(&Q);
      i++;
      regs[program[i] - 'a'] = x;
      break;
    case '<':
      i++;
      QueuePush(&Q, regs[program[i] - 'a']);
      break;
    case 'P':
    case 'C':
      if (i == len - 1 || program[i + 1] == '\n')
      {
        x = QueueHead(&Q);
        QueuePop(&Q);
      }
      else
      {
        i++;
        x = regs[program[i] - 'a'];
      }
      if (ch == 'P')
        printf("%i\n", x);
      else
        printf("%c", x % 256);
      break;
    case ':':
      while (i < len && program[i] != '\n')
        i++;
      if (program[i] == '\n')
        i--;
      break;
    case 'J':
    case 'Z':
    case 'E':
    case 'G':
      b = (ch == 'J' ||
           ch == 'Z' && regs[program[++i] - 'a'] == 0 ||
           ch == 'E' && regs[program[++i] - 'a'] == regs[program[++i] - 'a'] ||
           ch == 'G' && regs[program[++i] - 'a'] > regs[program[++i] - 'a']);

      end = ++i;
      while (end < len && program[end] != '\n')
        end++;
      string = malloc(end - i + 1);
      memcpy(string, &program[i], end - i);
      string[end - i] = 0;
      jump = FindLabelPlace(&Labels, string) - 1;
      free(string);

      if (b)
        i = jump;
      else
        i = end - 1;
      break;
    case 'Q':
      run = 0;
      break;
    default:
      end = i;
      while (isdigit(program[end]))
        end++;
      string = malloc(end - i + 1);
      memcpy(string, &program[i], end - i);
      string[end - i] = 0;
      x = atoi(string);
      QueuePush(&Q, x);
      free(string);
      i = end - 1;
    }

    i++;
  }

  return 0;
}

/* end of 'main.c' file */
