#include "label.h"

HostLabel blank_label = {NONE, 0, NULL};

#ifdef LIST_HASHING
Bucket **list_store = NULL;

/* The list hash table has 400 buckets and is structured as follows:
 * Lists of length 1 occupy buckets 0 - 99.
 * Lists of length 2 occupy buckets 100 - 199.
 * Lists of length 3 occupy buckets 200 - 299.
 * Lists of length > 3 occupy buckets 300 - 399.
 * The first unit in the first atom (an integer or the first character of a string)
 * gives the index to add to the 'base' (the appropriate multiple of 100) as defined
 * in the function. */
static int hashHostList(HostAtom *list, int length)
{
   int base = length > 3 ? 300 : 100 * (length - 1);
   int offset = 0;
   HostAtom atom = list[0];
   /* Integers occupy indices 0-34. */
   if(atom.type == 'i') offset = abs(atom.num) % 35;
   else 
   {
      char first_char = atom.str[0];
      /* Digits occupy indices 35-44. 0's ASCII value is 48. */
      if(first_char >= '0' && first_char <= '9') offset = first_char - 13;
      /* Upper case letters occupy indices 45-70. A's ASCII value is 65. */
      else if(first_char >= 'A' && first_char <= 'Z') offset = first_char - 20;
      /* Lower case letters occupy indices 71-96. a's ASCII value is 97. */
      else if(first_char >= 'a' && first_char <= 'z') offset = first_char - 26;
      /* _ occupies index 97, ' ' occupies index 98, and the empty string occupies index 99. */
      else if(first_char == '_') offset = 97;
      else if(first_char == ' ') offset = 98;
      else if(first_char == '\0') offset = 99;
      else
      {
         print_to_log("Error (hashHostList): Invalid character '%c'.\n", first_char);
         exit(1);
      }
   }
   return base + offset;
}
#endif

static HostList *appendHostAtom(HostList *list, HostAtom atom, bool free_strings)
{
   HostListItem *new_item = malloc(sizeof(HostListItem));
   if(new_item == NULL)
   {
      print_to_log("Error (appendAtom): malloc failure.\n");
      exit(1);
   }
   new_item->atom = atom;
   if(atom.type == 's') 
   {
      if(free_strings) new_item->atom.str = atom.str;
      else new_item->atom.str = strdup(atom.str);
   }
   new_item->next = NULL;

   if(list == NULL)
   {
      new_item->prev = NULL;
      HostList *new_list = malloc(sizeof(HostList));
      if(new_list == NULL)
      {
         print_to_log("Error (appendAtom): malloc failure.\n");
         exit(1);
      }
      new_list->first = new_item;
      new_list->last = new_item;
      return new_list;
   }
   else
   {
      list->last->next = new_item;
      new_item->prev = list->last;
      list->last = new_item;
      return list;
   }
}

#ifdef LIST_HASHING
/* Create a new bucket, allocate a list defined by the function arguments, and
 * point the bucket to that list. */
static Bucket *makeBucket(HostAtom *array, int length, bool free_strings)
{
   Bucket *bucket = malloc(sizeof(Bucket));
   if(bucket == NULL)
   {
      print_to_log("Error (makeBucket): malloc failure.\n");
      exit(1);
   }
   HostList *list = NULL;
   int index;
   for(index = 0; index < length; index++) 
      list = appendHostAtom(list, array[index], free_strings);
   bucket->list = list;
   bucket->next = NULL;
   return bucket;
}
#endif

/* Adds a host list, represented by the passed array and its length, to the hash
 * table. The array and the length is passed to the hashing function. 
 *
 * The free_strings flag is true if the strings in the passed array have already
 * been allocated by the caller. It controls the freeing of such strings in the
 * case that the passed list already exists in the hash table. 
 * This is a necessary inconvenience: addListToStore is called by the Bison/Flex generated
 * host graph parser which requires the strings it parses to be strdup'd (otherwise 
 * things go wrong). Calls to addListToStore in other contexts pass arrays with 
 * automatic strings which should not be freed. */
HostList *addHostList(HostAtom *array, int length, bool free_strings)
{
   #ifdef LIST_HASHING
      if(list_store == NULL)
      {
         list_store = calloc(LIST_TABLE_SIZE, sizeof(Bucket*));
         if(list_store == NULL)
         {
            print_to_log("Error(addListToStore): malloc failure.\n");
            exit(1);
         }
      }
      int hash = hashHostList(array, length);
      if(list_store[hash] == NULL)
      {
         Bucket *bucket = makeBucket(array, length, free_strings);
         list_store[hash] = bucket;
         return bucket->list;
      }
      /* Check each list in the bucket for equality with the list represented
       * by the passed array. */
      else
      {
         Bucket *bucket = list_store[hash];
         int index;
         bool make_bucket = true;
         while(bucket != NULL)
         {
            HostListItem *item = bucket->list->first;
            for(index = 0; index < length; index++) 
            {
               if(item == NULL) break;
               HostAtom atom = array[index];
               if(item->atom.type != atom.type) break;
               if(item->atom.type == 'i') 
               {
                  if(item->atom.num != atom.num) break;
               }
               else
               {
                  if(strcmp(item->atom.str, atom.str) != 0) break;
               }
               item = item->next;
            }
            /* The lists are equal if and only if the ends of both lists are reached.
             * If an atom comparison failed, the for loop breaks before the end of
             * either list is reached. If the array is shorter, then the for loop
             * exits before item reaches its terminating NULL pointer. If the list
             * is shorter, the first line in the for loop body will cause the loop
             * to break before index == length. */
            if(index == length && item == NULL)
            {
               make_bucket = false; 
               break;
            }
            /* Exit the loop while maintaining the pointer to the last item in the
             * bucket list, because a list is going to be appended! */
            if(bucket->next == NULL) break;
            bucket = bucket->next;
         }
         /* If control reaches this point, then no list in the bucket is equal to
          * the passed list. Make a new list! */
         if(make_bucket)
         {
            Bucket *new_bucket = makeBucket(array, length, free_strings);
            bucket->next = new_bucket;
            return new_bucket->list;
         }
         else 
         {
            if(free_strings)
            {
               for(index = 0; index < length; index++) 
                  if(array[index].type == 's') free(array[index].str);
            }
            return bucket->list;
         }
      }
   #else
      HostList *list = NULL;
      int index;
      for(index = 0; index < length; index++) 
         list = appendHostAtom(list, array[index], free_strings);
      return list;
   #endif
} 


HostLabel makeEmptyLabel(MarkType mark)
{
   HostLabel label = { .mark = mark, .length = 0, .list = NULL };
   return label;
}

HostLabel makeHostLabel(MarkType mark, int length, HostList *list)
{
   HostLabel label = { .mark = mark, .length = length, .list = list };
   return label;
}

bool equalHostLabels(HostLabel label1, HostLabel label2)
{
   if(label1.mark != label2.mark) return false;
   if(label1.length != label2.length) return false;
   if(label1.list != label2.list) return false;
   return true;
}

bool equalHostLists(HostAtom *left_list, HostAtom *right_list,
                    int left_length, int right_length)
{ 
   if(left_length != right_length) return false;
   int index;
   for(index = 0; index < left_length; index++)
   {
      HostAtom left_atom = left_list[index];
      HostAtom right_atom = right_list[index];

      if(left_atom.type != right_atom.type) return false;
      else if(left_atom.type == 'i')
      {
         if(left_atom.num != right_atom.num) return false;
      }
      else if(strcmp(left_atom.str, right_atom.str) != 0) return false;
   }
   return true;
}

int getListLength(HostList *list)
{
   if(list == NULL) return 0;
   HostListItem *item = list->first;
   int length = 0;
   while(item != NULL) 
   {
      length++;
      item = item->next;
   }
   return length;
}

HostList *copyHostList(HostList *list)
{
   if(list == NULL) return NULL;
   HostList *list_copy = NULL;
   HostListItem *item = list->first;
   while(item != NULL)
   {
      list_copy = appendHostAtom(list_copy, item->atom, false);
      item = item->next;
   }
   return list_copy;
}
   
void printHostLabel(HostLabel label, FILE *file) 
{
   if(label.length == 0) fprintf(file, "empty");
   else printHostList(label.list->first, file);
   if(label.mark == RED) fprintf(file, " # red"); 
   if(label.mark == GREEN) fprintf(file, " # green");
   if(label.mark == BLUE) fprintf(file, " # blue");
   if(label.mark == GREY) fprintf(file, " # grey");
   if(label.mark == DASHED) fprintf(file, " # dashed");
}

void printHostList(HostListItem *item, FILE *file)
{
   while(item != NULL)
   {
      if(item->atom.type == 'i') fprintf(file, "%d", item->atom.num);
      else fprintf(file, "\"%s\"", item->atom.str);
      if(item->next != NULL) fprintf(file, " : ");
      item = item->next;
   }
}

static void freeHostListItems(HostListItem *item)
{
   if(item == NULL) return;
   if(item->atom.type == 's') free(item->atom.str);
   freeHostListItems(item->next);
   free(item);
}

void freeHostList(HostList *list)
{
   if(list == NULL) return;
   freeHostListItems(list->first);
   free(list);
}

#ifdef LIST_HASHING
static void freeBucket(Bucket *bucket)
{
   if(bucket == NULL) return; 
   freeHostList(bucket->list);
   freeBucket(bucket->next);
   free(bucket);
}

void freeHostListStore(void)
{
   if(list_store == NULL) return;
   int index;
   for(index = 0; index < LIST_TABLE_SIZE; index++) freeBucket(list_store[index]);
   free(list_store);
}
#endif
