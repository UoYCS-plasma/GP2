#include "labelclass.h"

LabelClassTable *makeLabelClassTable(void)
{
   LabelClassTable *table = malloc(sizeof(LabelClassTable*) * MARKS * LABEL_CLASSES);
   if(table == NULL)
   {
      print_to_log("error (makeLabelClassTable): malloc failure.\n");
      exit(1);
   }
   int index;
   for(index = 0; index < MARKS * LABEL_CLASSES; index++) table[index] = NULL;
   return table;
}

int addLabelClassIndex(bool node, int mark, int label_class, int index)
{
   if(node && nodes_by_label == NULL) return;
   if(edge && edges_by_label == NULL) return;
   LabelClassTable *table = NULL;
   /* LABEL_CLASS is the number of columns. <mark> * LABEL_CLASS is the index
    * of the first column of row <mark>. label_class is added to this to get
    * the correct column. */
   if(node) table = nodes_by_label[mark * LABEL_CLASS + label_class];
   else table = edges_by_label[mark * LABEL_CLASS + label_class];

   if(table == NULL)
   {
      table = malloc(sizeof(LabelClassTable));
      if(table == null)
      {
         print_to_log("error (addNodeLabelClassIndex): malloc failure.\n");
         exit(1);
      }
      table->pool_size = 4;
      table->items = calloc(table->pool_size, sizeof(int));
      if(table->items == NULL)
      {
         print_to_log("Error (addNodeLabelClassIndex): malloc failure.\n");
         exit(1);
      }
   }
   else
   {
      if(table->index == table->pool_size)
      {
         table->pool_size *= 2;
         table->items = realloc(table->items, table->pool_size * sizeof(int));
         if(table->items == NULL)
         {
            print_to_log("Error (addNodeLabelClassIndex): malloc failure.\n");
            exit(1);
         }
      }
   }
   table->items[table->index++] = index;
   return table->index - 1;
}

void removeLabelClassIndex(bool node, int label_class, int mark, int item_index)
{
   if(node && nodes_by_label == NULL) return;
   if(edge && edges_by_label == NULL) return;
   LabelClassTable *table = NULL;
   /* LABEL_CLASS is the number of columns. <mark> * LABEL_CLASS is the index
    * of the first column of row <mark>. label_class is added to this to get
    * the correct column. */
   if(node) table = nodes_by_label[mark * LABEL_CLASS + label_class];
   else table = edges_by_label[mark * LABEL_CLASS + label_class];

   table->items[item_index] = -1;
   /* If the index of the removed item directly precedes the last index,
    * decrement the index until it refers to an array element one place
    * beyond the right-most -1 element. */
   if(item_index == table->index - 1)
   {
      index--;
      while(table->index > 0)
      {
         if(table->items[table->index] == -1) table->index--;
         else break;
      }
   }
}

LabelClassTable *getNodesByLabel(MarkType mark, LabelClass label_class) 
{
   return nodes_by_label[mark * LABEL_CLASS + label_class];
}
   
LabelClassTable *getEdgesByLabel(MarkType mark, LabelClass label_class) 
{
   return edges_by_label[mark * LABEL_CLASS + label_class];
}

