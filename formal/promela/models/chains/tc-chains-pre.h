/* Test case preamble for chains model */
#include <rtems/test.h>
#include <rtems/chain.h>

const char rtems_test_name[] = "CHAIN_MODEL_TESTS";

typedef struct {
  int val;
  rtems_chain_node node;
} item;

static item *get_item(rtems_chain_control *chain) {
  rtems_chain_node *node = rtems_chain_get_unprotected(chain);
  return (item*)node;
}

static void show_chain(rtems_chain_control *chain, char *buffer) {
  /* Chain visualization code */
  rtems_chain_node *node;
  char *ptr = buffer;
  
  ptr += sprintf(ptr, "[");
  for (node = rtems_chain_first(chain); 
       !rtems_chain_is_tail(chain, node); 
       node = rtems_chain_next(node)) {
    item *itm = (item*)node;
    ptr += sprintf(ptr, " %d", itm->val);
  }
  sprintf(ptr, " ]");
}
