#define CAT(a,b) XCAT(a,b)
#define XCAT(a,b) a ## b
#define STR(a) XSTR(a)
#define XSTR(a) #a

#include <HsFFI.h>

//extern void __stginit_BigchainDB(void);

static void library_init (void) __attribute__ ((constructor));
static void
library_init (void)
{
  /* This seems to be a no-op, but it makes the GHCRTS envvar work. */
  static char *argv[] = { "BigchainDB.so", 0 }, **argv_ = argv;
  static int argc = 1;

  hs_init (&argc, &argv_);
  //hs_add_root(__stginit_BigchainDB);
}

static void library_exit (void) __attribute__ ((destructor));
static void
library_exit (void)
{
  //hs_exit ();
}

