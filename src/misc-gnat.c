/*
 * This file contains all the code specific to GNAT.
 * If you want to port this binding to another compiler,
 * you will have to reimplement the following routines.
 *
 */

#include <gtk/gtk.h>
 
extern int gnat_argc;
extern char **gnat_argv;

void ag_gtk_init (void)
{
  gtk_init (&gnat_argc, &gnat_argv);
}
