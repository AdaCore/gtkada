with Interfaces.C;
with System;

package Glib is

   package C renames Interfaces.C;
   use type C.Int;
   use type C.Unsigned;

   ----------------------------------------
   --  The basic types  defined by glib  --
   ----------------------------------------

   type Gshort is new C.Short;
   type Glong  is new C.Long;
   type Gint   is new C.Int;

   type Gushort is new C.Unsigned_Short;
   type Gulong  is new C.Unsigned_Long;
   type Guint   is new C.Unsigned;

   type Gfloat  is new C.C_Float;
   type Gdouble is new C.Double;

   subtype Gint32 is Gint range -(2 ** 16) .. (2 ** 16 - 1);

   subtype Guint8  is Guint range Guint'First .. (2 ** 8 - 1);
   subtype Guint32 is Guint range Guint'First .. (2 ** 32 - 1);

   -------------------------------------------------
   --  The more evoluated types defined by glib   --
   -------------------------------------------------
   --
   --  Note : The services related to those types should be implemented in
   --         different packages.  Ex: Services related to Gdk_Window should
   --         be implemented in the package Gdk.Window just like it has been
   --         implemented in gtkwindow.c in the C version.
   --
   --         Note also that some of those type can not be declared here since
   --         they are "generic" types.



end Glib;
