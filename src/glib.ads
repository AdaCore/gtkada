with Interfaces.C;

package Glib is

   package C renames Interfaces.C;
   use type C.int;
   use type C.unsigned;

   ----------------------------------------
   --  The basic types  defined by glib  --
   ----------------------------------------

   type Gshort is new C.short;
   type Glong  is new C.long;
   type Gint   is new C.int;

   type Gushort is new C.unsigned_short;
   type Gulong  is new C.unsigned_long;
   type Guint   is new C.unsigned;

   type Gfloat  is new C.C_float;
   type Gdouble is new C.double;

   subtype Gint16 is Gint range -(2 ** 8) .. (2 ** 8 - 1);
   subtype Gint32 is Gint range -(2 ** 16) .. (2 ** 16 - 1);

   subtype Guint8  is Guint range Guint'First .. (2 ** 8 - 1);
   subtype Guint16 is Guint range Guint'First .. (2 ** 16 - 1);
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

   ------------------------
   --  Some Array types  --
   ------------------------

   type Gulong_Array is array (Positive range <>) of Gulong;


   ---------------------------
   --  Conversion services  --
   ---------------------------

   function To_Boolean (Value : in Gint) return Boolean;
   function To_Gint (Bool : in Boolean) return Gint;

end Glib;
