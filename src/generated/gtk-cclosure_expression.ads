------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  A variant of `GtkClosureExpression` using a C closure.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Values;    use Glib.Values;
with Gtk.Expression; use Gtk.Expression;

package Gtk.Cclosure_Expression is

   type Gtk_Cclosure_Expression_Record is new Gtk_Expression_Record with null record;
   type Gtk_Cclosure_Expression is access all Gtk_Cclosure_Expression_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cclosure_expression_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Gcallback is access procedure;
   pragma Convention (C, Gcallback);

   type GClosureNotify is access procedure
     (Data    : System.Address;
      Closure : System.Address);
   pragma Convention (C, GClosureNotify);

   type C_Marshaller is access procedure
     (Closure         : System.Address;
      Return_Value    : Glib.Values.GValue;--  Will contain returned value
      N_Params        : Glib.Guint;--  Number of entries in Params
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      Marsh_Data      : System.Address);
   pragma Convention (C, C_Marshaller);

   procedure Gtk_New
     (Self          : out Gtk_Cclosure_Expression;
      Value_Type    : GType;
      Marshal       : C_Marshaller;
      Params        : Gtk.Expression.Gtk_Expression_Array;
      Callback_Func : Gcallback;
      User_Data     : System.Address;
      User_Destroy  : GClosureNotify);
   procedure Initialize
     (Self          : not null access Gtk_Cclosure_Expression_Record'Class;
      Value_Type    : GType;
      Marshal       : C_Marshaller;
      Params        : Gtk.Expression.Gtk_Expression_Array;
      Callback_Func : Gcallback;
      User_Data     : System.Address;
      User_Destroy  : GClosureNotify);
   --  Creates a `GtkExpression` that calls `callback_func` when it is
   --  evaluated.
   --  This function is a variant of [ctorGtk.ClosureExpression.new] that
   --  creates a `GClosure` by calling g_cclosure_new with the given
   --  `callback_func`, `user_data` and `user_destroy`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Value_Type the type of the value that this expression evaluates
   --  to
   --  @param Marshal marshaller used for creating a closure
   --  @param Params expressions for each parameter
   --  @param Callback_Func callback used for creating a closure
   --  @param User_Data user data used for creating a closure
   --  @param User_Destroy destroy notify for User_Data

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Cclosure_Expression_Record;

private
   for Gtk_Cclosure_Expression_Record'External_Tag use "GtkCClosureExpression";
end Gtk.Cclosure_Expression;
