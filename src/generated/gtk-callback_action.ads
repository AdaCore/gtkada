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

--  Invokes a callback.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Glib.Variant;        use Glib.Variant;
with Gtk.Shortcut_Action; use Gtk.Shortcut_Action;
with Gtk.Widget;          use Gtk.Widget;

package Gtk.Callback_Action is

   type Gtk_Callback_Action_Record is new Gtk_Shortcut_Action_Record with null record;
   type Gtk_Callback_Action is access all Gtk_Callback_Action_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_callback_action_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Shortcut_Func is access function
     (Widget : Gtk_Widget;
      Args   : GLib.Variant.Gvariant)
   return Glib.Gboolean;
   --  Type for shortcuts based on user callbacks.
   --  @param Widget The widget passed to the activation
   --  @param Args The arguments passed to the activation, may be null
   --  @return true if the action was successful.

   procedure Gtk_New
     (Self     : out Gtk_Callback_Action;
      Callback : Shortcut_Func);
   procedure Initialize
     (Self     : not null access Gtk_Callback_Action_Record'Class;
      Callback : Shortcut_Func);
   --  Create a custom action that calls the given Callback when activated.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Callback the callback to call when the action is activated

   generic
   type User_Data_Type (<>) is private;
   with procedure Destroy (Data : in out User_Data_Type) is null;
   package Callback_Action_With_Data is

      type Shortcut_Data_Func is access function
        (Widget : Gtk_Widget;
         Args   : GLib.Variant.Gvariant;
         Data   : User_Data_Type)
      return Glib.Gboolean;
      --  Type for shortcuts based on user callbacks.
      --  @param Widget The widget passed to the activation
      --  @param Args The arguments passed to the activation, may be null
      --  @param Data The user data provided when activating the action
      --  @return true if the action was successful.

      procedure Gtk_New
        (Self     : out Gtk_Callback_Action;
         Callback : Shortcut_Data_Func;
         Data     : User_Data_Type);
      procedure Initialize
        (Self     : not null access Gtk_Callback_Action_Record'Class;
         Callback : Shortcut_Data_Func;
         Data     : User_Data_Type);
      --  Create a custom action that calls the given Callback when activated.
      --  Initialize does nothing if the object was already created with another
      --  call to Initialize* or G_New.
      --  @param Callback the callback to call when the action is activated
      --  @param Data the data to be passed to Callback
      --  finalized
   end Callback_Action_With_Data;

end Gtk.Callback_Action;
