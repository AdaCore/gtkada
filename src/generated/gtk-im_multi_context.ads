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

--  Supports switching between multiple input methods.
--
--  Text widgets such as `GtkText` or `GtkTextView` use a `GtkIMMultiContext`
--  to implement their `im-module` property for switching between different
--  input methods.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Gtk.IM_Context; use Gtk.IM_Context;

package Gtk.IM_Multi_Context is

   type Gtk_IM_Multi_Context_Record is new Gtk_IM_Context_Record with null record;
   type Gtk_IM_Multi_Context is access all Gtk_IM_Multi_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_IM_Multi_Context);
   procedure Initialize
      (Self : not null access Gtk_IM_Multi_Context_Record'Class);
   --  Creates a new `GtkIMMulticontext`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_IM_Multi_Context_New return Gtk_IM_Multi_Context;
   --  Creates a new `GtkIMMulticontext`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_im_multicontext_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Context_Id
      (Self : not null access Gtk_IM_Multi_Context_Record)
       return UTF8_String;
   --  Gets the id of the currently active delegate of the Context.
   --  @return the id of the currently active delegate

   procedure Set_Context_Id
      (Self       : not null access Gtk_IM_Multi_Context_Record;
       Context_Id : UTF8_String := "");
   --  Sets the context id for Context.
   --  This causes the currently active delegate of Context to be replaced by
   --  the delegate corresponding to the new context id.
   --  Setting this to a non-null value overrides the system-wide IM module
   --  setting. See the [propertyGtk.Settings:gtk-im-module] property.
   --  @param Context_Id the id to use

end Gtk.IM_Multi_Context;
