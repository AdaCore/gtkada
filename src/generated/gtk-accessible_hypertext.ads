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

--  An interface for accessible objects containing links.
--
--  The `GtkAccessibleHypertext` interfaces is meant to be implemented by
--  accessible objects that contain links. Those links don't necessarily have
--  to be part of text, they can be associated with images and other things.

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Types;  use Glib.Types;

package Gtk.Accessible_Hypertext is

   type Gtk_Accessible_Hypertext is new Glib.Types.GType_Interface;
   Null_Gtk_Accessible_Hypertext : constant Gtk_Accessible_Hypertext;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accessible_hypertext_get_type");

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Accessible_Hypertext"

   function "+" (W : Gtk_Accessible_Hypertext) return Gtk_Accessible_Hypertext;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Link is access function
     (Self  : Gtk_Accessible_Hypertext;
      Index : Guint) return System.Address;
   pragma Convention (C, Virtual_Get_Link);
   --  Retrieve the n-th link in the accessible object.
   --  Index must be smaller than the number of links.
   --  Since: gtk+ 4.22
   --  @param Index the index of the link
   --  @return the link

   type Virtual_Get_Link_At is access function
     (Self   : Gtk_Accessible_Hypertext;
      Offset : Guint) return Guint;
   pragma Convention (C, Virtual_Get_Link_At);

   type Virtual_Get_N_Links is access function (Self : Gtk_Accessible_Hypertext) return Guint;
   pragma Convention (C, Virtual_Get_N_Links);
   --  Retrieve the number of links in the accessible object.
   --  Since: gtk+ 4.22
   --  @return the number of links

   subtype Accessible_Hypertext_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Link
     (Self    : Accessible_Hypertext_Interface_Descr;
      Handler : Virtual_Get_Link);
   pragma Import (C, Set_Get_Link, "gtkada_Accessible_Hypertext_set_get_link");

   procedure Set_Get_Link_At
     (Self    : Accessible_Hypertext_Interface_Descr;
      Handler : Virtual_Get_Link_At);
   pragma Import (C, Set_Get_Link_At, "gtkada_Accessible_Hypertext_set_get_link_at");

   procedure Set_Get_N_Links
     (Self    : Accessible_Hypertext_Interface_Descr;
      Handler : Virtual_Get_N_Links);
   pragma Import (C, Set_Get_N_Links, "gtkada_Accessible_Hypertext_set_get_n_links");
   --  See Glib.Object.Add_Interface

private

Null_Gtk_Accessible_Hypertext : constant Gtk_Accessible_Hypertext :=
   Gtk_Accessible_Hypertext (Glib.Types.Null_Interface);
end Gtk.Accessible_Hypertext;
