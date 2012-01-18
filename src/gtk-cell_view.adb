------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Gdk.Color;         use Gdk.Color;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Tree_Model;    use Gtk.Tree_Model;
with Gtk.Widget;        use Gtk.Widget;
with System;            use System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Cell_View is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_View_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (View : out Gtk_Cell_View) is
   begin
      View := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize (View);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (View : access Gtk_Cell_View_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new");
   begin
      Set_Object (View, Internal);
   end Initialize;

   -----------------------
   -- Gtk_New_With_Text --
   -----------------------

   procedure Gtk_New_With_Text (View : out Gtk_Cell_View; Text : String) is
   begin
      View := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Text (View, Text);
   end Gtk_New_With_Text;

   --------------------------
   -- Initialize_With_Text --
   --------------------------

   procedure Initialize_With_Text
     (View : access Gtk_Cell_View_Record'Class; Text : String)
   is
      function Internal (Markup : String) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_text");
   begin
      Set_Object (View, Internal (Text & ASCII.NUL));
   end Initialize_With_Text;

   -------------------------
   -- Gtk_New_With_Markup --
   -------------------------

   procedure Gtk_New_With_Markup (View : out Gtk_Cell_View; Markup : String) is
   begin
      View := new Gtk_Cell_View_Record;
      Initialize_With_Markup (View, Markup);
   end Gtk_New_With_Markup;

   ----------------------------
   -- Initialize_With_Markup --
   ----------------------------

   procedure Initialize_With_Markup
     (View : access Gtk_Cell_View_Record'Class; Markup : String)
   is
      function Internal (Markup : String) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_markup");
   begin
      Set_Object (View, Internal (Markup & ASCII.NUL));
   end Initialize_With_Markup;

   -------------------------
   -- Gtk_New_With_Pixbuf --
   -------------------------

   procedure Gtk_New_With_Pixbuf
     (View : out Gtk_Cell_View; Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf) is
   begin
      View := new Gtk_Cell_View_Record;
      Initialize_With_Pixbuf (View, Pixbuf);
   end Gtk_New_With_Pixbuf;

   ----------------------------
   -- Initialize_With_Pixbuf --
   ----------------------------

   procedure Initialize_With_Pixbuf
     (View   : access Gtk_Cell_View_Record'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_pixbuf");
   begin
      Set_Object (View, Internal (Get_Object (Pixbuf)));
   end Initialize_With_Pixbuf;

   -----------------------
   -- Set_Displayed_Row --
   -----------------------

   procedure Set_Displayed_Row
     (Cell_View : access Gtk_Cell_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
        (Cell_View : System.Address;
         Path      : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_cell_view_set_displayed_row");
   begin
      Internal (Get_Object (Cell_View), Path);
   end Set_Displayed_Row;

   -----------------------
   -- Get_Displayed_Row --
   -----------------------

   function Get_Displayed_Row
     (Cell_View : access Gtk_Cell_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal (Cell_View : System.Address) return Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_cell_view_get_displayed_row");
   begin
      return Internal (Get_Object (Cell_View));
   end Get_Displayed_Row;

   ---------------------
   -- Get_Size_Of_Row --
   ---------------------

   function Get_Size_Of_Row
     (Cell_View   : access Gtk_Cell_View_Record;
      Path        : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Widget.Gtk_Requisition
   is
      function Internal
        (Cell_View   : System.Address;
         Path        : Gtk_Tree_Path;
         Requisition : access Gtk_Requisition)
         return Gboolean;
      pragma Import (C, Internal, "gtk_cell_view_get_size_of_row");
      Req : aliased Gtk_Requisition;
      Tmp : Gboolean;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Internal
        (Get_Object (Cell_View), Path, Req'Access);
      return Req;
   end Get_Size_Of_Row;

   --------------------------
   -- Set_Background_Color --
   --------------------------

   procedure Set_Background_Color
     (Cell_View : access Gtk_Cell_View_Record;
      Color     : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Cell_View : System.Address;
         Color     : System.Address);
      pragma Import (C, Internal, "gtk_cell_view_set_background_color");
      C : aliased Gdk_Color := Color;
   begin
      if Color = Null_Color then
         Internal (Get_Object (Cell_View), System.Null_Address);
      else
         Internal (Get_Object (Cell_View), C'Address);
      end if;
   end Set_Background_Color;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Cell_View : access Gtk_Cell_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
        (Cell_View : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_get_model");
      Stub : Gtk_Tree_Model_Record;
   begin
      return Gtk.Tree_Model.Gtk_Tree_Model
        (Get_User_Data
          (Internal (Get_Object (Cell_View)), Stub));
   end Get_Model;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (Cell_View : access Gtk_Cell_View_Record;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
        (Cell_View : System.Address;
         Model     : System.Address);
      pragma Import (C, Internal, "gtk_cell_view_set_model");
   begin
      Internal (Get_Object (Cell_View), Get_Object (Model));
   end Set_Model;

end Gtk.Cell_View;
