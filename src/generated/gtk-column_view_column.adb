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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Column_View_Column is

   package Type_Conversion_Gtk_Column_View_Column is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Column_View_Column_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Column_View_Column);

   --------------------------------
   -- Gtk_Column_View_Column_New --
   --------------------------------

   function Gtk_Column_View_Column_New
      (Title   : UTF8_String := "";
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
       return Gtk_Column_View_Column
   is
      Self : constant Gtk_Column_View_Column := new Gtk_Column_View_Column_Record;
   begin
      Gtk.Column_View_Column.Initialize (Self, Title, Factory);
      return Self;
   end Gtk_Column_View_Column_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self    : out Gtk_Column_View_Column;
       Title   : UTF8_String := "";
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
   begin
      Self := new Gtk_Column_View_Column_Record;
      Gtk.Column_View_Column.Initialize (Self, Title, Factory);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self    : not null access Gtk_Column_View_Column_Record'Class;
       Title   : UTF8_String := "";
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
      function Internal
         (Title   : Gtkada.Types.Chars_Ptr;
          Factory : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_column_view_column_new");
      Tmp_Title  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         if Title = "" then
            Tmp_Title := Gtkada.Types.Null_Ptr;
         else
            Tmp_Title := New_String (Title);
         end if;
         Tmp_Return := Internal (Tmp_Title, Get_Object_Or_Null (GObject (Factory)));
         Free (Tmp_Title);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   ---------------------
   -- Get_Column_View --
   ---------------------

   function Get_Column_View
      (Self : not null access Gtk_Column_View_Column_Record)
       return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_column_view_column_get_column_view");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Column_View;

   ----------------
   -- Get_Expand --
   ----------------

   function Get_Expand
      (Self : not null access Gtk_Column_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_column_view_column_get_expand");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Expand;

   -----------------
   -- Get_Factory --
   -----------------

   function Get_Factory
      (Self : not null access Gtk_Column_View_Column_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_column_view_column_get_factory");
      Stub_Gtk_List_Item_Factory : Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record;
   begin
      return Gtk.List_Item_Factory.Gtk_List_Item_Factory (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_List_Item_Factory));
   end Get_Factory;

   ---------------------
   -- Get_Fixed_Width --
   ---------------------

   function Get_Fixed_Width
      (Self : not null access Gtk_Column_View_Column_Record)
       return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_column_view_column_get_fixed_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Fixed_Width;

   ---------------------
   -- Get_Header_Menu --
   ---------------------

   function Get_Header_Menu
      (Self : not null access Gtk_Column_View_Column_Record)
       return Glib.Menu_Model.Gmenu_Model
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_column_view_column_get_header_menu");
      Stub_Gmenu_Model : Glib.Menu_Model.Gmenu_Model_Record;
   begin
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Internal (Get_Object (Self)), Stub_Gmenu_Model));
   end Get_Header_Menu;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
      (Self : not null access Gtk_Column_View_Column_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_column_view_column_get_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Id;

   -------------------
   -- Get_Resizable --
   -------------------

   function Get_Resizable
      (Self : not null access Gtk_Column_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_column_view_column_get_resizable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Resizable;

   ----------------
   -- Get_Sorter --
   ----------------

   function Get_Sorter
      (Self : not null access Gtk_Column_View_Column_Record)
       return Gtk.Sorter.Gtk_Sorter
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_column_view_column_get_sorter");
      Stub_Gtk_Sorter : Gtk.Sorter.Gtk_Sorter_Record;
   begin
      return Gtk.Sorter.Gtk_Sorter (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Sorter));
   end Get_Sorter;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Self : not null access Gtk_Column_View_Column_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_column_view_column_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Title;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Self : not null access Gtk_Column_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_column_view_column_get_visible");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Visible;

   ----------------
   -- Set_Expand --
   ----------------

   procedure Set_Expand
      (Self   : not null access Gtk_Column_View_Column_Record;
       Expand : Boolean)
   is
      procedure Internal (Self : System.Address; Expand : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_column_view_column_set_expand");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Expand));
   end Set_Expand;

   -----------------
   -- Set_Factory --
   -----------------

   procedure Set_Factory
      (Self    : not null access Gtk_Column_View_Column_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
      procedure Internal (Self : System.Address; Factory : System.Address);
      pragma Import (C, Internal, "gtk_column_view_column_set_factory");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Factory)));
   end Set_Factory;

   ---------------------
   -- Set_Fixed_Width --
   ---------------------

   procedure Set_Fixed_Width
      (Self        : not null access Gtk_Column_View_Column_Record;
       Fixed_Width : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Fixed_Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_column_view_column_set_fixed_width");
   begin
      Internal (Get_Object (Self), Fixed_Width);
   end Set_Fixed_Width;

   ---------------------
   -- Set_Header_Menu --
   ---------------------

   procedure Set_Header_Menu
      (Self : not null access Gtk_Column_View_Column_Record;
       Menu : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal (Self : System.Address; Menu : System.Address);
      pragma Import (C, Internal, "gtk_column_view_column_set_header_menu");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Menu)));
   end Set_Header_Menu;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id
      (Self : not null access Gtk_Column_View_Column_Record;
       Id   : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Id   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_column_view_column_set_id");
      Tmp_Id : Gtkada.Types.Chars_Ptr;
   begin
      if Id = "" then
         Tmp_Id := Gtkada.Types.Null_Ptr;
      else
         Tmp_Id := New_String (Id);
      end if;
      Internal (Get_Object (Self), Tmp_Id);
      Free (Tmp_Id);
   end Set_Id;

   -------------------
   -- Set_Resizable --
   -------------------

   procedure Set_Resizable
      (Self      : not null access Gtk_Column_View_Column_Record;
       Resizable : Boolean)
   is
      procedure Internal (Self : System.Address; Resizable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_column_view_column_set_resizable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Resizable));
   end Set_Resizable;

   ----------------
   -- Set_Sorter --
   ----------------

   procedure Set_Sorter
      (Self   : not null access Gtk_Column_View_Column_Record;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
   is
      procedure Internal (Self : System.Address; Sorter : System.Address);
      pragma Import (C, Internal, "gtk_column_view_column_set_sorter");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Sorter)));
   end Set_Sorter;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gtk_Column_View_Column_Record;
       Title : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_column_view_column_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr;
   begin
      if Title = "" then
         Tmp_Title := Gtkada.Types.Null_Ptr;
      else
         Tmp_Title := New_String (Title);
      end if;
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
      (Self    : not null access Gtk_Column_View_Column_Record;
       Visible : Boolean)
   is
      procedure Internal (Self : System.Address; Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_column_view_column_set_visible");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Visible));
   end Set_Visible;

end Gtk.Column_View_Column;
