-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Assistant is

   function To_Gtk_Assistant_Page_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Assistant_Page_Func);

   procedure C_Gtk_Assistant_Set_Forward_Page_Func
      (Assistant : System.Address;
       Page_Func : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Assistant_Set_Forward_Page_Func, "gtk_assistant_set_forward_page_func");
   --  Sets the page forwarding function to be Page_Func, this function will
   --  be used to determine what will be the next page when the user presses
   --  the forward button. Setting Page_Func to null will make the assistant to
   --  use the default forward function, which just goes to the next visible
   --  page.
   --  Since: gtk+ 2.10
   --  "page_func": the Gtk.Assistant.Gtk_Assistant_Page_Func, or null to use
   --  the default one
   --  "data": user data for Page_Func
   --  "destroy": destroy notifier for Data

   function Internal_Gtk_Assistant_Page_Func
      (Current_Page : Gint;
       Data         : System.Address) return Gint;
   pragma Convention (C, Internal_Gtk_Assistant_Page_Func);
   --  "current_page": The page number used to calculate the next page.
   --  "data": user data.

   --------------------------------------
   -- Internal_Gtk_Assistant_Page_Func --
   --------------------------------------

   function Internal_Gtk_Assistant_Page_Func
      (Current_Page : Gint;
       Data         : System.Address) return Gint
   is
      Func : constant Gtk_Assistant_Page_Func := To_Gtk_Assistant_Page_Func (Data);
   begin
      return Func (Current_Page);
   end Internal_Gtk_Assistant_Page_Func;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Assistant_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Assistant : out Gtk_Assistant) is
   begin
      Assistant := new Gtk_Assistant_Record;
      Gtk.Assistant.Initialize (Assistant);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Assistant : access Gtk_Assistant_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_assistant_new");
   begin
      Set_Object (Assistant, Internal);
   end Initialize;

   -----------------------
   -- Add_Action_Widget --
   -----------------------

   procedure Add_Action_Widget
      (Assistant : access Gtk_Assistant_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Child     : System.Address);
      pragma Import (C, Internal, "gtk_assistant_add_action_widget");
   begin
      Internal (Get_Object (Assistant), Get_Object (Child));
   end Add_Action_Widget;

   -----------------
   -- Append_Page --
   -----------------

   function Append_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_assistant_append_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page));
   end Append_Page;

   ------------
   -- Commit --
   ------------

   procedure Commit (Assistant : access Gtk_Assistant_Record) is
      procedure Internal (Assistant : System.Address);
      pragma Import (C, Internal, "gtk_assistant_commit");
   begin
      Internal (Get_Object (Assistant));
   end Commit;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page
      (Assistant : access Gtk_Assistant_Record) return Gint
   is
      function Internal (Assistant : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_assistant_get_current_page");
   begin
      return Internal (Get_Object (Assistant));
   end Get_Current_Page;

   -----------------
   -- Get_N_Pages --
   -----------------

   function Get_N_Pages
      (Assistant : access Gtk_Assistant_Record) return Gint
   is
      function Internal (Assistant : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_assistant_get_n_pages");
   begin
      return Internal (Get_Object (Assistant));
   end Get_N_Pages;

   ------------------
   -- Get_Nth_Page --
   ------------------

   function Get_Nth_Page
      (Assistant : access Gtk_Assistant_Record;
       Page_Num  : Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Assistant : System.Address;
          Page_Num  : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_nth_page");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Assistant), Page_Num), Stub_Gtk_Widget));
   end Get_Nth_Page;

   -----------------------
   -- Get_Page_Complete --
   -----------------------

   function Get_Page_Complete
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_assistant_get_page_complete");
   begin
      return Boolean'Val (Internal (Get_Object (Assistant), Get_Object (Page)));
   end Get_Page_Complete;

   ---------------------------
   -- Get_Page_Header_Image --
   ---------------------------

   function Get_Page_Header_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_page_header_image");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Assistant), Get_Object (Page)), Stub_Gdk_Pixbuf));
   end Get_Page_Header_Image;

   -------------------------
   -- Get_Page_Side_Image --
   -------------------------

   function Get_Page_Side_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_page_side_image");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Assistant), Get_Object (Page)), Stub_Gdk_Pixbuf));
   end Get_Page_Side_Image;

   --------------------
   -- Get_Page_Title --
   --------------------

   function Get_Page_Title
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_assistant_get_page_title");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Assistant), Get_Object (Page)));
   end Get_Page_Title;

   -------------------
   -- Get_Page_Type --
   -------------------

   function Get_Page_Type
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Assistant_Page_Type
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Gtk_Assistant_Page_Type;
      pragma Import (C, Internal, "gtk_assistant_get_page_type");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page));
   end Get_Page_Type;

   -----------------
   -- Insert_Page --
   -----------------

   function Insert_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Gint) return Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Position  : Gint) return Gint;
      pragma Import (C, Internal, "gtk_assistant_insert_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page), Position);
   end Insert_Page;

   ------------------
   -- Prepend_Page --
   ------------------

   function Prepend_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_assistant_prepend_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page));
   end Prepend_Page;

   --------------------------
   -- Remove_Action_Widget --
   --------------------------

   procedure Remove_Action_Widget
      (Assistant : access Gtk_Assistant_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Child     : System.Address);
      pragma Import (C, Internal, "gtk_assistant_remove_action_widget");
   begin
      Internal (Get_Object (Assistant), Get_Object (Child));
   end Remove_Action_Widget;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
      (Assistant : access Gtk_Assistant_Record;
       Page_Num  : Gint)
   is
      procedure Internal (Assistant : System.Address; Page_Num : Gint);
      pragma Import (C, Internal, "gtk_assistant_set_current_page");
   begin
      Internal (Get_Object (Assistant), Page_Num);
   end Set_Current_Page;

   ---------------------------
   -- Set_Forward_Page_Func --
   ---------------------------

   procedure Set_Forward_Page_Func
      (Assistant : access Gtk_Assistant_Record;
       Page_Func : Gtk_Assistant_Page_Func)
   is
   begin
      C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Assistant), Internal_Gtk_Assistant_Page_Func'Address, Page_Func'Address, System.Null_Address);
   end Set_Forward_Page_Func;

   package body Set_Forward_Page_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);
      function To_Gtk_Assistant_Page_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Assistant_Page_Func);

      function Internal_Cb
         (Current_Page : Gint;
          Data         : System.Address) return Gint;
      --  A function used by Gtk.Assistant.Set_Forward_Page_Func to know which
      --  is the next page given a current one. It's called both for computing
      --  the next page when the user presses the "forward" button and for
      --  handling the behavior of the "last" button.
      --  "current_page": The page number used to calculate the next page.
      --  "data": user data.

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Current_Page : Gint;
          Data         : System.Address) return Gint
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return To_Gtk_Assistant_Page_Func (D.Func) (Current_Page, D.Data.all);
      end Internal_Cb;

      ---------------------------
      -- Set_Forward_Page_Func --
      ---------------------------

      procedure Set_Forward_Page_Func
         (Assistant : access Gtk.Assistant.Gtk_Assistant_Record'Class;
          Page_Func : Gtk_Assistant_Page_Func;
          Data      : User_Data_Type)
      is
      begin
         C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Assistant), Internal_Cb'Address, Users.Build (Page_Func'Address, Data), Users.Free_Data'Address);
      end Set_Forward_Page_Func;

   end Set_Forward_Page_Func_User_Data;

   -----------------------
   -- Set_Page_Complete --
   -----------------------

   procedure Set_Page_Complete
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Complete  : Boolean)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Complete  : Integer);
      pragma Import (C, Internal, "gtk_assistant_set_page_complete");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Boolean'Pos (Complete));
   end Set_Page_Complete;

   ---------------------------
   -- Set_Page_Header_Image --
   ---------------------------

   procedure Set_Page_Header_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_assistant_set_page_header_image");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Get_Object (Pixbuf));
   end Set_Page_Header_Image;

   -------------------------
   -- Set_Page_Side_Image --
   -------------------------

   procedure Set_Page_Side_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_assistant_set_page_side_image");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Get_Object (Pixbuf));
   end Set_Page_Side_Image;

   --------------------
   -- Set_Page_Title --
   --------------------

   procedure Set_Page_Title
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Title     : UTF8_String)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Title     : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_assistant_set_page_title");
      Tmp_Title : Interfaces.C.Strings.chars_ptr := New_String (Title);
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Tmp_Title);
      Free (Tmp_Title);
   end Set_Page_Title;

   -------------------
   -- Set_Page_Type --
   -------------------

   procedure Set_Page_Type
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       The_Type  : Gtk_Assistant_Page_Type)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          The_Type  : Gtk_Assistant_Page_Type);
      pragma Import (C, Internal, "gtk_assistant_set_page_type");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), The_Type);
   end Set_Page_Type;

   --------------------------
   -- Update_Buttons_State --
   --------------------------

   procedure Update_Buttons_State (Assistant : access Gtk_Assistant_Record) is
      procedure Internal (Assistant : System.Address);
      pragma Import (C, Internal, "gtk_assistant_update_buttons_state");
   begin
      Internal (Get_Object (Assistant));
   end Update_Buttons_State;

end Gtk.Assistant;
