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
with Ada.Unchecked_Conversion;
with Gdk.Display;
with Gdk.Surface;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Assistant is

   procedure C_Gtk_Assistant_Set_Forward_Page_Func
      (Self      : System.Address;
       Page_Func : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Assistant_Set_Forward_Page_Func, "gtk_assistant_set_forward_page_func");
   pragma Obsolescent (C_Gtk_Assistant_Set_Forward_Page_Func);
   --  Sets the page forwarding function to be Page_Func.
   --  This function will be used to determine what will be the next page when
   --  the user presses the forward button. Setting Page_Func to null will make
   --  the assistant to use the default forward function, which just goes to
   --  the next visible page.
   --  Deprecated since 4.10, 1
   --  @param Page_Func the `GtkAssistantPageFunc`, or null to use the default
   --  one
   --  @param Data user data for Page_Func
   --  @param Destroy destroy notifier for Data

   function To_Gtk_Assistant_Page_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Assistant_Page_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Assistant_Page_Func, System.Address);

   function Internal_Gtk_Assistant_Page_Func
      (Current_Page : Glib.Gint;
       Data         : System.Address) return Glib.Gint;
   pragma Convention (C, Internal_Gtk_Assistant_Page_Func);
   --  @param Current_Page The page number used to calculate the next page.
   --  @param Data user data.

   --------------------------------------
   -- Internal_Gtk_Assistant_Page_Func --
   --------------------------------------

   function Internal_Gtk_Assistant_Page_Func
      (Current_Page : Glib.Gint;
       Data         : System.Address) return Glib.Gint
   is
      Func : constant Gtk_Assistant_Page_Func := To_Gtk_Assistant_Page_Func (Data);
   begin
      return Func (Current_Page);
   end Internal_Gtk_Assistant_Page_Func;

   package Type_Conversion_Gtk_Assistant is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Assistant_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Assistant);

   -----------------------
   -- Gtk_Assistant_New --
   -----------------------

   function Gtk_Assistant_New return Gtk_Assistant is
      Self : constant Gtk_Assistant := new Gtk_Assistant_Record;
   begin
      Gtk.Assistant.Initialize (Self);
      return Self;
   end Gtk_Assistant_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Assistant) is
   begin
      Self := new Gtk_Assistant_Record;
      Gtk.Assistant.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Assistant_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_assistant_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------------
   -- Add_Action_Widget --
   -----------------------

   procedure Add_Action_Widget
      (Self  : not null access Gtk_Assistant_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_assistant_add_action_widget");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Add_Action_Widget;

   -----------------
   -- Append_Page --
   -----------------

   function Append_Page
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Page : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_append_page");
   begin
      return Internal (Get_Object (Self), Get_Object (Page));
   end Append_Page;

   ------------
   -- Commit --
   ------------

   procedure Commit (Self : not null access Gtk_Assistant_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_assistant_commit");
   begin
      Internal (Get_Object (Self));
   end Commit;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page
      (Self : not null access Gtk_Assistant_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_get_current_page");
   begin
      return Internal (Get_Object (Self));
   end Get_Current_Page;

   -----------------
   -- Get_N_Pages --
   -----------------

   function Get_N_Pages
      (Self : not null access Gtk_Assistant_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_get_n_pages");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Pages;

   ------------------
   -- Get_Nth_Page --
   ------------------

   function Get_Nth_Page
      (Self     : not null access Gtk_Assistant_Record;
       Page_Num : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Self     : System.Address;
          Page_Num : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_nth_page");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self), Page_Num), Stub_Gtk_Widget));
   end Get_Nth_Page;

   --------------
   -- Get_Page --
   --------------

   function Get_Page
      (Self  : not null access Gtk_Assistant_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Assistant_Page.Gtk_Assistant_Page
   is
      function Internal
         (Self  : System.Address;
          Child : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_page");
      Stub_Gtk_Assistant_Page : Gtk.Assistant_Page.Gtk_Assistant_Page_Record;
   begin
      return Gtk.Assistant_Page.Gtk_Assistant_Page (Get_User_Data (Internal (Get_Object (Self), Get_Object (Child)), Stub_Gtk_Assistant_Page));
   end Get_Page;

   -----------------------
   -- Get_Page_Complete --
   -----------------------

   function Get_Page_Complete
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Self : System.Address;
          Page : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_assistant_get_page_complete");
   begin
      return Internal (Get_Object (Self), Get_Object (Page)) /= 0;
   end Get_Page_Complete;

   --------------------
   -- Get_Page_Title --
   --------------------

   function Get_Page_Title
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String
   is
      function Internal
         (Self : System.Address;
          Page : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_assistant_get_page_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self), Get_Object (Page)));
   end Get_Page_Title;

   -------------------
   -- Get_Page_Type --
   -------------------

   function Get_Page_Type
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Assistant_Page.Gtk_Assistant_Page_Type
   is
      function Internal
         (Self : System.Address;
          Page : System.Address)
          return Gtk.Assistant_Page.Gtk_Assistant_Page_Type;
      pragma Import (C, Internal, "gtk_assistant_get_page_type");
   begin
      return Internal (Get_Object (Self), Get_Object (Page));
   end Get_Page_Type;

   ---------------
   -- Get_Pages --
   ---------------

   function Get_Pages
      (Self : not null access Gtk_Assistant_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_assistant_get_pages");
   begin
      return Internal (Get_Object (Self));
   end Get_Pages;

   -----------------
   -- Insert_Page --
   -----------------

   function Insert_Page
      (Self     : not null access Gtk_Assistant_Record;
       Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Self     : System.Address;
          Page     : System.Address;
          Position : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_insert_page");
   begin
      return Internal (Get_Object (Self), Get_Object (Page), Position);
   end Insert_Page;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Self : not null access Gtk_Assistant_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_assistant_next_page");
   begin
      Internal (Get_Object (Self));
   end Next_Page;

   ------------------
   -- Prepend_Page --
   ------------------

   function Prepend_Page
      (Self : not null access Gtk_Assistant_Record;
       Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Page : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_prepend_page");
   begin
      return Internal (Get_Object (Self), Get_Object (Page));
   end Prepend_Page;

   -------------------
   -- Previous_Page --
   -------------------

   procedure Previous_Page (Self : not null access Gtk_Assistant_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_assistant_previous_page");
   begin
      Internal (Get_Object (Self));
   end Previous_Page;

   --------------------------
   -- Remove_Action_Widget --
   --------------------------

   procedure Remove_Action_Widget
      (Self  : not null access Gtk_Assistant_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_assistant_remove_action_widget");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Remove_Action_Widget;

   -----------------
   -- Remove_Page --
   -----------------

   procedure Remove_Page
      (Self     : not null access Gtk_Assistant_Record;
       Page_Num : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Page_Num : Glib.Gint);
      pragma Import (C, Internal, "gtk_assistant_remove_page");
   begin
      Internal (Get_Object (Self), Page_Num);
   end Remove_Page;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
      (Self     : not null access Gtk_Assistant_Record;
       Page_Num : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Page_Num : Glib.Gint);
      pragma Import (C, Internal, "gtk_assistant_set_current_page");
   begin
      Internal (Get_Object (Self), Page_Num);
   end Set_Current_Page;

   ---------------------------
   -- Set_Forward_Page_Func --
   ---------------------------

   procedure Set_Forward_Page_Func
      (Self      : not null access Gtk_Assistant_Record;
       Page_Func : Gtk_Assistant_Page_Func)
   is
   begin
      if Page_Func = null then
         C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Self), Internal_Gtk_Assistant_Page_Func'Address, To_Address (Page_Func), System.Null_Address);
      end if;
   end Set_Forward_Page_Func;

   package body Set_Forward_Page_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Assistant_Page_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Assistant_Page_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Assistant_Page_Func, System.Address);

      function Internal_Cb
         (Current_Page : Glib.Gint;
          Data         : System.Address) return Glib.Gint;
      pragma Convention (C, Internal_Cb);
      --  Type of callback used to calculate the next page in a
      --  `GtkAssistant`.
      --  It's called both for computing the next page when the user presses
      --  the "forward" button and for handling the behavior of the "last"
      --  button.
      --  See [methodGtk.Assistant.set_forward_page_func].
      --  @param Current_Page The page number used to calculate the next page.
      --  @param Data user data.
      --  @return The next page number

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Current_Page : Glib.Gint;
          Data         : System.Address) return Glib.Gint
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return To_Gtk_Assistant_Page_Func (D.Func) (Current_Page, D.Data.all);
      end Internal_Cb;

      ---------------------------
      -- Set_Forward_Page_Func --
      ---------------------------

      procedure Set_Forward_Page_Func
         (Self      : not null access Gtk.Assistant.Gtk_Assistant_Record'Class;
          Page_Func : Gtk_Assistant_Page_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Page_Func = null then
            C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Page_Func), Data);
            C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Forward_Page_Func;

   end Set_Forward_Page_Func_User_Data;

   -----------------------
   -- Set_Page_Complete --
   -----------------------

   procedure Set_Page_Complete
      (Self     : not null access Gtk_Assistant_Record;
       Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Complete : Boolean)
   is
      procedure Internal
         (Self     : System.Address;
          Page     : System.Address;
          Complete : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_assistant_set_page_complete");
   begin
      Internal (Get_Object (Self), Get_Object (Page), Boolean'Pos (Complete));
   end Set_Page_Complete;

   --------------------
   -- Set_Page_Title --
   --------------------

   procedure Set_Page_Title
      (Self  : not null access Gtk_Assistant_Record;
       Page  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Title : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Page  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_assistant_set_page_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Self), Get_Object (Page), Tmp_Title);
      Free (Tmp_Title);
   end Set_Page_Title;

   -------------------
   -- Set_Page_Type --
   -------------------

   procedure Set_Page_Type
      (Self     : not null access Gtk_Assistant_Record;
       Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       The_Type : Gtk.Assistant_Page.Gtk_Assistant_Page_Type)
   is
      procedure Internal
         (Self     : System.Address;
          Page     : System.Address;
          The_Type : Gtk.Assistant_Page.Gtk_Assistant_Page_Type);
      pragma Import (C, Internal, "gtk_assistant_set_page_type");
   begin
      Internal (Get_Object (Self), Get_Object (Page), The_Type);
   end Set_Page_Type;

   --------------------------
   -- Update_Buttons_State --
   --------------------------

   procedure Update_Buttons_State
      (Self : not null access Gtk_Assistant_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_assistant_update_buttons_state");
   begin
      Internal (Get_Object (Self));
   end Update_Buttons_State;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Assistant_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Assistant_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_Assistant_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X := Acc_X;
      Y := Acc_Y;
      Width := Acc_Width;
      Height := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gtk_Assistant_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------
   -- Get_Focus --
   ---------------

   function Get_Focus
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_focus");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Focus;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Assistant_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Assistant_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gtk_Assistant_Record) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_native_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   ---------------------------
   -- Get_Surface_Transform --
   ---------------------------

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Assistant_Record;
       X    : out Gdouble;
       Y    : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          X    : out Gdouble;
          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_native_get_surface_transform");
   begin
      Internal (Get_Object (Self), X, Y);
   end Get_Surface_Transform;

   -------------
   -- Realize --
   -------------

   procedure Realize (Self : not null access Gtk_Assistant_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_realize");
   begin
      Internal (Get_Object (Self));
   end Realize;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Assistant_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_Assistant_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_Assistant_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Assistant_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
      (Self  : not null access Gtk_Assistant_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Focus : System.Address);
      pragma Import (C, Internal, "gtk_root_set_focus");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Focus)));
   end Set_Focus;

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Self : not null access Gtk_Assistant_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_unrealize");
   begin
      Internal (Get_Object (Self));
   end Unrealize;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Assistant_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_Assistant_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Assistant_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Assistant_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Assistant_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Assistant_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Void);

   procedure Connect
      (Object  : access Gtk_Assistant_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Assistant_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Assistant_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Assistant_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Assistant_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Assistant_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Assistant_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Assistant_Gtk_Widget_Void);

   procedure Marsh_Gtk_Assistant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Assistant_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Assistant_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Assistant_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Assistant_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Assistant_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Assistant_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Assistant_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Assistant_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Assistant_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------
   -- Marsh_GObject_Gtk_Widget_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Void;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -----------------------------------------
   -- Marsh_Gtk_Assistant_Gtk_Widget_Void --
   -----------------------------------------

   procedure Marsh_Gtk_Assistant_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Assistant_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Assistant := Gtk_Assistant (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Assistant_Gtk_Widget_Void;

   ------------------------------
   -- Marsh_Gtk_Assistant_Void --
   ------------------------------

   procedure Marsh_Gtk_Assistant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Assistant_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Assistant := Gtk_Assistant (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Assistant_Void;

   --------------
   -- On_Apply --
   --------------

   procedure On_Apply
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "apply" & ASCII.NUL, Call, After);
   end On_Apply;

   --------------
   -- On_Apply --
   --------------

   procedure On_Apply
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "apply" & ASCII.NUL, Call, After, Slot);
   end On_Apply;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancel" & ASCII.NUL, Call, After);
   end On_Cancel;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cancel" & ASCII.NUL, Call, After, Slot);
   end On_Cancel;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "close" & ASCII.NUL, Call, After);
   end On_Close;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "close" & ASCII.NUL, Call, After, Slot);
   end On_Close;

   ---------------
   -- On_Escape --
   ---------------

   procedure On_Escape
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "escape" & ASCII.NUL, Call, After);
   end On_Escape;

   ---------------
   -- On_Escape --
   ---------------

   procedure On_Escape
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "escape" & ASCII.NUL, Call, After, Slot);
   end On_Escape;

   ----------------
   -- On_Prepare --
   ----------------

   procedure On_Prepare
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_Gtk_Assistant_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "prepare" & ASCII.NUL, Call, After);
   end On_Prepare;

   ----------------
   -- On_Prepare --
   ----------------

   procedure On_Prepare
      (Self  : not null access Gtk_Assistant_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "prepare" & ASCII.NUL, Call, After, Slot);
   end On_Prepare;

end Gtk.Assistant;
