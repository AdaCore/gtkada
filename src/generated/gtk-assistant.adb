------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Assistant is

   procedure C_Gtk_Assistant_Set_Forward_Page_Func
      (Assistant : System.Address;
       Page_Func : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Assistant_Set_Forward_Page_Func, "gtk_assistant_set_forward_page_func");
   --  Sets the page forwarding function to be Page_Func.
   --  This function will be used to determine what will be the next page when
   --  the user presses the forward button. Setting Page_Func to null will make
   --  the assistant to use the default forward function, which just goes to
   --  the next visible page.
   --  Since: gtk+ 2.10
   --  "page_func": the Gtk_Assistant_Page_Func, or null to use the default
   --  one
   --  "data": user data for Page_Func
   --  "destroy": destroy notifier for Data

   function To_Gtk_Assistant_Page_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Assistant_Page_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Assistant_Page_Func, System.Address);

   function Internal_Gtk_Assistant_Page_Func
      (Current_Page : Glib.Gint;
       Data         : System.Address) return Glib.Gint;
   pragma Convention (C, Internal_Gtk_Assistant_Page_Func);
   --  "current_page": The page number used to calculate the next page.
   --  "data": user data.

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
      Assistant : constant Gtk_Assistant := new Gtk_Assistant_Record;
   begin
      Gtk.Assistant.Initialize (Assistant);
      return Assistant;
   end Gtk_Assistant_New;

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

   procedure Initialize
      (Assistant : not null access Gtk_Assistant_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_assistant_new");
   begin
      if not Assistant.Is_Created then
         Set_Object (Assistant, Internal);
      end if;
   end Initialize;

   -----------------------
   -- Add_Action_Widget --
   -----------------------

   procedure Add_Action_Widget
      (Assistant : not null access Gtk_Assistant_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
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
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_append_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page));
   end Append_Page;

   ------------
   -- Commit --
   ------------

   procedure Commit (Assistant : not null access Gtk_Assistant_Record) is
      procedure Internal (Assistant : System.Address);
      pragma Import (C, Internal, "gtk_assistant_commit");
   begin
      Internal (Get_Object (Assistant));
   end Commit;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page
      (Assistant : not null access Gtk_Assistant_Record) return Glib.Gint
   is
      function Internal (Assistant : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_get_current_page");
   begin
      return Internal (Get_Object (Assistant));
   end Get_Current_Page;

   -----------------
   -- Get_N_Pages --
   -----------------

   function Get_N_Pages
      (Assistant : not null access Gtk_Assistant_Record) return Glib.Gint
   is
      function Internal (Assistant : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_get_n_pages");
   begin
      return Internal (Get_Object (Assistant));
   end Get_N_Pages;

   ------------------
   -- Get_Nth_Page --
   ------------------

   function Get_Nth_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page_Num  : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Assistant : System.Address;
          Page_Num  : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_nth_page");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Assistant), Page_Num), Stub_Gtk_Widget));
   end Get_Nth_Page;

   -----------------------
   -- Get_Page_Complete --
   -----------------------

   function Get_Page_Complete
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_assistant_get_page_complete");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page)) /= 0;
   end Get_Page_Complete;

   --------------------------
   -- Get_Page_Has_Padding --
   --------------------------

   function Get_Page_Has_Padding
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_assistant_get_page_has_padding");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page)) /= 0;
   end Get_Page_Has_Padding;

   ---------------------------
   -- Get_Page_Header_Image --
   ---------------------------

   function Get_Page_Header_Image
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
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
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
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
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_assistant_get_page_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Assistant), Get_Object (Page)));
   end Get_Page_Title;

   -------------------
   -- Get_Page_Type --
   -------------------

   function Get_Page_Type
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
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
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Position  : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_insert_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page), Position);
   end Insert_Page;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Assistant : not null access Gtk_Assistant_Record) is
      procedure Internal (Assistant : System.Address);
      pragma Import (C, Internal, "gtk_assistant_next_page");
   begin
      Internal (Get_Object (Assistant));
   end Next_Page;

   ------------------
   -- Prepend_Page --
   ------------------

   function Prepend_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_assistant_prepend_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page));
   end Prepend_Page;

   -------------------
   -- Previous_Page --
   -------------------

   procedure Previous_Page
      (Assistant : not null access Gtk_Assistant_Record)
   is
      procedure Internal (Assistant : System.Address);
      pragma Import (C, Internal, "gtk_assistant_previous_page");
   begin
      Internal (Get_Object (Assistant));
   end Previous_Page;

   --------------------------
   -- Remove_Action_Widget --
   --------------------------

   procedure Remove_Action_Widget
      (Assistant : not null access Gtk_Assistant_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Child     : System.Address);
      pragma Import (C, Internal, "gtk_assistant_remove_action_widget");
   begin
      Internal (Get_Object (Assistant), Get_Object (Child));
   end Remove_Action_Widget;

   -----------------
   -- Remove_Page --
   -----------------

   procedure Remove_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page_Num  : Glib.Gint)
   is
      procedure Internal (Assistant : System.Address; Page_Num : Glib.Gint);
      pragma Import (C, Internal, "gtk_assistant_remove_page");
   begin
      Internal (Get_Object (Assistant), Page_Num);
   end Remove_Page;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
      (Assistant : not null access Gtk_Assistant_Record;
       Page_Num  : Glib.Gint)
   is
      procedure Internal (Assistant : System.Address; Page_Num : Glib.Gint);
      pragma Import (C, Internal, "gtk_assistant_set_current_page");
   begin
      Internal (Get_Object (Assistant), Page_Num);
   end Set_Current_Page;

   ---------------------------
   -- Set_Forward_Page_Func --
   ---------------------------

   procedure Set_Forward_Page_Func
      (Assistant : not null access Gtk_Assistant_Record;
       Page_Func : Gtk_Assistant_Page_Func)
   is
   begin
      if Page_Func = null then
         C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Assistant), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Assistant), Internal_Gtk_Assistant_Page_Func'Address, To_Address (Page_Func), System.Null_Address);
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
         (Assistant : not null access Gtk.Assistant.Gtk_Assistant_Record'Class;
          Page_Func : Gtk_Assistant_Page_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Page_Func = null then
            C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Assistant), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Page_Func), Data);
            C_Gtk_Assistant_Set_Forward_Page_Func (Get_Object (Assistant), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Forward_Page_Func;

   end Set_Forward_Page_Func_User_Data;

   -----------------------
   -- Set_Page_Complete --
   -----------------------

   procedure Set_Page_Complete
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Complete  : Boolean)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Complete  : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_assistant_set_page_complete");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Boolean'Pos (Complete));
   end Set_Page_Complete;

   --------------------------
   -- Set_Page_Has_Padding --
   --------------------------

   procedure Set_Page_Has_Padding
      (Assistant   : not null access Gtk_Assistant_Record;
       Page        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Has_Padding : Boolean)
   is
      procedure Internal
         (Assistant   : System.Address;
          Page        : System.Address;
          Has_Padding : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_assistant_set_page_has_padding");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Boolean'Pos (Has_Padding));
   end Set_Page_Has_Padding;

   ---------------------------
   -- Set_Page_Header_Image --
   ---------------------------

   procedure Set_Page_Header_Image
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_assistant_set_page_header_image");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Get_Object_Or_Null (GObject (Pixbuf)));
   end Set_Page_Header_Image;

   -------------------------
   -- Set_Page_Side_Image --
   -------------------------

   procedure Set_Page_Side_Image
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_assistant_set_page_side_image");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Get_Object_Or_Null (GObject (Pixbuf)));
   end Set_Page_Side_Image;

   --------------------
   -- Set_Page_Title --
   --------------------

   procedure Set_Page_Title
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Title     : UTF8_String)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Title     : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_assistant_set_page_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Tmp_Title);
      Free (Tmp_Title);
   end Set_Page_Title;

   -------------------
   -- Set_Page_Type --
   -------------------

   procedure Set_Page_Type
      (Assistant : not null access Gtk_Assistant_Record;
       Page      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
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

   procedure Update_Buttons_State
      (Assistant : not null access Gtk_Assistant_Record)
   is
      procedure Internal (Assistant : System.Address);
      pragma Import (C, Internal, "gtk_assistant_update_buttons_state");
   begin
      Internal (Get_Object (Assistant));
   end Update_Buttons_State;

   use type System.Address;

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
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
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
