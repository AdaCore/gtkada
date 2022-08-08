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

package body Gtk.Print_Operation is

   package Type_Conversion_Gtk_Print_Operation is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Print_Operation_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Print_Operation);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Print_Operation) is
   begin
      Self := new Gtk_Print_Operation_Record;
      Gtk.Print_Operation.Initialize (Self);
   end Gtk_New;

   -----------------------------
   -- Gtk_Print_Operation_New --
   -----------------------------

   function Gtk_Print_Operation_New return Gtk_Print_Operation is
      Self : constant Gtk_Print_Operation := new Gtk_Print_Operation_Record;
   begin
      Gtk.Print_Operation.Initialize (Self);
      return Self;
   end Gtk_Print_Operation_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Print_Operation_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_print_operation_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Self : not null access Gtk_Print_Operation_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_cancel");
   begin
      Internal (Get_Object (Self));
   end Cancel;

   ----------------------
   -- Draw_Page_Finish --
   ----------------------

   procedure Draw_Page_Finish
      (Self : not null access Gtk_Print_Operation_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_draw_page_finish");
   begin
      Internal (Get_Object (Self));
   end Draw_Page_Finish;

   ----------------------------
   -- Get_Default_Page_Setup --
   ----------------------------

   function Get_Default_Page_Setup
      (Self : not null access Gtk_Print_Operation_Record)
       return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_operation_get_default_page_setup");
      Stub_Gtk_Page_Setup : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Page_Setup));
   end Get_Default_Page_Setup;

   --------------------------
   -- Get_Embed_Page_Setup --
   --------------------------

   function Get_Embed_Page_Setup
      (Self : not null access Gtk_Print_Operation_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_operation_get_embed_page_setup");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Embed_Page_Setup;

   ---------------
   -- Get_Error --
   ---------------

   procedure Get_Error (Self : not null access Gtk_Print_Operation_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_get_error");
   begin
      Internal (Get_Object (Self));
   end Get_Error;

   -----------------------
   -- Get_Has_Selection --
   -----------------------

   function Get_Has_Selection
      (Self : not null access Gtk_Print_Operation_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_operation_get_has_selection");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Selection;

   --------------------------
   -- Get_N_Pages_To_Print --
   --------------------------

   function Get_N_Pages_To_Print
      (Self : not null access Gtk_Print_Operation_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_print_operation_get_n_pages_to_print");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Pages_To_Print;

   ------------------------
   -- Get_Print_Settings --
   ------------------------

   function Get_Print_Settings
      (Self : not null access Gtk_Print_Operation_Record)
       return Gtk.Print_Settings.Gtk_Print_Settings
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_operation_get_print_settings");
      Stub_Gtk_Print_Settings : Gtk.Print_Settings.Gtk_Print_Settings_Record;
   begin
      return Gtk.Print_Settings.Gtk_Print_Settings (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Print_Settings));
   end Get_Print_Settings;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
      (Self : not null access Gtk_Print_Operation_Record)
       return Gtk_Print_Status
   is
      function Internal (Self : System.Address) return Gtk_Print_Status;
      pragma Import (C, Internal, "gtk_print_operation_get_status");
   begin
      return Internal (Get_Object (Self));
   end Get_Status;

   -----------------------
   -- Get_Status_String --
   -----------------------

   function Get_Status_String
      (Self : not null access Gtk_Print_Operation_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_operation_get_status_string");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Status_String;

   ---------------------------
   -- Get_Support_Selection --
   ---------------------------

   function Get_Support_Selection
      (Self : not null access Gtk_Print_Operation_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_operation_get_support_selection");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Support_Selection;

   -----------------
   -- Is_Finished --
   -----------------

   function Is_Finished
      (Self : not null access Gtk_Print_Operation_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_operation_is_finished");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Finished;

   ---------
   -- Run --
   ---------

   function Run
      (Self   : not null access Gtk_Print_Operation_Record;
       Action : Gtk_Print_Operation_Action;
       Parent : access Gtk.Window.Gtk_Window_Record'Class)
       return Gtk_Print_Operation_Result
   is
      function Internal
         (Self   : System.Address;
          Action : Gtk_Print_Operation_Action;
          Parent : System.Address) return Gtk_Print_Operation_Result;
      pragma Import (C, Internal, "gtk_print_operation_run");
   begin
      return Internal (Get_Object (Self), Action, Get_Object_Or_Null (GObject (Parent)));
   end Run;

   ---------------------
   -- Set_Allow_Async --
   ---------------------

   procedure Set_Allow_Async
      (Self        : not null access Gtk_Print_Operation_Record;
       Allow_Async : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Allow_Async : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_allow_async");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Allow_Async));
   end Set_Allow_Async;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
      (Self         : not null access Gtk_Print_Operation_Record;
       Current_Page : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Current_Page : Glib.Gint);
      pragma Import (C, Internal, "gtk_print_operation_set_current_page");
   begin
      Internal (Get_Object (Self), Current_Page);
   end Set_Current_Page;

   --------------------------
   -- Set_Custom_Tab_Label --
   --------------------------

   procedure Set_Custom_Tab_Label
      (Self  : not null access Gtk_Print_Operation_Record;
       Label : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_operation_set_custom_tab_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Tmp_Label);
      Free (Tmp_Label);
   end Set_Custom_Tab_Label;

   ----------------------------
   -- Set_Default_Page_Setup --
   ----------------------------

   procedure Set_Default_Page_Setup
      (Self               : not null access Gtk_Print_Operation_Record;
       Default_Page_Setup : access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class)
   is
      procedure Internal
         (Self               : System.Address;
          Default_Page_Setup : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_set_default_page_setup");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Default_Page_Setup)));
   end Set_Default_Page_Setup;

   -----------------------
   -- Set_Defer_Drawing --
   -----------------------

   procedure Set_Defer_Drawing
      (Self : not null access Gtk_Print_Operation_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_set_defer_drawing");
   begin
      Internal (Get_Object (Self));
   end Set_Defer_Drawing;

   --------------------------
   -- Set_Embed_Page_Setup --
   --------------------------

   procedure Set_Embed_Page_Setup
      (Self  : not null access Gtk_Print_Operation_Record;
       Embed : Boolean)
   is
      procedure Internal (Self : System.Address; Embed : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_embed_page_setup");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Embed));
   end Set_Embed_Page_Setup;

   -------------------------
   -- Set_Export_Filename --
   -------------------------

   procedure Set_Export_Filename
      (Self     : not null access Gtk_Print_Operation_Record;
       Filename : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Filename : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_operation_set_export_filename");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
   begin
      Internal (Get_Object (Self), Tmp_Filename);
      Free (Tmp_Filename);
   end Set_Export_Filename;

   -----------------------
   -- Set_Has_Selection --
   -----------------------

   procedure Set_Has_Selection
      (Self          : not null access Gtk_Print_Operation_Record;
       Has_Selection : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Has_Selection : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_has_selection");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Has_Selection));
   end Set_Has_Selection;

   ------------------
   -- Set_Job_Name --
   ------------------

   procedure Set_Job_Name
      (Self     : not null access Gtk_Print_Operation_Record;
       Job_Name : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Job_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_operation_set_job_name");
      Tmp_Job_Name : Gtkada.Types.Chars_Ptr := New_String (Job_Name);
   begin
      Internal (Get_Object (Self), Tmp_Job_Name);
      Free (Tmp_Job_Name);
   end Set_Job_Name;

   -----------------
   -- Set_N_Pages --
   -----------------

   procedure Set_N_Pages
      (Self    : not null access Gtk_Print_Operation_Record;
       N_Pages : Glib.Gint)
   is
      procedure Internal (Self : System.Address; N_Pages : Glib.Gint);
      pragma Import (C, Internal, "gtk_print_operation_set_n_pages");
   begin
      Internal (Get_Object (Self), N_Pages);
   end Set_N_Pages;

   ------------------------
   -- Set_Print_Settings --
   ------------------------

   procedure Set_Print_Settings
      (Self           : not null access Gtk_Print_Operation_Record;
       Print_Settings : access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class)
   is
      procedure Internal
         (Self           : System.Address;
          Print_Settings : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_set_print_settings");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Print_Settings)));
   end Set_Print_Settings;

   -----------------------
   -- Set_Show_Progress --
   -----------------------

   procedure Set_Show_Progress
      (Self          : not null access Gtk_Print_Operation_Record;
       Show_Progress : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Show_Progress : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_show_progress");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Show_Progress));
   end Set_Show_Progress;

   ---------------------------
   -- Set_Support_Selection --
   ---------------------------

   procedure Set_Support_Selection
      (Self              : not null access Gtk_Print_Operation_Record;
       Support_Selection : Boolean)
   is
      procedure Internal
         (Self              : System.Address;
          Support_Selection : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_support_selection");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Support_Selection));
   end Set_Support_Selection;

   ----------------------------
   -- Set_Track_Print_Status --
   ----------------------------

   procedure Set_Track_Print_Status
      (Self         : not null access Gtk_Print_Operation_Record;
       Track_Status : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Track_Status : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_track_print_status");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Track_Status));
   end Set_Track_Print_Status;

   --------------
   -- Set_Unit --
   --------------

   procedure Set_Unit
      (Self : not null access Gtk_Print_Operation_Record;
       Unit : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal (Self : System.Address; Unit : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_print_operation_set_unit");
   begin
      Internal (Get_Object (Self), Unit);
   end Set_Unit;

   -----------------------
   -- Set_Use_Full_Page --
   -----------------------

   procedure Set_Use_Full_Page
      (Self      : not null access Gtk_Print_Operation_Record;
       Full_Page : Boolean)
   is
      procedure Internal (Self : System.Address; Full_Page : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_operation_set_use_full_page");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Full_Page));
   end Set_Use_Full_Page;

   -----------------
   -- End_Preview --
   -----------------

   procedure End_Preview
      (Preview : not null access Gtk_Print_Operation_Record)
   is
      procedure Internal (Preview : System.Address);
      pragma Import (C, Internal, "gtk_print_operation_preview_end_preview");
   begin
      Internal (Get_Object (Preview));
   end End_Preview;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
      (Preview : not null access Gtk_Print_Operation_Record;
       Page_Nr : Glib.Gint) return Boolean
   is
      function Internal
         (Preview : System.Address;
          Page_Nr : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_operation_preview_is_selected");
   begin
      return Internal (Get_Object (Preview), Page_Nr) /= 0;
   end Is_Selected;

   -----------------
   -- Render_Page --
   -----------------

   procedure Render_Page
      (Preview : not null access Gtk_Print_Operation_Record;
       Page_Nr : Glib.Gint)
   is
      procedure Internal (Preview : System.Address; Page_Nr : Glib.Gint);
      pragma Import (C, Internal, "gtk_print_operation_preview_render_page");
   begin
      Internal (Get_Object (Preview), Page_Nr);
   end Render_Page;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Gtk_Print_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Gtk_Print_Context_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Print_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Print_Context_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_GObject, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_GObject);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_GObject, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_GObject);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Print_Operation_Result_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Print_Operation_Result_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Print_Context_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Print_Context_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Gtk_Print_Context_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Gtk_Print_Context_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Print_Context_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Print_Context_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Context_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_GObject;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Context_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GObject;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Operation_Result_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_GObject
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_GObject);

   procedure Marsh_GObject_Gtk_Print_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Print_Context_Boolean);

   procedure Marsh_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void);

   procedure Marsh_GObject_Gtk_Print_Context_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Print_Context_Gint_Void);

   procedure Marsh_GObject_Gtk_Print_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Print_Context_Void);

   procedure Marsh_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean);

   procedure Marsh_GObject_Gtk_Print_Operation_Result_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Print_Operation_Result_Void);

   procedure Marsh_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void);

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

   procedure Marsh_Gtk_Print_Operation_GObject
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_GObject);

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Gtk_Print_Context_Boolean);

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void);

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void);

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Gtk_Print_Context_Void);

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean);

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void);

   procedure Marsh_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void);

   procedure Marsh_Gtk_Print_Operation_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Gtk_Widget_Void);

   procedure Marsh_Gtk_Print_Operation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Context_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Gtk_Print_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_GObject;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_GObject'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Context_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Gtk_Print_Context_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Print_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GObject;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_GObject'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
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

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Operation_Result_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Print_Operation_Result_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Print_Context_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Print_Context_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Print_Operation_Record'Class;
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
      (Object  : access Gtk_Print_Operation_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------
   -- Marsh_GObject_GObject --
   ---------------------------

   procedure Marsh_GObject_GObject
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_GObject := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased not null access Glib.Object.GObject_Record'Class := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_GObject;

   ---------------------------------------------
   -- Marsh_GObject_Gtk_Print_Context_Boolean --
   ---------------------------------------------

   procedure Marsh_GObject_Gtk_Print_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Print_Context_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Print_Context_Boolean;

   --------------------------------------------------------------
   -- Marsh_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void --
   --------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2), Gtk.Page_Setup.Gtk_Page_Setup (Unchecked_To_Object (Params, 3)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;

   -----------------------------------------------
   -- Marsh_GObject_Gtk_Print_Context_Gint_Void --
   -----------------------------------------------

   procedure Marsh_GObject_Gtk_Print_Context_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Print_Context_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Print_Context_Gint_Void;

   ------------------------------------------
   -- Marsh_GObject_Gtk_Print_Context_Void --
   ------------------------------------------

   procedure Marsh_GObject_Gtk_Print_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Print_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Print_Context_Void;

   ------------------------------------------------------------------------------------
   -- Marsh_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean --
   ------------------------------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview (Unchecked_To_Interface (Params, 1)), Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 2)), Gtk.Window.Gtk_Window (Unchecked_To_Object (Params, 3)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;

   ---------------------------------------------------
   -- Marsh_GObject_Gtk_Print_Operation_Result_Void --
   ---------------------------------------------------

   procedure Marsh_GObject_Gtk_Print_Operation_Result_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Print_Operation_Result_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Print_Operation_Result (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Print_Operation_Result_Void;

   ---------------------------------------------------------------------
   -- Marsh_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void --
   ---------------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Gtk.Page_Setup.Gtk_Page_Setup (Unchecked_To_Object (Params, 2)), Gtk.Print_Settings.Gtk_Print_Settings (Unchecked_To_Object (Params, 3)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;

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

   ---------------------------------------
   -- Marsh_Gtk_Print_Operation_GObject --
   ---------------------------------------

   procedure Marsh_Gtk_Print_Operation_GObject
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_GObject := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
      V   : aliased not null access Glib.Object.GObject_Record'Class := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_GObject;

   ---------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Gtk_Print_Context_Boolean --
   ---------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Gtk_Print_Context_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Gtk_Print_Context_Boolean;

   --------------------------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void --
   --------------------------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2), Gtk.Page_Setup.Gtk_Page_Setup (Unchecked_To_Object (Params, 3)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;

   -----------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void --
   -----------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void;

   ------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Gtk_Print_Context_Void --
   ------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Gtk_Print_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Gtk_Print_Context_Void;

   ------------------------------------------------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean --
   ------------------------------------------------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview (Unchecked_To_Interface (Params, 1)), Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 2)), Gtk.Window.Gtk_Window (Unchecked_To_Object (Params, 3)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;

   ---------------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void --
   ---------------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Print_Operation_Result (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void;

   ---------------------------------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void --
   ---------------------------------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Gtk.Page_Setup.Gtk_Page_Setup (Unchecked_To_Object (Params, 2)), Gtk.Print_Settings.Gtk_Print_Settings (Unchecked_To_Object (Params, 3)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;

   -----------------------------------------------
   -- Marsh_Gtk_Print_Operation_Gtk_Widget_Void --
   -----------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Gtk_Widget_Void;

   ------------------------------------
   -- Marsh_Gtk_Print_Operation_Void --
   ------------------------------------

   procedure Marsh_Gtk_Print_Operation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation := Gtk_Print_Operation (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Void;

   --------------------
   -- On_Begin_Print --
   --------------------

   procedure On_Begin_Print
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "begin-print" & ASCII.NUL, Call, After);
   end On_Begin_Print;

   --------------------
   -- On_Begin_Print --
   --------------------

   procedure On_Begin_Print
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "begin-print" & ASCII.NUL, Call, After, Slot);
   end On_Begin_Print;

   -----------------------------
   -- On_Create_Custom_Widget --
   -----------------------------

   procedure On_Create_Custom_Widget
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_GObject;
       After : Boolean := False)
   is
   begin
      Connect (Self, "create-custom-widget" & ASCII.NUL, Call, After);
   end On_Create_Custom_Widget;

   -----------------------------
   -- On_Create_Custom_Widget --
   -----------------------------

   procedure On_Create_Custom_Widget
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_GObject;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "create-custom-widget" & ASCII.NUL, Call, After, Slot);
   end On_Create_Custom_Widget;

   ----------------------------
   -- On_Custom_Widget_Apply --
   ----------------------------

   procedure On_Custom_Widget_Apply
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "custom-widget-apply" & ASCII.NUL, Call, After);
   end On_Custom_Widget_Apply;

   ----------------------------
   -- On_Custom_Widget_Apply --
   ----------------------------

   procedure On_Custom_Widget_Apply
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "custom-widget-apply" & ASCII.NUL, Call, After, Slot);
   end On_Custom_Widget_Apply;

   -------------
   -- On_Done --
   -------------

   procedure On_Done
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "done" & ASCII.NUL, Call, After);
   end On_Done;

   -------------
   -- On_Done --
   -------------

   procedure On_Done
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Operation_Result_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "done" & ASCII.NUL, Call, After, Slot);
   end On_Done;

   ------------------
   -- On_Draw_Page --
   ------------------

   procedure On_Draw_Page
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "draw-page" & ASCII.NUL, Call, After);
   end On_Draw_Page;

   ------------------
   -- On_Draw_Page --
   ------------------

   procedure On_Draw_Page
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "draw-page" & ASCII.NUL, Call, After, Slot);
   end On_Draw_Page;

   ------------------
   -- On_End_Print --
   ------------------

   procedure On_End_Print
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "end-print" & ASCII.NUL, Call, After);
   end On_End_Print;

   ------------------
   -- On_End_Print --
   ------------------

   procedure On_End_Print
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "end-print" & ASCII.NUL, Call, After, Slot);
   end On_End_Print;

   -----------------
   -- On_Paginate --
   -----------------

   procedure On_Paginate
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "paginate" & ASCII.NUL, Call, After);
   end On_Paginate;

   -----------------
   -- On_Paginate --
   -----------------

   procedure On_Paginate
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "paginate" & ASCII.NUL, Call, After, Slot);
   end On_Paginate;

   ----------------
   -- On_Preview --
   ----------------

   procedure On_Preview
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "preview" & ASCII.NUL, Call, After);
   end On_Preview;

   ----------------
   -- On_Preview --
   ----------------

   procedure On_Preview
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "preview" & ASCII.NUL, Call, After, Slot);
   end On_Preview;

   ---------------------------
   -- On_Request_Page_Setup --
   ---------------------------

   procedure On_Request_Page_Setup
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "request-page-setup" & ASCII.NUL, Call, After);
   end On_Request_Page_Setup;

   ---------------------------
   -- On_Request_Page_Setup --
   ---------------------------

   procedure On_Request_Page_Setup
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "request-page-setup" & ASCII.NUL, Call, After, Slot);
   end On_Request_Page_Setup;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   procedure On_Status_Changed
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "status-changed" & ASCII.NUL, Call, After);
   end On_Status_Changed;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   procedure On_Status_Changed
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "status-changed" & ASCII.NUL, Call, After, Slot);
   end On_Status_Changed;

   -----------------------------
   -- On_Update_Custom_Widget --
   -----------------------------

   procedure On_Update_Custom_Widget
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "update-custom-widget" & ASCII.NUL, Call, After);
   end On_Update_Custom_Widget;

   -----------------------------
   -- On_Update_Custom_Widget --
   -----------------------------

   procedure On_Update_Custom_Widget
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "update-custom-widget" & ASCII.NUL, Call, After, Slot);
   end On_Update_Custom_Widget;

end Gtk.Print_Operation;
