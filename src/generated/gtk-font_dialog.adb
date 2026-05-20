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
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Font_Dialog is

   procedure C_Gtk_Font_Dialog_Choose_Face
      (Self          : System.Address;
       Parent        : System.Address;
       Initial_Value : System.Address;
       Cancellable   : System.Address;
       Callback      : System.Address;
       User_Data     : System.Address);
   pragma Import (C, C_Gtk_Font_Dialog_Choose_Face, "gtk_font_dialog_choose_face");
   --  Presents a font chooser dialog to the user.
   --  The font chooser dialog will be set up for selecting a font face.
   --  A font face represents a font family and style, but no specific font
   --  size.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Initial_Value the initial value
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_Font_Dialog_Choose_Family
      (Self          : System.Address;
       Parent        : System.Address;
       Initial_Value : System.Address;
       Cancellable   : System.Address;
       Callback      : System.Address;
       User_Data     : System.Address);
   pragma Import (C, C_Gtk_Font_Dialog_Choose_Family, "gtk_font_dialog_choose_family");
   --  Presents a font chooser dialog to the user.
   --  The font chooser dialog will be set up for selecting a font family.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Initial_Value the initial value
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_Font_Dialog_Choose_Font
      (Self          : System.Address;
       Parent        : System.Address;
       Initial_Value : Pango.Font.Pango_Font_Description;
       Cancellable   : System.Address;
       Callback      : System.Address;
       User_Data     : System.Address);
   pragma Import (C, C_Gtk_Font_Dialog_Choose_Font, "gtk_font_dialog_choose_font");
   --  Presents a font chooser dialog to the user.
   --  The font chooser dialog will be set up for selecting a font.
   --  If you want to let the user select font features as well, use
   --  [methodGtk.FontDialog.choose_font_and_features] instead.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Initial_Value the font to select initially
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gasync_Ready_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gasync_Ready_Callback, System.Address);

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address);
   pragma Convention (C, Internal_Gasync_Ready_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param User_Data user data passed to the callback.

   ------------------------------------
   -- Internal_Gasync_Ready_Callback --
   ------------------------------------

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address)
   is
      Func         : constant Gasync_Ready_Callback := To_Gasync_Ready_Callback (User_Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Gasync_Ready_Callback;

   package Type_Conversion_Gtk_Font_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Font_Dialog);

   -------------------------
   -- Gtk_Font_Dialog_New --
   -------------------------

   function Gtk_Font_Dialog_New return Gtk_Font_Dialog is
      Self : constant Gtk_Font_Dialog := new Gtk_Font_Dialog_Record;
   begin
      Gtk.Font_Dialog.Initialize (Self);
      return Self;
   end Gtk_Font_Dialog_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Font_Dialog) is
   begin
      Self := new Gtk_Font_Dialog_Record;
      Gtk.Font_Dialog.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Font_Dialog_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_dialog_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------
   -- Choose_Face --
   -----------------

   procedure Choose_Face
      (Self          : not null access Gtk_Font_Dialog_Record;
       Parent        : access Gtk.Window.Gtk_Window_Record'Class;
       Initial_Value : access Pango.Font_Face.Pango_Font_Face_Record'Class;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Font_Dialog_Choose_Face (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Initial_Value)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Font_Dialog_Choose_Face (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Initial_Value)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Choose_Face;

   ------------------------
   -- Choose_Face_Finish --
   ------------------------

   function Choose_Face_Finish
      (Self   : not null access Gtk_Font_Dialog_Record;
       Result : Glib.G_Async_Result) return Pango.Font_Face.Pango_Font_Face
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "gtk_font_dialog_choose_face_finish");
      Stub_Pango_Font_Face : Pango.Font_Face.Pango_Font_Face_Record;
   begin
      return Pango.Font_Face.Pango_Font_Face (Get_User_Data (Internal (Get_Object (Self), Result), Stub_Pango_Font_Face));
   end Choose_Face_Finish;

   -------------------
   -- Choose_Family --
   -------------------

   procedure Choose_Family
      (Self          : not null access Gtk_Font_Dialog_Record;
       Parent        : access Gtk.Window.Gtk_Window_Record'Class;
       Initial_Value : access Pango.Font_Family.Pango_Font_Family_Record'Class;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Font_Dialog_Choose_Family (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Initial_Value)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Font_Dialog_Choose_Family (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Initial_Value)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Choose_Family;

   --------------------------
   -- Choose_Family_Finish --
   --------------------------

   function Choose_Family_Finish
      (Self   : not null access Gtk_Font_Dialog_Record;
       Result : Glib.G_Async_Result)
       return Pango.Font_Family.Pango_Font_Family
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "gtk_font_dialog_choose_family_finish");
      Stub_Pango_Font_Family : Pango.Font_Family.Pango_Font_Family_Record;
   begin
      return Pango.Font_Family.Pango_Font_Family (Get_User_Data (Internal (Get_Object (Self), Result), Stub_Pango_Font_Family));
   end Choose_Family_Finish;

   -----------------
   -- Choose_Font --
   -----------------

   procedure Choose_Font
      (Self          : not null access Gtk_Font_Dialog_Record;
       Parent        : access Gtk.Window.Gtk_Window_Record'Class;
       Initial_Value : Pango.Font.Pango_Font_Description;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Font_Dialog_Choose_Font (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Initial_Value, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Font_Dialog_Choose_Font (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Initial_Value, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Choose_Font;

   ------------------------
   -- Choose_Font_Finish --
   ------------------------

   function Choose_Font_Finish
      (Self   : not null access Gtk_Font_Dialog_Record;
       Result : Glib.G_Async_Result)
       return Pango.Font.Pango_Font_Description
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result)
          return Pango.Font.Pango_Font_Description;
      pragma Import (C, Internal, "gtk_font_dialog_choose_font_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Choose_Font_Finish;

   ------------------
   -- Get_Font_Map --
   ------------------

   function Get_Font_Map
      (Self : not null access Gtk_Font_Dialog_Record)
       return Pango.Font_Map.Pango_Font_Map
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_dialog_get_font_map");
      Stub_Pango_Font_Map : Pango.Font_Map.Pango_Font_Map_Record;
   begin
      return Pango.Font_Map.Pango_Font_Map (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Font_Map));
   end Get_Font_Map;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
      (Self : not null access Gtk_Font_Dialog_Record)
       return Pango.Language.Pango_Language
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_dialog_get_language");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Language;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
      (Self : not null access Gtk_Font_Dialog_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_dialog_get_modal");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Modal;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Self : not null access Gtk_Font_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_dialog_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Title;

   ------------------
   -- Set_Font_Map --
   ------------------

   procedure Set_Font_Map
      (Self    : not null access Gtk_Font_Dialog_Record;
       Fontmap : access Pango.Font_Map.Pango_Font_Map_Record'Class)
   is
      procedure Internal (Self : System.Address; Fontmap : System.Address);
      pragma Import (C, Internal, "gtk_font_dialog_set_font_map");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Fontmap)));
   end Set_Font_Map;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
      (Self     : not null access Gtk_Font_Dialog_Record;
       Language : Pango.Language.Pango_Language)
   is
      procedure Internal (Self : System.Address; Language : System.Address);
      pragma Import (C, Internal, "gtk_font_dialog_set_language");
   begin
      Internal (Get_Object (Self), Get_Object (Language));
   end Set_Language;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
      (Self  : not null access Gtk_Font_Dialog_Record;
       Modal : Boolean)
   is
      procedure Internal (Self : System.Address; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_dialog_set_modal");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Modal));
   end Set_Modal;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gtk_Font_Dialog_Record;
       Title : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_dialog_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

end Gtk.Font_Dialog;
