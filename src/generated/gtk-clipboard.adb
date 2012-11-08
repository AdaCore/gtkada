------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with GtkAda.C;                   use GtkAda.C;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Clipboard is

   package Atom_Arrays is new Gtkada.C.Unbounded_Arrays
     (Gdk.Types.Gdk_Atom, Gdk.Types.Gdk_None,
      Natural, Gdk.Types.Gdk_Atom_Array);

   ----------------------
   -- Wait_For_Targets --
   ----------------------

   function Wait_For_Targets
     (Clipboard : not null access Gtk_Clipboard_Record)
   return Gdk.Types.Gdk_Atom_Array
   is
      use Atom_Arrays;
      function Internal
        (Clipboard : System.Address;
         Targets   : access Unbounded_Array_Access;
         N_Targets : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_targets");

      Output    : aliased Unbounded_Array_Access;
      N_Targets : aliased Gint;
   begin
      if Internal
        (Get_Object (Clipboard),
         Output'Unchecked_Access,
         N_Targets'Unchecked_Access) = 0
      then
         G_Free (Output);
         Output := null;
      end if;

      declare
         Result : constant Gdk_Atom_Array :=
         To_Array (Output, Integer (N_Targets));
      begin
         if Output /= null then
            G_Free (Output);
         end if;

         return Result;
      end;
   end Wait_For_Targets;

   function To_Gtk_Clipboard_Received_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Clipboard_Received_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Clipboard_Received_Func, System.Address);

   function To_Gtk_Clipboard_Image_Received_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Clipboard_Image_Received_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Clipboard_Image_Received_Func, System.Address);

   function To_Gtk_Clipboard_Rich_Text_Received_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Clipboard_Rich_Text_Received_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Clipboard_Rich_Text_Received_Func, System.Address);

   function To_Gtk_Clipboard_Targets_Received_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Clipboard_Targets_Received_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Clipboard_Targets_Received_Func, System.Address);

   function To_Gtk_Clipboard_Text_Received_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Clipboard_Text_Received_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Clipboard_Text_Received_Func, System.Address);

   procedure C_Gtk_Clipboard_Request_Contents
      (Clipboard : System.Address;
       Target    : Gdk.Types.Gdk_Atom;
       Callback  : System.Address;
       User_Data : System.Address);
   pragma Import (C, C_Gtk_Clipboard_Request_Contents, "gtk_clipboard_request_contents");
   --  Requests the contents of clipboard as the given target. When the
   --  results of the result are later received the supplied callback will be
   --  called.
   --  "target": an atom representing the form into which the clipboard owner
   --  should convert the selection.
   --  "callback": A function to call when the results are received (or the
   --  retrieval fails). If the retrieval fails the length field of
   --  Selection_Data will be negative.
   --  "user_data": user data to pass to Callback

   procedure C_Gtk_Clipboard_Request_Image
      (Clipboard : System.Address;
       Callback  : System.Address;
       User_Data : System.Address);
   pragma Import (C, C_Gtk_Clipboard_Request_Image, "gtk_clipboard_request_image");
   --  Requests the contents of the clipboard as image. When the image is
   --  later received, it will be converted to a Gdk.Pixbuf.Gdk_Pixbuf, and
   --  Callback will be called.
   --  The Pixbuf parameter to Callback will contain the resulting
   --  Gdk.Pixbuf.Gdk_Pixbuf if the request succeeded, or null if it failed.
   --  This could happen for various reasons, in particular if the clipboard
   --  was empty or if the contents of the clipboard could not be converted
   --  into an image.
   --  Since: gtk+ 2.6
   --  "callback": a function to call when the image is received, or the
   --  retrieval fails. (It will always be called one way or the other.)
   --  "user_data": user data to pass to Callback.

   procedure C_Gtk_Clipboard_Request_Rich_Text
      (Clipboard : System.Address;
       Buffer    : Glib.Object.GObject;
       Callback  : System.Address;
       User_Data : System.Address);
   pragma Import (C, C_Gtk_Clipboard_Request_Rich_Text, "gtk_clipboard_request_rich_text");
   --  Requests the contents of the clipboard as rich text. When the rich text
   --  is later received, Callback will be called.
   --  The Text parameter to Callback will contain the resulting rich text if
   --  the request succeeded, or null if it failed. The Length parameter will
   --  contain Text's length. This function can fail for various reasons, in
   --  particular if the clipboard was empty or if the contents of the
   --  clipboard could not be converted into rich text form.
   --  Since: gtk+ 2.10
   --  "buffer": a Gtk.Text_Buffer.Gtk_Text_Buffer
   --  "callback": a function to call when the text is received, or the
   --  retrieval fails. (It will always be called one way or the other.)
   --  "user_data": user data to pass to Callback.

   procedure C_Gtk_Clipboard_Request_Targets
      (Clipboard : System.Address;
       Callback  : System.Address;
       User_Data : System.Address);
   pragma Import (C, C_Gtk_Clipboard_Request_Targets, "gtk_clipboard_request_targets");
   --  Requests the contents of the clipboard as list of supported targets.
   --  When the list is later received, Callback will be called.
   --  The Targets parameter to Callback will contain the resulting targets if
   --  the request succeeded, or null if it failed.
   --  Since: gtk+ 2.4
   --  "callback": a function to call when the targets are received, or the
   --  retrieval fails. (It will always be called one way or the other.)
   --  "user_data": user data to pass to Callback.

   procedure C_Gtk_Clipboard_Request_Text
      (Clipboard : System.Address;
       Callback  : System.Address;
       User_Data : System.Address);
   pragma Import (C, C_Gtk_Clipboard_Request_Text, "gtk_clipboard_request_text");
   --  Requests the contents of the clipboard as text. When the text is later
   --  received, it will be converted to UTF-8 if necessary, and Callback will
   --  be called.
   --  The Text parameter to Callback will contain the resulting text if the
   --  request succeeded, or null if it failed. This could happen for various
   --  reasons, in particular if the clipboard was empty or if the contents of
   --  the clipboard could not be converted into text form.
   --  "callback": a function to call when the text is received, or the
   --  retrieval fails. (It will always be called one way or the other.)
   --  "user_data": user data to pass to Callback.

   procedure Internal_Gtk_Clipboard_Image_Received_Func
      (Clipboard : System.Address;
       Pixbuf    : System.Address;
       Data      : System.Address);
   pragma Convention (C, Internal_Gtk_Clipboard_Image_Received_Func);
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "pixbuf": the received image
   --  "data": the User_Data supplied to Gtk.Clipboard.Request_Image.

   procedure Internal_Gtk_Clipboard_Received_Func
      (Clipboard      : System.Address;
       Selection_Data : System.Address;
       Data           : System.Address);
   pragma Convention (C, Internal_Gtk_Clipboard_Received_Func);
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "selection_data": a Gtk.Selection_Data.Gtk_Selection_Data containing
   --  the data was received. If retrieving the data failed, then then length
   --  field of Selection_Data will be negative.
   --  "data": the User_Data supplied to Gtk.Clipboard.Request_Contents.

   procedure Internal_Gtk_Clipboard_Rich_Text_Received_Func
      (Clipboard : System.Address;
       Format    : Gdk.Types.Gdk_Atom;
       Text      : in out guint8;
       Length    : gsize;
       Data      : System.Address);
   pragma Convention (C, Internal_Gtk_Clipboard_Rich_Text_Received_Func);

   procedure Internal_Gtk_Clipboard_Targets_Received_Func
      (Clipboard : System.Address;
       Atoms     : in out Gdk.Types.Gdk_Atom;
       N_Atoms   : Gint;
       Data      : System.Address);
   pragma Convention (C, Internal_Gtk_Clipboard_Targets_Received_Func);
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "atoms": the supported targets, as array of Gdk.Types.Gdk_Atom, or null
   --  if retrieving the data failed.
   --  "n_atoms": the length of the Atoms array.
   --  "data": the User_Data supplied to Gtk.Clipboard.Request_Targets.

   procedure Internal_Gtk_Clipboard_Text_Received_Func
      (Clipboard : System.Address;
       Text      : Interfaces.C.Strings.chars_ptr;
       Data      : System.Address);
   pragma Convention (C, Internal_Gtk_Clipboard_Text_Received_Func);
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "text": the text received, as a UTF-8 encoded string, or null if
   --  retrieving the data failed.
   --  "data": the User_Data supplied to Gtk.Clipboard.Request_Text.

   ------------------------------------------------
   -- Internal_Gtk_Clipboard_Image_Received_Func --
   ------------------------------------------------

   procedure Internal_Gtk_Clipboard_Image_Received_Func
      (Clipboard : System.Address;
       Pixbuf    : System.Address;
       Data      : System.Address)
   is
      Func               : constant Gtk_Clipboard_Image_Received_Func := To_Gtk_Clipboard_Image_Received_Func (Data);
      Stub_Gtk_Clipboard : Gtk_Clipboard_Record;
      Stub_Gdk_Pixbuf    : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      Func (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Pixbuf, Stub_Gdk_Pixbuf)));
   end Internal_Gtk_Clipboard_Image_Received_Func;

   ------------------------------------------
   -- Internal_Gtk_Clipboard_Received_Func --
   ------------------------------------------

   procedure Internal_Gtk_Clipboard_Received_Func
      (Clipboard      : System.Address;
       Selection_Data : System.Address;
       Data           : System.Address)
   is
      Func               : constant Gtk_Clipboard_Received_Func := To_Gtk_Clipboard_Received_Func (Data);
      Stub_Gtk_Clipboard : Gtk_Clipboard_Record;
   begin
      Func (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), From_Object (Selection_Data));
   end Internal_Gtk_Clipboard_Received_Func;

   ----------------------------------------------------
   -- Internal_Gtk_Clipboard_Rich_Text_Received_Func --
   ----------------------------------------------------

   procedure Internal_Gtk_Clipboard_Rich_Text_Received_Func
      (Clipboard : System.Address;
       Format    : Gdk.Types.Gdk_Atom;
       Text      : in out guint8;
       Length    : gsize;
       Data      : System.Address)
   is
      Func               : constant Gtk_Clipboard_Rich_Text_Received_Func := To_Gtk_Clipboard_Rich_Text_Received_Func (Data);
      Stub_Gtk_Clipboard : Gtk_Clipboard_Record;
   begin
      Func (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), Format, Text, Length);
   end Internal_Gtk_Clipboard_Rich_Text_Received_Func;

   --------------------------------------------------
   -- Internal_Gtk_Clipboard_Targets_Received_Func --
   --------------------------------------------------

   procedure Internal_Gtk_Clipboard_Targets_Received_Func
      (Clipboard : System.Address;
       Atoms     : in out Gdk.Types.Gdk_Atom;
       N_Atoms   : Gint;
       Data      : System.Address)
   is
      Func               : constant Gtk_Clipboard_Targets_Received_Func := To_Gtk_Clipboard_Targets_Received_Func (Data);
      Stub_Gtk_Clipboard : Gtk_Clipboard_Record;
   begin
      Func (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), Atoms, N_Atoms);
   end Internal_Gtk_Clipboard_Targets_Received_Func;

   -----------------------------------------------
   -- Internal_Gtk_Clipboard_Text_Received_Func --
   -----------------------------------------------

   procedure Internal_Gtk_Clipboard_Text_Received_Func
      (Clipboard : System.Address;
       Text      : Interfaces.C.Strings.chars_ptr;
       Data      : System.Address)
   is
      Func               : constant Gtk_Clipboard_Text_Received_Func := To_Gtk_Clipboard_Text_Received_Func (Data);
      Stub_Gtk_Clipboard : Gtk_Clipboard_Record;
   begin
      Func (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), Interfaces.C.Strings.Value (Text));
   end Internal_Gtk_Clipboard_Text_Received_Func;

   package Type_Conversion_Gtk_Clipboard is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Clipboard_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Clipboard);

   -----------
   -- Clear --
   -----------

   procedure Clear (Clipboard : not null access Gtk_Clipboard_Record) is
      procedure Internal (Clipboard : System.Address);
      pragma Import (C, Internal, "gtk_clipboard_clear");
   begin
      Internal (Get_Object (Clipboard));
   end Clear;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Gdk.Display.Gdk_Display
   is
      function Internal (Clipboard : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_clipboard_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Get_Object (Clipboard)), Stub_Gdk_Display));
   end Get_Display;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Glib.Object.GObject
   is
      function Internal (Clipboard : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_clipboard_get_owner");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Clipboard)), Stub_GObject);
   end Get_Owner;

   ----------------------
   -- Request_Contents --
   ----------------------

   procedure Request_Contents
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom;
       Callback  : Gtk_Clipboard_Received_Func)
   is
   begin
      if Callback = null then
         C_Gtk_Clipboard_Request_Contents (Get_Object (Clipboard), Target, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Clipboard_Request_Contents (Get_Object (Clipboard), Target, Internal_Gtk_Clipboard_Received_Func'Address, To_Address (Callback));
      end if;
   end Request_Contents;

   package body Request_Contents_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Clipboard_Received_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Clipboard_Received_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Clipboard_Received_Func, System.Address);

      procedure Internal_Cb
         (Clipboard      : System.Address;
          Selection_Data : System.Address;
          Data           : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function to be called when the results of
      --  Gtk.Clipboard.Request_Contents are received, or when the request
      --  fails.
      --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
      --  "selection_data": a Gtk.Selection_Data.Gtk_Selection_Data containing
      --  the data was received. If retrieving the data failed, then then
      --  length field of Selection_Data will be negative.
      --  "data": the User_Data supplied to Gtk.Clipboard.Request_Contents.

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Clipboard      : System.Address;
          Selection_Data : System.Address;
          Data           : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Clipboard : Gtk.Clipboard.Gtk_Clipboard_Record;
      begin
         To_Gtk_Clipboard_Received_Func (D.Func) (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), From_Object (Selection_Data), D.Data.all);
      end Internal_Cb;

      ----------------------
      -- Request_Contents --
      ----------------------

      procedure Request_Contents
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Target    : Gdk.Types.Gdk_Atom;
          Callback  : Gtk_Clipboard_Received_Func;
          User_Data : User_Data_Type)
      is
      begin
         if Callback = null then
            C_Gtk_Clipboard_Request_Contents (Get_Object (Clipboard), Target, System.Null_Address, System.Null_Address);
         else
            C_Gtk_Clipboard_Request_Contents (Get_Object (Clipboard), Target, Internal_Cb'Address, Users.Build (To_Address (Callback), User_Data));
         end if;
      end Request_Contents;

   end Request_Contents_User_Data;

   -------------------
   -- Request_Image --
   -------------------

   procedure Request_Image
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Image_Received_Func)
   is
   begin
      if Callback = null then
         C_Gtk_Clipboard_Request_Image (Get_Object (Clipboard), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Clipboard_Request_Image (Get_Object (Clipboard), Internal_Gtk_Clipboard_Image_Received_Func'Address, To_Address (Callback));
      end if;
   end Request_Image;

   package body Request_Image_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Clipboard_Image_Received_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Clipboard_Image_Received_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Clipboard_Image_Received_Func, System.Address);

      procedure Internal_Cb
         (Clipboard : System.Address;
          Pixbuf    : System.Address;
          Data      : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function to be called when the results of
      --  Gtk.Clipboard.Request_Image are received, or when the request fails.
      --  Since: gtk+ 2.6
      --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
      --  "pixbuf": the received image
      --  "data": the User_Data supplied to Gtk.Clipboard.Request_Image.

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Clipboard : System.Address;
          Pixbuf    : System.Address;
          Data      : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Clipboard : Gtk.Clipboard.Gtk_Clipboard_Record;
         Stub_Gdk_Pixbuf    : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      begin
         To_Gtk_Clipboard_Image_Received_Func (D.Func) (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Pixbuf, Stub_Gdk_Pixbuf)), D.Data.all);
      end Internal_Cb;

      -------------------
      -- Request_Image --
      -------------------

      procedure Request_Image
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Callback  : Gtk_Clipboard_Image_Received_Func;
          User_Data : User_Data_Type)
      is
      begin
         if Callback = null then
            C_Gtk_Clipboard_Request_Image (Get_Object (Clipboard), System.Null_Address, System.Null_Address);
         else
            C_Gtk_Clipboard_Request_Image (Get_Object (Clipboard), Internal_Cb'Address, Users.Build (To_Address (Callback), User_Data));
         end if;
      end Request_Image;

   end Request_Image_User_Data;

   -----------------------
   -- Request_Rich_Text --
   -----------------------

   procedure Request_Rich_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Buffer    : Glib.Object.GObject;
       Callback  : Gtk_Clipboard_Rich_Text_Received_Func)
   is
   begin
      if Callback = null then
         C_Gtk_Clipboard_Request_Rich_Text (Get_Object (Clipboard), Buffer, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Clipboard_Request_Rich_Text (Get_Object (Clipboard), Buffer, Internal_Gtk_Clipboard_Rich_Text_Received_Func'Address, To_Address (Callback));
      end if;
   end Request_Rich_Text;

   package body Request_Rich_Text_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Clipboard_Rich_Text_Received_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Clipboard_Rich_Text_Received_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Clipboard_Rich_Text_Received_Func, System.Address);

      procedure Internal_Cb
         (Clipboard : System.Address;
          Format    : Gdk.Types.Gdk_Atom;
          Text      : in out guint8;
          Length    : gsize;
          Data      : System.Address);
      pragma Convention (C, Internal_Cb);

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Clipboard : System.Address;
          Format    : Gdk.Types.Gdk_Atom;
          Text      : in out guint8;
          Length    : gsize;
          Data      : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Clipboard : Gtk.Clipboard.Gtk_Clipboard_Record;
      begin
         To_Gtk_Clipboard_Rich_Text_Received_Func (D.Func) (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), Format, Text, Length, D.Data.all);
      end Internal_Cb;

      -----------------------
      -- Request_Rich_Text --
      -----------------------

      procedure Request_Rich_Text
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Buffer    : Glib.Object.GObject;
          Callback  : Gtk_Clipboard_Rich_Text_Received_Func;
          User_Data : User_Data_Type)
      is
      begin
         if Callback = null then
            C_Gtk_Clipboard_Request_Rich_Text (Get_Object (Clipboard), Buffer, System.Null_Address, System.Null_Address);
         else
            C_Gtk_Clipboard_Request_Rich_Text (Get_Object (Clipboard), Buffer, Internal_Cb'Address, Users.Build (To_Address (Callback), User_Data));
         end if;
      end Request_Rich_Text;

   end Request_Rich_Text_User_Data;

   ---------------------
   -- Request_Targets --
   ---------------------

   procedure Request_Targets
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Targets_Received_Func)
   is
   begin
      if Callback = null then
         C_Gtk_Clipboard_Request_Targets (Get_Object (Clipboard), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Clipboard_Request_Targets (Get_Object (Clipboard), Internal_Gtk_Clipboard_Targets_Received_Func'Address, To_Address (Callback));
      end if;
   end Request_Targets;

   package body Request_Targets_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Clipboard_Targets_Received_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Clipboard_Targets_Received_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Clipboard_Targets_Received_Func, System.Address);

      procedure Internal_Cb
         (Clipboard : System.Address;
          Atoms     : in out Gdk.Types.Gdk_Atom;
          N_Atoms   : Gint;
          Data      : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function to be called when the results of
      --  Gtk.Clipboard.Request_Targets are received, or when the request
      --  fails.
      --  Since: gtk+ 2.4
      --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
      --  "atoms": the supported targets, as array of Gdk.Types.Gdk_Atom, or
      --  null if retrieving the data failed.
      --  "n_atoms": the length of the Atoms array.
      --  "data": the User_Data supplied to Gtk.Clipboard.Request_Targets.

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Clipboard : System.Address;
          Atoms     : in out Gdk.Types.Gdk_Atom;
          N_Atoms   : Gint;
          Data      : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Clipboard : Gtk.Clipboard.Gtk_Clipboard_Record;
      begin
         To_Gtk_Clipboard_Targets_Received_Func (D.Func) (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), Atoms, N_Atoms, D.Data.all);
      end Internal_Cb;

      ---------------------
      -- Request_Targets --
      ---------------------

      procedure Request_Targets
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Callback  : Gtk_Clipboard_Targets_Received_Func;
          User_Data : User_Data_Type)
      is
      begin
         if Callback = null then
            C_Gtk_Clipboard_Request_Targets (Get_Object (Clipboard), System.Null_Address, System.Null_Address);
         else
            C_Gtk_Clipboard_Request_Targets (Get_Object (Clipboard), Internal_Cb'Address, Users.Build (To_Address (Callback), User_Data));
         end if;
      end Request_Targets;

   end Request_Targets_User_Data;

   ------------------
   -- Request_Text --
   ------------------

   procedure Request_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Text_Received_Func)
   is
   begin
      if Callback = null then
         C_Gtk_Clipboard_Request_Text (Get_Object (Clipboard), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Clipboard_Request_Text (Get_Object (Clipboard), Internal_Gtk_Clipboard_Text_Received_Func'Address, To_Address (Callback));
      end if;
   end Request_Text;

   package body Request_Text_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Clipboard_Text_Received_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Clipboard_Text_Received_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Clipboard_Text_Received_Func, System.Address);

      procedure Internal_Cb
         (Clipboard : System.Address;
          Text      : Interfaces.C.Strings.chars_ptr;
          Data      : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function to be called when the results of
      --  Gtk.Clipboard.Request_Text are received, or when the request fails.
      --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
      --  "text": the text received, as a UTF-8 encoded string, or null if
      --  retrieving the data failed.
      --  "data": the User_Data supplied to Gtk.Clipboard.Request_Text.

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Clipboard : System.Address;
          Text      : Interfaces.C.Strings.chars_ptr;
          Data      : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Clipboard : Gtk.Clipboard.Gtk_Clipboard_Record;
      begin
         To_Gtk_Clipboard_Text_Received_Func (D.Func) (Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Clipboard, Stub_Gtk_Clipboard)), Interfaces.C.Strings.Value (Text), D.Data.all);
      end Internal_Cb;

      ------------------
      -- Request_Text --
      ------------------

      procedure Request_Text
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Callback  : Gtk_Clipboard_Text_Received_Func;
          User_Data : User_Data_Type)
      is
      begin
         if Callback = null then
            C_Gtk_Clipboard_Request_Text (Get_Object (Clipboard), System.Null_Address, System.Null_Address);
         else
            C_Gtk_Clipboard_Request_Text (Get_Object (Clipboard), Internal_Cb'Address, Users.Build (To_Address (Callback), User_Data));
         end if;
      end Request_Text;

   end Request_Text_User_Data;

   -------------------
   -- Set_Can_Store --
   -------------------

   procedure Set_Can_Store
      (Clipboard : not null access Gtk_Clipboard_Record;
       Targets   : Gtk.Target_List.Target_Entry_Array;
       N_Targets : Gint)
   is
      procedure Internal
         (Clipboard : System.Address;
          Targets   : Gtk.Target_List.Target_Entry_Array;
          N_Targets : Gint);
      pragma Import (C, Internal, "gtk_clipboard_set_can_store");
   begin
      Internal (Get_Object (Clipboard), Targets, N_Targets);
   end Set_Can_Store;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
      (Clipboard : not null access Gtk_Clipboard_Record;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Clipboard : System.Address;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_clipboard_set_image");
   begin
      Internal (Get_Object (Clipboard), Get_Object (Pixbuf));
   end Set_Image;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Text      : UTF8_String)
   is
      procedure Internal
         (Clipboard : System.Address;
          Text      : Interfaces.C.Strings.chars_ptr;
          Len       : Gint := -1);
      pragma Import (C, Internal, "gtk_clipboard_set_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
   begin
      Internal (Get_Object (Clipboard), Tmp_Text, -1);
      Free (Tmp_Text);
   end Set_Text;

   -----------
   -- Store --
   -----------

   procedure Store (Clipboard : not null access Gtk_Clipboard_Record) is
      procedure Internal (Clipboard : System.Address);
      pragma Import (C, Internal, "gtk_clipboard_store");
   begin
      Internal (Get_Object (Clipboard));
   end Store;

   -----------------------
   -- Wait_For_Contents --
   -----------------------

   function Wait_For_Contents
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom)
       return Gtk.Selection_Data.Gtk_Selection_Data
   is
      function Internal
         (Clipboard : System.Address;
          Target    : Gdk.Types.Gdk_Atom) return System.Address;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_contents");
   begin
      return From_Object (Internal (Get_Object (Clipboard), Target));
   end Wait_For_Contents;

   --------------------
   -- Wait_For_Image --
   --------------------

   function Wait_For_Image
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Clipboard : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_image");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Clipboard)), Stub_Gdk_Pixbuf));
   end Wait_For_Image;

   -------------------
   -- Wait_For_Text --
   -------------------

   function Wait_For_Text
      (Clipboard : not null access Gtk_Clipboard_Record) return UTF8_String
   is
      function Internal
         (Clipboard : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Clipboard)));
   end Wait_For_Text;

   -------------------
   -- Wait_For_Uris --
   -------------------

   function Wait_For_Uris
      (Clipboard : not null access Gtk_Clipboard_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Clipboard : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_uris");
   begin
      return To_String_List (Internal (Get_Object (Clipboard)).all);
   end Wait_For_Uris;

   -----------------------------
   -- Wait_Is_Image_Available --
   -----------------------------

   function Wait_Is_Image_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean
   is
      function Internal (Clipboard : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_image_available");
   begin
      return Boolean'Val (Internal (Get_Object (Clipboard)));
   end Wait_Is_Image_Available;

   ---------------------------------
   -- Wait_Is_Rich_Text_Available --
   ---------------------------------

   function Wait_Is_Rich_Text_Available
      (Clipboard : not null access Gtk_Clipboard_Record;
       Buffer    : Glib.Object.GObject) return Boolean
   is
      function Internal
         (Clipboard : System.Address;
          Buffer    : Glib.Object.GObject) return Integer;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_rich_text_available");
   begin
      return Boolean'Val (Internal (Get_Object (Clipboard), Buffer));
   end Wait_Is_Rich_Text_Available;

   ------------------------------
   -- Wait_Is_Target_Available --
   ------------------------------

   function Wait_Is_Target_Available
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom) return Boolean
   is
      function Internal
         (Clipboard : System.Address;
          Target    : Gdk.Types.Gdk_Atom) return Integer;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_target_available");
   begin
      return Boolean'Val (Internal (Get_Object (Clipboard), Target));
   end Wait_Is_Target_Available;

   ----------------------------
   -- Wait_Is_Text_Available --
   ----------------------------

   function Wait_Is_Text_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean
   is
      function Internal (Clipboard : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_text_available");
   begin
      return Boolean'Val (Internal (Get_Object (Clipboard)));
   end Wait_Is_Text_Available;

   ----------------------------
   -- Wait_Is_Uris_Available --
   ----------------------------

   function Wait_Is_Uris_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean
   is
      function Internal (Clipboard : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_uris_available");
   begin
      return Boolean'Val (Internal (Get_Object (Clipboard)));
   end Wait_Is_Uris_Available;

   ---------
   -- Get --
   ---------

   function Get
      (Selection : Gdk.Types.Gdk_Atom := Gdk.Types.Gdk_None)
       return Gtk_Clipboard
   is
      function Internal
         (Selection : Gdk.Types.Gdk_Atom) return System.Address;
      pragma Import (C, Internal, "gtk_clipboard_get");
      Stub_Gtk_Clipboard : Gtk_Clipboard_Record;
   begin
      return Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Internal (Selection), Stub_Gtk_Clipboard));
   end Get;

   ---------------------
   -- Get_For_Display --
   ---------------------

   function Get_For_Display
      (Display   : not null access Gdk.Display.Gdk_Display_Record'Class;
       Selection : Gdk.Types.Gdk_Atom := Gdk.Types.Gdk_None)
       return Gtk_Clipboard
   is
      function Internal
         (Display   : System.Address;
          Selection : Gdk.Types.Gdk_Atom) return System.Address;
      pragma Import (C, Internal, "gtk_clipboard_get_for_display");
      Stub_Gtk_Clipboard : Gtk_Clipboard_Record;
   begin
      return Gtk.Clipboard.Gtk_Clipboard (Get_User_Data (Internal (Get_Object (Display), Selection), Stub_Gtk_Clipboard));
   end Get_For_Display;

end Gtk.Clipboard;
