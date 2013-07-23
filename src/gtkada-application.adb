------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

with Interfaces.C.Strings;       use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

with Glib.Values;

with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;

package body Gtkada.Application is

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtkada_Application_Files, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtkada_Application_Files);

   procedure Marsh_Gapplication_Files
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gapplication_Files);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self           : out Gtkada_Application;
      Application_Id : UTF8_String := "";
      Flags          : Glib.Application.GApplication_Flags)
   is
   begin
      Self := new Gtkada_Application_Record;
      Gtkada.Application.Initialize (Self, Application_Id, Flags);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : not null access Gtkada_Application_Record'Class;
      Application_Id : UTF8_String := "";
      Flags          : Glib.Application.GApplication_Flags)
   is
      procedure C_Setup (Obj : System.Address);
      pragma Import (C, C_Setup, "ada_gtk_setup_application");
   begin
      Gtk.Application.Initialize (Self, Application_Id, Flags);
      C_Setup (Self.Get_Object);
   end Initialize;

   -------------------------
   -- Gtk_Application_New --
   -------------------------

   function Gtk_Application_New
     (Application_Id : UTF8_String := "";
      Flags          : Glib.Application.GApplication_Flags)
      return Gtkada_Application
   is
      Ret : constant Gtkada_Application := new Gtkada_Application_Record;
   begin
      Gtkada.Application.Initialize (Ret, Application_Id, Flags);

      return Ret;
   end Gtk_Application_New;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (File : GFile) return UTF8_String is
      function Internal (File : GFile) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_file_get_path");
      C_Path : Interfaces.C.Strings.chars_ptr := Internal (File);
      Path   : constant String := Interfaces.C.Strings.Value (C_Path);

   begin
      Interfaces.C.Strings.Free (C_Path);
      return Path;
   end Get_Path;

   -------------
   -- On_Open --
   -------------

   procedure On_Open
     (Self      : not null access Gtkada_Application_Record;
      Call      : Cb_Gtkada_Application_Files)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Self,
         C_Name      => Glib.Application.Signal_Open & ASCII.NUL,
         Marshaller  => Marsh_Gapplication_Files'Access,
         Handler     => Cb_To_Address (Call),
         After       => False);
   end On_Open;

   ------------------------------
   -- Marsh_Gapplication_Files --
   ------------------------------

   procedure Marsh_Gapplication_Files
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced
        (N_Params, Invocation_Hint, Return_Value, User_Data);
      H     : constant Cb_Gtkada_Application_Files :=
                Address_To_Cb (Get_Callback (Closure));
      Obj   : constant Gtkada_Application :=
                Gtkada_Application (Unchecked_To_Object (Params, 0));
      Ptr   : constant System.Address := Unchecked_To_Address (Params, 1);
      Nb    : constant Glib.Gint := Unchecked_To_Gint (Params, 2);
      Files : GFile_Array (1 .. Natural (Nb));
      for Files'Address use Ptr;

   begin
      H (Obj, Files);
      exception when E : others => Process_Exception (E);
   end Marsh_Gapplication_Files;

end Gtkada.Application;
