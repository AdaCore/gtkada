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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Glib.App_Launch_Context is

   package Type_Conversion_Gapp_Launch_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gapp_Launch_Context_Record);
   pragma Unreferenced (Type_Conversion_Gapp_Launch_Context);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gapp_Launch_Context) is
   begin
      Self := new Gapp_Launch_Context_Record;
      Glib.App_Launch_Context.Initialize (Self);
   end G_New;

   -----------------------------
   -- Gapp_Launch_Context_New --
   -----------------------------

   function Gapp_Launch_Context_New return Gapp_Launch_Context is
      Self : constant Gapp_Launch_Context := new Gapp_Launch_Context_Record;
   begin
      Glib.App_Launch_Context.Initialize (Self);
      return Self;
   end Gapp_Launch_Context_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gapp_Launch_Context_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_app_launch_context_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment
      (Self : not null access Gapp_Launch_Context_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_app_launch_context_get_environment");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end Get_Environment;

   -------------------
   -- Launch_Failed --
   -------------------

   procedure Launch_Failed
      (Self              : not null access Gapp_Launch_Context_Record;
       Startup_Notify_Id : UTF8_String)
   is
      procedure Internal
         (Self              : System.Address;
          Startup_Notify_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_app_launch_context_launch_failed");
      Tmp_Startup_Notify_Id : Gtkada.Types.Chars_Ptr := New_String (Startup_Notify_Id);
   begin
      Internal (Get_Object (Self), Tmp_Startup_Notify_Id);
      Free (Tmp_Startup_Notify_Id);
   end Launch_Failed;

   ------------
   -- Setenv --
   ------------

   procedure Setenv
      (Self     : not null access Gapp_Launch_Context_Record;
       Variable : UTF8_String;
       Value    : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Variable : Gtkada.Types.Chars_Ptr;
          Value    : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_app_launch_context_setenv");
      Tmp_Variable : Gtkada.Types.Chars_Ptr := New_String (Variable);
      Tmp_Value    : Gtkada.Types.Chars_Ptr := New_String (Value);
   begin
      Internal (Get_Object (Self), Tmp_Variable, Tmp_Value);
      Free (Tmp_Value);
      Free (Tmp_Variable);
   end Setenv;

   --------------
   -- Unsetenv --
   --------------

   procedure Unsetenv
      (Self     : not null access Gapp_Launch_Context_Record;
       Variable : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Variable : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_app_launch_context_unsetenv");
      Tmp_Variable : Gtkada.Types.Chars_Ptr := New_String (Variable);
   begin
      Internal (Get_Object (Self), Tmp_Variable);
      Free (Tmp_Variable);
   end Unsetenv;

end Glib.App_Launch_Context;
