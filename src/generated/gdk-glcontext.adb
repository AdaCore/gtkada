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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gdk.GLContext is

   package Type_Conversion_Gdk_GLContext is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_GLContext_Record);
   pragma Unreferenced (Type_Conversion_Gdk_GLContext);

   -----------------------
   -- Get_Debug_Enabled --
   -----------------------

   function Get_Debug_Enabled
      (Self : not null access Gdk_GLContext_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_gl_context_get_debug_enabled");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Debug_Enabled;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_GLContext_Record)
       return Gdk.Display.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gl_context_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   ----------------------------
   -- Get_Forward_Compatible --
   ----------------------------

   function Get_Forward_Compatible
      (Self : not null access Gdk_GLContext_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_gl_context_get_forward_compatible");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Forward_Compatible;

   --------------------------
   -- Get_Required_Version --
   --------------------------

   procedure Get_Required_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Major : out Glib.Gint;
          Minor : out Glib.Gint);
      pragma Import (C, Internal, "gdk_gl_context_get_required_version");
   begin
      Internal (Get_Object (Self), Major, Minor);
   end Get_Required_Version;

   ------------------------
   -- Get_Shared_Context --
   ------------------------

   function Get_Shared_Context
      (Self : not null access Gdk_GLContext_Record) return Gdk_GLContext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gl_context_get_shared_context");
      Stub_Gdk_GLContext : Gdk_GLContext_Record;
   begin
      return Gdk.GLContext.Gdk_GLContext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_GLContext));
   end Get_Shared_Context;

   ----------------
   -- Get_Use_Es --
   ----------------

   function Get_Use_Es
      (Self : not null access Gdk_GLContext_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_gl_context_get_use_es");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Es;

   -----------------
   -- Get_Version --
   -----------------

   procedure Get_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Major : out Glib.Gint;
          Minor : out Glib.Gint);
      pragma Import (C, Internal, "gdk_gl_context_get_version");
   begin
      Internal (Get_Object (Self), Major, Minor);
   end Get_Version;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
      (Self : not null access Gdk_GLContext_Record) return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_gl_context_get_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Window;

   ---------------
   -- Is_Legacy --
   ---------------

   function Is_Legacy
      (Self : not null access Gdk_GLContext_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_gl_context_is_legacy");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Legacy;

   ------------------
   -- Make_Current --
   ------------------

   procedure Make_Current (Self : not null access Gdk_GLContext_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_gl_context_make_current");
   begin
      Internal (Get_Object (Self));
   end Make_Current;

   -------------
   -- Realize --
   -------------

   function Realize
      (Self : not null access Gdk_GLContext_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_gl_context_realize");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Realize;

   -----------------------
   -- Set_Debug_Enabled --
   -----------------------

   procedure Set_Debug_Enabled
      (Self    : not null access Gdk_GLContext_Record;
       Enabled : Boolean)
   is
      procedure Internal (Self : System.Address; Enabled : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_gl_context_set_debug_enabled");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Enabled));
   end Set_Debug_Enabled;

   ----------------------------
   -- Set_Forward_Compatible --
   ----------------------------

   procedure Set_Forward_Compatible
      (Self       : not null access Gdk_GLContext_Record;
       Compatible : Boolean)
   is
      procedure Internal (Self : System.Address; Compatible : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_gl_context_set_forward_compatible");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Compatible));
   end Set_Forward_Compatible;

   --------------------------
   -- Set_Required_Version --
   --------------------------

   procedure Set_Required_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : Glib.Gint;
       Minor : Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Major : Glib.Gint;
          Minor : Glib.Gint);
      pragma Import (C, Internal, "gdk_gl_context_set_required_version");
   begin
      Internal (Get_Object (Self), Major, Minor);
   end Set_Required_Version;

   ----------------
   -- Set_Use_Es --
   ----------------

   procedure Set_Use_Es
      (Self   : not null access Gdk_GLContext_Record;
       Use_Es : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Use_Es : Glib.Gint);
      pragma Import (C, Internal, "gdk_gl_context_set_use_es");
   begin
      Internal (Get_Object (Self), Use_Es);
   end Set_Use_Es;

   -------------------
   -- Clear_Current --
   -------------------

   procedure Clear_Current is
      procedure Internal;
      pragma Import (C, Internal, "gdk_gl_context_clear_current");
   begin
      Internal;
   end Clear_Current;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current return Gdk_GLContext is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_gl_context_get_current");
      Stub_Gdk_GLContext : Gdk_GLContext_Record;
   begin
      return Gdk.GLContext.Gdk_GLContext (Get_User_Data (Internal, Stub_Gdk_GLContext));
   end Get_Current;

end Gdk.GLContext;
