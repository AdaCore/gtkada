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

package body Pango.Fontset is

   procedure C_Pango_Fontset_Foreach
      (Self : System.Address;
       Func : System.Address;
       Data : System.Address);
   pragma Import (C, C_Pango_Fontset_Foreach, "pango_fontset_foreach");
   --  Iterates through all the fonts in a fontset, calling Func for each one.
   --  If Func returns True, that stops the iteration.
   --  Since: gtk+ 1.4
   --  "func": Callback function
   --  "data": data to pass to the callback function

   function To_Pango_Fontset_Foreach_Func is new Ada.Unchecked_Conversion
     (System.Address, Pango_Fontset_Foreach_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Pango_Fontset_Foreach_Func, System.Address);

   function Internal_Pango_Fontset_Foreach_Func
      (Fontset   : System.Address;
       Font      : System.Address;
       User_Data : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Pango_Fontset_Foreach_Func);
   --  "fontset": a Pango.Fontset.Pango_Fontset
   --  "font": a font from Fontset
   --  "user_data": callback data

   -----------------------------------------
   -- Internal_Pango_Fontset_Foreach_Func --
   -----------------------------------------

   function Internal_Pango_Fontset_Foreach_Func
      (Fontset   : System.Address;
       Font      : System.Address;
       User_Data : System.Address) return Glib.Gboolean
   is
      Func               : constant Pango_Fontset_Foreach_Func := To_Pango_Fontset_Foreach_Func (User_Data);
      Stub_Pango_Fontset : Pango_Fontset_Record;
      Stub_Pango_Font    : Pango.Font.Pango_Font_Record;
   begin
      return Boolean'Pos (Func (Pango.Fontset.Pango_Fontset (Get_User_Data (Fontset, Stub_Pango_Fontset)), Pango.Font.Pango_Font (Get_User_Data (Font, Stub_Pango_Font))));
   end Internal_Pango_Fontset_Foreach_Func;

   package Type_Conversion_Pango_Fontset is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Pango_Fontset_Record);
   pragma Unreferenced (Type_Conversion_Pango_Fontset);

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Self : not null access Pango_Fontset_Record;
       Func : Pango_Fontset_Foreach_Func)
   is
   begin
      if Func = null then
         C_Pango_Fontset_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
      else
         C_Pango_Fontset_Foreach (Get_Object (Self), Internal_Pango_Fontset_Foreach_Func'Address, To_Address (Func));
      end if;
   end Foreach;

   package body Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Pango_Fontset_Foreach_Func is new Ada.Unchecked_Conversion
        (System.Address, Pango_Fontset_Foreach_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Pango_Fontset_Foreach_Func, System.Address);

      function Internal_Cb
         (Fontset   : System.Address;
          Font      : System.Address;
          User_Data : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  A callback function used by Pango.Fontset.Foreach when enumerating
      --  the fonts in a fontset.
      --  Since: gtk+ 1.4
      --  "fontset": a Pango.Fontset.Pango_Fontset
      --  "font": a font from Fontset
      --  "user_data": callback data

      -------------
      -- Foreach --
      -------------

      procedure Foreach
         (Self : not null access Pango.Fontset.Pango_Fontset_Record'Class;
          Func : Pango_Fontset_Foreach_Func;
          Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Pango_Fontset_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Pango_Fontset_Foreach (Get_Object (Self), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Fontset   : System.Address;
          Font      : System.Address;
          User_Data : System.Address) return Glib.Gboolean
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Pango_Fontset : Pango.Fontset.Pango_Fontset_Record;
         Stub_Pango_Font    : Pango.Font.Pango_Font_Record;
      begin
         return Boolean'Pos (To_Pango_Fontset_Foreach_Func (D.Func) (Pango.Fontset.Pango_Fontset (Get_User_Data (Fontset, Stub_Pango_Fontset)), Pango.Font.Pango_Font (Get_User_Data (Font, Stub_Pango_Font)), D.Data.all));
      end Internal_Cb;

   end Foreach_User_Data;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
      (Self : not null access Pango_Fontset_Record;
       Wc   : Guint) return Pango.Font.Pango_Font
   is
      function Internal
         (Self : System.Address;
          Wc   : Guint) return System.Address;
      pragma Import (C, Internal, "pango_fontset_get_font");
      Stub_Pango_Font : Pango.Font.Pango_Font_Record;
   begin
      return Pango.Font.Pango_Font (Get_User_Data (Internal (Get_Object (Self), Wc), Stub_Pango_Font));
   end Get_Font;

   -----------------
   -- Get_Metrics --
   -----------------

   function Get_Metrics
      (Self : not null access Pango_Fontset_Record)
       return Pango.Font_Metrics.Pango_Font_Metrics
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_fontset_get_metrics");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Metrics;

end Pango.Fontset;
