-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2002 ACT-Europe                --
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

with Unchecked_Conversion;
with System; use System;
with Glib.Values; use Glib.Values;

package body Gtk.Marshallers is

   ------------------------
   -- Return_Marshallers --
   ------------------------

   package body Return_Marshallers is

      ------------------------
      -- Generic_Marshaller --
      ------------------------

      package body Generic_Marshaller is

         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         function Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler) return Return_Type
         is
            Func : constant Handler := To_Handler (Cb);
         begin
            return Func (Widget, Conversion (Nth (Params, 1)));
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            --  We must have at least one argument in the real callback.
            --  We should create an object of type Widget_Type to be able
            --  to get the Get_Type from it, and then test the number of
            --  arguments
            --  pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);

            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : Base_Type) return Return_Type
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : Base_Type;
               Ret    : out Integer);
            pragma Import (C, Internal, "g_signal_emit_by_name");

            B : Integer;
         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal (Get_Object (Object), Name & ASCII.NUL, Param, B);
            return Return_Type'Val (B);
         end Emit_By_Name;

         --------------------------
         -- Emit_By_Name_Generic --
         --------------------------

         function Emit_By_Name_Generic
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : Base_Type) return Return_Type
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : System.Address;
               Ret    : out Integer);
            pragma Import (C, Internal, "g_signal_emit_by_name");
            B : Integer;

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal
              (Get_Object (Object), Name & ASCII.NUL, Conversion (Param), B);
            return Return_Type'Val (B);
         end Emit_By_Name_Generic;

      end Generic_Marshaller;

      -------------------------------
      -- Generic_Widget_Marshaller --
      -------------------------------

      package body Generic_Widget_Marshaller is
         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         function Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler) return Return_Type
         is
            use Gtk.Widget;

            Func : constant Handler := To_Handler (Cb);
            Obj  : Gtk_Widget := Convert (Get_Address (Nth (Params, 1)));
            B    : aliased Base_Type;

         begin
            if Obj = null then
               return Func (Widget, B'Access);
            else
               return Func (Widget, Access_Type (Obj));
            end if;
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            --  We must have at least one argument in the real callback.
            --  pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);

            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : access Base_Type'Class) return Return_Type
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : System.Address;
               Ret    : out Integer);
            pragma Import (C, Internal, "g_signal_emit_by_name");

            R : Integer;

         begin
            --   pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal
              (Get_Object (Object), Name & ASCII.NUL, Get_Object (Param), R);
            return Return_Type'Val (R);
         end Emit_By_Name;

      end Generic_Widget_Marshaller;

      ---------------------
      -- Void_Marshaller --
      ---------------------

      package body Void_Marshaller is
         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         function Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler) return Return_Type
         is
            pragma Unreferenced (Params);

            Func : constant Handler := To_Handler (Cb);
         begin
            return Func (Widget);
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String) return Return_Type
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Ret    : out Integer);
            pragma Import (C, Internal, "g_signal_emit_by_name");

            R : Integer;

         begin
            --   pragma Assert (Count_Arguments (Get_Type (Object), Name) = 0);
            Internal (Get_Object (Object), Name & ASCII.NUL, R);
            return Return_Type'Val (R);
         end Emit_By_Name;

      end Void_Marshaller;
   end Return_Marshallers;

   -----------------------------
   -- User_Return_Marshallers --
   -----------------------------

   package body User_Return_Marshallers is

      ------------------------
      -- Generic_Marshaller --
      ------------------------

      package body Generic_Marshaller is

         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         function Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type) return Return_Type
         is
            Func : constant Handler := To_Handler (Cb);
         begin
            return Func (Widget, Conversion (Nth (Params, 1)), User_Data);
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            --  We must have at least one argument the real callback.
            --  pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);

            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : Base_Type) return Return_Type
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : Base_Type;
               Ret    : out Integer);
            pragma Import (C, Internal, "g_signal_emit_by_name");

            B : Integer;
         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal (Get_Object (Object), Name & ASCII.NUL, Param, B);
            return Return_Type'Val (B);
         end Emit_By_Name;

         --------------------------
         -- Emit_By_Name_Generic --
         --------------------------

         function Emit_By_Name_Generic
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : Base_Type) return Return_Type
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : System.Address;
               Ret    : out Integer);
            pragma Import (C, Internal, "g_signal_emit_by_name");

            B : Integer;

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal
              (Get_Object (Object), Name & ASCII.NUL, Conversion (Param), B);
            return Return_Type'Val (B);
         end Emit_By_Name_Generic;

      end Generic_Marshaller;

      -------------------------------
      -- Generic_Widget_Marshaller --
      -------------------------------

      package body Generic_Widget_Marshaller is

         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         function Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type) return Return_Type
         is
            use Gtk.Widget;
            Func : constant Handler := To_Handler (Cb);
            Obj  : Gtk_Widget := Convert (Get_Address (Nth (Params, 1)));
            B    : aliased Base_Type;

         begin
            if Obj = null then
               return Func (Widget, B'Access, User_Data);
            else
               return Func (Widget, Access_Type (Obj), User_Data);
            end if;
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            --  We must have at least one argument in the real callback.
            --  pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);

            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : access Base_Type'Class) return Return_Type
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : System.Address;
               Ret    : out Integer);
            pragma Import (C, Internal, "g_signal_emit_by_name");

            R : Integer;

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal
              (Get_Object (Object), Name & ASCII.NUL, Get_Object (Param), R);
            return Return_Type'Val (R);
         end Emit_By_Name;

      end Generic_Widget_Marshaller;

      ---------------------
      -- Void_Marshaller --
      ---------------------

      package body Void_Marshaller is

         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         function Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type) return Return_Type
         is
            pragma Unreferenced (Params);

            Func : constant Handler := To_Handler (Cb);
         begin
            return Func (Widget, User_Data);
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String) return Return_Type
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Ret    : out Integer);
            pragma Import (C, Internal, "g_signal_emit_by_name");

            R : Integer;

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 0);
            Internal (Get_Object (Object), Name & ASCII.NUL, R);
            return Return_Type'Val (R);
         end Emit_By_Name;

      end Void_Marshaller;

   end User_Return_Marshallers;

   ----------------------
   -- Void_Marshallers --
   ----------------------

   package body Void_Marshallers is

      ------------------------
      -- Generic_Marshaller --
      ------------------------

      package body Generic_Marshaller is
         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         procedure Call
           (Widget  : access Widget_Type'Class;
            Params  : Glib.Values.GValues;
            Cb      : General_Handler)
         is
            Func : constant Handler := To_Handler (Cb);
         begin
            Func (Widget, Conversion (Nth (Params, 1)));
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            --  We must have at least one argument in the real callback.
            --  pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);

            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : Base_Type)
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : Base_Type);
            pragma Import (C, Internal, "g_signal_emit_by_name");

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal (Get_Object (Object), Name & ASCII.NUL, Param);
         end Emit_By_Name;

         --------------------------
         -- Emit_By_Name_Generic --
         --------------------------

         procedure Emit_By_Name_Generic
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : Base_Type)
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : System.Address);
            pragma Import (C, Internal, "g_signal_emit_by_name");

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal
              (Get_Object (Object), Name & ASCII.NUL, Conversion (Param));
         end Emit_By_Name_Generic;

      end Generic_Marshaller;

      -------------------------------
      -- Generic_Widget_Marshaller --
      -------------------------------

      package body Generic_Widget_Marshaller is
         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         procedure Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler)
         is
            use Gtk.Widget;

            Func : constant Handler := To_Handler (Cb);
            Obj  : Gtk_Widget := Convert (Get_Address (Nth (Params, 1)));
            B    : aliased Base_Type;

         begin
            if Obj = null then
               Func (Widget, B'Access);
            else
               Func (Widget, Access_Type (Obj));
            end if;
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            --  We must have at least one argument in the real callback.
            --  pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);

            return (Func  => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : access Base_Type'Class)
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : System.Address);
            pragma Import (C, Internal, "g_signal_emit_by_name");

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal
              (Get_Object (Object), Name & ASCII.NUL, Get_Object (Param));
         end Emit_By_Name;

      end Generic_Widget_Marshaller;

      ---------------------
      -- Void_Marshaller --
      ---------------------

      package body Void_Marshaller is
         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         procedure Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler)
         is
            pragma Unreferenced (Params);

            Func : constant Handler := To_Handler (Cb);
         begin
            Func (Widget);
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String)
         is
            procedure Internal (Object : System.Address; Name : String);
            pragma Import (C, Internal, "g_signal_emit_by_name");

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 0);
            Internal (Get_Object (Object), Name & ASCII.NUL);
         end Emit_By_Name;

      end Void_Marshaller;

   end Void_Marshallers;

   -------------------------------
   -- User_Callback_Marshallers --
   -------------------------------

   package body User_Void_Marshallers is

      ------------------------
      -- Generic_Marshaller --
      ------------------------

      package body Generic_Marshaller is

         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         procedure Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type)
         is
            Func : constant Handler := To_Handler (Cb);
         begin
            Func (Widget, Conversion (Nth (Params, 1)), User_Data);
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            --  We must have at least one argument in the real callback.
            --  pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);

            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : Base_Type)
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : Base_Type);
            pragma Import (C, Internal, "g_signal_emit_by_name");

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal (Get_Object (Object), Name & ASCII.NUL, Param);
         end Emit_By_Name;

         --------------------------
         -- Emit_By_Name_Generic --
         --------------------------

         procedure Emit_By_Name_Generic
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : Base_Type)
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : System.Address);
            pragma Import (C, Internal, "g_signal_emit_by_name");

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal
              (Get_Object (Object), Name & ASCII.NUL, Conversion (Param));
         end Emit_By_Name_Generic;

      end Generic_Marshaller;

      -------------------------------
      -- Generic_Widget_Marshaller --
      -------------------------------

      package body Generic_Widget_Marshaller is

         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         procedure Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type)
         is
            use Gtk.Widget;

            Func : constant Handler := To_Handler (Cb);
            Obj  : Gtk_Widget := Convert (Get_Address (Nth (Params, 1)));
            B    : aliased Base_Type;

         begin
            if Obj = null then
               Func (Widget, B'Access, User_Data);
            else
               Func (Widget, Access_Type (Obj), User_Data);
            end if;
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            --  We must have at least one argument in the real callback.
            --  pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);

            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String;
            Param  : access Base_Type'Class)
         is
            procedure Internal
              (Object : System.Address;
               Name   : String;
               Param  : System.Address);
            pragma Import (C, Internal, "g_signal_emit_by_name");

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
            Internal
              (Get_Object (Object), Name & ASCII.NUL, Get_Object (Param));
         end Emit_By_Name;

      end Generic_Widget_Marshaller;

      ---------------------
      -- Void_Marshaller --
      ---------------------

      package body Void_Marshaller is

         function To_Handler is new
           Unchecked_Conversion (General_Handler, Handler);
         function To_General_Handler is new
           Unchecked_Conversion (Handler, General_Handler);

         ----------
         -- Call --
         ----------

         procedure Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type)
         is
            pragma Unreferenced (Params);

            Func : constant Handler := To_Handler (Cb);
         begin
            Func (Widget, User_Data);
         end Call;

         -------------------
         -- To_Marshaller --
         -------------------

         function To_Marshaller (Cb : Handler) return Marshaller is
         begin
            return (Func => To_General_Handler (Cb), Proxy => Call'Access);
         end To_Marshaller;

         ------------------
         -- Emit_By_Name --
         ------------------

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : String)
         is
            procedure Internal (Object : System.Address; Name : String);
            pragma Import (C, Internal, "g_signal_emit_by_name");

         begin
            --  pragma Assert (Count_Arguments (Get_Type (Object), Name) = 0);
            Internal (Get_Object (Object), Name & ASCII.NUL);
         end Emit_By_Name;

      end Void_Marshaller;

   end User_Void_Marshallers;

end Gtk.Marshallers;
