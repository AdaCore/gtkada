-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
with Unchecked_Deallocation;
with Gtk.Object;  use Gtk.Object;
with Interfaces.C.Strings;
with Gdk; use Gdk;
with Ada.Text_IO;

--  This package tries to handle three problems encountered with callbacks
--    1) the memory allocated internally by our Ada functions must be freed
--       when the callback is destroyed. For this, we used the 'destroy_func'
--       field in gtk
--    2) The object passed to the Ada callback must be the same one that was
--       passed to connect. For this, widgets are defined as pointers to a
--       a structure, the real type beeing known thanks to the generic packages
--    3) in gtk, callbacks may have different number of arguments, depending
--       on the signal and the widget. For this, we use a C function with a
--       variable number of argument as the real callback. This C function in
--       turns calls a General_Cb function in Ada with creates the correct
--       widget type. 'General_Cb' then calls the user callback.
--
--  Note that the instantiations of the generic packages below have to be
--  done at a library level, as the callbacks are in fact pointers to
--  functions in this package. Thus the functions have to be found when the
--  callback is found

package body Gtk.Signal is

   type DestroyFunc is access procedure (Data : in System.Address);
   type GtkArgArray is new System.Address;
   type MarshallerFunc is access procedure (Object    : in System.Address;
                                            User_Data : in System.Address;
                                            Nparams   : in Guint;
                                            Params    : in GtkArgArray);
   function Do_Signal_Connect (Object     : in Gtk.Object.Gtk_Object;
                               Name       : in String;
                               Marshaller : in System.Address;
                               Func_Data  : in System.Address;
                               Destroy    : in System.Address;
                               After      : in Boolean)
                              return          Guint;
   --  Internal function used to connect the signal.

   -------------------
   -- Argument_Type --
   -------------------

   function Argument_Type
     (The_Type : Gtk_Type; Name : in String; Num : in Guint) return Gtk_Type
   is
      function Internal
        (The_Type : Gtk_Type; Name : String; Num  : Guint) return Gtk_Type;
      pragma Import (C, Internal, "ada_signal_argument_type");
   begin
      return Internal (The_Type, Name & ASCII.Nul, Num);
   end Argument_Type;

   ---------------------
   -- Count_Arguments --
   ---------------------

   function Count_Arguments
     (The_Type : Gtk_Type; Name : in String) return Guint
   is
      function Internal (The_Type : Gtk_Type; Name : String) return Guint;
      pragma Import (C, Internal, "ada_signal_count_arguments");

   begin
      return Internal (The_Type, Name & ASCII.Nul);
   end Count_Arguments;

   -----------------------
   -- Do_Signal_Connect --
   -----------------------

   function Do_Signal_Connect (Object     : in Gtk.Object.Gtk_Object;
                               Name       : in String;
                               Marshaller : in System.Address;
                               Func_Data  : in System.Address;
                               Destroy    : in System.Address;
                               After      : in Boolean)
                               return         Guint
   is
      function Internal (Object        : System.Address;
                         Name          : String;
                         Func          : System.Address;
                         Marshaller    : System.Address;
                         Func_Data     : System.Address;
                         Destroy       : System.Address;
                         Object_Signal : Gint;
                         After         : Gint)
                         return          Guint;
      pragma Import (C, Internal, "gtk_signal_connect_full");
   begin
      return Internal (Get_Object (Object.all),
                       Name & Ascii.NUL,
                       System.Null_Address,
                       Marshaller,
                       Func_Data,
                       Destroy,
                       Boolean'Pos (False),
                       Boolean'Pos (After));
   end Do_Signal_Connect;

   --------------
   -- Callback --
   --------------

   package body Callback is

      type Data_Access is access Data_Type;
      type Data_Type_Record is
         record
            Data   : Data_Access;
            Func   : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      type Acc is access all Base_Type;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);
      function Convert is new Unchecked_Conversion (System.Address,
                                                    Data_Type_Access);

      procedure Free_Data (Data : in System.Address);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal_2 is new Unchecked_Deallocation
           (Data_Type, Data_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal_2 (D.Data);
         Internal (D);
      end Free_Data;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         Data : Data_Type_Access := Convert (User_Data);
         Stub : Base_Type;
      begin
         if Data.Func /= null then
            Data.Func (Acc (Get_User_Data (Object, Stub)), Data.Data.all);
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj       : access Base_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
        return Guint
      is
         D : Data_Type_Access :=
          new Data_Type_Record'(Data => new Data_Type'(Func_Data),
                                Func => Func);
      begin
         --  Check that the real gtk+ callback has at least as many
         --  parameters as given in this package
         pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 0);
         pragma Assert (Argument_Type (Get_Type (Obj), Name, -1)
                        = Gtk_Type_None);

         return Do_Signal_Connect (Gtk.Object.Gtk_Object (Obj),
                                   Name,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free_Data'Address,
                                   After);
      end Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object : access Base_Type'Class;
         Name   : in String)
      is
         procedure Internal (Object : in System.Address;
                             Name   : in String);
         pragma Import (C, Internal, "gtk_signal_emit_by_name");
      begin
         --  for Emit_By_Name, we want to make sure that we pass at least
         --  as many arguments to the gtk handler as are expected.
         pragma Assert (Count_Arguments (Get_Type (Object), Name) <= 0);
         Internal (Get_Object (Object.all), Name & Ascii.NUL);
      end Emit_By_Name;

   end Callback;

   ------------------
   -- Two_Callback --
   ------------------

   package body Two_Callback is

      type Data_Access is access Data_Type;
      type Data_Type_Record is
         record
            Data   : Data_Access;
            Func   : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      type Acc is access all Base_Type;

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);
      function Convert is new Unchecked_Conversion (System.Address,
                                                    Data_Type_Access);

      procedure Free_Data (Data : in System.Address);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal_2 is new Unchecked_Deallocation
           (Data_Type, Data_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal_2 (D.Data);
         Internal (D);
      end Free_Data;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         function Internal (Params : in GtkArgArray;
                            Num    : in Guint)
                            return System.Address;
         pragma Import (C, Internal, "ada_gtkarg_value_object");
         use type System.Address;
         Stub    : Base_Type;
         Data    : Data_Type_Access := Convert (User_Data);
         Widget2 : Cb_Type;
         Tmp     : System.Address := Internal (Params, 0);
      begin
         if Nparams = 0 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Wrong number of arguments in Two_Callback");
         end if;
         if Data.Func /= null then
            Gdk.Set_Object (Widget2, Tmp);
            Data.Func (Acc (Get_User_Data (Object, Stub)),
                       Widget2, Data.Data.all);
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj       : access Base_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
         return Guint
      is
         D : Data_Type_Access :=
          new Data_Type_Record'(Data => new Data_Type'(Func_Data),
                                Func => Func);
      begin
         --  Check that the real gtk+ callback has at least as many
         --  parameters as given in this package
         pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);
         pragma Assert
           (Argument_Type (Get_Type (Obj), Name, -1) = Gtk_Type_None);

         return Do_Signal_Connect (Gtk.Object.Gtk_Object (Obj),
                                   Name,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free_Data'Address,
                                   After);
      end Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object  : access Base_Type'Class;
         Name    : in String;
         Cb_Data : in Cb_Type)
      is
         procedure Internal (Object  : in System.Address;
                             Name    : in String;
                             Cb_Data : in System.Address);
         pragma Import (C, Internal, "gtk_signal_emit_by_name");
      begin
         --  for Emit_By_Name, we want to make sure that we pass at least
         --  as many arguments to the gtk handler as are expected.
         pragma Assert (Count_Arguments (Get_Type (Object), Name) <= 1);
         Internal (Get_Object (Object.all), Name & Ascii.Nul,
                   Get_Object (Cb_Data));
      end Emit_By_Name;

   end Two_Callback;

   ----------------------
   -- Two_Callback_Gtk --
   ----------------------

   package body Two_Callback_Gtk is
      type Data_Access is access Data_Type;
      type Data_Type_Record is
         record
            Data   : Data_Access;
            Func   : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      type Acc is access all Base_Type;
      type Acc2 is access all Cb_Type;

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : in System.Address);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal_2 is new Unchecked_Deallocation
           (Data_Type, Data_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal_2 (D.Data);
         Internal (D);
      end Free_Data;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         function Internal (Params : in GtkArgArray;
                            Num    : in Guint)
                            return System.Address;
         pragma Import (C, Internal, "ada_gtkarg_value_object");
         use type System.Address;
         Stub   : Base_Type;
         Stub2  : aliased Cb_Type;
         Data   : Data_Type_Access := Convert (User_Data);
         Tmp    : System.Address := Internal (Params, 0);
      begin
         if Nparams = 0 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Wrong number of arguments in Two_Callback");
         end if;
         if Data.Func /= null then
            if Tmp = System.Null_Address then
               Data.Func (Acc (Get_User_Data (Object, Stub)),
                          Stub2'Access, Data.Data.all);
            else
               Data.Func (Acc (Get_User_Data (Object, Stub)),
                          Acc2 (Get_User_Data (Tmp, Stub2)),
                          Data.Data.all);
            end if;
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj       : access Base_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
         return Guint
      is
         D : Data_Type_Access :=
          new Data_Type_Record'(Data => new Data_Type'(Func_Data),
                                Func => Func);
      begin
         --  Check that the real gtk+ callback has at least as many
         --  parameters as given in this package
         pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 1);
         pragma Assert
           (Argument_Type (Get_Type (Obj), Name, -1) = Gtk_Type_None);

         return Do_Signal_Connect (Gtk.Object.Gtk_Object (Obj),
                                   Name,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free_Data'Address,
                                   After);
      end Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object  : access Base_Type'Class;
         Name    : in String;
         Cb_Data : in Cb_Type)
      is
         procedure Internal (Object  : in System.Address;
                             Name    : in String;
                             Cb_Data : in System.Address);
         pragma Import (C, Internal, "gtk_signal_emit_by_name");
      begin
         --  for Emit_By_Name, we want to make sure that we pass at least
         --  as many arguments to the gtk handler as are expected.
         pragma Assert (Count_Arguments (Get_Type (Object), Name) <= 1);
         Internal (Get_Object (Object.all), Name & Ascii.Nul,
                   Get_Object (Cb_Data));
      end Emit_By_Name;

   end Two_Callback_Gtk;

   ---------------------
   -- Record_Callback --
   ---------------------

   package body Record_Callback is

      type Data_Type_Record is
         record
            Func   : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      type Acc is access all Base_Type;

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);
      function Convert is new Unchecked_Conversion (System.Address,
                                                    Data_Type_Access);

      procedure Free_Data (Data : in System.Address);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal (D);
      end Free_Data;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         function Internal (Params : in GtkArgArray;
                            Num    : in Guint)
                            return System.Address;
         pragma Import (C, Internal, "ada_gtkarg_value_object");
         use type System.Address;
         Stub   : Base_Type;
         Data   : Data_Type_Access := Convert (User_Data);
         Tmp    : System.Address := Internal (Params, 0);

         Rec    : Simple_Record;
         for Rec'Address use Tmp;
      begin
         if Data.Func /= null then
            Data.Func (Acc (Get_User_Data (Object, Stub)), Rec);
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj       : access Base_Type'Class;
         Name      : in String;
         Func      : in Callback;
         After     : in Boolean := False)
         return Guint
      is
         D : Data_Type_Access := new Data_Type_Record'(Func => Func);
      begin
         --  Check that the real gtk+ callback has at least as many
         --  parameters as given in this package
         pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 0);
         pragma Assert
           (Argument_Type (Get_Type (Obj), Name, -1) = Gtk_Type_None);

         return Do_Signal_Connect (Gtk.Object.Gtk_Object (Obj),
                                   Name,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free_Data'Address,
                                   After);
      end Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object : access Base_Type'Class;
         Name   : in String)
      is
         procedure Internal (Object : in System.Address;
                             Name   : in String);
         pragma Import (C, Internal, "gtk_signal_emit_by_name");
      begin
         --  for Emit_By_Name, we want to make sure that we pass at least
         --  as many arguments to the gtk handler as are expected.
         pragma Assert (Count_Arguments (Get_Type (Object), Name) <= 0);
         Internal (Get_Object (Object.all), Name & Ascii.NUL);
      end Emit_By_Name;

   end Record_Callback;

   -------------------------
   -- Tips_Query_Callback --
   -------------------------

   package body Tips_Query_Callback is

      type Data_Access is access Data_Type'Class;
      type Data_Type_Record is
         record
            Data   : Data_Access;
            Func   : Callback;
         end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);
      function Convert is new Unchecked_Conversion (System.Address,
                                                    Data_Type_Access);

      procedure Free_Data (Data : in System.Address);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal_2 is new Unchecked_Deallocation
           (Data_Type'Class, Data_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal_2 (D.Data);
         Internal (D);
      end Free_Data;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         function Internal (Params : in GtkArgArray;
                            Num    : in Guint)
                            return System.Address;
         pragma Import (C, Internal, "ada_gtkarg_value_object");
         use type System.Address;

         function To_String (S : System.Address) return String;

         function To_String (S : System.Address) return String is
            function Convert is new Unchecked_Conversion
              (System.Address, Interfaces.C.Strings.chars_ptr);
            use type System.Address;
         begin
            if S /= System.Null_Address then
               return Interfaces.C.Strings.Value (Convert (S));
            else
               return String'(1 .. 0 => ' ');
            end if;
         end To_String;

         Data    : Data_Type_Access;
         Widget  : aliased Gtk.Tips_Query.Gtk_Tips_Query_Record;
         Widget2 : aliased Gtk.Widget.Gtk_Widget_Record;
      begin
         if Nparams < 3 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Wrong number of arguments in Tips_Query_Callback");
            return;
         end if;
         Data := Convert (User_Data);
         if Data.Func /= null then
            Set_Object (Widget, Object);
            Set_Object (Widget2, Internal (Params, 0));
            Data.Func (Widget'Access, Widget2'Access,
                       To_String (Internal (Params, 1)),
                       To_String (Internal (Params, 2)),
                       Data.Data);
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj   : access Gtk.Tips_Query.Gtk_Tips_Query_Record'Class;
         Name  : in String;
         Func  : in Callback;
         Data  : access Data_Type'Class;
         After : in Boolean := False)
         return Guint
      is
         D : Data_Type_Access :=
          new Data_Type_Record'(Data => new Data_Type'Class'(Data.all),
                                Func => Func);
      begin
         --  Check that the real gtk+ callback has at least as many
         --  parameters as given in this package
         pragma Assert
           (Count_Arguments (Get_Type (Gtk_Object (Obj)), Name) >= 2);
         pragma Assert (Argument_Type (Get_Type (Gtk_Object (Obj)), Name, -1) =
           Gtk_Type_None);

         return Do_Signal_Connect (Gtk.Object.Gtk_Object (Obj),
                                   Name,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free_Data'Address,
                                   After);
      end Connect;

   end Tips_Query_Callback;

   ------------------------------------------------------------
   -- Void_Callback                                          --
   ------------------------------------------------------------

   package body Void_Callback is

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      type Acc is access all Base_Type;

      function Convert is new Unchecked_Conversion (System.Address,
                                                    Callback);
      function Convert is new Unchecked_Conversion (Callback,
                                                    System.Address);

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         Data : Callback := Convert (User_Data);
         Stub : Base_Type;
      begin
         Data (Acc (Get_User_Data (Object, Stub)));
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj   : access Base_Type'Class;
         Name  : in String;
         Func  : in Callback;
         After : in Boolean := False)
         return Guint
      is
      begin
         --  Check that the real gtk+ callback has at least as many
         --  parameters as given in this package
         pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 0);
         pragma Assert
           (Argument_Type (Get_Type (Obj), Name, -1) = Gtk_Type_None);

         return Do_Signal_Connect (Gtk.Object.Gtk_Object (Obj),
                                   Name,
                                   Marshaller'Address,
                                   Convert (Func),
                                   System.Null_Address,
                                   After);
      end Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object : access Base_Type'Class;
         Name   : in String)
      is
         procedure Internal (Object : in System.Address;
                             Name   : in String);
         pragma Import (C, Internal, "gtk_signal_emit_by_name");
      begin
         --  for Emit_By_Name, we want to make sure that we pass at least
         --  as many arguments to the gtk handler as are expected.
         pragma Assert (Count_Arguments (Get_Type (Object), Name) <= 0);
         Internal (Get_Object (Object.all), Name & Ascii.NUL);
      end Emit_By_Name;

   end Void_Callback;


   ---------------------------------------------------------------
   -- Object_Callback                                           --
   ---------------------------------------------------------------

   package body Object_Callback is

      type Widget_Type is access all Base_Type;
      type Data_Type_Record is
         record
            Func : Callback;
            Data : Widget_Type;
         end record;
      type Data_Type_Access is access Data_Type_Record;

      pragma Convention (C, Data_Type_Access);

      function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                    System.Address);
      function Convert is new Unchecked_Conversion (System.Address,
                                                    Data_Type_Access);

      procedure Free_Data (Data : in System.Address);
      pragma Convention (C, Free_Data);

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray);
      pragma Convention (C, Marshaller);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         D : Data_Type_Access := Convert (Data);
      begin
         Internal (D);
      end Free_Data;

      ----------------
      -- Marshaller --
      ----------------

      procedure Marshaller (Object    : in System.Address;
                            User_Data : in System.Address;
                            Nparams   : in Guint;
                            Params    : in GtkArgArray)
      is
         Data : Data_Type_Access := Convert (User_Data);
      begin
         if Data.Func /= null then
            Data.Func (Data.Data);
         end if;
      end Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Obj         : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in String;
         Func        : in Callback;
         Slot_Object : access Base_Type'Class;
         After       : in Boolean := False)
         return Guint
      is
         D : Data_Type_Access
           := new Data_Type_Record'(Data => Widget_Type (Slot_Object),
                                    Func => Func);
      begin
         --  Check that the real gtk+ callback has at least as many
         --  parameters as given in this package
         pragma Assert (Count_Arguments (Get_Type (Obj), Name) >= 0);
         pragma Assert
           (Argument_Type (Get_Type (Obj), Name, -1) = Gtk_Type_None);

         return Do_Signal_Connect (Gtk.Object.Gtk_Object (Obj),
                                   Name,
                                   Marshaller'Address,
                                   Convert (D),
                                   Free_Data'Address,
                                   After);
      end Connect;
   end Object_Callback;

   ----------------------
   -- C_Unsafe_Connect --
   ----------------------

   function C_Unsafe_Connect
     (Object      : access Gtk.Object.Gtk_Object_Record'Class;
      Name        : in String;
      Func        : in System.Address;
      Slot_Object : access Gtk.Object.Gtk_Object_Record'Class)
      return Guint
   is
      function Internal (Object      : in System.Address;
                         Name        : in String;
                         Func        : in System.Address;
                         Slot_Object : in System.Address)
                         return Guint;
      pragma Import (C, Internal, "gtk_signal_connect_object");
   begin
      return Internal (Get_Object (Object.all), Name & Ascii.NUL,
                       Func, Get_Object (Slot_Object.all));
   end C_Unsafe_Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Object     : access Gtk.Object.Gtk_Object_Record'Class;
      Handler_Id : in Guint)
   is
      procedure Internal (Obj : System.Address; Id  : Guint);
      pragma Import (C, Internal, "gtk_signal_disconnect");

   begin
      Internal (Obj => Get_Object (Object.all),
                Id  => Handler_Id);
   end Disconnect;

   -----------------------
   -- Emit_Stop_By_Name --
   -----------------------

   procedure Emit_Stop_By_Name
     (Object : access Gtk.Object.Gtk_Object_Record'Class;
      Name   : in String)
   is
      procedure Internal (Object : in System.Address;
                          Name   : in String);
      pragma Import (C, Internal, "gtk_signal_emit_stop_by_name");
   begin
      Internal (Get_Object (Object.all), Name & Ascii.NUL);
   end Emit_Stop_By_Name;

   -------------------
   -- Handler_Block --
   -------------------

   procedure Handler_Block
     (Obj        : access Gtk.Object.Gtk_Object_Record'Class;
      Handler_Id : in Guint)
   is
      procedure Internal (Obj : in System.Address; Id  : in Guint);
      pragma Import (C, Internal, "gtk_signal_handler_block");
   begin
      Internal (Obj => Get_Object (Obj.all), Id  => Handler_Id);
   end Handler_Block;

   ----------------------
   -- Handlers_Destroy --
   ----------------------

   procedure Handlers_Destroy (Obj : access Object.Gtk_Object_Record'Class)
   is
      procedure Internal (Obj : System.Address);
      pragma Import (C, Internal, "gtk_signal_handlers_destroy");
   begin
      Internal (Obj => Get_Object (Obj.all));
   end Handlers_Destroy;

   ---------------------
   -- Handler_Unblock --
   ---------------------

   procedure Handler_Unblock
     (Obj        : access Gtk.Object.Gtk_Object_Record'Class;
      Handler_Id : in Guint)
   is
      procedure Internal (Obj : in System.Address; Id  : in Guint);
      pragma Import (C, Internal, "gtk_signal_handler_unblock");
   begin
      Internal (Obj => Get_Object (Obj.all), Id  => Handler_Id);
   end Handler_Unblock;

end Gtk.Signal;
