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

with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Window;    use Gdk.Window;
with Gdk.Types;     use Gdk.Types;
with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package body Gdk.Event is

   ---------------------
   --  Design issues  --
   ---------------------

   --  It is often useful to create events directly from Ada code, for instance
   --  when you want to send such an event (expose, ...) to a widget.
   --  One requirement for this package is thus to be able to create such types
   --  from Ada.
   --  On the other hand, C events have to be converted to Ada objects before
   --  being sent to handlers. The usual way would be to have an Ada structure
   --  with a pointer to the underlying C structure, but we then have the
   --  memory management problem (see below).
   --  Last requirement: As usual with Gdk structures, memory allocation is a
   --  a problem. If we allocate memory internally, the user will have to
   --  free it himself, since gdk does not provide any hook for this, as gtk
   --  does.

   --  They are two kinds of events:
   --  * the ones created by gtk+ itself, after receiving an event from X11.
   --    This events are automatically allocated and deallocated by gtk+, so
   --    we don't have anything to do
   --  * The ones created by the user, generally just before an Emit_By_Name.
   --    Since emit does not queue the event (and thus does not need to
   --    keep a copy of the parameters), it is possible to free the memory
   --    in the same subprogram where the event was created.
   --
   --  Event types:
   --  There are two possible way to implement the hierarchives of events
   --  (distinguish from one type to the other):
   --  * Having a discriminant to Gdk_Event.
   --    Pros: The interface in this package would be lighter, since there
   --          could be only one Get_X function, one Get_Y function, ...
   --    Cons: Some runtime tests have to be made to test whether a function
   --          is available or not for an event type (these tests could be
   --          done at the C level, where we need anyway to do some casts to
   --          have access to the field.
   --  * Having a tagged type hierarchy
   --    Pros: No test at run-time, everything can be done at compile time
   --    Cons: Much harder to convert from one event type to the other, unless
   --          we provide a gdk-event specific function to convert from
   --          Any_Event to a specific type.

   --  Up to version 1.2.3, callbacks are always created as a Gdk_Event,
   --  and the user needs to do an unchecked cast to create the correct
   --  tagged type in the hierarchy. Two solutions to this major problem:
   --  * With Discriminants:  just create the appropriate type, since the
   --    functions to get the fields are going to test the type anyway
   --  * With tagged type: we need to provide some kind of function, like
   --    Create_From_C to create the exact type (This can not be part of
   --    Gtk.Type_Conversions since the Gtk_Types do not exist for Gdk).


   ----------------------------------
   --  Some constants  used below  --
   ----------------------------------
   --  This constants have the '-1' since in some cases gtk itself uses
   --  the extrema to return some meaningful value (for instance, the result
   --  of Get_Area can have the values Guint16'Last to mean the whole area).

   Invalid_Gdouble_Value : constant Gdouble := Gdouble'Last - 1.0;
   Invalid_Gint_Value    : constant Gint    := Gint'Last - 1;
   Invalid_Gint16_Value  : constant Gint16  := Gint16'Last - 1;
   Invalid_Guint_Value   : constant Guint   := Guint'Last - 1;
   Invalid_Guint16_Value : constant Guint16 := Guint16'Last - 1;
   Invalid_Guint32_Value : constant Guint32 := Guint32'Last - 1;
   Invalid_Gulong_Value  : constant Gulong  := Gulong'Last - 1;

   pragma Export (C, Invalid_Gdouble_Value, "ada_gdk_invalid_gdouble_value");
   pragma Export (C, Invalid_Gint_Value, "ada_gdk_invalid_gint_value");
   pragma Export (C, Invalid_Gint16_Value, "ada_gdk_invalid_gint16_value");
   pragma Export (C, Invalid_Guint_Value, "ada_gdk_invalid_guint_value");
   pragma Export (C, Invalid_Guint16_Value, "ada_gdk_invalid_guint16_value");
   pragma Export (C, Invalid_Guint32_Value, "ada_gdk_invalid_guint32_value");
   pragma Export (C, Invalid_Gulong_Value, "ada_gdk_invalid_gulong_value");

   --------------------
   -- Get_Send_Event --
   --------------------

   function Get_Send_Event (Event : in Gdk_Event) return Boolean is
      function Internal (Event : in Gdk_Event) return Gint8;
      pragma Import (C, Internal, "ada_gdk_event_get_send_event");
   begin
      return Boolean'Val (Internal (Event));
   end Get_Send_Event;

   -----------
   -- Get_X --
   -----------

   function Get_X (Event : in Gdk_Event) return Gdouble is
      function Internal (Event : in Gdk_Event) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_get_x");
      X : constant Gdouble := Internal (Event);
   begin
      if X = Invalid_Gdouble_Value then
         raise Invalid_Field;
      end if;
      return X;
   end Get_X;

   -----------
   -- Get_Y --
   -----------

   function Get_Y (Event : in Gdk_Event) return Gdouble is
      function Internal (Event : in Gdk_Event) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_get_y");
      Y : constant Gdouble := Internal (Event);
   begin
      if Y = Invalid_Gdouble_Value then
         raise Invalid_Field;
      end if;
      return Y;
   end Get_Y;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Event : in Gdk_Event) return Gint16 is
      function Internal (Event : in Gdk_Event) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_get_width");
      Width : constant Gint16 := Internal (Event);
   begin
      if Width = Invalid_Gint16_Value then
         raise Invalid_Field;
      end if;
      return Width;
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Event : in Gdk_Event) return Gint16 is
      function Internal (Event : in Gdk_Event) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_get_height");
      Height : constant Gint16 := Internal (Event);
   begin
      if Height = Invalid_Gint16_Value then
         raise Invalid_Field;
      end if;
      return Height;
   end Get_Height;

   ----------------
   -- Get_X_Root --
   ----------------

   function Get_X_Root (Event : in Gdk_Event) return Gdouble is
      function Internal (Event : in Gdk_Event) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_get_x_root");
      X : constant Gdouble := Internal (Event);
   begin
      if X = Invalid_Gdouble_Value then
         raise Invalid_Field;
      end if;
      return X;
   end Get_X_Root;

   ----------------
   -- Get_Y_Root --
   ----------------

   function Get_Y_Root (Event : in Gdk_Event) return Gdouble is
      function Internal (Event : in Gdk_Event) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_get_y_root");
      Y : constant Gdouble := Internal (Event);
   begin
      if Y = Invalid_Gdouble_Value then
         raise Invalid_Field;
      end if;
      return Y;
   end Get_Y_Root;

   ----------------
   -- Get_Button --
   ----------------

   function Get_Button (Event : in Gdk_Event) return Guint is
      function Internal (Event : in Gdk_Event) return Guint;
      pragma Import (C, Internal, "ada_gdk_event_get_button");
      Button : constant Guint := Internal (Event);
   begin
      if Button = Invalid_Guint_Value then
         raise Invalid_Field;
      end if;
      return Button;
   end Get_Button;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Event : in Gdk_Event) return Gdk_Modifier_Type is
      function Internal (Event : in Gdk_Event) return Guint;
      pragma Import (C, Internal, "ada_gdk_event_get_state");
      State : constant Guint := Internal (Event);
   begin
      if State = Invalid_Guint_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Modifier_Type'Val (State);
   end Get_State;

   -------------------
   -- Get_Subwindow --
   -------------------

   function Get_Subwindow (Event : in Gdk_Event) return Gdk_Window is
      function Internal (Event : in Gdk_Event) return Gdk_Window;
      pragma Import (C, Internal, "ada_gdk_event_get_subwindow");
      Addr : constant Gdk_Window := Internal (Event);
   begin
      if Addr = Null_Window then
         raise Invalid_Field;
      end if;
      return Addr;
   end Get_Subwindow;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode (Event : in Gdk_Event) return Gdk_Crossing_Mode is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_mode");
      Mode : constant Gint := Internal (Event);
   begin
      if Mode = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Crossing_Mode'Val (Mode);
   end Get_Mode;

   ----------------
   -- Get_Detail --
   ----------------

   function Get_Detail (Event : in Gdk_Event) return Gdk_Notify_Type is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_detail");
      Detail : constant Gint := Internal (Event);
   begin
      if Detail = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Notify_Type'Val (Detail);
   end Get_Detail;

   ---------------
   -- Get_Focus --
   ---------------

   function Get_Focus (Event : in Gdk_Event) return Boolean is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_focus");
      Focus : constant Gint := Internal (Event);
   begin
      if Focus = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Boolean'Val (Focus);
   end Get_Focus;

   ------------------
   -- Get_Pressure --
   ------------------

   function Get_Pressure (Event : in Gdk_Event) return Gdouble is
      function Internal (Event : in Gdk_Event) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_get_pressure");
      Pressure : constant Gdouble := Internal (Event);
   begin
      if Pressure = Invalid_Gdouble_Value then
         raise Invalid_Field;
      end if;
      return Pressure;
   end Get_Pressure;

   ---------------
   -- Get_Xtilt --
   ---------------

   function Get_Xtilt (Event : in Gdk_Event) return Gdouble is
      function Internal (Event : in Gdk_Event) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_get_xtilt");
      Xtilt : constant Gdouble := Internal (Event);
   begin
      if Xtilt = Invalid_Gdouble_Value then
         raise Invalid_Field;
      end if;
      return Xtilt;
   end Get_Xtilt;

   ---------------
   -- Get_Ytilt --
   ---------------

   function Get_Ytilt (Event : in Gdk_Event) return Gdouble is
      function Internal (Event : in Gdk_Event) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_get_ytilt");
      Ytilt : constant Gdouble := Internal (Event);
   begin
      if Ytilt = Invalid_Gdouble_Value then
         raise Invalid_Field;
      end if;
      return Ytilt;
   end Get_Ytilt;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Event : in Gdk_Event) return Gdk_Input_Source is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_source");
      Source : constant Gint := Internal (Event);
   begin
      if Source = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Input_Source'Val (Source);
   end Get_Source;

   -------------------
   -- Get_Device_Id --
   -------------------

   function Get_Device_Id (Event : in Gdk_Event) return Gdk_Device_Id is
      function Internal (Event : in Gdk_Event) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_get_device_id");
      Device : constant Guint32 := Internal (Event);
   begin
      if Device = Invalid_Guint32_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Device_Id (Device);
   end Get_Device_Id;

   --------------
   -- Get_Area --
   --------------

   function Get_Area (Event : in Gdk_Event) return Rectangle.Gdk_Rectangle is
      procedure Internal (Event : Gdk_Event; Area : System.Address);
      pragma Import (C, Internal, "ada_gdk_event_get_area");
      Rec : aliased Rectangle.Gdk_Rectangle;
   begin
      Internal (Event, Rec'Address);
      if Rec.Width = Invalid_Guint16_Value then
         raise Invalid_Field;
      end if;
      return Rec;
   end Get_Area;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count (Event : in Gdk_Event) return Gint is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_count");
      Count : constant Gint := Internal (Event);
   begin
      if Count = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Count;
   end Get_Count;

   ------------
   -- Get_In --
   ------------

   function Get_In (Event : in Gdk_Event) return Boolean is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_in");
      Value : constant Gint := Internal (Event);
   begin
      if Value = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Boolean'Val (Value);
   end Get_In;

   -----------------
   -- Get_Is_Hint --
   -----------------

   function Get_Is_Hint (Event : in Gdk_Event) return Boolean is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_is_hint");
      Hint : constant Gint := Internal (Event);
   begin
      if Hint = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Boolean'Val (Hint);
   end Get_Is_Hint;

   -----------------
   -- Get_Key_Val --
   -----------------

   function Get_Key_Val (Event : in Gdk_Event) return Gdk_Key_Type is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_key_val");
      Key : constant Gint := Internal (Event);
   begin
      if Key = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Key_Type (Key);
   end Get_Key_Val;

   ----------------
   -- Get_String --
   ----------------

   function Get_String  (Event : in Gdk_Event) return String is
      function Internal (Event : in Gdk_Event)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gdk_event_get_string");
      use type Interfaces.C.Strings.chars_ptr;
      Str : constant Interfaces.C.Strings.chars_ptr
        := Internal (Event);
   begin
      if Str = Interfaces.C.Strings.Null_Ptr then
         raise Invalid_Field;
      end if;
      return Interfaces.C.Strings.Value (Str);
   end Get_String;

   --------------
   -- Get_Atom --
   --------------

   function Get_Atom (Event : in Gdk_Event) return Gdk_Atom is
      function Internal (Event : in Gdk_Event) return Gulong;
      pragma Import (C, Internal, "ada_gdk_event_get_atom");
      Atom : constant Gulong := Internal (Event);
   begin
      if Atom = Invalid_Gulong_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Atom (Atom);
   end Get_Atom;

   ------------------------
   -- Get_Property_State --
   ------------------------

   function Get_Property_State (Event : in Gdk_Event) return Guint is
      function Internal (Event : Gdk_Event) return Guint;
      pragma Import (C, Internal, "ada_gdk_event_get_property_state");
      State : constant Guint := Internal (Event);
   begin
      if State = Invalid_Guint_Value then
         raise Invalid_Field;
      end if;
      return State;
   end Get_Property_State;

   --------------------------
   -- Get_Visibility_State --
   --------------------------

   function Get_Visibility_State (Event : in Gdk_Event)
                                 return Gdk_Visibility_State
   is
      function Internal (Event : in Gdk_Event) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_get_visibility_state");
      State : constant Gint := Internal (Event);
   begin
      if State = Invalid_Gint_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Visibility_State'Val (State);
   end Get_Visibility_State;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection (Event : in Gdk_Event) return Gdk_Atom is
      function Internal (Event : in Gdk_Event) return Gulong;
      pragma Import (C, Internal, "ada_gdk_event_get_selection");
      Selection : constant Gulong := Internal (Event);
   begin
      if Selection = Invalid_Gulong_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Atom (Selection);
   end Get_Selection;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target (Event : in Gdk_Event) return Gdk_Atom is
      function Internal (Event : in Gdk_Event) return Gulong;
      pragma Import (C, Internal, "ada_gdk_event_get_target");
      Target : constant Gulong := Internal (Event);
   begin
      if Target = Invalid_Gulong_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Atom (Target);
   end Get_Target;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property (Event : in Gdk_Event) return Gdk_Atom is
      function Internal (Event : in Gdk_Event) return Gulong;
      pragma Import (C, Internal, "ada_gdk_event_get_property");
      Prop : constant Gulong := Internal (Event);
   begin
      if Prop = Invalid_Gulong_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Atom (Prop);
   end Get_Property;

   -------------------
   -- Get_Requestor --
   -------------------

   function Get_Requestor (Event : in Gdk_Event) return Guint32 is
      function Internal (Event : in Gdk_Event) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_get_requestor");
      Req : constant Guint32 := Internal (Event);
   begin
      if Req = Invalid_Guint32_Value then
         raise Invalid_Field;
      end if;
      return Req;
   end Get_Requestor;

   ----------------------
   -- Get_Message_Type --
   ----------------------

   function Get_Message_Type (Event : in Gdk_Event) return Gdk_Atom is
      function Internal (Event : in Gdk_Event) return Gulong;
      pragma Import (C, Internal, "ada_gdk_event_get_message_type");
      Message : constant Gulong := Internal (Event);
   begin
      if Message = Invalid_Gulong_Value then
         raise Invalid_Field;
      end if;
      return Gdk_Atom (Message);
   end Get_Message_Type;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Event : in Gdk_Event) return Gdk_Event_Client_Data is

      package IC  renames Interfaces.C;
      package ICS renames Interfaces.C.Strings;

      type Local_Short_Array is array (Natural range <>) of aliased IC.short;
      package Shorts_Ptr is new IC.Pointers
        (Index => Natural,
         Element => IC.short,
         Element_Array => Local_Short_Array,
         Default_Terminator => 0);

      type Local_Long_Array is array (Natural range <>) of aliased IC.long;
      package Longs_Ptr is new IC.Pointers
        (Index => Natural,
         Element => IC.long,
         Element_Array => Local_Long_Array,
         Default_Terminator => 0);

      function Format_Of (Event : in Gdk_Event)
                         return Gdk_Event_Client_Data_Format;
      pragma Import (C, Format_Of, "ada_gdk_event_client_get_data_format");

      function B_Of (Event : in Gdk_Event) return ICS.chars_ptr;
      pragma Import (C, B_Of, "ada_gdk_event_client_get_b");

      function S_Of (Event : in Gdk_Event) return Shorts_Ptr.Pointer;
      pragma Import (C, S_Of, "ada_gdk_event_client_get_s");

      function L_Of (Event : in Gdk_Event) return Longs_Ptr.Pointer;
      pragma Import (C, L_Of, "ada_gdk_event_client_get_l");

      Result : Gdk_Event_Client_Data
        (Format => Format_Of (Event));

   begin

      case Result.Format is

         when Char_Array =>
            Result.B :=
              IC.To_Ada (ICS.Value (Item => B_Of (Event),
                                    Length => Number_Of_Characters),
                         Trim_Nul => False);

         when Short_Array =>
            declare
               Tmp : constant Local_Short_Array :=
                 Shorts_Ptr.Value (Ref => S_Of (Event),
                                   Length => Number_Of_Shorts);
            begin
               for Index in 1 .. Number_Of_Shorts loop
                  Result.S (Index) := Gshort (Tmp (Index));
               end loop;
            end;

         when Long_Array =>
            declare
               Tmp : constant Local_Long_Array :=
                 Longs_Ptr.Value (Ref => L_Of (Event),
                                  Length => Number_Of_Longs);
            begin
               for Index in 1 .. Number_Of_Shorts loop
                  Result.L (Index) := Glong (Tmp (Index));
               end loop;
            end;

      end case;
      return Result;
   end Get_Data;

   ---------------------------
   --  Get_Graphics_Expose  --
   ---------------------------

   procedure Get_Graphics_Expose (Event  : out Gdk_Event_Expose;
                                  Window : in Gdk.Window.Gdk_Window)
   is
      function Internal (Window : in Gdk.Window.Gdk_Window)
                        return Gdk_Event;
      pragma Import (C, Internal, "gdk_event_get_graphics_expose");
   begin
      Event := Internal (Window);
   end Get_Graphics_Expose;

   ---------------
   -- Deep_Copy --
   ---------------

   procedure Deep_Copy (From : Gdk_Event; To : out Gdk_Event) is
      function Internal (Event : Gdk_Event) return Gdk_Event;
      pragma Import (C, Internal, "gdk_event_copy");
   begin
      To := Internal (From);
   end Deep_Copy;

   --------------------
   -- Events_Pending --
   --------------------

   function Events_Pending return Boolean is
      function Internal return Gboolean;
      pragma Import (C, Internal, "gdk_events_pending");
   begin
      return Boolean'Val (Internal);
   end Events_Pending;

   ---------
   -- Get --
   ---------

   procedure Get (Event : out Gdk_Event) is
      function Internal return Gdk_Event;
      pragma Import (C, Internal, "gdk_event_get");
   begin
      Event := Internal;
   end Get;

   ----------
   -- Peek --
   ----------

   procedure Peek (Event : out Gdk_Event) is
      function Internal return Gdk_Event;
      pragma Import (C, Internal, "gdk_event_peek");
   begin
      Event := Internal;
   end Peek;

   ---------------------
   -- Set_Show_Events --
   ---------------------

   procedure Set_Show_Events (Show_Events : in Boolean := True) is
      procedure Internal (Show_Events : in Gint);
      pragma Import (C, Internal, "gdk_set_show_events");
   begin
      Internal (To_Gint (Show_Events));
   end Set_Show_Events;

   ---------------------
   -- Get_Show_Events --
   ---------------------

   function Get_Show_Events return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_get_show_events");
   begin
      return Boolean'Val (Internal);
   end Get_Show_Events;

   -------------------------
   -- Send_Client_Message --
   -------------------------

   function Send_Client_Message (Event : in Gdk_Event;
                                 Xid   : in Guint32)
                                return Boolean
   is
      function Internal (Event  : in Gdk_Event;
                         Xid    : in Guint32) return Gboolean;
      pragma Import (C, Internal, "gdk_event_send_client_message");
   begin
      return Boolean'Val (Internal (Event, Xid));
   end Send_Client_Message;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Event      : out Gdk_Event;
                       Event_Type : in Types.Gdk_Event_Type;
                       Window     : in Gdk.Window.Gdk_Window)
   is
      function Internal (Event_Type : Gdk_Event_Type;
                         Win        : Gdk.Window.Gdk_Window)
                        return Gdk_Event;
      pragma Import (C, Internal, "ada_gdk_event_create");
   begin
      Event := Internal (Event_Type, Window);
   end Allocate;

   ----------
   -- Free --
   ----------

   procedure Free (Event : in out Gdk_Event) is
      procedure Internal (Event : Gdk_Event);
      pragma Import (C, Internal, "gdk_event_free");
   begin
      Internal (Event);
      Event := null;
   end Free;

   -----------
   -- Set_X --
   -----------

   procedure Set_X (Event : in Gdk_Event; X : Gdouble) is
      function Internal (Event : in Gdk_Event;  X : Gdouble)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_x");
   begin
      if Internal (Event, X) = 0 then
         raise Invalid_Field;
      end if;
   end Set_X;

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y (Event : in Gdk_Event; Y : Gdouble) is
      function Internal (Event : in Gdk_Event; Y : Gdouble)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_y");
   begin
      if Internal (Event, Y) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Y;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width (Event : in Gdk_Event; Width : Gint16) is
      function Internal (Event : in Gdk_Event;  Width : Gint16)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_width");
   begin
      if Internal (Event, Width) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Width;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height (Event : in Gdk_Event; Height : Gint16) is
      function Internal (Event : in Gdk_Event;  Height : Gint16)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_height");
   begin
      if Internal (Event, Height) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Height;

   ----------------
   -- Set_Button --
   ----------------

   procedure Set_Button (Event : in Gdk_Event; Button : Guint) is
      function Internal (Event : in Gdk_Event; Button : Guint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_button");
   begin
      if Internal (Event, Button) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Button;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State (Event : in Gdk_Event; State : Gdk_Modifier_Type) is
      function Internal (Event : in Gdk_Event; State : Guint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_state");
   begin
      if Internal (Event, Gdk_Modifier_Type'Pos (State)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_State;

   -------------------
   -- Set_Subwindow --
   -------------------

   procedure Set_Subwindow (Event : in Gdk_Event; Window : Gdk_Window) is
      function Internal (Event : in Gdk_Event;
                         Win   : Gdk_Window)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_subwindow");
   begin
      if Internal (Event, Window) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Subwindow;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (Event : in Gdk_Event; Mode : Gdk_Crossing_Mode) is
      function Internal (Event : in Gdk_Event; Mode : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_mode");
   begin
      if Internal (Event, Gdk_Crossing_Mode'Pos (Mode)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Mode;

   ----------------
   -- Set_Detail --
   ----------------

   procedure Set_Detail (Event : in Gdk_Event; Detail : Gdk_Notify_Type) is
      function Internal (Event : in Gdk_Event; Detail : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_detail");
   begin
      if Internal (Event, Gdk_Notify_Type'Pos (Detail)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Detail;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus (Event : in Gdk_Event; Has_Focus : Boolean) is
      function Internal (Event : in Gdk_Event; Focus : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_focus");
   begin
      if Internal (Event, Boolean'Pos (Has_Focus)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Focus;

   --------------
   -- Set_Area --
   --------------

   procedure Set_Area (Event : in Gdk_Event; Area : Gdk_Rectangle) is
      function Internal (Event : in Gdk_Event;
                         Rec   : in Gdk_Rectangle)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_area");
   begin
      if Internal (Event, Area) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Area;

   ------------
   -- Set_In --
   ------------

   procedure Set_In (Event : in Gdk_Event; Focus_In : Boolean) is
      function Internal (Event : in Gdk_Event; Focus_In : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_in");
   begin
      if Internal (Event, Boolean'Pos (Focus_In)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_In;

   -----------------
   -- Set_Is_Hint --
   -----------------

   procedure Set_Is_Hint (Event : in Gdk_Event; Is_Hint : Boolean) is
      function Internal (Event : in Gdk_Event; Is_Hint : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_is_hint");
   begin
      if Internal (Event, Boolean'Pos (Is_Hint)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Is_Hint;

   -----------------
   -- Set_Key_Val --
   -----------------

   procedure Set_Key_Val (Event : in Gdk_Event; Key : Gdk_Key_Type) is
      function Internal (Event : in Gdk_Event; Key : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_key_val");
   begin
      if Internal (Event, Gdk_Key_Type'Pos (Key)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Key_Val;

   --------------
   -- Set_Atom --
   --------------

   procedure Set_Atom (Event : in Gdk_Event; Atom : Gdk_Atom) is
      function Internal (Event : in Gdk_Event; Atom : Gulong)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_atom");
   begin
      if Internal (Event, Gulong (Atom)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Atom;

   ------------------------
   -- Set_Property_State --
   ------------------------

   procedure Set_Property_State (Event : in Gdk_Event; State : Guint) is
      function Internal (Event : Gdk_Event; State : Guint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_property_state");
   begin
      if Internal (Event, State) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Property_State;

   --------------------------
   -- Set_Visibility_State --
   --------------------------

   procedure Set_Visibility_State (Event : in Gdk_Event;
                                   State : Gdk_Visibility_State)
   is
      function Internal (Event : in Gdk_Event; State : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_visibility_state");
   begin
      if Internal (Event, Gdk_Visibility_State'Pos (State)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Visibility_State;

   -------------------
   -- Set_Selection --
   -------------------

   procedure Set_Selection (Event : in Gdk_Event; Selection : Gdk_Atom) is
      function Internal (Event : in Gdk_Event; Selection : Gulong)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_selection");
   begin
      if Internal (Event, Gulong (Selection)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Selection;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target (Event : in Gdk_Event; Target : Gdk_Atom) is
      function Internal (Event : in Gdk_Event; Target : Gulong)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_target");
   begin
      if Internal (Event, Gulong (Target)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Target;

   --------------
   -- Set_Time --
   --------------

   procedure Set_Time (Event : in Gdk_Event; Time : Guint32) is
      function Internal (Event : in Gdk_Event; Time : Guint32)
         return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_time");
   begin
      if Internal (Event, Time) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Time;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property (Event : in Gdk_Event; Property : Gdk_Atom) is
      function Internal (Event : in Gdk_Event; Property : Gulong)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_property");
   begin
      if Internal (Event, Gulong (Property)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Property;

   -------------------
   -- Set_Requestor --
   -------------------

   procedure Set_Requestor (Event : in Gdk_Event; Requestor : Guint32) is
      function Internal (Event : in Gdk_Event; Requestor : Guint32)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_requestor");
   begin
      if Internal (Event, Requestor) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Requestor;

   ----------------------
   -- Set_Message_Type --
   ----------------------

   procedure Set_Message_Type (Event : in Gdk_Event; Typ : Gdk_Atom) is
      function Internal (Event : in Gdk_Event; Typ : Gulong)
                        return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_message_type");
   begin
      if Internal (Event, Gulong (Typ)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_Message_Type;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (Event : in Gdk_Event; Str : String) is
      function Internal (Event : in Gdk_Event; Str : chars_ptr) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_set_string");
   begin
      if Internal (Event, New_String (Str)) = 0 then
         raise Invalid_Field;
      end if;
   end Set_String;

   ------------------
   -- From_Address --
   ------------------

   function From_Address (C : System.Address) return Gdk_Event is
   begin
      return Gdk_Event (Gdk.Convert (C));
   end From_Address;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (C : Gdk_Event) return System.Address is
   begin
      return Gdk.Convert (Gdk.C_Proxy (C));
   end To_Address;

   ----------------
   -- Is_Created --
   ----------------

   function Is_Created (E : Gdk_Event) return Boolean is
   begin
      return E /= null;
   end Is_Created;

   -----------------------
   -- Event_Handler_Set --
   -----------------------

   procedure Event_Handler_Set
     (Func : Event_Handler_Func; Data : System.Address)
   is
      procedure Internal (Func : Event_Handler_Func;
                          Data : System.Address;
                          Destroy_Notify : System.Address);
      pragma Import (C, Internal, "gdk_event_handler_set");
   begin
      Internal (Func, Data, System.Null_Address);
   end Event_Handler_Set;

end Gdk.Event;
