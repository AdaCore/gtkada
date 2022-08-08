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

package body Gtk.Revealer is

   package Type_Conversion_Gtk_Revealer is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Revealer_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Revealer);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Revealer) is
   begin
      Self := new Gtk_Revealer_Record;
      Gtk.Revealer.Initialize (Self);
   end Gtk_New;

   ----------------------
   -- Gtk_Revealer_New --
   ----------------------

   function Gtk_Revealer_New return Gtk_Revealer is
      Self : constant Gtk_Revealer := new Gtk_Revealer_Record;
   begin
      Gtk.Revealer.Initialize (Self);
      return Self;
   end Gtk_Revealer_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Revealer_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_revealer_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------------
   -- Get_Child_Revealed --
   ------------------------

   function Get_Child_Revealed
      (Self : not null access Gtk_Revealer_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_revealer_get_child_revealed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Child_Revealed;

   ----------------------
   -- Get_Reveal_Child --
   ----------------------

   function Get_Reveal_Child
      (Self : not null access Gtk_Revealer_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_revealer_get_reveal_child");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Reveal_Child;

   -----------------------------
   -- Get_Transition_Duration --
   -----------------------------

   function Get_Transition_Duration
      (Self : not null access Gtk_Revealer_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_revealer_get_transition_duration");
   begin
      return Internal (Get_Object (Self));
   end Get_Transition_Duration;

   -------------------------
   -- Get_Transition_Type --
   -------------------------

   function Get_Transition_Type
      (Self : not null access Gtk_Revealer_Record)
       return Gtk_Revealer_Transition_Type
   is
      function Internal
         (Self : System.Address) return Gtk_Revealer_Transition_Type;
      pragma Import (C, Internal, "gtk_revealer_get_transition_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Transition_Type;

   ----------------------
   -- Set_Reveal_Child --
   ----------------------

   procedure Set_Reveal_Child
      (Self         : not null access Gtk_Revealer_Record;
       Reveal_Child : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Reveal_Child : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_revealer_set_reveal_child");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Reveal_Child));
   end Set_Reveal_Child;

   -----------------------------
   -- Set_Transition_Duration --
   -----------------------------

   procedure Set_Transition_Duration
      (Self     : not null access Gtk_Revealer_Record;
       Duration : Guint)
   is
      procedure Internal (Self : System.Address; Duration : Guint);
      pragma Import (C, Internal, "gtk_revealer_set_transition_duration");
   begin
      Internal (Get_Object (Self), Duration);
   end Set_Transition_Duration;

   -------------------------
   -- Set_Transition_Type --
   -------------------------

   procedure Set_Transition_Type
      (Self       : not null access Gtk_Revealer_Record;
       Transition : Gtk_Revealer_Transition_Type)
   is
      procedure Internal
         (Self       : System.Address;
          Transition : Gtk_Revealer_Transition_Type);
      pragma Import (C, Internal, "gtk_revealer_set_transition_type");
   begin
      Internal (Get_Object (Self), Transition);
   end Set_Transition_Type;

end Gtk.Revealer;
