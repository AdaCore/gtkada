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

--  An event related to a key-based device.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Enums;     use Gdk.Enums;
with Gdk.Key_Match; use Gdk.Key_Match;
with Glib;          use Glib;

package Gdk.Event.Key_Event is

   type Gdk_Key_Event is new Gdk_Event with null record;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_key_event_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Consumed_Modifiers
      (Self : Gdk.Event.Gdk_Event) return Gdk.Enums.Gdk_Modifier_Type;
   --  Extracts the consumed modifiers from a key event.
   --  @return the consumed modifiers or Event

   function Get_Keycode (Self : Gdk.Event.Gdk_Event) return Guint;
   --  Extracts the keycode from a key event.
   --  @return the keycode of Event

   function Get_Keyval (Self : Gdk.Event.Gdk_Event) return Guint;
   --  Extracts the keyval from a key event.
   --  @return the keyval of Event

   function Get_Layout (Self : Gdk.Event.Gdk_Event) return Guint;
   --  Extracts the layout from a key event.
   --  @return the layout of Event

   function Get_Level (Self : Gdk.Event.Gdk_Event) return Guint;
   --  Extracts the shift level from a key event.
   --  @return the shift level of Event

   function Get_Match
      (Self      : Gdk.Event.Gdk_Event;
       Keyval    : out Guint;
       Modifiers : out Gdk.Enums.Gdk_Modifier_Type) return Boolean;
   --  Gets a keyval and modifier combination that will match the event.
   --  See [methodGdk.KeyEvent.matches].
   --  @param Keyval return location for a keyval
   --  @param Modifiers return location for modifiers
   --  @return True on success

   function Is_Modifier (Self : Gdk.Event.Gdk_Event) return Boolean;
   --  Extracts whether the key event is for a modifier key.
   --  @return True if the Event is for a modifier key

   function Matches
      (Self      : Gdk.Event.Gdk_Event;
       Keyval    : Guint;
       Modifiers : Gdk.Enums.Gdk_Modifier_Type)
       return Gdk.Key_Match.Gdk_Key_Match;
   --  Matches a key event against a keyval and modifiers.
   --  This is typically used to trigger keyboard shortcuts such as Ctrl-C.
   --  Partial matches are possible where the combination matches if the
   --  currently active group is ignored.
   --  Note that we ignore Caps Lock for matching.
   --  @param Keyval the keyval to match
   --  @param Modifiers the modifiers to match
   --  @return a `GdkKeyMatch` value describing whether Event matches

end Gdk.Event.Key_Event;
