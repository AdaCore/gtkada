-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gtk; use Gtk;
with System;

package body Gtk.Text_Tag is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Text_Tag;
      Name   : String := "") is
   begin
      Widget := new Gtk_Text_Tag_Record;
      Initialize (Widget, Name);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Text_Tag_Record'Class;
      Name   : String := "")
   is
      function Internal (Name : String) return System.Address;
      pragma Import (C, Internal, "gtk_text_tag_new");
      function Internal_No_Name (Dummy : System.Address) return System.Address;
      pragma Import (C, Internal_No_Name, "gtk_text_tag_new");
      --  Same as Internal except that we need to pass a null address for
      --  the name.
   begin
      if Name = "" then
         Set_Object (Widget, Internal_No_Name (System.Null_Address));
      else
         Set_Object (Widget, Internal (Name & ASCII.NUL));
      end if;
      Initialize_User_Data (Widget);
   end Initialize;

   -----------
   -- Event --
   -----------

   --  function Event
   --    (Tag          : access Gtk_Text_Tag_Record;
   --     Event_Object : access Gtk.Object.Gtk_Object_Record'Class;
   --     Event        : Gdk.Event.Gdk_Event;
   --     Iter         : access Gtk.Text_Iter.Gtk_Text_Iter_Record'Class)
   --     return Gint
   --  is
   --     function Internal
   --       (Tag          : System.Address;
   --        Event_Object : System.Address;
   --        Event        : GdkEvent;
   --        Iter         : System.Address)
   --        return Gint;
   --     pragma Import (C, Internal, "gtk_text_tag_event");
   --  begin
   --     return Internal (Get_Object (Tag),
   --                      Get_Object (Event_Object),
   --                      Event,
   --                      Get_Object (Iter));
   --  end Event;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (Tag    : access Gtk_Text_Tag_Record)
                          return Gint
   is
      function Internal (Tag    : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_tag_get_priority");
   begin
      return Internal (Get_Object (Tag));
   end Get_Priority;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (Tag      : access Gtk_Text_Tag_Record;
      Priority : Gint)
   is
      procedure Internal
        (Tag      : System.Address;
         Priority : Gint);
      pragma Import (C, Internal, "gtk_text_tag_set_priority");
   begin
      Internal (Get_Object (Tag), Priority);
   end Set_Priority;

   ------------------------
   --  Set_Property_Name --
   ------------------------

   procedure Set_Property_Name
     (Tag  : access Gtk_Text_Tag_Record;
      Name : String)
   is
      procedure Internal
        (Object : System.Address;
         Property_Name : String;
         Value : String;
         No_More_Args : System.Address := System.Null_Address);
      pragma Import (C, Internal, "g_object_set");
   begin
      Internal (Get_Object (Tag), "name" & ASCII.NUL, Name & ASCII.NUL);
   end Set_Property_Name;

   ------------------------------
   --  Set_Property_Background --
   ------------------------------

   procedure Set_Property_Background
     (Tag        : access Gtk_Text_Tag_Record;
      Background : String)
   is
      procedure Internal
        (Object : System.Address;
         Property_Name : String;
         Value : String;
         No_More_Args : System.Address := System.Null_Address);
      pragma Import (C, Internal, "g_object_set");
   begin
      Internal
        (Get_Object (Tag), "background" & ASCII.NUL, Background & ASCII.NUL);
   end Set_Property_Background;

   ---------------------------
   -- Set_Property_Editable --
   ---------------------------

   procedure Set_Property_Editable
     (Tag       : access Gtk_Text_Tag_Record;
      Editable  : Boolean := True)
   is
      procedure Internal
        (Object : System.Address;
         Property_Name : String;
         Value : Boolean;
         No_More_Args : System.Address := System.Null_Address);
      pragma Import (C, Internal, "g_object_set");
   begin
      Internal (Get_Object (Tag), "editable" & ASCII.NUL, Editable);
   end Set_Property_Editable;

   ------------------------------
   --  Set_Property_Foreground --
   ------------------------------

   procedure Set_Property_Foreground
     (Tag        : access Gtk_Text_Tag_Record;
      Foreground : String)
   is
      procedure Internal
        (Object : System.Address;
         Property_Name : String;
         Value : String;
         No_More_Args : System.Address := System.Null_Address);
      pragma Import (C, Internal, "g_object_set");
   begin
      Internal
         (Get_Object (Tag), "foreground" & ASCII.NUL, Foreground & ASCII.NUL);
   end Set_Property_Foreground;

   ----------------------------
   -- Set_Property_Invisible --
   ----------------------------

   procedure Set_Property_Invisible
     (Tag        : access Gtk_Text_Tag_Record;
      Invisible  : Boolean := True)
   is
      procedure Internal
        (Object : System.Address;
         Property_Name : String;
         Value : Boolean;
         No_More_Args : System.Address := System.Null_Address);
      pragma Import (C, Internal, "g_object_set");
   begin
      Internal (Get_Object (Tag), "invisible" & ASCII.NUL, Invisible);
   end Set_Property_Invisible;

   ---------------------------------
   --  Set_Property_Strikethrough --
   ---------------------------------

   procedure Set_Property_Strikethrough
     (Tag           : access Gtk_Text_Tag_Record;
      Strikethrough : Boolean := True)
   is
      procedure Internal
        (Object : System.Address;
         Property_Name : String;
         Value : Gboolean;
         No_More_Args : System.Address := System.Null_Address);
      pragma Import (C, Internal, "g_object_set");
   begin
      Internal
        (Get_Object (Tag),
         "strikethrough" & ASCII.NUL,
         To_Gboolean (Strikethrough));
   end Set_Property_Strikethrough;

end Gtk.Text_Tag;
