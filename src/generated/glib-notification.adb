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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Glib.Notification is

   package Type_Conversion_Gnotification is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gnotification_Record);
   pragma Unreferenced (Type_Conversion_Gnotification);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gnotification; Title : UTF8_String) is
   begin
      Self := new Gnotification_Record;
      Glib.Notification.Initialize (Self, Title);
   end G_New;

   -----------------------
   -- Gnotification_New --
   -----------------------

   function Gnotification_New (Title : UTF8_String) return Gnotification is
      Self : constant Gnotification := new Gnotification_Record;
   begin
      Glib.Notification.Initialize (Self, Title);
      return Self;
   end Gnotification_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gnotification_Record'Class;
       Title : UTF8_String)
   is
      function Internal
         (Title : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_notification_new");
      Tmp_Title  : Gtkada.Types.Chars_Ptr := New_String (Title);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Title);
         Free (Tmp_Title);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   ----------------
   -- Add_Button --
   ----------------

   procedure Add_Button
      (Self            : not null access Gnotification_Record;
       Label           : UTF8_String;
       Detailed_Action : UTF8_String)
   is
      procedure Internal
         (Self            : System.Address;
          Label           : Gtkada.Types.Chars_Ptr;
          Detailed_Action : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_notification_add_button");
      Tmp_Label           : Gtkada.Types.Chars_Ptr := New_String (Label);
      Tmp_Detailed_Action : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action);
   begin
      Internal (Get_Object (Self), Tmp_Label, Tmp_Detailed_Action);
      Free (Tmp_Detailed_Action);
      Free (Tmp_Label);
   end Add_Button;

   ----------------------------------
   -- Add_Button_With_Target_Value --
   ----------------------------------

   procedure Add_Button_With_Target_Value
      (Self   : not null access Gnotification_Record;
       Label  : UTF8_String;
       Action : UTF8_String;
       Target : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self   : System.Address;
          Label  : Gtkada.Types.Chars_Ptr;
          Action : Gtkada.Types.Chars_Ptr;
          Target : System.Address);
      pragma Import (C, Internal, "g_notification_add_button_with_target_value");
      Tmp_Label  : Gtkada.Types.Chars_Ptr := New_String (Label);
      Tmp_Action : Gtkada.Types.Chars_Ptr := New_String (Action);
   begin
      Internal (Get_Object (Self), Tmp_Label, Tmp_Action, Get_Object (Target));
      Free (Tmp_Action);
      Free (Tmp_Label);
   end Add_Button_With_Target_Value;

   --------------
   -- Set_Body --
   --------------

   procedure Set_Body
      (Self     : not null access Gnotification_Record;
       The_Body : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          The_Body : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_notification_set_body");
      Tmp_The_Body : Gtkada.Types.Chars_Ptr;
   begin
      if The_Body = "" then
         Tmp_The_Body := Gtkada.Types.Null_Ptr;
      else
         Tmp_The_Body := New_String (The_Body);
      end if;
      Internal (Get_Object (Self), Tmp_The_Body);
      Free (Tmp_The_Body);
   end Set_Body;

   ------------------------
   -- Set_Default_Action --
   ------------------------

   procedure Set_Default_Action
      (Self            : not null access Gnotification_Record;
       Detailed_Action : UTF8_String)
   is
      procedure Internal
         (Self            : System.Address;
          Detailed_Action : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_notification_set_default_action");
      Tmp_Detailed_Action : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action);
   begin
      Internal (Get_Object (Self), Tmp_Detailed_Action);
      Free (Tmp_Detailed_Action);
   end Set_Default_Action;

   -----------------------------------------
   -- Set_Default_Action_And_Target_Value --
   -----------------------------------------

   procedure Set_Default_Action_And_Target_Value
      (Self   : not null access Gnotification_Record;
       Action : UTF8_String;
       Target : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self   : System.Address;
          Action : Gtkada.Types.Chars_Ptr;
          Target : System.Address);
      pragma Import (C, Internal, "g_notification_set_default_action_and_target_value");
      Tmp_Action : Gtkada.Types.Chars_Ptr := New_String (Action);
   begin
      Internal (Get_Object (Self), Tmp_Action, Get_Object (Target));
      Free (Tmp_Action);
   end Set_Default_Action_And_Target_Value;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
      (Self : not null access Gnotification_Record;
       Icon : Glib.G_Icon.G_Icon)
   is
      procedure Internal (Self : System.Address; Icon : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "g_notification_set_icon");
   begin
      Internal (Get_Object (Self), Icon);
   end Set_Icon;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
      (Self     : not null access Gnotification_Record;
       Priority : GNotification_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Priority : GNotification_Priority);
      pragma Import (C, Internal, "g_notification_set_priority");
   begin
      Internal (Get_Object (Self), Priority);
   end Set_Priority;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gnotification_Record;
       Title : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_notification_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   ----------------
   -- Set_Urgent --
   ----------------

   procedure Set_Urgent
      (Self   : not null access Gnotification_Record;
       Urgent : Boolean)
   is
      procedure Internal (Self : System.Address; Urgent : Glib.Gboolean);
      pragma Import (C, Internal, "g_notification_set_urgent");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Urgent));
   end Set_Urgent;

end Glib.Notification;
