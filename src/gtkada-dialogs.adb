------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2019, AdaCore                     --
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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Gdk.Event;         use Gdk.Event;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gdk.Window;        use Gdk.Window;

with Glib;              use Glib;

with Gtk.Box;           use Gtk.Box;
with Gtk.Image;         use Gtk.Image;
with Gtk.Label;         use Gtk.Label;
with Gtk.Widget;        use Gtk.Widget;

with Gtkada.Intl;       use Gtkada.Intl;
with Gtkada.Pixmaps;    use Gtkada.Pixmaps;

package body Gtkada.Dialogs is

   subtype String_6 is String (1 .. 6);
   type String_Const_Ptr is access constant String;

   Dialog_Button_String : constant array (Button_Range) of String_6 :=
     ("Yes   ",
      "No    ",
      "All   ",
      "OK    ",
      "Cancel",
      "Abort ",
      "Retry ",
      "Ignore",
      "Help  ");

   Yes    : aliased constant String := "gtk-yes";
   No     : aliased constant String := "gtk-no";
   Ok     : aliased constant String := "gtk-ok";
   Cancel : aliased constant String := "gtk-cancel";
   Help   : aliased constant String := "gtk-help";

   --  ??? We used to reference Gtk.Stock.Stock_* instead, but there is
   --  apparently a bug in GCC when generating 'Access to these variables.

   Dialog_Button_Stock : constant array (Button_Range) of String_Const_Ptr :=
     (Yes'Access,
      No'Access,
      null,
      Ok'Access,
      Cancel'Access,
      null,
      null,
      null,
      Help'Access);

   --------------------
   -- Message_Dialog --
   --------------------

   function Message_Dialog
     (Msg            : UTF8_String;
      Dialog_Type    : Message_Dialog_Type := Information;
      Buttons        : Message_Dialog_Buttons := Button_OK or Button_Help;
      Default_Button : Message_Dialog_Buttons := Button_OK;
      Help_Msg       : UTF8_String := "";
      Title          : UTF8_String := "";
      Justification  : Gtk_Justification := Justify_Center;
      Parent         : Gtk.Window.Gtk_Window := null;
      Icon_Name      : String := "")
      return Message_Dialog_Buttons
   is
      Dialog   : constant Gtk_Dialog := Create_Gtk_Dialog
        (Msg           => Msg,
         Dialog_Type   => Dialog_Type,
         Title         => Title,
         Justification => Justification,
         Parent        => Parent,
         Icon_Name     => Icon_Name);
      Button   : Gtk_Widget;
      Result   : Message_Dialog_Buttons;
      Response : Gtk_Response_Type;

   begin
      if Parent = null
         or else not Get_Realized (Parent)
      then
         Set_Position (Dialog, Win_Pos_Mouse);

      elsif (Get_State (Get_Window (Parent))
             and Window_State_Iconified) /= 0
      then
         --  If the parent is not visible, do not center the dialog on it,
         --  let the window manager decide the position instead.

         Set_Position (Dialog, Win_Pos_None);
      end if;

      for J in Button_Range loop
         if (Buttons and 2 ** Integer (J)) /= 0 then
            --  Use Gtk_Response_Cancel for the Cancel or No buttons, so that
            --  Esc can be used to close the dialog. Likewise, if none of these
            --  two was provided, simply use Cancel for the OK button
            if J = 4
              or else (J = 1 and then (Buttons and Button_Cancel) = 0)
              or else (J = 3 and then (Buttons and Button_Cancel) = 0
                       and then (Buttons and Button_No) = 0)
            then
               Response := Gtk_Response_Cancel;
            else
               Response := Gtk_Response_Type (2 ** Integer (J));
            end if;

            if Dialog_Button_Stock (J) = null then
               Button := Add_Button
                 (Dialog,
                  Text => Trim (-Dialog_Button_String (J), Right),
                  Response_Id => Response);
            else
               Button := Add_Button
                 (Dialog,
                  Text => Dialog_Button_Stock (J).all,
                  Response_Id => Response);
            end if;

            if Default_Button = 2 ** Integer (J) then
               Grab_Default (Button);
            end if;
         end if;
      end loop;

      Show_All (Dialog);

      loop
         Response := Run (Dialog);

         if Response = Gtk_Response_Delete_Event then
            Destroy (Dialog);
            return Button_None;

         elsif Response = Gtk_Response_Cancel then
            Destroy (Dialog);
            if (Buttons and Button_Cancel) /= 0 then
               return Button_Cancel;
            elsif (Buttons and Button_No) /= 0 then
               return Button_No;
            else
               return Button_OK;
            end if;

         elsif Response >= 0 then
            Result := Message_Dialog_Buttons (Response);

            case Result is
               when Button_Yes
                    | Button_No
                    | Button_All
                    | Button_OK
                    | Button_Cancel
                    | Button_Abort
                    | Button_Retry
                    | Button_Ignore =>

                  Destroy (Dialog);
                  return Result;

               when Button_Help =>
                  if Help_Msg /= "" then
                     Result := Message_Dialog
                       (Help_Msg, Buttons => Button_OK, Title => -"Help");
                  else
                     Result := Message_Dialog
                       (-"No help available",
                        Buttons => Button_OK, Title => -"Help");
                  end if;

               when others =>
                  null;
            end case;
         end if;
      end loop;
   end Message_Dialog;

   -----------------------
   -- Create_Gtk_Dialog --
   -----------------------

   function Create_Gtk_Dialog
     (Msg           : UTF8_String;
      Dialog_Type   : Message_Dialog_Type := Information;
      Title         : UTF8_String := "";
      Justification : Gtk_Justification := Justify_Center;
      Parent        : Gtk.Window.Gtk_Window := null;
      Icon_Name     : String := "")
      return Gtk.Dialog.Gtk_Dialog
   is
      Dialog      : Gtk_Dialog;
      Label       : Gtk_Label;
      Box         : Gtk_Box;
      Icon_Img    : Gtk_Image;

      procedure Create_Dialog_Icon;

      ------------------------
      -- Create_Dialog_Icon --
      ------------------------

      procedure Create_Dialog_Icon is
         Pixmap   : Gdk_Pixbuf;
      begin
         if Icon_Name /= "" then
            Gtk_New_From_Icon_Name
              (Icon_Img,
               Icon_Name => Icon_Name,
               Size      => Icon_Size_Dialog);
         else
            Pixmap :=
              (case Dialog_Type is
                  when Warning      =>
                    Gdk_New_From_Xpm_Data (Warning_Xpm),
                  when Error        =>
                    Gdk_New_From_Xpm_Data (Error_Xpm),
                  when Information  =>
                    Gdk_New_From_Xpm_Data (Information_Xpm),
                  when Confirmation =>
                    Gdk_New_From_Xpm_Data (Confirmation_Xpm),
                  when Custom       => null);
            Gtk_New (Icon_Img, Pixmap);

            --  Unref the pixmap since the Gtk_Image now holds its
            --  own reference to it.

            Pixmap.Unref;
         end if;
      end Create_Dialog_Icon;

   begin
      Gtk_New
        (Dialog,
         Title  => Title,
         Parent => Parent,
         Flags  => Modal or Destroy_With_Parent);

      --  Realize it so that we force the creation of its Gdk_Window.
      --  This is needed below to create a pixmap.

      Realize (Dialog);

      case Dialog_Type is
         when Warning =>
            if Title = "" then
               Set_Title (Dialog, -"Warning");
            end if;

         when Error =>
            if Title = "" then
               Set_Title (Dialog, -"Error");
            end if;

         when Information =>
            if Title = "" then
               Set_Title (Dialog, -"Information");
            end if;

         when Confirmation =>
            if Title = "" then
               Set_Title (Dialog, -"Confirmation");
            end if;

         when Custom =>
            null;
      end case;

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Content_Area (Dialog), Box, Padding => 10);

      Create_Dialog_Icon;

      if Icon_Img /= null then
         Pack_Start (Box, Icon_Img, Padding => 10);
      end if;

      Gtk_New (Label, Msg);
      Set_Selectable (Label, True);
      Set_Justify (Label, Justification);
      Pack_Start (Box, Label, Padding => 10);

      return Dialog;
   end Create_Gtk_Dialog;

end Gtkada.Dialogs;
